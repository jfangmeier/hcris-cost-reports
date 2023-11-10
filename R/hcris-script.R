library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
library(here)
library(readxl)
library(lubridate)
library(janitor)
library(httr)
library(rvest)
library(rmarkdown)
library(knitr)
library(fs)

#Pull the current release dates of the cost reports
release_html <-
  read_html(
    "https://www.cms.gov/data-research/statistics-trends-and-reports/cost-reports"
  )

release_years <- 
  release_html %>%
  html_node(xpath = "//table") %>%
  html_table() %>%
  rename(
    system = 1,
    form = 2,
    fy_beg = 3,
    fy_end = 4
  ) %>% 
  filter(system == "Hospitals - 2010")

release_txt <- 
  release_html %>% 
  html_elements('h2') %>% 
  html_text() %>% 
  str_subset(pattern = "released") %>% 
  str_extract_all(., pattern = "[:digit:]{1,2}\\/[:digit:]{1,2}\\/[:digit:]{4}")

release_df <- 
  release_years %>% 
  mutate(
    update_date = mdy(pluck(release_txt, 1, 1)),
    release_date = mdy(pluck(release_txt, 1, 2)))

#Select fiscal year range for HCRIS download
hcris_years <-
  c(
    pull(release_df, fy_beg):
    pull(release_df, fy_end)
    )

##1. Download and uncompress HCRIS cost report files
hcris_path <- tempdir()

hcris_download <- function(x) {
  print(x)
  RETRY(
    verb = "GET",
    url = paste0("http://downloads.cms.gov/Files/hcris/HOSP10FY", x, ".zip"),
    times = 5,
    pause_base = 1,
    pause_cap = 30,
    quiet = FALSE,
    terminate_on = NULL,
    write_disk(file.path(
      hcris_path, paste0("HOSP10FY", x, ".zip")
    ), overwrite = TRUE),
    progress()
  )
  unzip(
    zipfile = file.path(hcris_path, paste0("HOSP10FY", x, ".zip")),
    exdir = hcris_path,
    overwrite = TRUE
  )
}

walk(hcris_years, hcris_download)

##2. Load worksheet lookup, general information, and taxonomy tables
lookup <- read_excel(here("data-raw", "hcris_lookup.xlsx")) %>% 
  filter(enabled == 1)

hcris_num_vars <- lookup %>% 
  filter(num_var == 1) %>% 
  pull(var)

hcris_synth_vars <- lookup %>% 
  filter(num_var == 1 & characteristic_val == 0) %>% 
  pull(var)

gen_info <-
  # read_csv(url('https://data.cms.gov/provider-data/sites/default/files/resources/092256becd267d9eeccf73bf7d16c46b_1689206722/Hospital_General_Information.csv')) %>% 
  read_csv(here("data-raw", "hospital-general-information.csv")) %>%
  mutate(
    prvdr_num = str_pad(
      as.character(prvdr_num),
      width = 6,
      side = "left",
      pad = "0"
    ),
    zip_code = str_pad(
      as.character(zip_code),
      width = 5,
      side = "left",
      pad = "0"
    )
  )

hosp_compendium <- 
  read_csv(url("https://www.ahrq.gov/sites/default/files/wysiwyg/chsp/compendium/compendium_system_hospital_linkage_file.csv")) %>% 
  select(
    prvdr_num = ccn,
    health_sys_name) %>% 
  filter(!is.na(prvdr_num))

facility_type <- tribble(
  ~ type, ~ class,
  "Short Term Acute Care", 1:899,
  "Childrens", 3300:3399,
  "Critical Access", 1300:1399,
  "Long Term", 2000:2299,
  "Psychiatric", 4000:4499,
  "Rehabilitation", 3025:3099
) %>%
  unnest_longer(class) %>%
  mutate(class = str_pad(
    as.character(class),
    width = 4,
    pad = "0",
    side = "left"
  ))

##3. Extract variables, remove duplicate reports, and generate synthetic hospital-calendar year reports
#Extract select variables and append cost report CSVs
hcris_process_fcn <- function(x) {
  print(x)
  read_csv(
    file.path(hcris_path, paste0("HOSP10_", x, "_RPT.CSV")),
    col_names = FALSE,
    col_types = cols(.default = "c")
  ) %>%
    rename(
      rpt_rec_num = X1,
      control = X2,
      prvdr_num = X3,
      rptstat = X5,
      fy_beg = X6,
      fy_end = X7,
      procdt = X8,
      initrpt = X9,
      finrpt = X10,
      util_level = X15,
      rcptdt = X18
    ) %>%
    mutate(across(c(rpt_rec_num, control, rptstat), as.numeric)) %>%
    mutate(across(c(fy_beg, fy_end, procdt, rcptdt), mdy)) %>%
    select(-starts_with("X")) %>%
    inner_join(
      read_csv(file.path(
        hcris_path, paste0("HOSP10_", x, "_ALPHA.CSV")
      ), col_names = FALSE) %>%
        rename(
          rpt_rec_num = X1,
          wksht_cd = X2,
          line_num = X3,
          clmn_num = X4,
          item = X5
        ) %>%
        inner_join(lookup, by = c("wksht_cd", "clmn_num", "line_num")) %>%
        select(rpt_rec_num, var, item) %>%
        spread(var, item),
      by = "rpt_rec_num"
    ) %>%
    inner_join(
      read_csv(file.path(
        hcris_path, paste0("HOSP10_", x, "_NMRC.CSV")
      ), col_names = FALSE) %>%
        rename(
          rpt_rec_num = X1,
          wksht_cd = X2,
          line_num = X3,
          clmn_num = X4,
          item = X5
        ) %>%
        inner_join(lookup, by = c("wksht_cd", "clmn_num", "line_num")) %>%
        select(rpt_rec_num, var, item) %>%
        spread(var, item),
      by = "rpt_rec_num"
    ) %>%
    mutate(fy = x)
}

#Append fiscal year cost reports and remove duplicate reports for the same facility in the same fiscal year
hcris_raw <- map_dfr(hcris_years, hcris_process_fcn)

hcris_df <- 
  hcris_raw %>% 
  arrange(prvdr_num, fy) %>% 
  select(prvdr_num, everything()) %>% 
  mutate(
    fy_beg_y = year(fy_beg),
    fy_end_y = year(fy_end)) %>% 
  group_by(prvdr_num, fy) %>% 
  mutate(
    dup = n(),
    maxdate = max(procdt)) %>% 
  ungroup() %>% 
  filter(dup == 1 | (dup > 1 & maxdate == procdt)) %>% #Pick the observation that has the latest HCRIS processing date
  select(-dup, -maxdate) %>% 
  group_by(prvdr_num, fy) %>% 
  mutate(
    dup = n(),
    minstat = min(rptstat)) %>% 
  ungroup() %>% 
  filter(dup == 1 | (dup >1 & rptstat == minstat)) %>% #Pick the observations that are audited over those that are not (lower is better)
  select(-dup, -minstat) %>% 
  group_by(prvdr_num, fy) %>% 
  mutate(
    dup = n(),
    period = case_when(
      fy_end-fy_beg < 364 ~ 0,
      fy_end-fy_beg < 366 ~ 1,
      TRUE ~ 0)) %>% 
  ungroup() %>% 
  filter(dup == 1 | (dup > 1 & period == 1)) %>% #Use observations that are repeats but represent one year
  select(-dup, -period, -rptstat, -procdt, -initrpt, -finrpt, -util_level, -rcptdt)

#Create hospital fiscal year data (as reported)
hcris_df_fy <- 
  hcris_df %>% 
  rename(
    period_beg = fy_beg,
    period_end = fy_end,
    year = fy) %>% 
  mutate(reporting = "FY") %>% 
  mutate(across(one_of(hcris_num_vars), ~ifelse(is.na(.), 0, .))) %>%
  select(prvdr_num, year, period_beg, period_end, reporting, control, teaching, one_of(hcris_num_vars))

#Create synthetic hospital calendar year values from the fiscal year reports
hcris_df_cy <- 
  hcris_df %>% 
  mutate(across(one_of(hcris_num_vars), ~ifelse(is.na(.), 0, .))) %>% 
  arrange(prvdr_num, fy) %>% 
  mutate(years_spanned = fy_end_y - fy_beg_y + 1) %>% #apportion each report to the years it spans
  uncount(years_spanned) %>% 
  group_by(rpt_rec_num) %>% 
  mutate(seq = row_number() - 1) %>% 
  ungroup() %>% 
  mutate(
    year = fy_beg_y + seq, #days of the target year covered by the cost report
    first_day_in_year = pmax(mdy(paste0("01", "01", year)), fy_beg),
    last_day_in_year = pmin(mdy(paste0("12", "31", year)), fy_end),
    days_in_year = last_day_in_year - first_day_in_year + 1,
    days_spanned = fy_end-fy_beg + 1,
    frac_rpt_in_year = as.integer(days_in_year) / as.integer(days_spanned)) %>% #share of the report's days that fell into the target year
  group_by(rpt_rec_num) %>% 
  mutate(totfrac = sum(frac_rpt_in_year)) %>% 
  filter(totfrac == 1) %>% 
  ungroup() %>% 
  mutate(frac_year_covered = as.integer(days_in_year) / as.integer(mdy(paste0("12","31", year)) - mdy(paste0("01","01",year)) + 1)) %>% 
  mutate(across(one_of(hcris_synth_vars), ~.*frac_rpt_in_year)) %>% #scale the flows by the share of the report that was in the target year
  group_by(prvdr_num, year) %>% 
  mutate(tot_frac_year = sum(frac_year_covered)) %>% 
  filter(tot_frac_year > .7) %>% 
  #collapse the rows and prorate if reports cover >70% of the target year
  mutate(across(one_of(hcris_synth_vars), ~sum(.)/tot_frac_year)) %>% 
  ungroup() %>% 
  distinct(prvdr_num, year, .keep_all = TRUE) %>% 
  mutate(
    period_beg = ymd(paste0(year, "-01-01")),
    period_end = ymd(paste0(year, "-12-31")),
    reporting = "CY") %>% 
  select(prvdr_num, year, period_beg, period_end, reporting, control, teaching, one_of(hcris_num_vars))
  
#Create data frames with final set of variables
hcris_df_final <- 
  bind_rows(
    hcris_df_cy,
    hcris_df_fy) %>% 
  mutate(
    medical_education_funding = ime1 + ime2 + gme_parta + gme_partb,
    dsh_funding = dsh1 + dsh2,
    operating_margin = (net_patient_revenue - operating_expense) / net_patient_revenue,
    excess_margin = (net_patient_revenue - operating_expense + nonoperating_revenue) / (net_patient_revenue + nonoperating_revenue),
    class = substr(prvdr_num, 3, 6),
    control = factor(
      control,
      levels = 1:13,
      labels = c(
        "Voluntary Nonprofit, Church",
        "Voluntary Nonprofit, Other",
        "Proprietary, Individual",
        "Proprietary, Corporation",
        "Proprietary, Partnership",
        "Proprietary, Other",
        "Governmental, Federal",
        "Governmental, City-County",
        "Governmental, County",
        "Governmental, State",
        "Governmental Hospital District",
        "Governmental, City",
        "Governmental, Other"
      ))) %>% 
  inner_join(gen_info, by = "prvdr_num") %>% 
  inner_join(facility_type, by = "class") %>% 
  left_join(hosp_compendium, by = "prvdr_num") %>% 
  select(prvdr_num, year, hospital_name, city, state, type, everything(), -gme_parta, -gme_partb, -ime1, -ime2, -dsh1, -dsh2, -ccr, -class) %>% 
  mutate(health_sys_name = ifelse(is.na(health_sys_name), "No Affiliation", health_sys_name))

##4. Save data frame as RDS file
write_rds(hcris_df_final, here("data", "costreports.rds"))
write_rds(release_df, here("data", "release.rds"))
write_csv(release_df, here("data", "release.csv"))

##5. Knit README
render("README.Rmd")
