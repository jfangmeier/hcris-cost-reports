library(tidyverse)
library(readxl)
library(reshape2)
library(lubridate)
library(janitor)
library(httr)
library(rvest)
library(here)

#Set parameters for years to download and path to store downloaded HCRIS files
hcris_path <- file.path("C:","Users","Josh","Dropbox","Projects","data","hcris-data") #Where you download the HCRIS files
hcris_years <- 2011:2018 #Selected fiscal year range for HCRIS download

#List the current release dates of the cost reports and keep the hospitals (2010) release date
release_date_df <- read_html("https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/Cost-Reports/") %>% 
  html_node(xpath = "//table") %>% 
  html_table() %>% 
  as_tibble() %>% 
  clean_names()

release_date_df

release_date <- release_date_df %>% 
  filter(system == "Hospitals - 2010") %>% 
  pull(release_date) %>% 
  mdy() %>% 
  as.character()

#1. Download and uncompress HCRIS cost report files
hcris_download <- function(x){
  print(x)
  RETRY(verb = "GET", 
        url = paste0("http://downloads.cms.gov/Files/hcris/HOSP10FY",x,".zip"), 
        times = 5,
        pause_base = 1,
        pause_cap = 30,
        quiet = FALSE,
        terminate_on = NULL,
        write_disk(file.path(hcris_path,paste0("HOSP10FY",x,"_",release_date,".zip")), overwrite = TRUE),
        progress())
  unzip(zipfile = file.path(hcris_path,paste0("HOSP10FY",x,"_",release_date,".zip")),
        exdir = hcris_path,
        overwrite = TRUE)
}

map(hcris_years, hcris_download)


#2. Load worksheet lookup, general information, and taxonomy tables
lookup <- read_excel(file.path("misc","lookup.xlsx")) %>% 
  filter(enabled == 1)

all_vars <- lookup %>% 
  pull(var)

fin_vars <- lookup %>% #financial variables for synthetic years
  filter(financial_val == 1) %>% 
  pull(var)

char_vars <- lookup %>% #characteristic variables for synthetic years
  filter(characteristic_val == 1) %>% 
  pull(var)

gen_info <- read_csv(file.path("misc","hospital-general-information.csv")) %>% 
  mutate(prvdr_num = str_pad(as.character(prvdr_num), width = 6, side = "left", pad = "0"),
         zip_code = str_pad(as.character(zip_code), width = 5, side = "left", pad = "0"))

taxonomy <- read_csv(file.path("misc","hospital-taxonomy.csv")) %>% 
  mutate(type = as_factor(type))


#3. Extract select variables and append cost report CSVs
hcris_process <- function(x){
  print(x)
  read_csv(file.path(hcris_path,paste0("hosp10_",x,"_RPT.csv")), col_names = FALSE, col_types = cols(.default = "c")) %>% 
    rename(rpt_rec_num = X1,
           control = X2,
           prvdr_num = X3,
           rptstat = X5,
           fy_beg = X6,
           fy_end = X7,
           procdt = X8,
           initrpt = X9,
           finrpt = X10,
           util_level = X15,
           rcptdt = X18) %>% 
    mutate_at(vars(one_of("rpt_rec_num","control","rptstat")),
              funs(as.numeric(.))) %>% 
    mutate_at(vars(one_of("fy_beg","fy_end","procdt","rcptdt")),
              funs(mdy(.))) %>% 
    select(-starts_with("X")) %>% 
    inner_join(
      read_csv(file.path(hcris_path,paste0("hosp10_",x,"_ALPHA.csv")), col_names = FALSE) %>% 
        rename(rpt_rec_num = X1,
               wksht_cd = X2,
               line_num = X3,
               clmn_num = X4,
               item = X5) %>% 
        inner_join(lookup, by = c("wksht_cd","clmn_num","line_num")) %>% 
        select(rpt_rec_num, var, item) %>% 
        dcast(rpt_rec_num ~ var),
      by = "rpt_rec_num"
    ) %>% 
    inner_join(
      read_csv(file.path(hcris_path,paste0("hosp10_",x,"_NMRC.csv")), col_names = FALSE) %>% 
        rename(rpt_rec_num = X1,
               wksht_cd = X2,
               line_num = X3,
               clmn_num = X4,
               item = X5) %>% 
        inner_join(lookup, by = c("wksht_cd","clmn_num","line_num")) %>% 
        select(rpt_rec_num, var, item) %>% 
        dcast(rpt_rec_num ~ var) %>% 
        replace(., is.na(.), 0),
      by = "rpt_rec_num"
    ) %>% 
    mutate(fy = x)
}

for (x in years){
  print(x)
rpt <- read_csv(file.path(hcris_path,paste0("hosp10_",x,"_RPT.csv")), col_names = FALSE, col_types = cols(.default = "c")) %>% 
  rename(rpt_rec_num = X1,
         control = X2,
         prvdr_num = X3,
         rptstat = X5,
         fy_beg = X6,
         fy_end = X7,
         procdt = X8,
         initrpt = X9,
         finrpt = X10,
         util_level = X15,
         rcptdt = X18) %>% 
  mutate_at(vars(one_of("rpt_rec_num","control","rptstat")),
            funs(as.numeric(.))) %>% 
  mutate_at(vars(one_of("fy_beg","fy_end","procdt","rcptdt")),
            funs(mdy(.))) %>% 
  select(-starts_with("X"))

alph <- read_csv(file.path(hcris_path,paste0("hosp10_",x,"_ALPHA.csv")), col_names = FALSE) %>% 
  rename(rpt_rec_num = X1,
         wksht_cd = X2,
         line_num = X3,
         clmn_num = X4,
         item = X5) %>% 
  inner_join(lookup, by = c("wksht_cd","clmn_num","line_num")) %>% 
  select(rpt_rec_num, var, item) %>% 
  dcast(rpt_rec_num ~ var)

nmrc <- read_csv(file.path(hcris_path,paste0("hosp10_",x,"_NMRC.csv")), col_names = FALSE) %>% 
  rename(rpt_rec_num = X1,
         wksht_cd = X2,
         line_num = X3,
         clmn_num = X4,
         item = X5) %>% 
  inner_join(lookup, by = c("wksht_cd","clmn_num","line_num")) %>% 
  select(rpt_rec_num, var, item) %>% 
  dcast(rpt_rec_num ~ var) %>% 
  replace(., is.na(.), 0) #zero out numeric variables if missing

report <- rpt %>% 
  inner_join(alph, by = "rpt_rec_num") %>% 
  inner_join(nmrc, by = "rpt_rec_num") %>% 
  mutate(fy = x) %>% 
  arrange(prvdr_num)

assign(paste0("report_",x),report)
}

#Append fiscal year cost reports and remove duplicate reports for the same facility in the same fiscal year
hcris_df <- map_dfr(hcris_years, hcris_process) %>% 
  arrange(prvdr_num, fy) %>% 
  select(prvdr_num, everything()) %>% 
  mutate(fy_beg_y = year(fy_beg),
         fy_end_y = year(fy_end)) %>% 
  group_by(prvdr_num, fy) %>% 
  mutate(dup = n(),
         maxdate = max(procdt)) %>% 
  ungroup() %>% 
  filter(dup == 1 | (dup > 1 & maxdate == procdt)) %>% #Pick the observation that has the latest HCRIS processing date
  select(-dup, -maxdate) %>% 
  group_by(prvdr_num, fy) %>% 
  mutate(dup = n(),
         minstat = min(rptstat)) %>% 
  ungroup() %>% 
  filter(dup == 1 | (dup >1 & rptstat == minstat)) %>% #Pick the observations that are audited over those that are not (lower is better)
  select(-dup, -minstat) %>% 
  group_by(prvdr_num, fy) %>% 
  mutate(dup = n(),
         period = if_else(fy_end-fy_beg < 364,0,
                          if_else(fy_end-fy_beg < 366,1,0))) %>% 
  ungroup() %>% 
  filter(dup == 1 | (dup > 1 & period == 1)) %>% #Use observations that are repeats but represent one year
  select(-dup, -period, -rptstat, -procdt, -initrpt, -finrpt, -util_level, -rcptdt)




cr_list <- paste0("report_",years)

cr_append <- bind_rows(mget(cr_list)) %>% 
  arrange(prvdr_num, fy) %>% 
  select(prvdr_num, everything()) %>% 
  mutate(fy_beg_y = year(fy_beg),
         fy_end_y = year(fy_end)) %>% 
  group_by(prvdr_num, fy) %>% 
  mutate(dup = n(),
         maxdate = max(procdt)) %>% 
  ungroup() %>% 
  filter(dup == 1 | (dup > 1 & maxdate == procdt)) %>% #Pick the observation that has the latest HCRIS processing date
  select(-dup, -maxdate) %>% 
  group_by(prvdr_num, fy) %>% 
  mutate(dup = n(),
         minstat = min(rptstat)) %>% 
  ungroup() %>% 
  filter(dup == 1 | (dup >1 & rptstat == minstat)) %>% #Pick the observations that are audited over those that are not (lower is better)
  select(-dup, -minstat) %>% 
  group_by(prvdr_num, fy) %>% 
  mutate(dup = n(),
         period = if_else(fy_end-fy_beg < 364,0,
                          if_else(fy_end-fy_beg < 366,1,0))) %>% 
  ungroup() %>% 
  filter(dup == 1 | (dup > 1 & period == 1)) %>% #Use observations that are repeats but represent one year
  select(-dup, -period, -rptstat, -procdt, -initrpt, -finrpt, -util_level, -rcptdt)

#Create synthetic calendar year values from the fiscal year reports
hcris_df_synth <- hcris_df %>% 
  arrange(prvdr_num, fy) %>% 
  mutate(years_spanned = fy_end_y - fy_beg_y + 1) %>% #apportion each report to the years it spans
  uncount(years_spanned) %>% 
  group_by(rpt_rec_num) %>% 
  mutate(seq = row_number() - 1) %>% 
  ungroup() %>% 
  mutate(year = fy_beg_y + seq, #days of the target year covered by the cost report
         first_day_in_year = pmax(mdy(paste0("01","01",year)),fy_beg),
         last_day_in_year = pmin(mdy(paste0("12","31",year)),fy_end),
         days_in_year = last_day_in_year-first_day_in_year+1,
         days_spanned = fy_end-fy_beg+1,
         frac_rpt_in_year = as.integer(days_in_year)/as.integer(days_spanned)) %>% #share of the report's days that fell into the target year
  group_by(rpt_rec_num) %>% 
  mutate(totfrac = sum(frac_rpt_in_year)) %>% 
  filter(totfrac == 1) %>% 
  ungroup() %>% 
  mutate(frac_year_covered = as.integer(days_in_year)/as.integer(mdy(paste0("12","31",year))-mdy(paste0("01","01",year))+1)) %>% 
  mutate_at(vars(one_of(fin_vars)), funs(.*frac_rpt_in_year)) %>% #scale the flows by the share of the report that was in the target year
  mutate_at(vars(one_of(char_vars)), funs(.*frac_year_covered)) %>% 
  group_by(prvdr_num, year) %>% 
  mutate(tot_frac_year = sum(frac_year_covered)) %>% 
  filter(tot_frac_year > .7) %>% 
  mutate_at(vars(one_of(fin_vars)), #collapse the rows and prorate if reports cover >70% of the target year
            funs(sum(.)/tot_frac_year)) %>% 
  mutate_at(vars(one_of(char_vars)), 
            funs(as.integer(sum(.)/tot_frac_year))) %>%
  ungroup() %>% 
  distinct(prvdr_num, year, .keep_all = TRUE) %>% 
  select(prvdr_num, year, control, teaching, one_of(char_vars), one_of(fin_vars))
  
hcris_df_final <- hcris_df_synth %>% 
  mutate(income = netpatrev + othinc,
         totcost = opexp + othexp,
         dgme = gme_parta + gme_partb,
         ime = ime1 + ime2,
         dsh = dsh1 + dsh2,
         margin = (income/totcost)/income,
         class = as.numeric(substr(prvdr_num,3,6)),
         control = factor(control,
                          levels = c(1:13),
                          labels = c("Voluntary Nonprofit, Church",
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
                                     "Governmental, Other"))) %>% 
  inner_join(gen_info, by = "prvdr_num") %>% 
  inner_join(taxonomy, by = "class") %>% 
  select(prvdr_num, year, hospital_name, city, state, type, everything(), -gme_parta, -gme_partb, -ime1, -ime2, -dsh1, -dsh2, -ccr, -class)

saveRDS(cr_final, file.path(data,"costreports.rds"))
  



