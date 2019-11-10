library(tidyverse)
library(here)
library(glue)
library(crosswalkr)
library(leaflet)
library(htmltools)
library(shiny)
library(shinyWidgets)
library(DT)
library(scales)
library(gt)

#data <- file.path("C:","Users","Josh","Dropbox","Projects","data","hcris-data")

#costreport_df <- readRDS(url('https://www.dropbox.com/s/5pcqif0f8cg4yu4/costreports.rds?raw=1'))
costreports <- readRDS(here::here("data", "costreports.rds"))

costreport_var_df <- 
tibble::tribble(
                       ~variable,                   ~var_name, ~position,   ~format, ~table,
                          "year",                      "Year",         1,   "other",      1,
                     "prvdr_num",                    "Number",         2,   "other",      0,
                 "hospital_name",                      "Name",         3,   "other",      0,
                          "city",                      "City",         4,   "other",      0,
                         "state",                     "State",         5,   "other",      0,
                       "address",                   "Address",         6,   "other",      0,
                          "type",                      "Type",         7,   "other",      0,
                    "beds_total",                      "Beds",         8,   "comma",      1,
                      "teaching",           "Teaching Status",        17,   "other",      1,
           "net_patient_revenue",       "Net Patient Revenue",        12,  "dollar",      1,
                    "net_income",                "Net Income",        13,  "dollar",      1,
                       "uc_cost",   "Uncompensated Care Cost",        16,  "dollar",      1,
             "ip_bed_days_total",               "IP Bed Days",        11,   "comma",      1,
           "ip_discharges_total",             "IP Discharges",        10,   "comma",      1,
       "total_residents_interns",         "Residents/Interns",        18,   "comma",      1,
               "total_employees",                 "Employees",         9,   "comma",      1,
     "medical_education_funding", "Medical Education Funding",        19,  "dollar",      1,
                   "dsh_funding",               "DSH Funding",        16,  "dollar",      1,
              "operating_margin",          "Operating Margin",        14, "percent",      1,
                 "excess_margin",             "Excess Margin",        15, "percent",      1,
                      "zip_code",                       "ZIP",        20,   "other",      0,
                           "lat",                       "Lat",        21,   "other",      0,
                           "lon",                       "Lon",        22,   "other",      0
     )

costreports_df <- 
  costreports %>% 
  crosswalkr::renamefrom(cw_file = costreport_var_df,
                         raw = variable,
                         clean = var_name)

vars_dollar <- costreport_var_df %>% filter(format == "dollar") %>% pull(var_name)
vars_percent <- costreport_var_df %>% filter(format == "percent") %>% pull(var_name)
vars_comma <- costreport_var_df %>% filter(format == "comma") %>% pull(var_name)
vars_table <- costreport_var_df %>% filter(table == 1) %>% pull(var_name)

input_list_df <- costreports_df %>% 
  filter(State %in% c("DC", state.abb)) %>% 
  group_by(Number) %>% 
  arrange(-Year) %>%  
  slice(1) %>% 
  ungroup() %>% 
  mutate(Hospital = paste0(Number, ": ", Name)) %>% 
  distinct(State, City, Hospital)

state_list <- input_list_df %>% 
  pull(State) %>% 
  unique() %>% 
  sort()

city_list <- input_list_df %>% 
  filter(State == "MN") %>% 
  pull(City) %>% 
  unique() %>% 
  sort()

hospital_list <- input_list_df %>% 
  filter(State == "MN") %>% 
  pull(Hospital) %>% 
  unique() %>% 
  sort()

hospital_name_fcn <- function(name, number, type, address){
  div(style = "background: white; color: black",
      h1(name),
      h2(paste0("CCN: ", number)),
      h2(paste0("Type: ", type)),
      h3(address)
  )
}
