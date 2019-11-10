library(tidyverse)
library(here)
library(glue)
library(leaflet)
library(htmltools)
library(shiny)
library(shinyWidgets)
library(DT)
library(janitor)
library(scales)
library(rpivotTable)

#data <- file.path("C:","Users","jfang","Dropbox","Projects","data","hcris-data")

costreport_df <- readRDS(url('https://www.dropbox.com/s/5pcqif0f8cg4yu4/costreports.rds?raw=1'))

costreport_df <- costreport_df %>% 
  filter(year %in% (2012:2016)) %>% 
  group_by(prvdr_num) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count == 5) %>%
  select(
    `Hospital ID` = prvdr_num,
    Year = year,
    Hospital = hospital_name,
    City = city,
    State = state,
    Beds = beds_total,
    `Teaching Status` = teaching,
    `Residents/Interns` = ftedu,
    `Total Staff` = ftemp,
    IME = ime,
    DGME = dgme,
    Income = income,
    Lat = lat,
    Long = lon)

state_list <- costreport_df %>% 
  pull(State) %>% 
  unique() %>% 
  sort()

year_list <- as.character(2012:2016)