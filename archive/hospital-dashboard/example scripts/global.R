library(tidyverse)
library(glue)
library(leaflet)
library(htmltools)
library(shiny)
library(shinyWidgets)
library(DT)
library(here)
library(measurements)
library(janitor)

clinics <- readRDS(here::here("data", "clinic_address_final.rds"))
worksites <-
  readRDS(here::here("data", "worksite_fte_address_final.rds"))
distance <-
  readRDS(here::here("data", "worksite_clinic_distance.rds"))
peip_crosswalk <- 
  readRDS(here::here("data", "peip_ws_cl_crosswalk.rds"))

ws_cl_crosswalk <- worksites %>%
  select(worksite_label, worksite_fte, county_name) %>%
  distinct(worksite_label, .keep_all = TRUE) %>%
  inner_join(distance, by = "worksite_label") %>%
  inner_join(
    clinics %>%
      select(
        clinic_label,
        mmb_num,
        clinic_name,
        full_address,
        clinic_costlevel2020,
        candidate,
        afford,
        clinic_access_prior_year,
        clinic_doctors
      ),
    by = "clinic_label"
  ) %>%
  mutate(
    provide_access_25_miles = if_else(candidate == 1 &
                                        drive_dist <= 25 & afford == 1, 1, 0),
    provide_access_30_miles = if_else(candidate == 1 &
                                        drive_dist <= 30 & afford == 1, 1, 0)
  ) %>%
  group_by(worksite_label) %>%
  mutate(
    worksite_access_clinics_25_miles = sum(provide_access_25_miles),
    worksite_access_clinics_30_miles = sum(provide_access_30_miles)
  ) %>%
  ungroup() %>%
  mutate_at(vars(dist_line, drive_dist, drive_time), list( ~ round(., digits = 1)))

worksite_list_25_miles <- ws_cl_crosswalk %>%
  filter(worksite_access_clinics_25_miles == 0) %>% #worksite with no affordable candidates within 25 miles
  pull(worksite_label) %>%
  unique()

worksite_list_all <- ws_cl_crosswalk %>%
  pull(worksite_label) %>%
  unique() %>%
  sort()

clinic_list_30_miles <- ws_cl_crosswalk %>%
  filter(worksite_access_clinics_25_miles == 0) %>% #worksite with no affordable candidates within 25 miles
  filter(drive_dist <= 30 &
           candidate == 1) %>% #candidate clinics within 30 miles of these worksites
  arrange(mmb_num) %>%
  pull(clinic_label) %>%
  unique()

clinic_list_all <- ws_cl_crosswalk %>%
  arrange(mmb_num) %>%
  pull(clinic_label) %>%
  unique()

statewide_map_options <-
  c(paste0("Cost level: ", c("1", "2", "2*", "3", "4")), paste0(
    "Access: ",
    c("<25 miles", "25-30 miles", "30-35 miles", ">35 miles")
  ))


if (interactive()) {
  shinyApp(ui = "ui.R", server = "server.R")
}