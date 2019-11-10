library(RSocrata)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library("ggmap")

register_google(key = rstudioapi::showPrompt(
  title = "Google Maps API", message = "API Key", default = ""
)
)

path <- file.path("C:","Users","Josh","Dropbox","Projects","costreports","misc")
setwd(path)

#Download CMS data on hospital names and locations
hospital_raw <- read.socrata("https://data.medicare.gov/Hospital-Compare/Hospital-General-Information/xubh-q36u") %>% 
  rename_all(funs(str_replace(., "\\.", "_"))) %>% 
  rename_all(tolower) %>% 
  rename(prvdr_num = provider_id) %>% 
  separate(location, 
           into = c("address","coord"),
           sep = " \\(") %>% 
  separate(coord,
           into = c("lat","lon"),
           sep = ", ") %>% 
  mutate(lon = as.numeric(str_remove(lon, "\\)")),
         lat = as.numeric(lat),
         zip_code = str_pad(as.character(zip_code), width = 5, side = "left", pad = "0")) %>% 
  select(prvdr_num, hospital_name, address, city, state, zip_code, lat, lon)

hospital_coord_yes <- hospital_raw %>% 
  filter(!is.na(lat) & !is.na(lon))

hospital_coord_no <- hospital_raw %>% 
  filter(is.na(lat) | is.na(lon)) %>% 
  select(-lat, -lon) %>% 
  mutate_geocode(address)

hospital_gen_info <- bind_rows(hospital_coord_yes, hospital_coord_no)

write_csv(hospital_gen_info, "hospital-general-information.csv")