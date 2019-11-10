library(tidyverse)
library(janitor)
library(rvest)
library(measurements)
library(here)

st_centroids <- "https://en.wikipedia.org/wiki/List_of_geographic_centers_of_the_United_States" %>% 
  read_html() %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>% 
  html_table() %>% 
  clean_names() %>% 
  mutate_all(list(~str_squish(.))) %>%
  mutate_all(list(~gsub("[^\x20-\x7E]", "", .))) %>% #remove non-ASCII characters
  separate(coordinates, into = c("coord1", "coord2", "coord3"), sep = "\\/") %>% 
  separate(coord3, into = c("lat", "lon"), sep = "\\;") %>% 
  separate(lon, into = c("lon", "text"), sep = "\\(") %>% 
  mutate_at(vars(lat,lon), list(~as.numeric(.))) %>% 
  select(state_name = state, lat, lon) %>% 
  as_tibble()

st_area <- "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_area" %>% 
  read_html() %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>% 
  html_table() %>% 
  clean_names() %>% 
  mutate_all(list(~str_squish(.))) %>%
  mutate_all(list(~gsub("[^\x20-\x7E]", "", .))) %>% #remove non-ASCII characters
  filter(x %in% c(state.name, "District of Columbia")) %>% 
  select(state_name = x,
         squaremiles = total_area_2_2) %>% 
  mutate(squaremiles = as.numeric(str_remove(squaremiles, "\\,")))

st_crosswalk <- tibble(state_name = state.name) %>%
  bind_cols(tibble(state = state.abb)) %>% 
  bind_rows(tibble(state_name = "District of Columbia", state = "DC"))

st_bounds <- "http://www.ala.org/rt/magirt/publicationsab/usa" %>% 
  read_html() %>% 
  html_node(xpath = '//*[@id="node-1489"]/div[1]/div/div/table') %>% 
  html_table() %>% 
  clean_names() %>% 
  mutate_all(list(~str_squish(.))) %>%
  mutate_all(list(~gsub("[^\x20-\x7E]", "", .))) %>% #remove non-ASCII characters
  mutate_at(vars(north, south, east, west), list(~str_replace_all(., "O", "0"))) %>% 
  mutate_at(vars(north, south, east, west), list(~str_replace_all(., "l", "1"))) %>% 
  mutate_at(vars(north, south, east, west), list(~str_replace_all(., "\\'", " "))) %>% 
  mutate_at(vars(north, south, east, west), list(~str_replace_all(., " 0", " "))) %>% 
  mutate_at(vars(north, south, east, west), list(~if_else(substr(.,1,1) %in% c("W","S"),
                                                          as.numeric(measurements::conv_unit(substr(., 3, 20), from = 'deg_dec_min', to = 'dec_deg')) * -1,
                                                          as.numeric(measurements::conv_unit(substr(., 3, 20), from = 'deg_dec_min', to = 'dec_deg'))))) %>% 
  mutate(west = if_else(state_territory == "Alaska", -180, west)) %>% #fix Alaska's western boundary
  rename(state_name = state_territory)
  
state_data <- st_centroids %>% 
  inner_join(st_area, by = "state_name") %>% 
  inner_join(st_crosswalk, by = "state_name") %>% 
  inner_join(st_bounds, by = "state_name") %>% 
  select(state, everything())

saveRDS(state_data, file.path("data","state-data.rds"))