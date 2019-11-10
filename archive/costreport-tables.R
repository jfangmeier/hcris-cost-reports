library(tidyverse)
library(scales)
library(gt)

costreports <- readRDS(here::here("data", "costreports.rds"))

costreports %>% 
  filter(prvdr_num == "240106") %>% 
  select(year, net_patient_revenue, net_income, ip_discharges_total, operating_margin) %>% 
  mutate_at(vars(net_patient_revenue, net_income), ~scales::dollar(.)) %>% 
  mutate_at(vars(ip_discharges_total), ~scales::comma(.)) %>% 
  mutate_at(vars(operating_margin), ~scales::percent(.)) %>% 
  gather(var, value, -year) %>% 
  spread(year, value) %>% 
  mutate(var = str_to_title(str_replace_all(var, "\\_", " "))) %>% 
  rename(Indicator = var) %>% 
  gt()
  
  