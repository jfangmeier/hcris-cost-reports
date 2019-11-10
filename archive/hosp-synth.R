





cr_synth <- cr_append %>% 
  mutate(year_beg = ymd(paste0(fy_end_y,"0101")),
         year_end = ymd(paste0(fy_end_y,"1231")),
         fy_cy_ratio1 = (fy_end - year_beg)/365.25,
         fy_cy_ratio2 = (year_end - fy_end)/365.25,
         fy_len = (fy_end - fy_beg)/365.25) %>% 
  group_by(prvdr_num, fy_end_y) %>% 
  mutate(dup = n(),
         maxfylen = max(fy_len),
         maxfyend = max(fy_end)) %>% 
  ungroup() %>% 
  filter(dup ==1 | (dup > 1 & fy_end == maxfyend & fy_len == maxfylen)) %>%  #Remove CY duplicates of lesser fiscal year length or that end earlier
  select(-dup) %>% 
  filter(fy_end_y >= first_year) %>%  #Remove years prior to first year in time series
  arrange(prvdr_num, fy_end_y) %>% 
  group_by(prvdr_num) %>% 
  mutate(run = row_number(),
         maxrun = max(run)) %>% 
  ungroup() %>% 
  filter(maxrun >= 4) # Remove hospitals with less than enough reports to cover the reporting period