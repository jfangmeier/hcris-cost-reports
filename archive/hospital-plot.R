cost_reports %>% 
  filter(state == "MN" & year %in% 2012:2017) %>% 
  group_by(year) %>% 
  summarize(tot_ftemp = sum(ftemp)) %>% 
  ggplot(aes(x = year, y = tot_ftemp)) +
  geom_bar(stat = "sum")
  