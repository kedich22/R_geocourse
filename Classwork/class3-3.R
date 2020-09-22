library(tidyverse)

energy <- readr::read_table('wind_energy.txt', col_names = c('N', 'lat',
                                                   'long', 'kwt50', 'kwt100'))
energy_sum <- energy %>% 
  pivot_longer(cols = c(kwt50, kwt100), names_to = "height", 
               values_to = "kwt") %>% 
  separate(height, c('dummy', 'h'), 3) %>% 
  select(-dummy) %>% 
  mutate (h = as.integer(h))

energy_sum %>% 
  group_by(h) %>% 
  summarise(mean_kwt = mean(kwt))
