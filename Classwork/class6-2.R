library(tidyverse)
library(nasapower)
library(eurostat)

trades = get_eurostat('tet00034') %>% 
  label_eurostat() %>% 
  bind_rows() %>% 
  select(-geo) %>% 
  filter(stringr::str_detect(indic_et, 'Exports in|Imports in')) %>% 
  # pivot_wider(names_from = indic_et, values_from = values) %>%  
  # rename(export = `Exports in million of ECU/EURO`, 
  #        import = `Imports in million of ECU/EURO`) %>% 
  mutate(partner = as.factor(partner))

trades_tidy = trades %>% 
  group_by(indic_et, time) %>% 
  summarise(value = sum(values))

ggplot(trades_tidy, mapping = aes(time, value, color = indic_et)) +
  geom_line() +
  geom_point() +
  ggtitle('Импорт/экспорт') +
  xlab('Год') +
  ylab('млн. евро')

trades %>% 
  filter(time == as.Date('2019-01-01'), 
         indic_et == 'Exports in million of ECU/EURO') %>% 
ggplot(mapping = aes(x = '', y = values, fill = partner)) +
  geom_col() +
  coord_polar(theta = 'y')
