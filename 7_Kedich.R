library(tidyverse)
library(googlesheets4)
library(rlang)
library(lubridate)

catalog <- read_sheet('1Q6HCY4jxiYefjPdWrN5erwgEee-Gz_BywqZW2mQcftw')

meteo_data <- lapply(as.character(catalog$ID), function(X) read_sheet('1FWC_YBrlINnjR5POC2kxa_LnLC7n5fFA90oNA7dLTP0', sheet = X, col_types = 'ccncccnninncnnnnncc')) %>% 
  set_names(catalog$ID) %>% 
  bind_rows(.id = 'id') %>% 
  mutate(id = as.numeric(id)) %>% 
  full_join(catalog, by = c('id' = 'ID')) %>% 
  mutate(Datetime = as.POSIXct(Datetime)) %>% 
  relocate(id, .before = NAME) %>% 
  set_names(c('Datetime', 'Wdir', 'Wspd', 'Vis', 'Phen', 'Cloud', 'T', 'Td', 'F', 'Te', 'Tes', 'Comf', 'P', "Po", 'Tmin', 'Tmax', 'R', 'R24', 'S', 'ID', 'NAME', 'LON', 'LAT', 'H'))

filter_data <- meteo_data %>% 
  filter(str_detect(NAME, 'Балчуг') | str_detect(NAME, 'Сареево'))
filter_data_night <- filter_data %>% 
  filter(hour(Datetime) == 0 | hour(Datetime) == 3)

ggplot(filter_data, mapping = aes(Datetime, `T`, color = NAME)) +
  geom_point() +
  ggtitle('Плотности распределения температур на станциях "Малое Сараево", "Балчуг"') +
  ylab('C°') +
  xlab('Месяц') +
  labs(color = 'Метеостанция')

ggplot(filter_data_night, mapping = aes(Datetime, `T`, color = NAME)) +
  geom_point() +
  ggtitle('Плотности распределения ночных температур на станциях "Малое Сараево", "Балчуг"') +
  ylab('C°') +
  xlab('Месяц') +
  labs(color = 'Метеостанция')

ggplot(filter_data, mapping = aes(NAME, `T`, fill = NAME)) +
  geom_boxplot(alpha = 0.4, width = 0.6) +
  ylab('C°') +
  theme(axis.title.x = element_blank()) +
  ggtitle('Диаграммы размаха температур на метеостанциях') +
  theme(legend.position="none")

ggplot(filter_data_night, mapping = aes(NAME, `T`, fill = NAME)) +
  geom_boxplot(alpha = 0.4, width = 0.6) +
  ylab('C°') +
  theme(axis.title.x = element_blank()) +
  ggtitle('Диаграммы размаха температур на метеостанциях') +
  theme(legend.position="none")

t.test(filter_data %>% filter(ID == 27605) %>% pull(`T`),
       filter_data %>% filter(ID == 27518) %>% pull(`T`))
var.test(filter_data %>% filter(ID == 27605) %>% pull(`T`),
       filter_data %>% filter(ID == 27518) %>% pull(`T`))

t.test(filter_data_night %>% filter(ID == 27605) %>% pull(`T`),
       filter_data_night %>% filter(ID == 27518) %>% pull(`T`))
var.test(filter_data_night %>% filter(ID == 27605) %>% pull(`T`), 
         filter_data_night %>% filter(ID == 27518) %>% pull(`T`))

dif_df <- data.frame(Datetime = filter_data %>% filter(ID == 27605) %>% pull(Datetime)) %>% 
  mutate(temp_dif = (filter_data %>% filter(ID == 27605) %>% pull(`T`) -
         filter_data %>% filter(ID == 27518) %>% pull(`T`))) %>% 
  inner_join(meteo_data %>% group_by(Datetime) %>% summarise(mean_wind_spd = round(mean(Wspd, na.rm = T), 2)))
  
dif_df_night <- dif_df %>% 
  filter(hour(Datetime) == 0 | hour(Datetime) == 3)

ggplot(dif_df, mapping = aes(mean_wind_spd, temp_dif)) +
  geom_point(shape = 1, size = 0.8, color = 'red') +
  geom_smooth(method = 'loess', color = 'blue', size = 0.8, span = 0.5) +
  ggtitle('Диаграмма рассеяния между скоростью ветра\nи разницей температур (Малое Сареево и Балчуг)') +
  xlab('Скорость ветра, м/с') +
  ylab('Разница температур, C°')

ggplot(dif_df_night, mapping = aes(mean_wind_spd, temp_dif)) +
  geom_point(shape = 1, size = 0.8, color = 'red') +
  geom_smooth(method = 'loess', color = 'blue', size = 0.8, span = 0.9) +
  ggtitle('Диаграмма рассеяния между скоростью ветра\nи разницей ночных температур (Малое Сареево и Балчуг)') +
  xlab('Скорость ветра, м/с') +
  ylab('Разница температур, C°')

ggplot(dif_df, mapping = aes(log10(mean_wind_spd), temp_dif)) +
  geom_point(shape = 1, size = 0.8, color = 'red') +
  geom_smooth(method = 'lm', color = 'blue', size = 0.8) +
  ggtitle('Диаграмма рассеяния между скоростью ветра\nи разницей температур (Малое Сареево и Балчуг)') +
  xlab(expression('log'[10]*'(Скорость ветра), м/с')) +
  ylab('Разница температур, C°')

ggplot(dif_df_night, mapping = aes(log10(mean_wind_spd), temp_dif)) +
  geom_point(shape = 1, size = 0.8, color = 'red') +
  geom_smooth(method = 'lm', color = 'blue', size = 0.8) +
  ggtitle('Диаграмма рассеяния между скоростью ветра\nи разницей температур (Малое Сареево и Балчуг)') +
  xlab(expression('log'[10]*'(Скорость ветра), м/с')) +
  ylab('Разница температур, C°')  

cor.test(log10(dif_df$mean_wind_spd), dif_df$temp_dif)

cor.test(log10(dif_df_night$mean_wind_spd), dif_df_night$temp_dif)

linear_model = lm(log10(dif_df$mean_wind_spd) ~ dif_df$temp_dif)
summary(linear_model)
