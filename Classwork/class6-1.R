library(tidyverse)
library(nasapower)
library(eurostat)

daily_single_ag <- get_power(
  community = "AG",
  lonlat = c(60.59, 56.84),
  pars = c("RH2M", "T2M", "PRECTOT"),
  dates = c("1995-04-01", "1995-04-30"),
  temporal_average = "DAILY"
)

ggplot(daily_single_ag, 
       mapping = aes(YYYYMMDD, T2M)) +
  geom_line(color = 'steelblue') +
  geom_point() +
  ggtitle('Температура в г. Екатеринбург') +
  xlab ('Дата') +
  ylab ('Температура, [C°]') +
  theme_classic()

interannual_sse <- get_power(
  community = "SSE",
  lonlat = c(60.59, 56.84),
  dates = 1995:2015,
  temporal_average = "INTERANNUAL",
  pars = c("CLRSKY_SFC_SW_DWN",
           "ALLSKY_SFC_SW_DWN")
)

sse_tidy = interannual_sse %>% 
  select(-LAT, -LON, -ANN) %>% 
  pivot_longer(JAN:DEC, 
               names_to = 'month',
               values_to = 'value') %>% 
  mutate(month = factor(month, 
                        ordered = T,
                        levels = unique(month)))

sse_tidy %>% 
  filter(PARAMETER == 'ALLSKY_SFC_SW_DWN', YEAR == 1995) %>% 
ggplot(mapping = aes(month, value)) +
  geom_col(fill = 'lightblue') +
  xlab('Месяц') +
  ylab(expression(кВт/ч/м^2/день)) +
  theme_grey()
  
