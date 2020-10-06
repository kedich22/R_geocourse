library(tidyverse)
library(readxl)

gara <- read_xlsx('garabashi.xlsx') %>% 
  rename(year = 1, acc = 2, abl = 3, bal = 4) %>% 
  filter(!str_detect(year, '-')) %>% 
  separate(year, c('year', 'y'), 4) %>% 
  select(-y) %>% 
  mutate(year = as.integer(year), cumbal = cumsum(bal))

plot(gara$year, gara$cumbal, type = 'l', 
     ylim = c(-1200, 400), 
     main = 'Mass balance of Garabashi glacier',
     ylab = 'см в.э', 
     xlab = 'Year',
     lwd = 2, 
     col = 'steelblue')
abline(h = 0, lty = 2)
lines(gara$year, gara$acc, col = 'green')
lines(gara$year, -gara$abl, col = 'red')

legend('bottomleft', c('Аккумуляция', 'Абляция', 'Баланс'),
       title = 'Легенда',
       col = c('green', 'red', 'steelblue'),
       lwd = c(1,1,2))