library(readxl)
library(tidyverse)
tab <- read_xlsx('windmsk.xlsx') %>% 
  rename(dir = 1) %>% 
  filter(dir != 'штиль')

num = sapply(tab[-1], which.max)

prime_dir = tibble(month = colnames(tab[-1]),
                   dir = tab[num, 'dir'])


