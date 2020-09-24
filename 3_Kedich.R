library(tidyverse)
library(readxl)
library(writexl)

#запись данных из таблицы во фреймы
emis <- read_excel("emissions.xlsx", 1, col_types = c('text', rep('numeric', 8)), skip = 1)
colnames(emis)[1] <- "Region"
drop <- read_excel("emissions.xlsx", 2, col_types = c('text', rep('numeric', 8)), skip = 1, na = c("-", " "))
colnames(drop)[1] <- "Region"

#запись названий округов в новый столбец
emis <- emis %>% 
  mutate(f_okrug = if_else(str_detect(emis$Region, 'федеральный округ'), Region, NULL)) %>% 
  fill(f_okrug) %>% 
  filter(!str_detect(emis$Region, 'федеральный | Федерация'))

drop <- drop %>% 
  mutate(f_okrug = if_else(str_detect(drop$Region, 'федеральный округ'), Region, NULL)) %>% 
  fill(f_okrug) %>% 
  filter(!str_detect(drop$Region, 'федеральный | Федерация'))

#Преобразование данных во фреймах
emisnew <- pivot_longer(emis, cols = 2:9, names_to = "year", values_to = "emit")
dropnew <- pivot_longer(drop, cols = 2:9, names_to = "year", values_to = "drop")

#Соедениение таблиц
pollut <- inner_join(emisnew, dropnew, by = c("Region" = "Region", "year" = "year", "f_okrug" = "f_okrug")) %>%
  mutate(dif = drop - emit)
  
#группировка и определение наиболее загрязняющих регионов по годам
pollut <- pollut %>%
  group_by(f_okrug, year) %>%
  arrange(dif) %>% 
  filter(row_number() == 1)

pollut$year <- as.numeric(pollut$year)
colnames(pollut) <- c('Регион', 'Федеральный округ', 'Год', 'Выбросы', 'Улавливание', 'Разница')
#запись в файл
write_xlsx(pollut, "polluters.xlsx")
