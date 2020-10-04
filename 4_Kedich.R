library(tidyverse)
library(readxl)
library(writexl)

soil <- read_xls('soil_data.xls', 1)
desc <- read_xls('soil_data.xls', 2, col_names = F)
colnames(desc) <- c('ID', 'desc')

fill_status <- function(x) { #Функция для заполнения поля с качеств характеристикой
  if (x == 100) {
    status = 'полностью'
  } else if (x == 0) {
    status =  "не заполнена"
  } else {
    status = 'частично'
  }
  return(status)
}

# Создание функции для проверки заполненности ячеек
check_completeness <- function(df) { 

# Создание новой таблицы, обращение к созданным функциям
  dfnew <- tibble('names' = colnames(df), 'completeness' 
                  = sapply(df, function(x) {round((1 - (sum(is.na(x)) / length(x))) * 100, 0)}),
                  'status' = sapply(completeness, fill_status))
  return(dfnew)
}

# Создание нового списка, разделение по таксонам почв и применение ф-ции
newsoil <- soil %>% 
  split(soil$SOIL_ID) %>% 
  lapply(check_completeness) %>% 
  lapply(function(x) {mutate(x, 'desc' = desc$desc, .before = 2)})

write_xlsx(newsoil, 'completeness.xlsx')
