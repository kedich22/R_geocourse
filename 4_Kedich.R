library(tidyverse)
library(readxl)
library(writexl)

soil <- read_xlsx('soil_data.xlsx', 1)
desc <- read_xlsx('soil_data.xlsx', 2, col_names = F)
colnames(desc) <- c('ID', 'desc')

# Создание функции для проверки заполненности ячеек
check_completeness <- function(df) { 
  completeness <- function(x) { #Функция внутр для определения процента заполненности
    perc <- round((1 - (sum(is.na(x)) / length(x))) * 100, 0)
    return(perc)
  }
  stat <- function(x) { #Функция внутренняя для заполнения поля с качеств характеристикой
    if (x == 100) {
      status = 'полностью'
    } else if (x == 0) {
      status =  "не заполнена"
    } else {
      status = 'частично'
    }
    return(status)
  }
  # Создание новой таблицы, обращение к созданным функциям
  dfnew <- tibble('names' = colnames(df), 'desc' = desc[, 2], 'completeness' = sapply(df, completeness),
                  'status' = sapply(completeness, stat))
  return(dfnew)
}

# Создание нового списка, разделение по таксонам почв и применение ф-ции
newsoil <- soil %>% 
  split(soil$SOIL_ID) %>% 
  lapply(check_completeness) 

write_xlsx(newsoil, 'completeness.xlsx')
