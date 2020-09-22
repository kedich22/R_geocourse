library('writexl')
library('tidyverse')
# %>% - ctrl+shift+m 
# ctrl+shift+c - #
# as_tibble(starwars)

# tab1 <- data.frame(type = c("Деревья", "Кустарники", "Трава"), id = c(1,5,12), stringsAsFactors = T)
# tab2 <- tibble(type = c("Деревья", "Кустарники", "Трава"), id = c(1,5,12))

# тибл может использовать внутр переменные
# tabxy <- tibble(x = c(2,5,-12), y = x^2)

#Задача 1
# Таблица quakes из пакета datasets содержит магнитуду землетрясений в поле mag.
# Используя функции dplyr и пайп-оператор, создайте на ее основе таблицу с
# частотой (количеством штук) землетрясений каждой магнитуды

# группировка значений магнитуды по количеству встречаний, аггрегирование значение переменных
# summarise - работа с группами

quakes <- quakes %>% 
  mutate(is_south <- lat <= -23.5) %>%
  group_by(mag) %>% 
  summarise(count = n())




