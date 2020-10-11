library(tidyverse)
library(readxl)
library(RColorBrewer)

#Построение первого графика аномалии температур
temp <- read_table2('graph.txt', skip = 5, col_names = c('year', 'no_smooth', 'lowess'))

plot(temp$year, temp$no_smooth, 
     main = list('Аномалия температуры по отношению к средней\n за период 1951-1980 (по данным NASA, 2020 г.)', cex = 0.8),
     xlab = 'Год',
     ylab = '°С',
     col = 'grey',
     type = 'c') 
grid() #добавление сетки
points(temp$year, temp$no_smooth,
       pch = 20,
       col = 'grey') #точки среднегодовой температуры
abline(h = 0, col = 'red') #добавление линии 0
lines(temp$year, temp$lowess, col = 'blue') #добавлении линии скользящего среднего

text <- c('Скользяшее среднее', 'Среднегодовая температура')
legend('topleft', text,
       col = c('blue', 'grey'),
       lwd = c(2, 2),
       text.width = strwidth(text)[1]/1.8,
       pch = c(NA, 20),
       cex = 0.7,
       pt.cex = 1.2) # создание и модификация легенды

#----------------------------------------------------------------------------------

#Построение второго графика структуры лесного покрова Земли
trees <- read.csv('fao_treecover_extent__ha.csv') %>% 
  select(c(-1, -2, -6, -7)) %>% 
  sapply(sum, na.rm = T) %>% 
  as_tibble() %>% 
  mutate(name = c('Высаженный', 'Первичный', 'Восстановленный'), .before = 1) #преобразование исходных данных, получение аггрегированных значений по данным исходной таблицы

par(mar = c(2, 2, 2, 2))
names2 <- paste(trees$name, ' (', round((100* trees$value / sum(trees$value)), 0), '%)', sep = '')
pie(trees$value, names2,
    clockwise = T,
    main = "Структура лесного покрова Земли\n (по данным ФАО, 2015 г.)",
    col = c('olivedrab1', 'forestgreen', 'palegreen3')) #построение круговой диаграммы

#-----------------------------------------------------------------------------------

#Построение третьего графика содержания углерода по климатич регионам
soil <- read_xlsx('PgC.xlsx', skip = 1)[3:14,] %>% 
  rename(name = `...1`, Tier = `Tier 1`) 
soil <- tibble(soil$name, as.data.frame(lapply(soil[, 2:6], as.numeric))) %>% 
  rename(name = `soil$name`) #преобразования исходных данных в рабочий tibble

par(mar = c(5, 10, 4, 2))
barplot(soil$Topsoil, names.arg = soil$name, 
        horiz = T,
        las = 1,
        main = 'Содержание органического углерода в верхнем слое почвы (0-30 см)\n по климатическим регионам IPCC (2010 г.)',
        xlab = 'PgC',
        col = rainbow(length(soil$name))) #построене горизонатального столбчатого графика

#-----------------------------------------------------------------------------------

#Построение графика распределения высот вулканов
volcano <- read_xlsx('GVP_Volcano_List_Holocene.xlsx', skip = 1)
par(mar = c(5, 4, 4, 2))

my_hist <- hist(volcano$`Elevation (m)`,
     breaks = seq(-6000, 8000, 500),
     plot = F) #Основные параметры гистограммы

my_color <- ifelse(my_hist$breaks < 0, 'cyan', 'salmon') #выбор цветов для подводных и надводных
plot(my_hist, col = my_color,      xlim = c(-6000, 8000),
     main = 'Распределение высот действующих вулканов мира\n (по данным Смитсоновского института, 2020 г.',
     xlab = 'Абсолютная отметка вершины над уровнем моря, м',
     ylab = 'Количество') #создание самой гистограммы

text1 <- c('Подводные', 'Надводные')
legend('right', text1,
       fill = c('cyan', 'salmon'),
       cex = 0.9,
       y.intersp = 0.8,
       text.width = strwidth(text1)[1]/2.2) #создание легенды для гистограммы
