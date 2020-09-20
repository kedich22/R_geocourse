#создание исходного датафрейма 
num <- c(1:7)
city <- c('Орел', 'Калуга', 'Серпухов', 'Коломна',
          'Рязань', 'Муром', 'Нижний Новгород')
lat <- c(52.938023, 54.505862, 54.883524, 55.070474, 54.651824,
         55.576834, 56.210471)
long <- c(36.065684, 36.233586, 37.418129, 38.830827, 39.806943,
          42.072086, 43.872036)
head <- c(111, 391, 522, 645, 801, 1285, 1500)
mouth <- 1500 - head
level <- c(146.31, 116.72, 107.54, 100.26, 93.41, 73.27, 62)
cities <- data.frame(num, city, lat, long, head, mouth, level)

#создание расчетного датафрейма
lat <- lat * pi / 180
long <- long * pi / 180

sections <- data.frame("1st" = 1:6, "2nd" = 2:7, "drop" = -(cities$level[1:6] - cities$level[2:7]),
                       "dec" = (cities$level[1:6] - cities$level[2:7]) / (cities$head[2:7] - cities$head[1:6]),
                       "sinus" = (cities$head[2:7] - cities$head[1:6]) / 
                         (6371 * (acos(sin(lat[1:6]) * sin(lat[2:7]) + cos(lat[1:6])
                                      * cos(lat[2:7]) * cos(long[2:7] - long[1:6]))))) 

#расчет городов
a <-as.numeric(readline("Введите расстояние: "))
b <- vector(length = 7)
for (i in 1:length(cities$head)) {
  if (a > cities$head[i]) {
    b[i] <- F 
  }
  else {
    b[i] <- T
  }
} 
idx <- match(T, b)
two <- cities$city[idx]
one <- cities$city[idx-1]

#расчет высоты
newlev <- (cities$head[idx-1] - a) * (cities$level[idx] - cities$level[idx-1]) / (cities$head[idx] - cities$head[idx-1])
newlev <- cities$level[idx-1] + newlev

#вывод результатов
cat("Вы находитесь между городами ", one,' и ', two, ", пройдено ", round(a / 1500 * 100), "% маршрута, ваша примерная высота - ", round(newlev), " м.", sep = "")
