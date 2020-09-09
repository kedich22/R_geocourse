#введем координаты 2-ух пунктов
lat1 <- as.numeric(readline('Введите широту пункта отбытия: '))
long1 <- as.numeric(readline('Введите долготу пункта отбытия: '))
lat2 <- as.numeric(readline('Введите широту пункта прибытия: '))
long2 <- as.numeric(readline('Введите долготу пункта прибытия: '))
k <- pi / 180
lat1r <- lat1 * k
long1r <- long1 * k
lat2r <- lat2 * k
long2r <- long2 * k

#Подсчет расстояния по ортодромии
distance <- 6371 * (acos(sin(lat1r) * sin(lat2r) + cos(lat1r) * cos(lat2r) * cos(long2r-long1r)))
distance <- round(distance)

#Подсчет время полета
time <- distance / 850
time <- round(time / 0.5) * 0.5

#Определение пересечения линий 0 и 180
if (long1 * long2 < 0) {
  if (abs(long1 + long2) > 180) {
    a <- ("Маршрут полета не пересекает нулевой меридиан и пересекает 180.")
  } else {
    a <- ("Маршрут полета пересекает нулевой меридиан и не пересекает 180")
  }
} else {
  a <- ("Маршрут полета не пересекает нулевой меридиан и не пересекает 180.")
}

#вывод итоговой информации
cat("Длина полета составила ", distance, " км, время в пути ~", time, "ч. ", a, sep="")
