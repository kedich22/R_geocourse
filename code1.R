#введем координаты 2-ух пунктов
lat1 <- as.numeric(readline('Введите широту пункта отбытия: '))
long1 <- as.numeric(readline('Введите долготу пункта отбытия: '))
lat2 <- as.numeric(readline('Введите широту пункта прибытия: '))
long2 <- as.numeric(readline('Введите долготу пункта прибытия: '))
lat1r <- lat1*pi/180
long1r <- long1*pi/180
lat2r <- lat2*pi/180
long2r <- long2*pi/180
#Подсчет расстояния по ортодромии
distance <- 6371*(acos(sin(lat1r)*sin(lat2r)+cos(lat1r)*cos(lat2r)*cos(long2r-long1r)))
distance <- round(distance)
#Подсчет время полета
time <- distance/850
time <- round(time/0.5)*0.5
#Определение пересечения линий 0 и 180
if ((long1 > 0 & long1 < 90 & long2 < 0 & long2 > -90)|(long2 > 0 & long2 < 90 & long1 < 0 & long1 > -90)) {
  a <- ("Маршрут полета пересекает нулевой меридиан и не пересекает 180")
} else if ((long1 > 90 & long1 < 180 & long2 < -90 & long2 > -180)|(long2 > 90 & long2 < 180 & long1 < -90 & long1 > -180)) {
    a <- ("Маршрут полета не пересекает нулевой меридиан и пересекает 180")
} else {
  a <- ("Маршрут полета не пересекает нулевой меридиан и не пересекает 180")
}
#вывод итоговой информации
cat("Длина полета составила ", distance, " км, время в пути ~", time, "ч. ", a, sep="")
