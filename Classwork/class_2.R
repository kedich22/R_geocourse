temp <- c(-7.8, -6.9,	-2.2,	4.0,
          10.9, 15.6, 17.7,	16.2,	
          11.1,	5.7, 0.1,	-4.6)
dt <- diff(temp)
dt1 <- temp[1:11]
dt2 <- temp[2:12]
dt_new <- dt2 - dt1
identical(dt_new, dt)

#добавляем в конец вектора его первый элемент

temps_new <- c(temp, temp[1])
dt_fin <- diff(temps_new)

#ifelse векторизованный if
warming <- ifelse(dt_fin < 0, 'Похолодание', 'Потепление')

#ищем месяцы, которые удовлетворяют условию 

winter <- which(temp < 0)
summary(temp)
