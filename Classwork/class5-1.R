data(quakes)
hist(quakes$depth, col = rgb(0.5, 1, 0.8), xlab = 'Depth', ylab = 'Amount',
     main = 'Histogramm\n of depth distibution')

hist(quakes$mag, col = 'orangered', xlab = 'Magnitude', ylab = 'Amount',
     main = 'Histogramm\n of magnitude distibution')

plot(quakes$depth, quakes$mag, pch = 19, 
     cex = 0.2*(log(quakes$stations)), 
     col = rgb(1, 0, 0, 0.2),
     main = 'Earthquakes', 
     xlab = 'Depth, km', 
     ylab = 'Magnitude, score')

