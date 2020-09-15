#тема 2, задача 2
x1 <- 0
y1 <- 0
x2 <- 1000
y2 <- 1000

#применение runif - случайная выборка равноверное распределение
N <- 200
x <- runif(N, x1, x2)
y <- runif(N, y1, y2)
xy <- cbind(x, y)
plot(xy)
hist(x)
hist(y)

#нормальное распределение, подается среднее и СКО

x0 <- 1000
y0 <- 1000
sigma <- 500
x <- rnorm(N, x0, sigma)
y <- rnorm(N, y0, sigma)
