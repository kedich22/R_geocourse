# функция високосный год

is_leap <- function(year) {
  return(year %% 400 == 0 || 
           ((year %% 4 == 0) && (year %% 100 != 0)))
}

y = as.integer(readline('Введите год: '))
if (is_leap(y)) {
  cat('Високосный год')
} else {
  cat('Обычный год')
}