get_speed <- function(slope){
  return(6 * exp(-3.5 * abs(slope + 0.05)))
}

hiking_time <- function(profile){
  time = 0
  dh = diff(profile[, 2])
  dx = diff(profile[, 1])
  speed = get_speed(dh/dx)
  time = sqrt(dh * dh + dx * dx) / speed
  return(sum(time))
}

prof <- cbind(seq(0, 9000, 1000), 
              runif(10, 500, 1000))

hiking_time(prof / 1000)
plot(prof, type = 'l')
