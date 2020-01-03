



theta
data = data.frame(x = c(-3, 2, 3, -5),
                  y = c(-2, -3, 2, 6),
                  z = NA,
                  loc = NA,
                  theta = NA,
                  total_theta =NA)
data

for(i in 1:nrow(data)) {
  data$z[i] = sqrt((data$x[i]^2) + (data$y[i]^2))
}
data

rad = 57.295779513082323

for(i in 1:nrow(data)) {
  if(data$x[i] >0 & data$y[i] > 0) {
    data$loc[i] = "loc_1"
    data$theta[i] = atan2(data$y[i], data$x[i]) * rad
    data$total_theta[i] = data$theta 
  }
  else if(data$x[i] < 0 & data$y[i] > 0) {
    data$loc[i] = "loc_2"
    data$theta[i] = atan2(data$x[i], abs(data$y[i])) * rad
    data$total_theta[i] = data$theta +90
  }
  else if(data$x[i] < 0 & data$y[i] < 0) {
    data$loc[i] = "loc_3"
    data$theta[i] = atan2(abs(data$y[i]), abs(data$x[i])) * rad
    data$total_theta[i] = data$theta + 180
  }
  else if(data$x[i] > 0 & data$y[i] < 0){
    data$loc[i] = "loc_4"
    data$theta[i] = atan2(abs(data$x[i]), data$y[i]) * rad
    data$total_theta[i] = data$theta + 270
  }
  else {
    data$loc[i] = "NA"
    data$theta[i] = NA
    data$total_theta[i] = NA
  }
}

data






data2 = data.frame(x = sample(-10:10, 100 , replace = T),
                   y = sample(-10:10, 100 , replace = T),
                   z = NA,
                   loc = NA,
                   theta = NA,
                   total_theta = NA)

data2

for(i in 1:nrow(data2)) {
  data2$z[i] = sqrt((data2$x[i]^2) + (data2$y[i]^2))
}
data2

rad = 57.295779513082323

for(i in 1:nrow(data2)) {
  if(data2$x[i] >0 & data2$y[i] > 0) {
    data2$loc[i] = "loc_1"
    data2$theta[i] = atan2(data2$y[i], data2$x[i]) * rad
    data2$total_theta[i] = data2$theta 
  }
  else if(data2$x[i] < 0 & data2$y[i] > 0) {
    data2$loc[i] = "loc_2"
    data2$theta[i] = atan2(data2$x[i], abs(data2$y[i])) * rad
    data2$total_theta[i] = data2$theta +90
  }
  else if(data2$x[i] < 0 & data2$y[i] < 0) {
    data2$loc[i] = "loc_3"
    data2$theta[i] = atan2(abs(data2$y[i]), abs(data2$x[i])) * rad
    data2$total_theta[i] = data2$theta + 180
  }
  else if(data2$x[i] > 0 & data2$y[i] < 0){
    data2$loc[i] = "loc_4"
    data2$theta[i] = atan2(abs(data2$x[i]), data2$y[i]) * rad
    data2$total_theta[i] = data2$theta + 270
  }
  else {
    data2$loc[i] = "NA"
    data2$theta[i] = NA
    data2$total_theta[i] = NA
  }
}


data2