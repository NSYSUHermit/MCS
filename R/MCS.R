# Monte Carlo Simulation
MCS <- function(initial, mean, sd, steps = 2, paths = 5, log = FALSE) {
    if(log == "TRUE"){
      mean_log <- log(1+mean)
      sd_log <- log(1+sd)
      matrix <- matrix(rnorm(steps*paths, mean_log, sd_log), steps, paths)# 生成steps*paths個常態分佈的亂數
      return(data.frame(initial * exp(apply(matrix, 2, cumsum))))
    }
    else{
      matrix <- matrix(rnorm(steps*paths, 1+mean, 1+sd), steps, paths)# 生成steps*paths個常態分佈的亂數
      return(data.frame(initial + apply(matrix, 2, cumsum)))
    }

}

#Plot The MA
MAplot <- function(date,price,num_ma){
  library(forecast)
  library(ggplot2)
  data1 <- data.frame(price,date)
  names(data1) <- c("price","date")
  data1$MA <- ma(data1$price,order = num_ma)

  ggplot(data1,aes(x = date))+
    geom_line(aes(y = price, colour = 'Price'),size = 1.2)+
    geom_line(aes(y = MA, colour = 'MA'),size = 1.2)
}


#rate of return
RR <- function(data,type = "day"){

  data <- t(as.vector(data))
  if(type == "day"){
    rate <- vector()
    for(i in c(2:length(data))){
      ratio <- (data[i]-data[i-1])/data[i-1]
      rate[i-1] <- ratio
    }
  }
  else if(type == "week"){
    rate <- vector()
    for(i in c(1:floor(length(data)/5))){
      ratio <- (data[i+4*(i-1)+4]-data[i+4*(i-1)])/data[i+4*(i-1)]
      rate[i] <- ratio
    }
  }
  else if(type == "month"){
    rate <- vector()
    for(i in c(1:floor(length(data)/20))){
      ratio <- (data[i+19*(i-1)+19]-data[i+19*(i-1)])/data[i+19*(i-1)]
      rate[i] <- ratio
    }
  }
  else if(type == "season"){
    rate <- vector()
    for(i in c(1:floor(length(data)/60))){
      ratio <- (data[i+59*(i-1)+59]-data[i+59*(i-1)])/data[i+59*(i-1)]
      rate[i] <- ratio
    }
  }
  else if(type == "year"){
    rate <- vector()
    for(i in c(1:floor(length(data)/240))){
      ratio <- (data[i+239*(i-1)+239]-data[i+239*(i-1)])/data[i+239*(i-1)]
      rate[i] <- ratio
    }
  }
  return(rate)
}

