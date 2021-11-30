# ge(ngerate a sigle random series from GBM model
rGbmSingle <- function(len,start = 1,mu = 0,sigma = 0.1){
  series = rep(0,len)
  mu = mu - 0.5 * sigma^2
  walk = rnorm(len-1,mean = mu,sd = sigma)
  series[1] = log(start)
  for(ii in 2:len){
    series[ii] = series[ii-1] + mu + sigma * walk[ii-1]
  }
  series = round(exp(series),digits = 2)
  return(series)
}

# gengerate multiply random serieses from GBM model and name
# name is a vector
# n is a vector,n = c(nlines,len)
rGbm <- function(n,name,time,start = 1,mu = 0,sigma = 0.1){
  library(xts)
  # 序列预定义
  series = matrix(rep(0,n * length(date)),ncol = n,nrow = length(date))
  colnames(series) = name
  # 参数向量化
  rep_fun <- function(x){if(length(x) == 1L) return(rep(x,length(date)))}
  start = rep_fun(start)
  mu = rep_fun(mu)
  sigma = rep_fun(sigma)
  # 序列计算
  for(ii in seq_along(name)){
    series[,name[ii]] = rGbmSingle(length(date),start[ii],
                                   mu[ii] - 0.5 * sigma[ii]^2,sigma[ii])
  }
  series = xts(series,time)
  return(transform(series))
}

# 生成一段日期
library(lubridate)
date = as.Date("2015-01-01") + months(1:30)
# example
# n is a vector,n = c(nlines,len)
st <- rGbm(3,c("bear","tiger","swan"),date,start = 1,mu = 0.1,sigma = 0.2)
st


# ggplot绘图
library(tidyverse)
library(ggthemes)
myst <- as_tibble(st) %>% add_column(time = date)
p <- ggplot(data = myst) + geom_line(mapping = aes(time,tiger),color = "red",show.legend = "tiger") + 
  geom_line(mapping = aes(time,bear),color = "blue",show.legend = "bear") + 
  geom_line(mapping = aes(time,swan),color = "green",show.legend = "swan")
p + theme_few() + 
  scale_x_date(date_labels = "%Y/%m",date_breaks = "3 months") +
  xlab("")+ylab("")


