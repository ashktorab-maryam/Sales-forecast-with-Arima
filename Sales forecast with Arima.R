
# install following packages if not already installed.
#install.packages("readxl")

library(readxl)
library(ggplot2)
library(dplyr)
library(xts)
library(lubridate)
library(forecast)
library(tseries)
library(astsa)


sales = read.csv("UseCase.csv")

#Section One
#Data Cleaning

colnames(sales)[1:3] = c('Region','ProductCategory','Product')

str(sales)
head(sales)
sales[,1:3] = apply(sales[,1:3], MARGIN = 2, as.character)


sales = sales %>%   #Aggregating duplicates rows
  group_by(Region,ProductCategory,Product)%>%
  summarise_all(funs(sum))

# Data Exploring (Charts and Tables)
# Table 1 categories and regions
salesgroups = sales[,1:3]
addmargins(table(salesgroups$Region,salesgroups$ProductCategory))
cat('This table shows the number of products in each region and each product category')


# Table 2 categories and products
s2 = table(salesgroups$ProductCategory,salesgroups$Product)
nonzero_finder = function(a){sum(a!=0)}
apply(s2,MARGIN = 1,nonzero_finder)
cat("This table shows the number of distinct products in each category")


#Yearly Sales and Annual Growth Rate

sales000=sales[,c(-1,-2,-3)]
salesTTT = sales000 %>%
  summarise_all(funs(sum)) %>%
  xts(x = t(.[,2:ncol(.)]),order.by = as.yearmon(colnames(.)[2:ncol(.)], "%b.%y")) 

salesTTTY = aggregate(salesTTT,as.integer(format(index(salesTTT),"%Y")),sum)
salesTTTY=salesTTTY[c(-7),]
par( mex = 0.8, cex = 0.8)
barplot(salesTTTY/1000000,col=c("darkblue"),ylab='Sales (Quantity in Millions)',xlab='Time (Years)',ylim= c(0,2000))
title('Annual Sales')


growthTTT = na.omit((salesTTTY/lag(salesTTTY) -1)*100)
plot(growthTTT,type = 'l',ylab = 'Annual Growth Rate (Percentage)',xlab='Time (Years)')

# 1 Plotting slaes by Regions

options(scipen=10)
salesT00 = sales[,c(-2,-3)]
salesT00 = salesT00 %>%
  group_by(Region)%>%
  summarise_all(funs(sum)) %>%
  xts(x = t(.[,2:ncol(.)]),order.by = as.yearmon(colnames(.)[2:ncol(.)], "%b.%y"))
colnames(salesT00)= c('A','B','C')
salesT00Y = aggregate(salesT00,as.integer(format(index(salesT00),"%Y")),sum)
salesT00 = salesT00[c(-70,-71,-72,-73,-74,-75,-76),]
par( mex = 0.8, cex = 0.8)
plot(as.zoo(salesT00/1000000),xlab='Time ',ylab='Sales (Quantity in Millions)',main='',screens = 1,col = c('Black','Red','Green'),lwd = 3)

title('Monthly Sales by Regions ')
legend(x = "topright", legend = colnames(salesT00),
       col = c("black", "red","green"),
       lty = c(1, 1),cex = 0.6)

barplot(window(salesT00Y,start ='2013',end = '2016')/1000000,
        col = c('chartreuse2','dodgerblue3','brown2'),beside=TRUE,
        xlab ='Time (Years)',ylab = 'Sales (Quantity in Millions)',ylim = c(0,800))


title('Annual Sales by Region')
legend(x = "topright",
       legend = colnames(salesT00Y),
       col = c('chartreuse2','dodgerblue3','brown2'),
       lty = c(1, 1),cex = 0.9)

# 2 Ploting sales by Product Category
sales0T0=sales[,c(-1,-3)]
sales0T0 = sales0T0 %>%
  group_by(ProductCategory)%>%
  summarise_all(funs(sum)) %>%
  xts(x = t(.[,2:ncol(.)]),order.by = as.yearmon(colnames(.)[2:ncol(.)], "%b.%y"))

colnames(sales0T0)= c('A','B','C','D','E','F','G','H','I','J')

sales0T0Y = aggregate(sales0T0,as.integer(format(index(sales0T0),"%Y")),sum)
par( mex = 0.8, cex = 0.8)

frame = window(sales0T0,start ='Dec 2011',end = 'Sep 2016')/1000000
plot(as.zoo(frame),xlan='Time',ylab='Sales (Quantity in Millions)',main='',
     col =c("black","red","green","darkblue","darkred","blue","yellow","gray","cyan","lavender", "cornsilk"),
     screens = 1,lwd = 3,xlab = 'time')

title('Monthly Sales by Product Catgory')
legend(x = "topright", legend = colnames(sales0T0),
       col =c("black","red","green","darkblue","darkred","blue","yellow","gray","cyan","lavender", "cornsilk")
       ,lty = c(1, 1),cex = 0.6 )

barplot(window(sales0T0Y,start ='2013',end = '2016')/1000000,
        col =c("black","red","green","darkblue","darkred","lightblue","yellow","gray","lightcyan","lavender"),
        beside=TRUE,ylim= c(0,700),ylab='Sales (Quantity in Millions)',xlab='Time')
title('Annual Sales by Product Category',cex = 1)

legend(x = "topright", legend = colnames(sales0T0Y),col =c("black","red","green","darkblue","darkred","lightblue","yellow","gray","lightcyan","lavender")
       ,lty = c(1, 1),cex = 0.7 )


# 3 polt sales by Products

sales00T=sales[,c(-1,-2)]


sales00TY = sales00T %>%
  group_by(Product)%>%
  summarise_all(funs(sum))

colnames(sales00T)= c('A', 'A1', 'A2', 'A3', 'B', 'C', 'D', 'E', 'F', 'G', 'GD','H',
                      'I','J', 'K', 'L','M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
                      'U', 'V', 'W', 'X', 'Y', 'Z')

sales00TSUM=mutate(sales00TY,sum =rowSums(sales00TY[,2:77]))
sales00TSUM = sales00TSUM[,c(1,78)]
sales00TSUM = as.data.frame(sales00TSUM)
sales00TSUM = sales00TSUM%>%
  group_by(Product)%>%
  summarise_all(funs(sum))
sales00TSUM = as.vector(sales00TSUM)
par( mex = 0.15, cex = 0.15)
pie(dfProductSum$sum,labels = dfProductSum$Product,cex=6)
title(main = list('Sales Product Pie Chart', cex = 10),line = -50)




##Section two
##Forcasting the Next 12 Months on Producat Category "C"

remove_zeros = function(df){
  df[df == 0] = NA
  df[complete.cases(df)]
}

# Reading and Aggreagting data 
sales0A0 = sales %>%
  filter(ProductCategory == "C") %>%
  xts(x = t(.[,4:ncol(.)]),order.by = as.yearmon(colnames(.)[4:ncol(.)], "%b.%y")) %>%
  apply.monthly(FUN = sum)

sales0A0 = remove_zeros(sales0A0)      # remove zeros from the end
sales0A0 = sales0A0[-length(sales0A0)]  # remove last row because most of observations are zero
plot(sales0A0/1000000, main= "Total Monthly Sales in Product Category C",type = "o", ylab = "Quantity in Millions")

sales0A0_ts = as.ts(sales0A0)

# predict slaes with different models for future 12 months and creat plot for each
ses(sales0A0_ts/1000000 , h=12) %>% autoplot(type = "o", ylab = "Quantity in Millions", xlab = "Horizon (Years)") # simple exponential smoothing
holt(sales0A0_ts , h=12) %>% autoplot(type = "o", ylab = "Quantity in Millions", xlab = "Horizon (Years)") # simple exponential with additive trend and no seasonality
holt(sales0A0_ts ,damped = T, h=12) %>% autoplot(type = "o", ylab = "Quantity in Millions", xlab = "Horizon (Years)") # simple exponential with additive trend and no seasonality
snaive(sales0A0_ts , h = 12) %>% autoplot(type = "o", ylab = "Quantity in Millions", xlab = "Horizon (Years)")
hw(sales0A0_ts ,seasonal = "additive", h=12) %>% autoplot(type = "o", ylab = "Quantity in Millions", xlab = "Horizon (Years)")
hw(sales0A0_ts ,seasonal = "multiplicative", h=12) %>% autoplot(type = "o", ylab = "Quantity in Millions", xlab = "Horizon (Years)")
ets(sales0A0_ts) # just to show the best exponential model
sales0A0_ts %>% ets() %>% forecast(h=12) %>% autoplot(type = "o", ylab = "Quantity in Millions", xlab = "Horizon (Years)")


# sales0A0_ts %>% ets() %>% forecast(h=12) %>% checkresiduals() # residuals should look like white noise

##### ARIMA model #####

# take logs to analyze and predict growth rate, differences to make it stationary and remove seasonality
sales0A0_d1 = diff(log(sales0A0))
plot(sales0A0_d1)
sales0A0_d1d12 = diff(sales0A0_d1,12)
plot(sales0A0_d1d12)

## ACF and PACF for MA and AR orders
acf2(series = sales0A0_d1d12[complete.cases(sales0A0_d1d12)],max.lag = 24, main = "ACF and PACF")


# some sample Seasonal ARIMA models
sarima(xdata = log(sales0A0),0,1,0,0,1,0,12)
sarima.for(xdata = log(sales0A0),0,1,0,0,1,0,12,n.ahead = 12)

sarima(xdata = log(sales0A0),2,1,1,0,1,0,12)
sarima.for(log(sales0A0),2,1,1,0,1,0,12,n.ahead = 12)
# sarima(xdata = log(sales0A0),2,1,1,0,1,0,12,details = FALSE)$BIC  #the lower BIC the better

pred_0A0 = sarima.for(log(sales0A0),0,0,0,0,1,0,12,n.ahead = 12)
ts.plot(sales0A0_ts/1000000, exp(pred_0A0$pred)/1000000, type = "o", xlab = "Horizon (years)", ylab = "Total Sales in Product Category C (Millions)", col = c('black',3))


### running ARIMA models for different parameters to find the optimal combination. you sac set max higher but be aware that it takes longer
max.p = 2
max.d = 1
max.q = 2
max.P = 1
max.D = 1
max.Q = 1

# we set seasonality to 12 instead of checking for different values. Higher orders may be better but they come at cost of 
# more parameters to estimate in the first place and not so much improved predictibility power assuming limited observations available

param_mat = expand.grid(list(p = 0:max.p, d= 0:max.d, q = 0:max.q, P = 0:max.P, D = 0:max.D, Q = 0:max.Q))
for (i in 1:nrow(param_mat)){
  a = try(sarima(log(sales0A0),p = param_mat[i,1],d = param_mat[i,2],q = param_mat[i,3],P = param_mat[i,4],D = param_mat[i,5],Q = param_mat[i,6],S = 12,details = FALSE),silent = TRUE)
  param_mat[i,7] = ifelse(is.list(a),yes = a$AIC,no = NA)
}

# the for above check different models and chooses the best with lowest AIC. it is somehow an auto arima function that we have written ourselves.
param_mat[which.min(param_mat[,7]),] # best Arima model
i = which.min(param_mat[,7])
sarima(log(sales0A0),p = param_mat[i,1],d = param_mat[i,2],q = param_mat[i,3],P = param_mat[i,4],D = param_mat[i,5],Q = param_mat[i,6],S = 12)
model_arima = arima(log(sales0A0),order = c(param_mat[i,1],param_mat[i,2], param_mat[i,3]), seasonal = list(order = c(param_mat[i,4], param_mat[i,5], param_mat[i,6]),period = 12),method = "ML")
plot(forecast(model_arima,12))
a = forecast(model_arima,12)

ts.plot(sales0A0_ts/1000000, exp(a$mean)/1000000, type = "o", xlab = "Horizon (years)", ylab = "Total Sales in Product Category C (Millions)", col = c('black',3), main = a$method)

prediction_values_12_month = data.frame(C0 = a$mean)

###### comparing accuracy of models

hw(as.ts(window(sales0A0, end = "Aug 2015")) , h=12) %>% accuracy(sales0A0_ts)
#a = arima(as.ts(window(log(sales0A0), end = "Aug 2015")),order = c(param_mat[i,1],param_mat[i,2], param_mat[i,3]), seasonal = list(order = c(param_mat[i,4], param_mat[i,5], param_mat[i,6]),period = 12),method = "ML") %>% forecast(12)
#a$fitted %>% exp() %>% accuracy(sales0A0_ts)
a = arima(as.ts(window(log(sales0A0), end = "Aug 2015")),order = c(1,1,0), seasonal = list(order = c(0, 1, 0),period = 12),method = "ML") %>% forecast(12)
a$fitted %>% exp() %>% accuracy(sales0A0_ts)
as.ts(window(sales0A0, end = "Aug 2015")) %>% ets() %>% forecast() %>% accuracy(sales0A0_ts)
as.ts(window(sales0A0, end = "Aug 2015")) %>% snaive() %>% forecast() %>% accuracy(sales0A0_ts)
as.ts(window(sales0A0, end = "Aug 2015")) %>% hw(seasonal = "additive") %>% forecast() %>% accuracy(sales0A0_ts)
as.ts(window(sales0A0, end = "Aug 2015")) %>% hw(seasonal = "multiplicative") %>% forecast() %>% accuracy(sales0A0_ts)
as.ts(window(sales0A0, end = "Aug 2015")) %>% ses() %>% forecast() %>% accuracy(sales0A0_ts)
as.ts(window(sales0A0, end = "Aug 2015")) %>% holt() %>% forecast() %>% accuracy(sales0A0_ts)
as.ts(window(sales0A0, end = "Aug 2015")) %>% holt(damped = TRUE) %>% forecast() %>% accuracy(sales0A0_ts)


sales0A0_d1 = diff(sales0A0)
sales0A0_d1d12 = diff(sales0A0_d1,12)
max.p = 1
max.d = 1
max.q = 1
max.P = 1
max.D = 1
max.Q = 1
param_mat = expand.grid(list(p = 0:max.p, d= 0:max.d, q = 0:max.q, P = 0:max.P, D = 0:max.D, Q = 0:max.Q))
for (i in 1:nrow(param_mat)){
  a = try(sarima(sales0A0,p = param_mat[i,1],d = param_mat[i,2],q = param_mat[i,3],P = param_mat[i,4],D = param_mat[i,5],Q = param_mat[i,6],S = 12,details = FALSE),silent = TRUE)
  param_mat[i,7] = ifelse(is.list(a),yes = a$AIC,no = NA)
}
param_mat[which.min(param_mat[,7]),] # best Arima model
i = which.min(param_mat[,7])

a = arima(as.ts(window(sales0A0, end = "Aug 2015")),order = c(param_mat[i,1],param_mat[i,2], param_mat[i,3]), seasonal = list(order = c(param_mat[i,4], param_mat[i,5], param_mat[i,6]),period = 12),method = "ML") %>% forecast(12)
a$fitted %>% accuracy(sales0A0_ts)

# just to check what auto arima gives! which is not good!
# auto.arima(y = log(sales0A0), lambda = 0)




