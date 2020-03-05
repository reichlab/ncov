##read_data 
library(dplyr)
library(forecast)
ts <- read.csv("forecast/covid_19_data.csv")

us_data <- ts[ts$Country.Region== "US",] %>% group_by(ObservationDate) %>% summarise(Confirmed=sum(Confirmed))
plot(us_data$Confirmed,type='l')

model_fit <- auto.arima(us_data$Confirmed)
plot(forecast(model_fit,h=10))
