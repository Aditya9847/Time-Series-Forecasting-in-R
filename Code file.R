#Business Analytics Case study 4
#Time Series Case Study - UK Outward Passenger Movement

#Setting the working directory
setwd("C:/Users/Aditya/Documents/R's/RBA/R Business Analytics Case Study4/UK Outward Passengers Movement")

#Importing data
require(readxl)
pas_data <- read_xlsx("C:/Users/Aditya/Documents/R's/RBA/R Business Analytics Case Study4/UK Outward Passengers Movement/UK Outward Passengers Movement.xlsx")

# Indexing Data
pd.ireland <- ts(pas_data$Ireland, start=c(1996,1), end=c(2005,4), frequency=4)
pd.remaining.eu <- ts(pas_data$`Other EU(Not Ireland)`, start=c(1996,1), end=c(2005,4), frequency=4)
pd.rest.eu.med <- ts(pas_data$`Rest of Europe and Med`, start=c(1996,1), end=c(2005,4), frequency=4)
pd.rest.world <- ts(pas_data$`Rest of World`, start=c(1996,1), end=c(2005,4), frequency=4)
pd.total <- ts(pas_data$Total, start=c(1996,1), end=c(2005,4), frequency=4)

# Graph of each variable
plot(pd.ireland)
plot(pd.remaining.eu)
plot(pd.rest.eu.med)
plot(pd.rest.world)
plot(pd.total)

# Using stl function (Decomposition method) to forecast
stl.ireland      <-  stl(pd.ireland,s.window="periodic")
stl.remaining.eu <-  stl(pd.remaining.eu,s.window="periodic")
stl.rest.eu.med  <-  stl(pd.rest.eu.med,s.window="periodic")
stl.rest.world   <-  stl(pd.rest.world,s.window="periodic")
stl.total        <-  stl(pd.total,s.window="periodic")

# Graph of decomposed variables
plot(stl.ireland)
plot(stl.remaining.eu)
plot(stl.rest.eu.med)
plot(stl.rest.world)
plot(stl.total)

# Checking accuracy of the model (STL function-decomposition)
accuracy(forecast(stl.ireland,h=2))
accuracy(forecast(stl.remaining.eu,h=2))
accuracy(forecast(stl.rest.eu.med,h=2))
accuracy(forecast(stl.rest.world,h=2))
accuracy(forecast(stl.total,h=2))

# Forecasting variables for next 2 years
require(forecast)
forecast(stl.ireland, h=8)
forecast(stl.remaining.eu,h=8)
forecast(stl.rest.eu.med,h=8)
forecast(stl.rest.world,h=8)
forecast(stl.total,h=8)

# Graph of forecasted variables by stl(decomposition)
plot(forecast(stl.ireland))
plot(forecast(stl.remaining.eu))
plot(forecast(stl.rest.eu.med))
plot(forecast(stl.rest.world))
plot(forecast(stl.total))

# Using ets function (smoothening method) to forecast
ets_ireland <- ets(pd.ireland)
ets_remaining.eu <- ets(pd.remaining.eu)
ets_rest.eu.med <- ets(pd.rest.eu.med)
ets_rest.world <- ets(pd.rest.world)
ets_total <- ets(pd.total)

# Checking accuracy of the model (ETS-smoothening)
accuracy(ets_ireland$fitted,pd.ireland)
accuracy(ets_remaining.eu$fitted,pd.remaining.eu)
accuracy(ets_rest.eu.med$fitted,pd.rest.eu.med)
accuracy(ets_rest.world$fitted,pd.rest.world)
accuracy(ets_total$fitted,pd.total)

# Forecasting variables for next 2 years
forecast(ets_ireland,h=8)
forecast(ets_remaining.eu,h=8)
forecast(ets_rest.eu.med,h=8)
forecast(ets_rest.world,h=8)
forecast(ets_total,h=8)

# Graph of forecasted values by ets function (Smoothening Technique)
plot(forecast(ets_ireland))
plot(forecast(ets_remaining.eu))
plot(forecast(ets_rest.eu.med))
plot(forecast(ets_rest.world))
plot(forecast(ets_total))

# Using auto.arima function to forecst
arima_ireland <- auto.arima(pd.ireland)
arima_remaining.eu <- auto.arima(pd.remaining.eu)
arima_rest.eu.med <- auto.arima(pd.rest.eu.med)
arima_rest.world <- auto.arima(pd.rest.world)
arima_total <- auto.arima(pd.total)

# Checking accuracy of the model (ARIMA)
accuracy(arima_ireland)
accuracy(arima_remaining.eu)
accuracy(arima_rest.eu.med)
accuracy(arima_rest.world)
accuracy(arima_total)

# Forecasting variables for next 2 years
forecast(arima_ireland,h=8)
forecast(arima_remaining.eu,h=8)
forecast(arima_rest.eu.med,h=8)
forecast(arima_rest.world,h=8)
forecast(arima_total,h=8)

# Graph of forecasted values
plot(forecast(arima_ireland))
plot(forecast(arima_remaining.eu))
plot(forecast(arima_rest.eu.med))
plot(forecast(arima_rest.world))
plot(forecast(arima_total))
