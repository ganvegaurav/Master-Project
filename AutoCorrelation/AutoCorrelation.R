x <- 0:100
y <- sin(x) + rnorm(length(x))
In other words, a sinusoidal time series with noise

acf(y, lag.max=length(y))

head(data)

acf(data$System_supply_temperature,lag.max=1000, na.action=na.pass)
?acf
