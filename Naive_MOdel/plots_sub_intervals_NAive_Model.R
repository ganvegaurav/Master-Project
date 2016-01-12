######################### Plot ###########################################
library(scales)

sdate <- '2014-03-24 00:00:00'
edate <- '2014-03-26 00:00:00'

start <- which(data_naive[,'Timestamp']== sdate )
end <- which(data_naive[,'Timestamp']== edate )

head(data,2)
head(data_naive,2)

sub_interval1 <- data[start:end,1:2]
nrow(sub_interval1)
head(sub_interval1)
sub_interval1[,3] <- data_naive[start:end,2]
colnames(sub_interval1)[3] <- ("Naive_temp_setpoint")
sub_interval1[,4] <- data[start:end,3]
colnames(sub_interval1)[4] <- ("System_supply_temperature")
sub_interval1[,5] <- data_naive[start:end,3]
colnames(sub_interval1)[5] <- ("Naive_Sys_temp")
sub_interval1[,6] <- data[start:end,4]
colnames(sub_interval1)[6] <- ("Return_Temperature")
sub_interval1[,7] <- data[start:end,5]
colnames(sub_interval1)[7] <- ("System_Power")

a <- is.na(sub_interval1[,2])
b <- is.na(sub_interval1[,4])

head(sub_interval1)
library("ggplot2")
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)


plot1 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = Supply_temperature_setpoint)) +
        geom_point(data=sub_interval1[a,], aes(x = Timestamp, y = Naive_temp_setpoint, color = 'red'),size=1)  +
        guides(colour=FALSE) +
        #scale_x_continuous(breaks=seq()) + 
        #xlab(day) +
        ylab('Supply temp. setp.') + 
        ggtitle('Missing value imputaion using Naive Model')

plot2 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_supply_temperature)) +
        geom_point(data=sub_interval1[b,], aes(x = Timestamp, y = Naive_Sys_temp, color = 'red'),size=1)  + 
        guides(colour=FALSE) +
        #xlab(day) +
        ylab('Sys. supply temp.')

plot3 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = Return_Temperature)) + #xlab(day) +
        ylab('Return Temp.')

plot4 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_Power)) + #xlab(day) +
        ylab('Sys. Power')

library("grid")
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 1)))
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(2, 1))
print(plot3, vp = vplayout(3, 1))
print(plot4, vp = vplayout(4, 1))



#################################################################################

# Plots for time interval between 18th Sep and 20th Sep



sdate <- '2014-09-18 00:00:00'
edate <- '2014-09-20 00:00:00'

start <- which(data_naive[,'Timestamp']== sdate )
end <- which(data_naive[,'Timestamp']== edate )

head(data,2)
head(data_naive,2)
sum(is.na(sub_interval1[,1]))
sub_interval1 <- data[start:end,1:2]
nrow(sub_interval1)
head(sub_interval1)
sub_interval1[,3] <- data_naive[start:end,2]
colnames(sub_interval1)[3] <- ("Naive_temp_setpoint")
sub_interval1[,4] <- data[start:end,3]
colnames(sub_interval1)[4] <- ("System_supply_temperature")
sub_interval1[,5] <- data_naive[start:end,3]
colnames(sub_interval1)[5] <- ("Naive_Sys_temp")
sub_interval1[,6] <- data[start:end,4]
colnames(sub_interval1)[6] <- ("Return_Temperature")
sub_interval1[,7] <- data_naive[start:end,4]
colnames(sub_interval1)[7] <- ("Naive_Return_Temp")
sub_interval1[,8] <- data[start:end,5]
colnames(sub_interval1)[8] <- ("System_Power")
sub_interval1[,9] <- data_naive[start:end,5]
colnames(sub_interval1)[9] <- ("Naive_System_Power")

head(sub_interval1)
a <- is.na(sub_interval1[,2])
b <- is.na(sub_interval1[,4])
c <- is.na(sub_interval1[,6])
d <- is.na(sub_interval1[,8])


head(sub_interval1)
library("ggplot2")
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)


plot1 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = Supply_temperature_setpoint)) +
        geom_point(data=sub_interval1[a,], aes(x = Timestamp, y = Naive_temp_setpoint, color = 'red'),size=1)  +
        guides(colour=FALSE) +
        #scale_x_continuous(breaks=seq()) + 
        #xlab(day) +
        ylab('Supply temp. setp.') + 
        ggtitle('Missing value imputaion using Naive Model')

plot2 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_supply_temperature)) +
        geom_point(data=sub_interval1[b,], aes(x = Timestamp, y = Naive_Sys_temp, color = 'red'),size=1)  + 
        guides(colour=FALSE) +
        #xlab(day) +
        ylab('Sys. supply temp.')

plot3 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = Return_Temperature)) + #xlab(day) +
        geom_point(data=sub_interval1[c,], aes(x = Timestamp, y = Naive_Return_Temp, color = 'red'),size=1)  + 
        guides(colour=FALSE) +
        ylab('Return Temp.')

plot4 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_Power)) + #xlab(day) +
        geom_point(data=sub_interval1[d,], aes(x = Timestamp, y = Naive_System_Power, color = 'red'),size=1)  + 
        guides(colour=FALSE) +
        ylab('Sys. Power')

library("grid")
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 1)))
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(2, 1))
print(plot3, vp = vplayout(3, 1))
print(plot4, vp = vplayout(4, 1))

##################################################################################
# Read the test data file 
data_test <- readCsvData("ReferenceData.csv")
nrow(data_test)
summary(data_test)
# Measuring Error

MSE <- function(y, yhat) mean((y - yhat)^2)

Naive_MSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                        "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Naive_MSE[1,1] <- MSE(data_test[,2], data_naive[,2])
Naive_MSE[1,2] <- MSE(data_test[,3], data_naive[,3])
Naive_MSE[1,3] <- MSE(data_test[,4], data_naive[,4])
Naive_MSE[1,4] <- MSE(data_test[,5], data_naive[,5])
Naive_MSE[1,5] <- rowMeans(Naive_MSE[,1:4])
Naive_MSE


# RMSE 

RMSE <- function(y, yhat) sqrt(MSE(y, yhat))

Naive_RMSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                        "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Naive_RMSE[1,1] <- RMSE(data_test[,2], data_naive[,2])
Naive_RMSE[1,2] <- RMSE(data_test[,3], data_naive[,3])
Naive_RMSE[1,3] <- RMSE(data_test[,4], data_naive[,4])
Naive_RMSE[1,4] <- RMSE(data_test[,5], data_naive[,5])
Naive_RMSE[1,5] <- rowMeans(Naive_RMSE[,1:4])
Naive_RMSE

# RRSE

RRSE <- function(y, yhat) sqrt(sum((y - yhat)^2) / sum((y - mean(y))^2))

Naive_RRSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                         "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Naive_RRSE[1,1] <- RRSE(data_test[,2], data_naive[,2])
Naive_RRSE[1,2] <- RRSE(data_test[,3], data_naive[,3])
Naive_RRSE[1,3] <- RRSE(data_test[,4], data_naive[,4])
Naive_RRSE[1,4] <- RRSE(data_test[,5], data_naive[,5])
Naive_RRSE[1,5] <- rowMeans(Naive_RRSE[,1:4])
Naive_RRSE

