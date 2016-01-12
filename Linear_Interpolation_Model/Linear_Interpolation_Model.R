# Test code 

head(data,2)
nrow(data)

Linear_data <- data.frame()
Linear_data <- data[,1]
head(Linear_data)
length(Linear_data)
Linear_data <- data.frame(Linear_data)
head(Linear_data)
colnames(Linear_data)[1] <- "Timestamp"
Linear_data[,2] <- approxfun(1:nrow(data), data[,2])(1:nrow(data))
head(Linear_data)
colnames(Linear_data)[2] <- "Supply Temp. setp"

Linear_data[,3] <- approxfun(1:nrow(data), data[,3])(1:nrow(data))
head(Linear_data)
colnames(Linear_data)[3] <- "System_supply_temperature"

Linear_data[,4] <- approxfun(1:nrow(data), data[,4])(1:nrow(data))
head(Linear_data)
colnames(Linear_data)[4] <- "Return_Temperature"

Linear_data[,5] <- approxfun(1:nrow(data), data[,5])(1:nrow(data))
head(Linear_data)
colnames(Linear_data)[5] <- "System_Power"

head(Linear_data)
nrow(Linear_data)
is.na(Linear_data)

summary(Linear_data)

#####################################################################################

######################### Plot ###########################################
library(scales)

sdate <- '2014-03-24 00:00:00'
edate <- '2014-03-26 00:00:00'

start <- which(Linear_data[,'Timestamp']== sdate )
end <- which(Linear_data[,'Timestamp']== edate )

head(data,2)
head(Linear_data,2)

sub_interval1 <- data[start:end,1:2]
nrow(sub_interval1)
head(sub_interval1)
sub_interval1[,3] <- Linear_data[start:end,2]
colnames(sub_interval1)[3] <- ("Linear_temp_setpoint")
sub_interval1[,4] <- data[start:end,3]
colnames(sub_interval1)[4] <- ("System_supply_temperature")
sub_interval1[,5] <- Linear_data[start:end,3]
colnames(sub_interval1)[5] <- ("Linear_Sys_temp")
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
        geom_point(data=sub_interval1[a,], aes(x = Timestamp, y = Linear_temp_setpoint, color = 'red'),size=1)  +
        guides(colour=FALSE) +
        #scale_x_continuous(breaks=seq()) + 
        #xlab(day) +
        ylab('Supply temp. setp.') + 
        ggtitle('Missing value imputaion using Linear Interpolation Model')

plot2 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_supply_temperature)) +
        geom_point(data=sub_interval1[b,], aes(x = Timestamp, y = Linear_Sys_temp, color = 'red'),size=1)  + 
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

start <- which(Linear_data[,'Timestamp']== sdate )
end <- which(Linear_data[,'Timestamp']== edate )

head(data,2)
head(Linear_data,2)

sub_interval1 <- data[start:end,1:2]
nrow(sub_interval1)
head(sub_interval1)
sub_interval1[,3] <- Linear_data[start:end,2]
colnames(sub_interval1)[3] <- ("Linear_temp_setpoint")
sub_interval1[,4] <- data[start:end,3]
colnames(sub_interval1)[4] <- ("System_supply_temperature")
sub_interval1[,5] <- Linear_data[start:end,3]
colnames(sub_interval1)[5] <- ("Linear_Sys_temp")
sub_interval1[,6] <- data[start:end,4]
colnames(sub_interval1)[6] <- ("Return_Temperature")
sub_interval1[,7] <- Linear_data[start:end,4]
colnames(sub_interval1)[7] <- ("Linear_Return_Temp")
sub_interval1[,8] <- data[start:end,5]
colnames(sub_interval1)[8] <- ("System_Power")
sub_interval1[,9] <- Linear_data[start:end,5]
colnames(sub_interval1)[9] <- ("Linear_System_Power")

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
        geom_point(data=sub_interval1[a,], aes(x = Timestamp, y = Linear_temp_setpoint, color = 'red'),size=1)  +
        guides(colour=FALSE) +
        #scale_x_continuous(breaks=seq()) + 
        #xlab(day) +
        ylab('Supply temp. setp.') + 
        ggtitle('Missing value imputaion using Linear Interpolation Model')

plot2 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_supply_temperature)) +
        geom_point(data=sub_interval1[b,], aes(x = Timestamp, y = Linear_Sys_temp, color = 'red'),size=1)  + 
        guides(colour=FALSE) +
        #xlab(day) +
        ylab('Sys. supply temp.')

plot3 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = Return_Temperature)) + #xlab(day) +
        geom_point(data=sub_interval1[c,], aes(x = Timestamp, y = Linear_Return_Temp, color = 'red'),size=1)  + 
        guides(colour=FALSE) +
        ylab('Return Temp.')

plot4 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_Power)) + #xlab(day) +
        geom_point(data=sub_interval1[d,], aes(x = Timestamp, y = Linear_System_Power, color = 'red'),size=1)  + 
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

##################################################################################
# Measuring Error
ReferenceData.csv
file.exists("ReferenceData.csv")

data_test <- readCsvData("ReferenceData.csv")
nrow(data)
ncol(data)

MSE <- function(y, yhat) mean((y - yhat)^2)

Linear_MSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                           "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Linear_MSE[1,1] <- MSE(data_test[,2], Linear_data[,2])
Linear_MSE[1,2] <- MSE(data_test[,3], Linear_data[,3])
Linear_MSE[1,3] <- MSE(data_test[,4], Linear_data[,4])
Linear_MSE[1,4] <- MSE(data_test[,5], Linear_data[,5])
Linear_MSE[1,5] <- rowMeans(Linear_MSE[,1:4])
Linear_MSE


# RMSE 

RMSE <- function(y, yhat) sqrt(MSE(y, yhat))

Linear_RMSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                            "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Linear_RMSE[1,1] <- RMSE(data_test[,2], Linear_data[,2])
Linear_RMSE[1,2] <- RMSE(data_test[,3], Linear_data[,3])
Linear_RMSE[1,3] <- RMSE(data_test[,4], Linear_data[,4])
Linear_RMSE[1,4] <- RMSE(data_test[,5], Linear_data[,5])
Linear_RMSE[1,5] <- rowMeans(Linear_RMSE[,1:4])
Linear_RMSE

# RRSE

RRSE <- function(y, yhat) sqrt(sum((y - yhat)^2) / sum((y - mean(y))^2))

Linear_RRSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                            "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Linear_RRSE[1,1] <- RRSE(data_test[,2], Linear_data[,2])
Linear_RRSE[1,2] <- RRSE(data_test[,3], Linear_data[,3])
Linear_RRSE[1,3] <- RRSE(data_test[,4], Linear_data[,4])
Linear_RRSE[1,4] <- RRSE(data_test[,5], Linear_data[,5])
Linear_RRSE[1,5] <- rowMeans(Linear_RRSE[,1:4])
Linear_RRSE



