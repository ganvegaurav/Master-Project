# Subsetting data as per variables inorder to fit a naive model

data_supply_temp <- data[,1:2]
data_system_supply_temp <- data.frame(data[,3])
data_return_temp <- data.frame(data[,4])
data_sys_power <- data.frame(data[,5])
nrow(data_supply_temp)

sum(is.na(data_supply_temp[,2]))
sum(is.na(data_system_supply_temp[,1]))
sum(is.na(data_return_temp[,1]))
sum(is.na(data_sys_power[,1]))

# Creating a function 

Weighted_Avg_func <- function (data){
        for (i in 1:nrow(data)) {
                if (is.na(data[i,1])=="TRUE"){
                        data[i,1] <- (30*data[i-1,1] + 20*data[i-2,1] + 15*data[i-3,1] + 10*data[i-4,1] + 5*data[i-5,1])/80
                }
                
                
        }
        data <- data.frame(data)        
        data
}

supply_temp_weighted <- Weighted_Avg_func(data_supply_temp)
sum(is.na(supply_temp_weighted[,2]))
system_supply_temp_weighted <- Weighted_Avg_func(data_system_supply_temp)
sum(is.na(system_supply_temp_weighted[,1]))
data_return_temp_weighted <- Weighted_Avg_func(data_return_temp)
sum(is.na(data_return_temp_weighted[,1]))
data_sys_power_weighted <- Weighted_Avg_func(data_sys_power)
sum(is.na(data_sys_power_weighted[,1]))

data_weighted <- cbind(supply_temp_weighted,system_supply_temp_weighted,data_return_temp_weighted,data_sys_power_weighted)

class(data_weighted)
head(data_weighted)
colnames(data_weighted)[3] <- "System_supply_temperature"
colnames(data_weighted)[4] <- "Return_Temperature"
colnames(data_weighted)[5] <- "System_Power"
nrow(data_weighted)
nrow(data)

#####################################################################################

######################### Plot ###########################################
library(scales)

sdate <- '2014-03-24 00:00:00'
edate <- '2014-03-26 00:00:00'

start <- which(data_weighted[,'Timestamp']== sdate )
end <- which(data_weighted[,'Timestamp']== edate )

head(data,2)
head(data_weighted,2)

sub_interval1 <- data[start:end,1:2]
nrow(sub_interval1)
head(sub_interval1)
sub_interval1[,3] <- data_weighted[start:end,2]
colnames(sub_interval1)[3] <- ("weighted_temp_setpoint")
sub_interval1[,4] <- data[start:end,3]
colnames(sub_interval1)[4] <- ("System_supply_temperature")
sub_interval1[,5] <- data_weighted[start:end,3]
colnames(sub_interval1)[5] <- ("weighted_Sys_temp")
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
        geom_point(data=sub_interval1[a,], aes(x = Timestamp, y = weighted_temp_setpoint, color = 'red'),size=1)  +
        guides(colour=FALSE) +
        #scale_x_continuous(breaks=seq()) + 
        #xlab(day) +
        ylab('Supply temp. setp.') + 
        ggtitle('Missing value imputaion using Weighted Average Model')

plot2 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_supply_temperature)) +
        geom_point(data=sub_interval1[b,], aes(x = Timestamp, y = weighted_Sys_temp, color = 'red'),size=1)  + 
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

start <- which(data_weighted[,'Timestamp']== sdate )
end <- which(data_weighted[,'Timestamp']== edate )

head(data,2)
head(data_weighted,2)

sub_interval1 <- data[start:end,1:2]
nrow(sub_interval1)
head(sub_interval1)
sub_interval1[,3] <- data_weighted[start:end,2]
colnames(sub_interval1)[3] <- ("weighted_temp_setpoint")
sub_interval1[,4] <- data[start:end,3]
colnames(sub_interval1)[4] <- ("System_supply_temperature")
sub_interval1[,5] <- data_weighted[start:end,3]
colnames(sub_interval1)[5] <- ("weighted_Sys_temp")
sub_interval1[,6] <- data[start:end,4]
colnames(sub_interval1)[6] <- ("Return_Temperature")
sub_interval1[,7] <- data_weighted[start:end,4]
colnames(sub_interval1)[7] <- ("weighted_Return_Temp")
sub_interval1[,8] <- data[start:end,5]
colnames(sub_interval1)[8] <- ("System_Power")
sub_interval1[,9] <- data_weighted[start:end,5]
colnames(sub_interval1)[9] <- ("weighted_System_Power")

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
        geom_point(data=sub_interval1[a,], aes(x = Timestamp, y = weighted_temp_setpoint, color = 'red'),size=1)  +
        guides(colour=FALSE) +
        #scale_x_continuous(breaks=seq()) + 
        #xlab(day) +
        ylab('Supply temp. setp.') + 
        ggtitle('Missing value imputaion using weighted Average Model')

plot2 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_supply_temperature)) +
        geom_point(data=sub_interval1[b,], aes(x = Timestamp, y = weighted_Sys_temp, color = 'red'),size=1)  + 
        guides(colour=FALSE) +
        #xlab(day) +
        ylab('Sys. supply temp.')

plot3 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = Return_Temperature)) + #xlab(day) +
        geom_point(data=sub_interval1[c,], aes(x = Timestamp, y = weighted_Return_Temp, color = 'red'),size=1)  + 
        guides(colour=FALSE) +
        ylab('Return Temp.')

plot4 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_Power)) + #xlab(day) +
        geom_point(data=sub_interval1[d,], aes(x = Timestamp, y = weighted_System_Power, color = 'red'),size=1)  + 
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

Weighted_MSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                        "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Weighted_MSE[1,1] <- MSE(data_test[,2], data_weighted[,2])
Weighted_MSE[1,2] <- MSE(data_test[,3], data_weighted[,3])
Weighted_MSE[1,3] <- MSE(data_test[,4], data_weighted[,4])
Weighted_MSE[1,4] <- MSE(data_test[,5], data_weighted[,5])
Weighted_MSE[1,5] <- rowMeans(Weighted_MSE[,1:4])
Weighted_MSE


# RMSE 

RMSE <- function(y, yhat) sqrt(MSE(y, yhat))

Weighted_RMSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                         "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Weighted_RMSE[1,1] <- RMSE(data_test[,2], data_weighted[,2])
Weighted_RMSE[1,2] <- RMSE(data_test[,3], data_weighted[,3])
Weighted_RMSE[1,3] <- RMSE(data_test[,4], data_weighted[,4])
Weighted_RMSE[1,4] <- RMSE(data_test[,5], data_weighted[,5])
Weighted_RMSE[1,5] <- rowMeans(Weighted_RMSE[,1:4])
Weighted_RMSE

# RRSE

RRSE <- function(y, yhat) sqrt(sum((y - yhat)^2) / sum((y - mean(y))^2))

Weighted_RRSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                         "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Weighted_RRSE[1,1] <- RRSE(data_test[,2], data_weighted[,2])
Weighted_RRSE[1,2] <- RRSE(data_test[,3], data_weighted[,3])
Weighted_RRSE[1,3] <- RRSE(data_test[,4], data_weighted[,4])
Weighted_RRSE[1,4] <- RRSE(data_test[,5], data_weighted[,5])
Weighted_RRSE[1,5] <- rowMeans(Weighted_RRSE[,1:4])
Weighted_RRSE




