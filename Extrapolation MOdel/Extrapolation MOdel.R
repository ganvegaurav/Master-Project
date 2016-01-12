# Creating a function 

Extrapolation_func <- function (data){
        for (i in 1:nrow(data)) {
                if (is.na(data[i,1])=="TRUE"){
                        data[i,1] <- data[i-1,1] + abs(data[i-1,1] - data[i-2,1])
                }
                
                
        }
        data <- data.frame(data)        
        data
}

sum(is.na(data_supply_temp[,2]))
sum(is.na(data_system_supply_temp[,1]))
sum(is.na(data_return_temp[,1]))
sum(is.na(data_sys_power[,1]))

supply_temp_extra <- Extrapolation_func(data_supply_temp)
sum(is.na(supply_temp_extra[,2]))
system_supply_temp_extra <- Extrapolation_func(data_system_supply_temp)
sum(is.na(system_supply_temp_extra[,1]))
data_return_temp_extra <- Extrapolation_func(data_return_temp)
sum(is.na(data_return_temp_extra[,1]))
data_sys_power_extra <- Extrapolation_func(data_sys_power)
sum(is.na(data_sys_power_extra[,1]))


data_extra <- cbind(supply_temp_extra,system_supply_temp_extra,data_return_temp_extra,data_sys_power_extra)

class(data_extra)
head(data_extra)
colnames(data_extra)[3] <- "System_supply_temperature"
colnames(data_extra)[4] <- "Return_Temperature"
colnames(data_extra)[5] <- "System_Power"
nrow(data_extra)
nrow(data)
head(data)

#####################################################################################

######################### Plot ###########################################
library(scales)

sdate <- '2014-03-24 00:00:00'
edate <- '2014-03-26 00:00:00'

start <- which(data_extra[,'Timestamp']== sdate )
end <- which(data_extra[,'Timestamp']== edate )

head(data,2)
head(data_extra,2)

sub_interval1 <- data[start:end,1:2]
nrow(sub_interval1)
head(sub_interval1)
sub_interval1[,3] <- data_extra[start:end,2]
colnames(sub_interval1)[3] <- ("extra_temp_setpoint")
sub_interval1[,4] <- data[start:end,3]
colnames(sub_interval1)[4] <- ("System_supply_temperature")
sub_interval1[,5] <- data_extra[start:end,3]
colnames(sub_interval1)[5] <- ("extra_Sys_temp")
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
        geom_point(data=sub_interval1[a,], aes(x = Timestamp, y = extra_temp_setpoint, color = 'red'),size=1)  +
        guides(colour=FALSE) +
        #scale_x_continuous(breaks=seq()) + 
        #xlab(day) +
        ylab('Supply temp. setp.') + 
        ggtitle('Missing value imputaion using Extrapolation Model')

plot2 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_supply_temperature)) +
        geom_point(data=sub_interval1[b,], aes(x = Timestamp, y = extra_Sys_temp, color = 'red'),size=1)  + 
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

# Plots for time interval between 18th Sep and 20th Sep



sdate <- '2014-09-18 00:00:00'
edate <- '2014-09-20 00:00:00'

start <- which(data_extra[,'Timestamp']== sdate )
end <- which(data_extra[,'Timestamp']== edate )

head(data,2)
head(data_extra,2)

sub_interval1 <- data[start:end,1:2]
nrow(sub_interval1)
head(sub_interval1)
sub_interval1[,3] <- data_extra[start:end,2]
colnames(sub_interval1)[3] <- ("extra_temp_setpoint")
sub_interval1[,4] <- data[start:end,3]
colnames(sub_interval1)[4] <- ("System_supply_temperature")
sub_interval1[,5] <- data_extra[start:end,3]
colnames(sub_interval1)[5] <- ("extra_Sys_temp")
sub_interval1[,6] <- data[start:end,4]
colnames(sub_interval1)[6] <- ("Return_Temperature")
sub_interval1[,7] <- data_extra[start:end,4]
colnames(sub_interval1)[7] <- ("extra_Return_Temp")
sub_interval1[,8] <- data[start:end,5]
colnames(sub_interval1)[8] <- ("System_Power")
sub_interval1[,9] <- data_extra[start:end,5]
colnames(sub_interval1)[9] <- ("extra_System_Power")

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
        geom_point(data=sub_interval1[a,], aes(x = Timestamp, y = extra_temp_setpoint, color = 'red'),size=1)  +
        guides(colour=FALSE) +
        #scale_x_continuous(breaks=seq()) + 
        #xlab(day) +
        ylab('Supply temp. setp.') + 
        ggtitle('Missing value imputaion using Extrapolation Model')

plot2 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_supply_temperature)) +
        geom_point(data=sub_interval1[b,], aes(x = Timestamp, y = extra_Sys_temp, color = 'red'),size=1)  + 
        guides(colour=FALSE) +
        #xlab(day) +
        ylab('Sys. supply temp.')

plot3 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = Return_Temperature)) + #xlab(day) +
        geom_point(data=sub_interval1[c,], aes(x = Timestamp, y = extra_Return_Temp, color = 'red'),size=1)  + 
        guides(colour=FALSE) +
        ylab('Return Temp.')

plot4 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_Power)) + #xlab(day) +
        geom_point(data=sub_interval1[d,], aes(x = Timestamp, y = extra_System_Power, color = 'red'),size=1)  + 
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
# Measuring Error

MSE <- function(y, yhat) mean((y - yhat)^2)

Extra_MSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                        "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Extra_MSE[1,1] <- MSE(data_test[,2], data_extra[,2])
Extra_MSE[1,2] <- MSE(data_test[,3], data_extra[,3])
Extra_MSE[1,3] <- MSE(data_test[,4], data_extra[,4])
Extra_MSE[1,4] <- MSE(data_test[,5], data_extra[,5])
Extra_MSE[1,5] <- rowMeans(Extra_MSE[,1:4])
Extra_MSE


# RMSE 

RMSE <- function(y, yhat) sqrt(MSE(y, yhat))

Extra_RMSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                         "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Extra_RMSE[1,1] <- RMSE(data_test[,2], data_extra[,2])
Extra_RMSE[1,2] <- RMSE(data_test[,3], data_extra[,3])
Extra_RMSE[1,3] <- RMSE(data_test[,4], data_extra[,4])
Extra_RMSE[1,4] <- RMSE(data_test[,5], data_extra[,5])
Extra_RMSE[1,5] <- rowMeans(Extra_RMSE[,1:4])
Extra_RMSE

# RRSE

RRSE <- function(y, yhat) sqrt(sum((y - yhat)^2) / sum((y - mean(y))^2))

Extra_RRSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                         "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Extra_RRSE[1,1] <- RRSE(data_test[,2], data_extra[,2])
Extra_RRSE[1,2] <- RRSE(data_test[,3], data_extra[,3])
Extra_RRSE[1,3] <- RRSE(data_test[,4], data_extra[,4])
Extra_RRSE[1,4] <- RRSE(data_test[,5], data_extra[,5])
Extra_RRSE[1,5] <- rowMeans(Extra_RRSE[,1:4])
Extra_RRSE



