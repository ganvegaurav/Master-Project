head(data)
nrow(data)
?approxfun
#install.packages("zoo")
library(zoo)
approx_data <- data.frame()
approx_data <- data[,1]
head(approx_data)
length(approx_data)
approx_data <- data.frame(approx_data)
head(approx_data)
colnames(approx_data)[1] <- "Timestamp"
approx_data[,2] <- na.approx(data[,2])
head(approx_data)
colnames(approx_data)[2] <- "Supply Temp. setp"

approx_data[,3] <- na.approx(data[,3])
head(approx_data)
colnames(approx_data)[3] <- "System_supply_temperature"

approx_data[,4] <- na.approx(data[,4])
head(approx_data)
colnames(approx_data)[4] <- "Return_Temperature"

approx_data[,5] <- na.approx(data[,5])
head(approx_data)
colnames(approx_data)[5] <- "System_Power"


##################################################################################
# Measuring Error
ReferenceData.csv
file.exists("ReferenceData.csv")

data_test <- readCsvData("ReferenceData.csv")
nrow(data)
ncol(data)

MSE <- function(y, yhat) mean((y - yhat)^2)

Approx_MSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                         "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Approx_MSE[1,1] <- MSE(data_test[,2], approx_data[,2])
Approx_MSE[1,2] <- MSE(data_test[,3], approx_data[,3])
Approx_MSE[1,3] <- MSE(data_test[,4], approx_data[,4])
Approx_MSE[1,4] <- MSE(data_test[,5], approx_data[,5])
Approx_MSE[1,5] <- rowMeans(Approx_MSE[,1:4])
Approx_MSE


# RMSE 

RMSE <- function(y, yhat) sqrt(MSE(y, yhat))

Approx_RMSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                          "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Approx_RMSE[1,1] <- RMSE(data_test[,2], approx_data[,2])
Approx_RMSE[1,2] <- RMSE(data_test[,3], approx_data[,3])
Approx_RMSE[1,3] <- RMSE(data_test[,4], approx_data[,4])
Approx_RMSE[1,4] <- RMSE(data_test[,5], approx_data[,5])
Approx_RMSE[1,5] <- rowMeans(Approx_RMSE[,1:4])
Approx_RMSE

# RRSE

RRSE <- function(y, yhat) sqrt(sum((y - yhat)^2) / sum((y - mean(y))^2))

Approx_RRSE <- data.frame("Supply Temp. setp."= numeric(0),"Supply Temp."= numeric(0),
                          "Return temp" = numeric(0), "System Power" = numeric(0),"Mean" = numeric(0))

Approx_RRSE[1,1] <- RRSE(data_test[,2], Approx_data[,2])
Approx_RRSE[1,2] <- RRSE(data_test[,3], Approx_data[,3])
Approx_RRSE[1,3] <- RRSE(data_test[,4], Approx_data[,4])
Approx_RRSE[1,4] <- RRSE(data_test[,5], Approx_data[,5])
Approx_RRSE[1,5] <- rowMeans(Approx_RRSE[,1:4])
Approx_RRSE



