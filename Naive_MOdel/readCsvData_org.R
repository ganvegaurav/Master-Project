readCsvData <- function(filename=""){
  if (filename=="") stop("No file given");
  if (!file.exists(filename)) stop("File not found");
  
  data <- read.table(file=filename
                     , header=TRUE
                     , sep=";"
                     , dec="."
                     , na.strings="NA"
                     , as.is=TRUE)
  data[,1] <- as.POSIXct(strptime(data[,1], "%Y-%m-%d %H:%M:%S"));
  
  return(data);
}

# readCSVData("../../data/challengeData.CSV")

file.exists("challengeData.CSV")

data <- readCsvData("challengeData.CSV")
nrow(data)
ncol(data)
head(data)
summary(data)
str(data)

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

# Function to build Naive model for estimating the missing values

naive_func <- function (data){
        for (i in 1:nrow(data)) {
                if (is.na(data[i,1])=="TRUE"){
                        data[i,1] <- data[i-1,1]
                }
                
        }
        data <- data.frame(data)        
        data
}

supply_temp_naive <- naive_func(data_supply_temp)
sum(is.na(supply_temp_naive[,2]))
system_supply_temp_naive <- naive_func(data_system_supply_temp)
sum(is.na(system_supply_temp_naive[,1]))
data_return_temp_naive <- naive_func(data_return_temp)
sum(is.na(data_return_temp_naive[,1]))
data_sys_power_naive <- naive_func(data_sys_power)
sum(is.na(data_sys_power_naive[,1]))

data_naive <- cbind(supply_temp_naive,system_supply_temp_naive,data_return_temp_naive,data_sys_power_naive)

class(data_naive)
head(data_naive)
colnames(data_naive)[3] <- "System_supply_temperature"
colnames(data_naive)[4] <- "Return_Temperature"
colnames(data_naive)[5] <- "System_Power"
nrow(data_naive)
nrow(data)
head(data)

