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
readCsvData
head(readCsvData("F:/OTAGO/MBusDataSc/APPLIED PROJECT/Final Project/Industrial Challenge/ResourcePackage/data/challengeData.CSV"))
tail(readCsvData("F:/OTAGO/MBusDataSc/APPLIED PROJECT/Final Project/Industrial Challenge/ResourcePackage/data/challengeData.CSV"))

########################### My CODE ########################################
data <- read.csv("challengeData.CSV",header=T,sep=";", dec=".",na.strings="NA",as.is=TRUE)
nrow(data)
head(data)
data[,1] <- as.POSIXct(strptime(data[,1], "%Y-%m-%d %H:%M:%S"))
nrow(data)
head(data)
data[770,]
fix(data)
head(data,1000)
data[11211,]
attach(data)
print(ggplot(data[fromIndex:(fromIndex+length),], aes(x=Timestamp)) +
    geom_line(aes(y=Supply_temperature_setpoint)) + xlab("") + theme_bw())

#####################
plot.new
print(ggplot(data[1:10082,], aes(x=Timestamp)) +
    geom_line(aes(y=Supply_temperature_setpoint)) + xlab("Supply Temperature Setpoint") + theme_bw())

print(ggplot(data[1:10082,], aes(x=Timestamp)) +
    geom_line(aes(y=System_supply_temperature)) + xlab("") + theme_bw())

print(ggplot(data[1:10082,], aes(x=Timestamp)) +
    geom_line(aes(y=Return_Temperature)) + xlab("") + theme_bw())

print(ggplot(data[1:10082,], aes(x=Timestamp)) +
    geom_line(aes(y=System_Power)) + xlab("") + theme_bw())


print(ggplot(data[1130:11211,], aes(x=Timestamp)) +
    geom_line(aes(y=Supply_temperature_setpoint)) + 
geom_line(aes(y=System_supply_temperature)) +
##geom_line(aes(y=Return_Temperature)) +
##geom_line(aes(y=System_Power))+
xlab("Supply Temperature Setpoint") + theme_bw())

require(ggplot2)
p <- ggplot() + 
  geom_line(data=data[1130:11211,], aes(x = Timestamp, y = Supply_temperature_setpoint, color = "red")) +
  geom_line(data=data[1130:11211,], aes(x = Timestamp, y = System_supply_temperature, color = "blue"))  +
  xlab('data_date') +
  ylab('percent.change')

p

