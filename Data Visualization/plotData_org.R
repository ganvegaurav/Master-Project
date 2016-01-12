plotRDSData <- function(filename="", fromIndex=1, length=1000){
  require(ggplot2);
  if (filename=="") stop("No file given");
  if (!file.exists(filename)) stop("File not found");
  
  data <- readRDS(filename);
  if (fromIndex<1 || fromIndex>nrow(data)) stop("fromIndex out of bounds");
  if (fromIndex+length>nrow(data)) length <- nrow(data)-fromIndex-1;
    
  pdf(file=paste("ChallengeDataPlot",fromIndex,"-",length,".pdf", sep="", collapse=NULL));
  
  print(ggplot(data[fromIndex:(fromIndex+length),], aes(x=Timestamp)) +
    geom_line(aes(y=System_supply_temperature)) + xlab("") + theme_bw())
  
  
  print(ggplot(data[fromIndex:(fromIndex+length),], aes(x=Timestamp)) +
    geom_line(aes(y=System_Power)) + xlab("") + theme_bw())
  
  
  print(ggplot(data[fromIndex:(fromIndex+length),], aes(x=Timestamp)) +
    geom_line(aes(y=Return_Temperature)) + xlab("") + theme_bw())
  
  
  print(ggplot(data[fromIndex:(fromIndex+length),], aes(x=Timestamp)) +
    geom_line(aes(y=Supply_temperature_setpoint)) + xlab("") + theme_bw() )
  
  dev.off()
}

# plotRDSData("../../data/challengeData.RDS")