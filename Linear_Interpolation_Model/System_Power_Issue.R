# System POwer Issue

file.exists("ReferenceData.csv")

data_test <- readCsvData("ReferenceData.csv")

# Plots for time interval between 18th Sep and 20th Sep

sdate <- '2014-09-18 00:00:00'
edate <- '2014-09-20 00:00:00'

start <- which(Linear_data[,'Timestamp']== sdate )
end <- which(Linear_data[,'Timestamp']== edate )

sub_interval1 <- data[start:end,1]
length(sub_interval1)
head(sub_interval1)
sub_interval1 <- data.frame(sub_interval1)
head(sub_interval1)
colnames(sub_interval1)[1] <- ("Timestamp")
names(sub_interval1)
sub_interval1[,2] <- data[start:end,5]
colnames(sub_interval1)[2] <- ("System_Power")
sub_interval1[,3] <- Linear_data[start:end,5]
colnames(sub_interval1)[3] <- ("Linear_System_Power")
sub_interval1[,4] <- data_test[start:end,5]
colnames(sub_interval1)[4] <- ("TEST_System_Power")

head(sub_interval1)

a <- is.na(sub_interval1[,2])

library("ggplot2")
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)


plot1 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_Power)) +
        geom_point(data=sub_interval1[a,], aes(x = Timestamp, y = TEST_System_Power, color = 'yellow'),size=1)  +
        guides(colour=FALSE) +
        #scale_x_continuous(breaks=seq()) + 
        xlab('Missing Values imputed from Test Dataset') +
        ylab('System Power') +   
        ggtitle('Issue with System Power imputed values')

plot2 <- ggplot() + 
        geom_line(data=sub_interval1[,], aes(x = Timestamp, y = System_Power)) +
        geom_point(data=sub_interval1[a,], aes(x = Timestamp, y = Linear_System_Power, color = 'red'),size=1)  + 
        guides(colour=FALSE) +
        xlab('Missing Values imputed from Linear Interpolation Model') +
        ylab('System Power')

library("grid")
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(2, 1))

