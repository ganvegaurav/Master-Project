sdate <- '2013-11-18 05:12:00'
edate <- '2013-11-19 00:00:00'
#day <- weekdays(as.Date(sdate))
head(data,1)

start <- which(data[,'Timestamp']== sdate )
end <- which(data[,'Timestamp']== edate )

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

library(ggplot2)
plot1 <- ggplot(data[start:end,], aes(x=Timestamp)) +
  geom_line(aes(y=Supply_temperature_setpoint))  +  ylab('Supplytemp.setp.')+theme_bw()
plot2 <- ggplot(data[start:end,], aes(x=Timestamp)) +
  geom_line(aes(y=System_supply_temperature))  + ylab('Sys. supply temp.') + theme_bw()
plot3 <- ggplot(data[start:end,], aes(x=Timestamp)) +
  geom_line(aes(y=Return_Temperature))  + ylab('Return Temp.') + theme_bw()
plot4 <- ggplot(data[start:end,], aes(x=Timestamp)) +
  geom_line(aes(y=System_Power)) + ylab('Sys. Power') + theme_bw()


library("grid")
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 1)))
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(2, 1))
print(plot3, vp = vplayout(3, 1))
print(plot4, vp = vplayout(4, 1))

