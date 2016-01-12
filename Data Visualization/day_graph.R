sdate <- '2013-12-01 00:00:00'
edate <- '2013-12-02 23:00:00'
day <- weekdays(as.Date(sdate))

start <- which(data[,'Timestamp']== sdate )
end <- which(data[,'Timestamp']== edate )

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

plot1 <- ggplot(data[start:end,], aes(x=Timestamp)) +
  geom_line(aes(y=System_supply_temperature)) + theme_bw() #+ xlab(day) 
plot2 <- ggplot(data[start:end,], aes(x=Timestamp)) +
  geom_line(aes(y=Supply_temperature_setpoint)) + theme_bw() #+ xlab(day) 
plot3 <- ggplot(data[start:end,], aes(x=Timestamp)) +
  geom_line(aes(y=Return_Temperature)) + theme_bw() #+ xlab(day) + theme_bw()
plot4 <- ggplot(data[start:end,], aes(x=Timestamp)) +
  geom_line(aes(y=System_Power)) + theme_bw() #+ xlab(day) + theme_bw()


library("grid")
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 1)))
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(2, 1))
print(plot3, vp = vplayout(3, 1))
print(plot4, vp = vplayout(4, 1))
