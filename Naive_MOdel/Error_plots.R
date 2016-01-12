Naive_Model_RMSE <- c("5.118866", "3.440297","2.658365","6.056876","4.318601")
Linear_RMSE <- c("3.641991","2.394338","1.689203","4.727263","3.113199")
Weighted_RMSE <- c("5.074422", "3.483557","2.683999","6.077278","4.329814")
a <- c("System Temp. SetPoint","Supply Temp.","Return Temp.","System Power","RowMean")
x <- c(1:5)

par(mar=c(10, 4, 2, 2),bg="White" )
plot(x,Naive_Model_RMSE, xaxt="n",type = 'b',pch= 10,col="red",lty=7,ylim =c(1.6,6),xlab="",ylab="RMSE Errors")
lines(x,Linear_RMSE,type="b", pch=22, col="blue", lty=2)
lines(x,Weighted_RMSE,type="b", pch=16, col="green", lty=4)
grid(col = "lightgray", lty = "dotted")
axis(side=1, at=x, labels=a, , las = 2)
?legend


x <- c(1:10); y <- x; z <- 10/x
par(mar=c(5, 4, 4, 8) + 0.1)

plot(x, y,type="b", pch=21, col="red", 
     yaxt="n", lty=3, xlab="", ylab="")
lines(x, z, type="b", pch=22, col="blue", lty=2)
axis(2, at=x,labels=x, col.axis="red", las=2)

# draw an axis on the right, with smaller text and ticks 
axis(4, at=z,labels=round(z,digits=2),
     col.axis="blue", las=2, cex.axis=0.7, tck=-.01)

mtext("y=1/x", side=4, line=3, cex.lab=1,las=2, col="blue")
