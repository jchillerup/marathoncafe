
Beregn_Kumuleret_Antal_Streger <- function(data){
  
  #####
  # Cumuleret antal streger per minut
  data$sec <- unclass(as.POSIXlt(data$utime,origin="1970-01-01 00:00:00"))$sec
  data$hour <- unclass(as.POSIXlt(data$utime,origin="1970-01-01 00:00:00"))$hour
  data$yhour <- 24*unclass(as.POSIXlt(data$utime,origin="1970-01-01 00:00:00"))$yday + data$hour
  data$yhour <- data$yhour - min(data$yhour)
  data$min <- unclass(as.POSIXlt(data$utime,origin="1970-01-01 00:00:00"))$min
  data$min <- data$min - min(data$min) + data$yhour*60
  data$cumst <- cumsum(data$st)
 
  
if (print == 1){png(file = "display/plots/plot2-kumulerede-antal-streger.png",res=reso)}
  par(mar=c(6, 5, 4, 2) + 0.1)
  plot(data$min,data$cumst,type="l",col="blue",xaxt="n",las=1,ylab="",xlab="",cex=0.8,lwd=1.2)
abline(v=c(540,540+24*60),lty=1,lwd=1)
  if(max(data$yhour) < 24) {
    hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
    axis(1, at=seq(0,2880,60), labels=hours,las = 2,col="white")
  }else{
    hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
    axis(1, at=seq(0,2880,120), labels=hours,las = 2,col="white")

  }
  title(main="Kumulerede antal streger paa minut- \nbasis under hele marathoncafeen",xlab="Tidspunkt",mgp=c(4,1,0), font.main = 1)
  title(ylab="Antal streger",mgp=c(3.5,1,0))
  text(c(270,1440/2+540,2430),c(10,10,10), c("fredag","loerdag","soendag"))
  if (print == 1){dev.off()}
  
  
  return(data)
}