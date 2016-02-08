Beregn_Hamringsmomentum <- function(data,C){
  
  # if (print == 1){png(file = "display/plots/plot8.png",res=reso)}
  # barplot(count$momm,xlab="*En form for 'hamrings-hastigheds-speedometer'",ylab="Hamringsmomentum",names.arg = count[1:22,1],las=2,col=rainbow(23),main="Hamringsmomentum* paa koekkenerbasis \n med daempning paa 2.34% per minut", font.main = 1)
  # if (print == 1){dev.off()}
  
  # Dette skal laves paa en tilsvarende maade.
  
  datam <- data.frame(seq(0,max(data$min)-1),rep(0,max(data$min)))
  names(datam) <- c("min","st")
  datatemp <- data[,c("min","st")]
  datam <- merge(datam, datatemp, all = TRUE,by = c('min'))
  datam <- datam[,c(1,3)]
  names(datam) <- c("min","st")
  NonNAindex <- which(is.na(datam$st))
  if (length(NonNAindex) == 0){  
    print("Do nothing")
  } else if (length(NonNAindex) > 0){
    datam[NonNAindex,]$st = 0
  }
  datam <- data.table(datam)
  datam <- datam[ ,lapply(.SD, sum), by = "min"]
  datam$momm <- datam$st
  
  for(i in 2:dim(datam)[1]-1) {
    # CC <- tanh(datam$mom[i]/C)
    CC <- datam$mom[i]/(22*C)
    datam$momm[i+1] <- (1-CC)*(datam$mom[i]+datam$st[i])
  }  
  
  
#   
#   if (print == 1){png(file = "display/plots/plot9.png",res=reso)}
#   par(mar=c(5, 4.5, 4, 2) + 0.1)
#   plot(datam$min,datam$momm,type="l",col="blue",xaxt="n",las=1,ylab="",xlab="")
#   abline(v=c(540,540+24*60),lty=1,col="black")
#   if(max(data$min) < 1440) {
#     hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
#     axis(1, at=seq(0,2880,60), labels=hours,las = 2)
#   } else if (max(data$min) > 1440){
#     hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
#     axis(1, at=seq(0,2880,120), labels=hours,las = 2)
#   }
#   title(main="Hamringmomentum* for daempning \n paa 2.34% per minut",mgp=c(4,1,0), font.main = 1)
#   title(ylab="Hamringmomentum",mgp=c(3.0,1,0))
#   title(mgp=c(3.5,1,0),xlab="*En form for 'hamrings-hastigheds-speedometer'")
#   if (print == 1){dev.off()}
#   
  
  Hamrings_Momentum <- data.frame(datam$min,datam$momm)
  colnames(Hamrings_Momentum) <- c("min","momm")
  
  
 return(Hamrings_Momentum) 
}