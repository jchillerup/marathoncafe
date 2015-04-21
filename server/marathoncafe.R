# Barvagter 

rm(list = ls()) # Clear workspace
print = 1
install_packages = 0
load_packages = 1

if (install_packages == 1){
  install.packages("data.table")
  install.packages("plotrix")
  install.packages("RSQLite")
  install.packages("grid")
  install.packages("gridExtra")
  install.packages("ggplot2")
  install.packages("lattice")
  install.packages("zoo")
  install.packages("httpRequest")
  update.packages()}
if (load_packages == 1){
  library(data.table) 
  library(plotrix)
  library(RSQLite)
  library(grid)
  library(gridExtra)
  library(ggplot2) 
  library(lattice)
  library(zoo) 
  library(httpRequest)}
data <- read.table("data.csv", sep=";", quote="\"")    
names(data) <- c("ki","st","utime")
starttime = data$utime[1]
data$ti <- as.POSIXct(data[,3], origin="1970-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S")
endtime = data$utime[nrow(data)]
reso <- 91
udkomt <- 0 # 0 = udkommenteret

#####
# Streger per k??kken
ki <- c("1A","2A","3A","4A","5A","6A","7A","1B","2B","3B","4B","5B","6B","7B","1C","2C","3C","4C","5C","6C","7C","1D","2D","3D","4D","5D","6D","7D")
st <- rep(0,28)
tabtemp <- data.frame(ki,st)
count <- data.table(data[,1:2])
count <- count[ ,lapply(.SD, sum), by = ki]
count <- data.frame(count)
count <- merge(count, tabtemp, all = TRUE,by = c('ki'))
count <- count[,1:2]
names(count) <- c("ki","st")
NonNAindex <- which(is.na(count$st))
if (length(NonNAindex) == 0){  

} else if (length(NonNAindex) > 0){
  count[NonNAindex,]$st = 0
}
count <- count[order(count[,"st"]),] # Sorting
if (print == 1){png(file = "display/plots/plot1.png",res=reso)}
barplot(count$st,xlab="",ylab="Antal streger",names.arg = count[1:28,1],las=2,col=rainbow(23),main="Antal streger fordelt paa koekkener", font.main = 1)
title(xlab=paste("Present plots were generated: ",Sys.time()))
if (print == 1){dev.off()}

maximum <- count[dim(count)[1],2]

#####
# Cumuleret antal streger per minut
data$sec <- unclass(as.POSIXlt(data$utime,origin="1970-01-01 00:00:00"))$sec
data$hour <- unclass(as.POSIXlt(data$utime,origin="1970-01-01 00:00:00"))$hour
data$yhour <- 24*unclass(as.POSIXlt(data$utime,origin="1970-01-01 00:00:00"))$yday+data$hour
data$yhour <- data$yhour - min(data$yhour)
data$min <- unclass(as.POSIXlt(data$utime,origin="1970-01-01 00:00:00"))$min
data$min <- data$min - min(data$min) + data$yhour*60
data$cumst<-cumsum(data$st)

if (print == 1){png(file = "display/plots/plot2.png",res=reso)}
par(mar=c(6, 5, 4, 2) + 0.1)
plot(data$min,data$cumst,type="l",col="blue",xaxt="n",las=1,ylab="",xlab="",cex=0.8)
abline(v=c(540,540+24*60,540+24*60+24*60),lty=2,col="black")
if(max(data$yhour) < 24) {
  hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
  axis(1, at=seq(0,2880,60), labels=hours,las = 2)
}else{
  hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
  axis(1, at=seq(0,2880+1440,120), labels=hours,las = 2,cex=0.5)
}
title(main="Kumulerede antal streger paa minut- \nbasis under hele marathonbaren",xlab="Tidspunkt",mgp=c(4,1,0), font.main = 1)
title(ylab="Antal streger",mgp=c(3.5,1,0))
text(c(270,1440/2+540,2430,2430+1540),c(10,10,10,10), c("torsdag","fredag","loerdag","soendag"))
if (print == 1){dev.off()}


####
# Kommuleret antal fordelt p?? timer
# data$cumst<-cumsum(data$st)
# if (udkomt == 1){
# if (print == udkomt){png(file = "display/plots/plot4.png",res=reso)}
# par(mar=c(6, 6, 4, 2) + 0.1)
# plot(data$yhour,data$cumst,type="l",col="blue",xaxt="n",las=1,ylab="",xlab="")
# abline(v=c(9,9+24),lty=2,col="red")
# if(max(data$yhour) < 24) {
#   hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
#   axis(1, at=seq(0,48,1), labels=hours,las = 2)
# } else if (max(data$yhour) > 24){
#   hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
#   axis(1, at=seq(0,48,2), labels=hours,las = 2)
# }
# title(main="Kumulerede antal streger for alle koekkener \n for hele marathon-baren fordelt paa timer",xlab="Tidspunkt",mgp=c(4,1,0))
# title(ylab="Antal streger",mgp=c(5,1,0))
# text(c(4.5,9+12,42),c(10,10,10), c("torsdag","fredag","loerdag","soendag"))
# if (print == udkomt){dev.off()}
# }



#####
# Forbrug per time 
st <- rep(0,49)
yhour <- seq(0,48,1)
tabtemp <- data.frame(yhour,st) 
count <- data.table(data[,c("st","yhour")])
count <- count[ ,lapply(.SD, sum), by = yhour]
count <- merge(count, tabtemp, all = TRUE,by = c('yhour'))
count <- data.frame(count)
count <- count[,1:2]
names(count) <- c("yhour","st")

# if (print == 1){png(file = "display/plots/plot3.png",res=reso)}
# par(mar=c(5, 4.5, 4, 2) + 0.1)
# plot(count$st,type="l",col="blue",xaxt="n",las=1,ylab="",xlab="")
# abline(v=c(9,9+24),lty=2,col="red")
#   hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
#   axis(1, at=seq(0,48,2), labels=hours,las = 2)
# title(main="Antal streger fordelt paa timer for alle \n koekkener for hele marathon-baren",xlab="",mgp=c(3,1,0), font.main = 1)
# title(ylab="Antal streger",mgp=c(3.5,1,0))
# text(c(4.5,9+12,42),c(10,10,10), c("torsdag","fredag","loerdag","soendag"))
# if (print == 1){dev.off()}


#####
# Forbrug per minut 
st <- rep(0,2881)
min <- seq(0,2880,1)
tabtemp <- data.frame(min,st) 
count <- data.table(data[,c("st","min")])
count <- count[ ,lapply(.SD, sum), by = min]
count <- merge(count, tabtemp, all = TRUE,by = c('min'))
count <- data.frame(count)
count <- count[,1:2]
names(count) <- c("min","st")
NonNAindex <- which(is.na(count$st))
count[NonNAindex,]$st = 0
# 
# if (udkomt == 1){
# if (print == udkomt){png(file = "display/plots/plot4.png",res=reso)}
# par(mar=c(5, 4.5, 4, 2) + 0.1)
# plot(count$st,col="red",xaxt="n",las=1,ylab="",xlab="",type="l")
# abline(v=c(540,540+24*60),lty=1,col="black")
# hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
# axis(1, at=seq(0,2880,120), labels=hours,las = 2)
# title(main="Antal streger per minut for alle koekkener \n for hele marathon-baren",xlab="Tidspunkt",mgp=c(4,1,0))
# title(ylab="Antal streger",mgp=c(3.5,1,0))
# text(c(270,1440/2+540,2430),c(10,10,10), c("torsdag","fredag","loerdag","soendag"),col="black")
# if (print == udkomt){dev.off()}
# }

if (max(count$min)>120){
count120 <- count[which(count[,1]>max(count[,1])-120),]
if (print == 1){png(file = "display/plots/plot4.png",res=reso)}
par(mar=c(5, 4.5, 4, 2) + 0.1)
plot(count120$st,type="l",col="blue",xaxt="n",las=1,ylab="",xlab="")
abline(v=c(540,540+24*60),lty=2,col="red")
hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
axis(1, at=seq(0,2880+1440,60), labels=hours,las = 2)
title(main="Antal streger for alle koekkkener \n per minut de seneste to timer",xlab=paste("Tidspunkt (plots were generated: ",Sys.time(),")",sep=""),mgp=c(4,1,0), font.main = 1)
title(ylab="Antal streger",mgp=c(3.5,1,0))    
text(c(270,1440/2+540,2430),c(10,10,10), c("torsdag","fredag","loerdag","soendag"))
if (print == 1){dev.off()}
} else {
  if (print == 1){png(file = "display/plots/plot4.png",res=reso)}
  plot(0,col="white",xlab="",ylab="",xaxt="n",yaxt="n")  
  text(1,0,c("This plot will be generated when data is available \n Streger de seneste 120 minutter"),cex=0.85)
  text(1,-0.5,paste("Present plots were generated: ",Sys.time()),cex=0.85)
  if (print == 1){dev.off()}
}



##
# Barvagt plot - antal streger p?? egent k??kken. 
# HER SKAL MAN SKRIVE BARVAGTER IND! 
barvagt <- c("Personalet","5C","7B","Prof","5C","2A","5A","7C","1A","3B","6B","Prof","1C","xx","xx","Cafeen")
tidspunkt <- c(4*30,5*30,5*30,16*30,4*30,6*30,4*30,4*30,4*30,5*30,5*30,20*30,4*30,5*30,5*30,5*30)
tidspunkt <- cumsum(tidspunkt)
vagt <- data.frame(tidspunkt=tidspunkt,barvagt=barvagt)
data$vagt <- 0
if (length(which(data$ki == "5C" & data$min >= 120 & data$min < 270))>0){data[which(data$ki == "5C" & data$min >= 120 & data$min < 270),]$vagt = "5C"}
if (length(which(data$ki == "7B" & data$min >= 270 & data$min < 420))>0){data[which(data$ki == "7B" & data$min >= 270 & data$min < 420),]$vagt = "7B"}
if (length(which(data$ki == "5C" & data$min >= 900 & data$min < 1020))>0){data[which(data$ki == "5C" & data$min >= 900 & data$min < 1020),]$vagt = "5C"} # Forhalsbar

if (length(which(data$ki == "2A" & data$min >= 1020 & data$min < 1200))>0){data[which(data$ki == "2A" & data$min >= 1020 & data$min < 1200),]$vagt = "2A"} # Forhalsbar
if (length(which(data$ki == "5A" & data$min >= 1200 & data$min < 1320))>0){data[which(data$ki == "5A" & data$min >= 1200 & data$min < 1320),]$vagt = "5A"}

if (length(which(data$ki == "7C" & data$min >= 1320 & data$min < 1440))>0){data[which(data$ki == "7C" & data$min >= 1320 & data$min < 1440),]$vagt = "7C"} # Forhalsbar
if (length(which(data$ki == "1A" & data$min >= 1440 & data$min < 1560) )>0){data[which(data$ki == "1A" & data$min >= 1440 & data$min < 1560),]$vagt = "1A"}
if (length(which(data$ki == "3B" & data$min >= 1560 & data$min < 1710) )>0){data[which(data$ki == "3B" & data$min >= 1560 & data$min < 1710),]$vagt = "3B"}

if (length(which(data$ki == "6B" & data$min >= 1710 & data$min < 1860) )>0){data[which(data$ki == "6B" & data$min >= 1710 & data$min < 1860),]$vagt = "6B"}
if (length(which(data$ki == "1C" & data$min >= 2340 & data$min < 2460) )>0){data[which(data$ki == "1C" & data$min >= 2340 & data$min < 2460),]$vagt = "1C"}

# if (length(which(data$ki == "7C" & data$min >= 1380 & data$min < 1500) )>0){data[which(data$ki == "7C" & data$min >= 1380 & data$min < 1500),]$vagt = "7C"}
# if (length(which(data$ki == "6A" & data$min >= 1500 & data$min < 1620) )>0){data[which(data$ki == "6A" & data$min >= 1500 & data$min < 1620),]$vagt = "6A"}
# if (length(which(data$ki == "5A" & data$min >= 1740 & data$min < 1920) )>0){data[which(data$ki == "5A" & data$min >= 1740 & data$min < 1920),]$vagt = "5A"}
# if (length(which(data$ki == "6B" & data$min >= 1920 & data$min < 2070) )>0){data[which(data$ki == "6B" & data$min >= 1920 & data$min < 2070),]$vagt = "6B"}
# if (length(which(data$ki == "5B" & data$min >= 2010 & data$min < 2160) )>0){data[which(data$ki == "5B" & data$min >= 2010 & data$min < 2160),]$vagt = "5B"} # Forhalsbar
# if (length(which(data$ki == "6C" & data$min >= 2070 & data$min < 2220) )>0){data[which(data$ki == "6C" & data$min >= 2070 & data$min < 2220),]$vagt = "6C"}
# if (length(which(data$ki == "4A" & data$min >= 2220 & data$min < 2370) )>0){data[which(data$ki == "4A" & data$min >= 2220 & data$min < 2370),]$vagt = "4A"}
# if (length(which(data$ki == "1A" & data$min >= 2370 & data$min < 2520) )>0){data[which(data$ki == "1A" & data$min >= 2370 & data$min < 2520),]$vagt = "1A"}

vagt <- ki <- c("1A","2A","3A","4A","5A","6A","7A","1B","2B","3B","4B","5B","6B","7B","1C","2C","3C","4C","5C","6C","7C","1D","2D","3D","4D","5D","6D","7D")
st <- rep(0,28)
count <- data.table(data[,c("vagt","st")])
count <- count[ ,lapply(.SD, sum), by = vagt]
count <- count[order(count[,vagt]),] # Sorting
count <- data.frame(count)
count <- count[-1,]
tabtemp <- data.frame(vagt,st)
count <- merge(count, tabtemp, all = TRUE,by = c("vagt"))
count <- count[,1:2]
cheats <- count
names(cheats) <- c("ki","vagt")
NonNAindex <- which(is.na(cheats$vagt))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  cheats[NonNAindex,]$vagt = 0
}




####
# Kommuleret antal streger fordelt p?? minutter og k??kkener

start <- read.table("start.csv", sep=";", quote="\"") 
start2 <- read.table("start.csv", sep=";", quote="\"") 
start$V3 <- starttime
start2$V3 <- starttime
names(start) <- c("ki","st","utime")
names(start2) <- c("ki","st","utime")


data$cumst<-cumsum(data$st)
start[4:10] <- data[1,4:10,]
start2[4:10] <- data[1,4:10,]
start2$min <- max(data$min)
start$cumst <- 0
datac <- rbind(start,data,start2)











tt2C <- (datac[which(datac$ki == "2C"),])
tt3C <- (datac[which(datac$ki == "3C"),])
tt4C <- (datac[which(datac$ki == "4C"),])
tt5C <- (datac[which(datac$ki == "5C"),])
tt6C <- (datac[which(datac$ki == "6C"),])
tt7C <- (datac[which(datac$ki == "7C"),])
tt1D <- (datac[which(datac$ki == "1D"),])
tt2D <- (datac[which(datac$ki == "2D"),])
tt3D <- (datac[which(datac$ki == "3D"),])
tt4D <- (datac[which(datac$ki == "4D"),])
tt5D <- (datac[which(datac$ki == "5D"),])
tt6D <- (datac[which(datac$ki == "6D"),])
tt7D <- (datac[which(datac$ki == "7D"),])
tt2B <- (datac[which(datac$ki == "2B"),])
tt3B <- (datac[which(datac$ki == "3B"),])
tt4B <- (datac[which(datac$ki == "4B"),])
tt5B <- (datac[which(datac$ki == "5B"),])
tt6B <- (datac[which(datac$ki == "6B"),])
tt7B <- (datac[which(datac$ki == "7B"),])
tt1C <- (datac[which(datac$ki == "1C"),])
tt1A <- (datac[which(datac$ki == "1A"),])
tt2A <- (datac[which(datac$ki == "2A"),])
tt3A <- (datac[which(datac$ki == "3A"),])
tt4A <- (datac[which(datac$ki == "4A"),])
tt5A <- (datac[which(datac$ki == "5A"),])
tt6A <- (datac[which(datac$ki == "6A"),])
tt7A <- (datac[which(datac$ki == "7A"),])
tt1B <- (datac[which(datac$ki == "1B"),])
tt2C$cumst <- cumsum(tt2C$st)
tt3C$cumst <- cumsum(tt3C$st)
tt4C$cumst <- cumsum(tt4C$st)
tt5C$cumst <- cumsum(tt5C$st)
tt6C$cumst <- cumsum(tt6C$st)
tt7C$cumst <- cumsum(tt7C$st)
tt1D$cumst <- cumsum(tt1D$st)
tt2D$cumst <- cumsum(tt2D$st)
tt3D$cumst <- cumsum(tt3D$st)
tt4D$cumst <- cumsum(tt4D$st)
tt5D$cumst <- cumsum(tt5D$st)
tt6D$cumst <- cumsum(tt6D$st)
tt7D$cumst <- cumsum(tt7D$st)
tt2B$cumst <- cumsum(tt2B$st)
tt3B$cumst <- cumsum(tt3B$st)
tt4B$cumst <- cumsum(tt4B$st)
tt5B$cumst <- cumsum(tt5B$st)
tt6B$cumst <- cumsum(tt6B$st)
tt7B$cumst <- cumsum(tt7B$st)
tt1C$cumst <- cumsum(tt1C$st)
tt1A$cumst <- cumsum(tt1A$st)
tt2A$cumst <- cumsum(tt2A$st)
tt3A$cumst <- cumsum(tt3A$st)
tt4A$cumst <- cumsum(tt4A$st)
tt5A$cumst <- cumsum(tt5A$st)
tt6A$cumst <- cumsum(tt6A$st)
tt7A$cumst <- cumsum(tt7A$st)
tt1B$cumst <- cumsum(tt1B$st)

temporary <- c(tt2C$cumst,tt3C$cumst,tt4C$cumst,tt5C$cumst,tt6C$cumst,tt7C$cumst,tt1D$cumst,tt2D$cumst,tt3D$cumst,tt4D$cumst,tt5D$cumst,tt6D$cumst,tt7D$cumst,tt2B$cumst,tt3B$cumst,tt4B$cumst,tt5B$cumst,tt6B$cumst,tt7B$cumst,tt1C$cumst,tt1A$cumst,tt2A$cumst,tt3A$cumst,tt4A$cumst,tt5A$cumst,tt6A$cumst,tt7A$cumst,tt1B$cumst)
temporary <- max(temporary)
temporary2 <- c(tt2C$min,tt3C$min,tt4C$min,tt5C$min,tt6C$min,tt7C$min,tt1D$min,tt2D$min,tt3D$min,tt4D$min,tt5D$min,tt6D$min,tt7D$min,tt2B$min,tt3B$min,tt4B$min,tt5B$min,tt6B$min,tt7B$min,tt1C$min,tt1A$min,tt2A$min,tt3A$min,tt4A$min,tt5A$min,tt6A$min,tt7A$min,tt1B$min)
temporary2 <- max(temporary2)

if (data$min[length(data$min)] > 30){
if (print == 1){png(file = "display/plots/plot6.png",res=reso)}
par(mar=c(6, 5, 4, 2) + 0.1)
plot(tt2C$min,tt2C$cumst,type="l",col=1,xaxt="n",las=1,ylab="",xlab="",ylim = c(0,temporary),xlim=c(0,temporary2))
points(tt3C$min,tt3C$cumst,type="l",col=2,xaxt="n",las=1,ylab="",xlab="")
points(tt4C$min,tt4C$cumst,type="l",col=3,xaxt="n",las=1,ylab="",xlab="")
points(tt5C$min,tt5C$cumst,type="l",col=4,xaxt="n",las=1,ylab="",xlab="")
points(tt6C$min,tt6C$cumst,type="l",col=5,xaxt="n",las=1,ylab="",xlab="")
points(tt7C$min,tt7C$cumst,type="l",col=6,xaxt="n",las=1,ylab="",xlab="")
points(tt1D$min,tt1D$cumst,type="l",col=7,xaxt="n",las=1,ylab="",xlab="")
points(tt2D$min,tt2D$cumst,type="l",col=8,xaxt="n",las=1,ylab="",xlab="")
points(tt3D$min,tt3D$cumst,type="l",col=9,xaxt="n",las=1,ylab="",xlab="")
points(tt4D$min,tt4D$cumst,type="l",col=10,xaxt="n",las=1,ylab="",xlab="")
points(tt5D$min,tt5D$cumst,type="l",col=11,xaxt="n",las=1,ylab="",xlab="")
points(tt6D$min,tt6D$cumst,type="l",col=12,xaxt="n",las=1,ylab="",xlab="")
points(tt7D$min,tt7D$cumst,type="l",col=13,xaxt="n",las=1,ylab="",xlab="")
points(tt2B$min,tt2B$cumst,type="l",lty=14,col=1,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt3B$min,tt3B$cumst,type="l",lty=15,col=2,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt4B$min,tt4B$cumst,type="l",lty=16,col=3,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt5B$min,tt5B$cumst,type="l",lty=17,col=4,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt6B$min,tt6B$cumst,type="l",lty=18,col=5,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt7B$min,tt7B$cumst,type="l",lty=19,col=6,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt1C$min,tt1C$cumst,type="l",lty=20,col=7,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt1A$min,tt1A$cumst,type="l",lty=21,col=1,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt2A$min,tt2A$cumst,type="l",lty=22,col=2,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt3A$min,tt3A$cumst,type="l",lty=23,col=3,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt4A$min,tt4A$cumst,type="l",lty=24,col=4,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt5A$min,tt5A$cumst,type="l",lty=25,col=5,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt6A$min,tt6A$cumst,type="l",lty=26,col=6,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt7A$min,tt7A$cumst,type="l",lty=27,col=7,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(tt1B$min,tt1B$cumst,type="l",lty=28,col=8,xaxt="n",las=1,ylab="",xlab="",lwd=2)





abline(v=c(540,540+24*60,540+24*60+24*60),lty=2,col="black")
leg <- ki <- c("1A","2A","3A","4A","5A","6A","7A","1B","2B","3B","4B","5B","6B","7B","1C","2C","3C","4C","5C","6C","7C","1D","2D","3D","4D","5D","6D","7D")
legend("topleft", inset=.05,leg, horiz=FALSE,lty=c(rep(1,7),rep(2,7),rep(4,8)),col=c(1:7,1:7,1:8),ncol=3, cex=.75,lwd=2,bg="transparent")
if(max(data$yhour) < 24) {
  hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
  axis(1, at=seq(0,2880,60), labels=hours,las = 2)
} else if (max(data$yhour) > 24){
  hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
  axis(1, at=seq(0,2880+1440,120), labels=hours,las = 2)
}
title(main="Kumulerede antal streger fordelt paa \n koekkener for hele marathon-baren",mgp=c(4,1,0),cex=0.8,font.main=1)
title(ylab="Antal streger")
text(c(270,1440/2+540,2430,2430+1500),c(10,10,10,10), c("torsdag","fredag","loerdag","soendag"))
if (print == 1){dev.off()}
} else if (data$min[length(data$min)] <= 30){
  if (print == 1){png(file = "display/plots/plot6.png",res=reso)}
  plot(0,col="white",xlab="",ylab="",xaxt="n",yaxt="n")  
  text(1,0,c("This plot will be generated \n after 30 minutes"))
  text(1,-0.5,paste("Present plots were generated: ",Sys.time()),cex=0.85)
  if (print == 1){dev.off()}
}






count <- data.table(datac[,1:2])
count <- count[ ,lapply(.SD, sum), by = ki]
count <- data.frame(count)
names(tabtemp) <- c("ki","st")
count <- merge(count, tabtemp, all = TRUE, by = c('ki'))
count <- count[,1:2]
names(count) <- c("ki","st")
NonNAindex <- which(is.na(count$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  count[NonNAindex,]$st = 0
}
count <- count[order(count[,"st"]),] # Sorting

if (dim(count)[1] >= 5){
  if (print == 1){png(file = "display/plots/plot7.png",res=reso)}  
  nr1 <- count[dim(count)[1],]
  nr1 <- data.frame(lapply(nr1, as.character), stringsAsFactors=FALSE)[1]
  nr1 <- as.character(nr1)
  datanr1 <- datac[which(datac$ki == nr1,),]
  datanr1$cumst <- cumsum(datanr1$st)
  nr2 <- count[dim(count)[1]-1,]
  nr2 <- data.frame(lapply(nr2, as.character), stringsAsFactors=FALSE)[1]
  nr2 <- as.character(nr2)
  datanr2 <- datac[which(datac$ki == nr2,),]
  datanr2$cumst <- cumsum(datanr2$st)
  nr3 <- count[dim(count)[1]-2,]  
  nr3 <- data.frame(lapply(nr3, as.character), stringsAsFactors=FALSE)[1]
  nr3 <- as.character(nr3)  
  datanr3 <- datac[which(datac$ki == nr3,),]
  datanr3$cumst <- cumsum(datanr3$st)
  nr4 <- count[dim(count)[1]-3,]
  nr4 <- data.frame(lapply(nr4, as.character), stringsAsFactors=FALSE)[1]
  nr4 <- as.character(nr4)
  datanr4 <- datac[which(datac$ki == nr4,),]
  datanr4$cumst <- cumsum(datanr4$st)
  nr5 <- count[dim(count)[1]-4,]
  nr5 <- data.frame(lapply(nr5, as.character), stringsAsFactors=FALSE)[1]
  nr5 <- as.character(nr5)
  datanr5 <- datac[which(datac$ki == nr5,),]
  datanr5$cumst <- cumsum(datanr5$st)
  par(mar=c(5, 5, 4, 2) + 0.1)
  plot(datanr1$min,datanr1$cumst,type="l",col=1,xaxt="n",las=1,ylab="",xlab="",ylim = c(0,temporary)) 
  points(datanr2$min,datanr2$cumst,type="l",col=2,xaxt="n",las=1,ylab="",xlab="")
  points(datanr3$min,datanr3$cumst,type="l",col=3,xaxt="n",las=1,ylab="",xlab="")  
  points(datanr4$min,datanr4$cumst,type="l",col=4,xaxt="n",las=1,ylab="",xlab="")  
  points(datanr5$min,datanr5$cumst,type="l",col=5,xaxt="n",las=1,ylab="",xlab="")  
  leg <- c(nr1,nr2,nr3,nr4,nr5)
  abline(v=c(540,540+24*60,540+24*60+24*60),lty=2,col="black")
  legend("topleft", inset=.05,leg, horiz=FALSE,lty=1,col=c(1:5),ncol=2, cex=.90,lwd=1,bg="transparent")
  if(max(data$yhour) < 24) {
    hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
    axis(1, at=seq(0,2880,60), labels=hours,las = 2)
  } else if (max(data$yhour) > 24){
    hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
    axis(1, at=seq(0,2880+1440,120), labels=hours,las = 2)
  }
  title(main="Kumulerede antal streger per minut for de tre \n foerende koekkener for hele marathon-baren",mgp=c(3,1,0), font.main = 1)
  title(ylab="Antal streger",mgp=c(3.1,1,0))
  text(c(270,1440/2+540,2430,2430+1500),c(10,10,10,10), c("torsdag","fredag","loerdag","soendag"))
  if (print == 1){dev.off()}  
} else if (dim(count)[1] < 3){
  if (print == 1){png(file = "display/plots/plot7.png",res=reso)}  
  plot(0,col="white",xlab="",ylab="",xaxt="n",yaxt="n")  
  text(1,0,c("This plot will be generated when \n data is available"))
  text(1,-0.5,paste("Present plots were generated: ",Sys.time()),cex=0.85)
  if (print == 1){dev.off()}
}



# Implement hamring momentuma
tt2C <- tt2C[,c("min","st","cumst")]
tt3C <- tt3C[,c("min","st","cumst")]
tt4C <- tt4C[,c("min","st","cumst")]
tt5C <- tt5C[,c("min","st","cumst")]
tt6C <- tt6C[,c("min","st","cumst")]
tt7C <- tt7C[,c("min","st","cumst")]
tt1D <- tt1D[,c("min","st","cumst")]
tt2D <- tt2D[,c("min","st","cumst")]
tt3D <- tt3D[,c("min","st","cumst")]
tt4D <- tt4D[,c("min","st","cumst")]
tt5D <- tt5D[,c("min","st","cumst")]
tt6D <- tt6D[,c("min","st","cumst")]
tt7D <- tt7D[,c("min","st","cumst")]
tt2B <- tt2B[,c("min","st","cumst")]
tt3B <- tt3B[,c("min","st","cumst")]
tt4B <- tt4B[,c("min","st","cumst")]
tt5B <- tt5B[,c("min","st","cumst")]
tt6B <- tt6B[,c("min","st","cumst")]
tt7B <- tt7B[,c("min","st","cumst")]
tt1C <- tt1C[,c("min","st","cumst")]
tt1A <- tt1A[,c("min","st","cumst")]
tt2A <- tt2A[,c("min","st","cumst")]
tt3A <- tt3A[,c("min","st","cumst")]
tt4A <- tt4A[,c("min","st","cumst")]
tt5A <- tt5A[,c("min","st","cumst")]
tt6A <- tt6A[,c("min","st","cumst")]
tt7A <- tt7A[,c("min","st","cumst")]
tt1B <- tt1B[,c("min","st","cumst")]


if (length(tt1A$min)==1){
  tt1A=data.frame(t(matrix(rep(0,8),4)))
  names(tt1A)=c("min","st","cumst","momm")
}
if (length(tt2A$min)==1){
  tt2A=data.frame(t(matrix(rep(0,8),4)))
  names(tt2A)=c("min","st","cumst","momm")
}
if (length(tt3A$min)==1){
  tt3A=data.frame(t(matrix(rep(0,8),4)))
  names(tt3A)=c("min","st","cumst","momm")
}
if (length(tt4A$min)==1){
  tt4A=data.frame(t(matrix(rep(0,8),4)))
  names(tt4A)=c("min","st","cumst","momm")
}
if (length(tt5A$min)==1){
  tt5A=data.frame(t(matrix(rep(0,8),4)))
  names(tt5A)=c("min","st","cumst","momm")
}
if (length(tt6A$min)==1){
  tt6A<-data.frame(t(matrix(rep(0,8),4)))
  names(tt6A)=c("min","st","cumst","momm")
}
if (length(tt7A$min)==1){
  tt7A=data.frame(t(matrix(rep(0,8),4)))
  names(tt7A)=c("min","st","cumst","momm")
}
if (length(tt1B$min)==1){
  tt1B=data.frame(t(matrix(rep(0,8),4)))
  names(tt1B)=c("min","st","cumst","momm")
}
if (length(tt2B$min)==1){
  tt2B=data.frame(t(matrix(rep(0,8),4)))
  names(tt2B)=c("min","st","cumst","momm")
}
if (length(tt3B$min)==1){
  tt3B=data.frame(t(matrix(rep(0,8),4)))
  names(tt3B)=c("min","st","cumst","momm")
}
if (length(tt4B$min)==1){
  tt4B=data.frame(t(matrix(rep(0,8),4)))
  names(tt4B)=c("min","st","cumst","momm")
}
if (length(tt5B$min)==1){
  tt5B=data.frame(t(matrix(rep(0,8),4)))
  names(tt5B)=c("min","st","cumst","momm")
}
if (length(tt6B$min)==1){
  tt6B=data.frame(t(matrix(rep(0,8),4)))
  names(tt6B)=c("min","st","cumst","momm")
}
if (length(tt7B$min)==1){
  tt7B=data.frame(t(matrix(rep(0,8),4)))
  names(tt7B)=c("min","st","cumst","momm")
}
if (length(tt1C$min)==1){
  tt1C=data.frame(t(matrix(rep(0,8),4)))
  names(tt1C)=c("min","st","cumst","momm")
}
if (length(tt2C$min)==1){
  tt2C=data.frame(t(matrix(rep(0,8),4)))
  names(tt2C)=c("min","st","cumst","momm")
}
if (length(tt3C$min)==1){
  tt3C=data.frame(t(matrix(rep(0,8),4)))
  names(tt3C)=c("min","st","cumst","momm")
}
if (length(tt4C$min)==1){
  tt4C=data.frame(t(matrix(rep(0,8),4)))
  names(tt4C)=c("min","st","cumst","momm")
}
if (length(tt5C$min)==1){
  tt5C=data.frame(t(matrix(rep(0,8),4)))
  names(tt5C)=c("min","st","cumst","momm")
}
if (length(tt6C$min)==1){
  tt6C=data.frame(t(matrix(rep(0,8),4)))
  names(tt6C)=c("min","st","cumst","momm")
}
if (length(tt7C$min)==1){
  names(tt7C)=c("min","st","cumst","momm")
  tt7C=data.frame(t(matrix(rep(0,8),4)))
}
if (length(tt1D$min)==1){
  tt1D=data.frame(t(matrix(rep(0,8),4)))
  names(tt1D)=c("min","st","cumst","momm")
}
if (length(tt2D$min)==1){
  ttD=data.frame(t(matrix(rep(0,8),4)))
  names(tt2D)=c("min","st","cumst","momm")
}
if (length(tt3D$min)==1){
  tt3D=data.frame(t(matrix(rep(0,8),4)))
  names(tt3D)=c("min","st","cumst","momm")
}
if (length(tt4D$min)==1){
  tt4D=data.frame(t(matrix(rep(0,8),4)))
  names(tt4D)=c("min","st","cumst","momm")
}
if (length(tt5D$min)==1){
  tt5D=data.frame(t(matrix(rep(0,8),4)))
  names(tt5D)=c("min","st","cumst","momm")
}
if (length(tt6D$min)==1){
  tt6D=data.frame(t(matrix(rep(0,8),4)))
  names(tt6D)=c("min","st","cumst","momm")
}
if (length(tt7D$min)==1){
  tt7D=data.frame(t(matrix(rep(0,8),4)))
  names(tt7D)=c("min","st","cumst","momm")
}




C <- -0.0234
tt2Cm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt2Cm) <- c("min","st")
tt2C <- tt2C[,c("min","st")]
tt2Cm <- merge(tt2Cm, tt2C, all = TRUE,by = c('min'))
tt2Cm <- tt2Cm[,c(1,3)]
names(tt2Cm) <- c("ki","st")
NonNAindex <- which(is.na(tt2Cm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt2Cm[NonNAindex,]$st = 0
}
tt2Cm <- data.table(tt2Cm)
tt2Cm <- tt2Cm[ ,lapply(.SD, sum), by = "ki"]
tt2Cm$momm <- tt2Cm$st  
for(i in 2:dim(tt2Cm)[1]-1) {
  tt2Cm$momm[i+1]<-(tt2Cm$mom[i]+tt2Cm$st[i])*exp(C)
}  

tt3Cm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt3Cm) <- c("min","st")
tt3C <- tt3C[,c("min","st")]
tt3Cm <- merge(tt3Cm, tt3C, all = TRUE,by = c('min'))
tt3Cm <- tt3Cm[,c(1,3)]
names(tt3Cm) <- c("ki","st")
NonNAindex <- which(is.na(tt3Cm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt3Cm[NonNAindex,]$st = 0
}
tt3Cm <- data.table(tt3Cm)
tt3Cm <- tt3Cm[ ,lapply(.SD, sum), by = "ki"]
tt3Cm$momm <- tt3Cm$st  
for(i in 2:dim(tt3Cm)[1]-1) {
  tt3Cm$momm[i+1]<-(tt3Cm$mom[i]+tt3Cm$st[i])*exp(C)
}  

tt4Cm <-data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt4Cm) <- c("min","st")
tt4C <- tt4C[,c("min","st")]
tt4Cm <- merge(tt4Cm, tt4C, all = TRUE,by = c('min'))
tt4Cm <- tt4Cm[,c(1,3)]
names(tt4Cm) <- c("ki","st")
NonNAindex <- which(is.na(tt4Cm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt4Cm[NonNAindex,]$st = 0
}
tt4Cm <- data.table(tt4Cm)
tt4Cm <- tt4Cm[ ,lapply(.SD, sum), by = "ki"]
tt4Cm$momm <- tt4Cm$st  
for(i in 2:dim(tt4Cm)[1]-1) {
  tt4Cm$momm[i+1]<-(tt4Cm$mom[i]+tt4Cm$st[i])*exp(C)
}  

tt5Cm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt5Cm) <- c("min","st")
tt5C <- tt5C[,c("min","st")]
tt5Cm <- merge(tt5Cm, tt5C, all = TRUE,by = c('min'))
tt5Cm <- tt5Cm[,c(1,3)]
names(tt5Cm) <- c("ki","st")
NonNAindex <- which(is.na(tt5Cm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt5Cm[NonNAindex,]$st = 0
}
tt5Cm <- data.table(tt5Cm)
tt5Cm <- tt5Cm[ ,lapply(.SD, sum), by = "ki"]
tt5Cm$momm <- tt5Cm$st  
for(i in 2:dim(tt5Cm)[1]-1) {
  tt5Cm$momm[i+1]<-(tt5Cm$mom[i]+tt5Cm$st[i])*exp(C)
}  

tt6Cm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt6Cm) <- c("min","st")
tt6C <- tt6C[,c("min","st")]
tt6Cm <- merge(tt6Cm, tt6C, all = TRUE,by = c('min'))
tt6Cm <- tt6Cm[,c(1,3)]
names(tt6Cm) <- c("ki","st")
NonNAindex <- which(is.na(tt6Cm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt6Cm[NonNAindex,]$st = 0
}
tt6Cm <- data.table(tt6Cm)
tt6Cm <- tt6Cm[ ,lapply(.SD, sum), by = "ki"]
tt6Cm$momm <- tt6Cm$st  
for(i in 2:dim(tt6Cm)[1]-1) {
  tt6Cm$momm[i+1]<-(tt6Cm$mom[i]+tt6Cm$st[i])*exp(C)
}  

tt7Cm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt7Cm) <- c("min","st")
tt7C <- tt7C[,c("min","st")]
tt7Cm <- merge(tt7Cm, tt7C, all = TRUE,by = c('min'))
tt7Cm <- tt7Cm[,c(1,3)]
names(tt7Cm) <- c("ki","st")
NonNAindex <- which(is.na(tt7Cm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt7Cm[NonNAindex,]$st = 0
}
tt7Cm <- data.table(tt7Cm)
tt7Cm <- tt7Cm[ ,lapply(.SD, sum), by = "ki"]
tt7Cm$momm <- tt7Cm$st  
for(i in 2:dim(tt7Cm)[1]-1) {
  tt7Cm$momm[i+1]<-(tt7Cm$mom[i]+tt7Cm$st[i])*exp(C)
}  

tt1Dm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt1Dm) <- c("min","st")
tt1D <- tt1D[,c("min","st")]
tt1Dm <- merge(tt1Dm, tt1D, all = TRUE,by = c('min'))
tt1Dm <- tt1Dm[,c(1,3)]
names(tt1Dm) <- c("ki","st")
NonNAindex <- which(is.na(tt1Dm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt1Dm[NonNAindex,]$st = 0
}
tt1Dm <- data.table(tt1Dm)
tt1Dm <- tt1Dm[ ,lapply(.SD, sum), by = "ki"]
tt1Dm$momm <- tt1Dm$st  
for(i in 2:dim(tt1Dm)[1]-1) {
  tt1Dm$momm[i+1]<-(tt1Dm$mom[i]+tt1Dm$st[i])*exp(C)
}  

tt2Bm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt2Bm) <- c("min","st")
tt2B <- tt2B[,c("min","st")]
tt2Bm <- merge(tt2Bm, tt2B, all = TRUE,by = c('min'))
tt2Bm <- tt2Bm[,c(1,3)]
names(tt2Bm) <- c("ki","st")
NonNAindex <- which(is.na(tt2Bm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt2Bm[NonNAindex,]$st = 0
}
tt2Bm <- data.table(tt2Bm)
tt2Bm <- tt2Bm[ ,lapply(.SD, sum), by = "ki"]
tt2Bm$momm <- tt2Bm$st  
for(i in 2:dim(tt2Bm)[1]-1) {
  tt2Bm$momm[i+1]<-(tt2Bm$mom[i]+tt2Bm$st[i])*exp(C)
}  

tt3Bm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt3Bm) <- c("min","st")
tt3B <- tt3B[,c("min","st")]
tt3Bm <- merge(tt3Bm, tt3B, all = TRUE,by = c('min'))
tt3Bm <- tt3Bm[,c(1,3)]
names(tt3Bm) <- c("ki","st")
NonNAindex <- which(is.na(tt3Bm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt3Bm[NonNAindex,]$st = 0
}
tt3Bm <- data.table(tt3Bm)
tt3Bm <- tt3Bm[ ,lapply(.SD, sum), by = "ki"]
tt3Bm$momm <- tt3Bm$st  
for(i in 2:dim(tt3Bm)[1]-1) {
  tt3Bm$momm[i+1]<-(tt3Bm$mom[i]+tt3Bm$st[i])*exp(C)
}  

tt4Bm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt4Bm) <- c("min","st")
tt4B <- tt4B[,c("min","st")]
tt4Bm <- merge(tt4Bm, tt4B, all = TRUE,by = c('min'))
tt4Bm <- tt4Bm[,c(1,3)]
names(tt4Bm) <- c("ki","st")
NonNAindex <- which(is.na(tt4Bm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt4Bm[NonNAindex,]$st = 0
}
tt4Bm <- data.table(tt4Bm)
tt4Bm <- tt4Bm[ ,lapply(.SD, sum), by = "ki"]
tt4Bm$momm <- tt4Bm$st  
for(i in 2:dim(tt4Bm)[1]-1) {
  tt4Bm$momm[i+1]<-(tt4Bm$mom[i]+tt4Bm$st[i])*exp(C)
}  

tt5Bm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt5Bm) <- c("min","st")
tt5B <- tt5B[,c("min","st")]
tt5Bm <- merge(tt5Bm, tt5B, all = TRUE,by = c('min'))
tt5Bm <- tt5Bm[,c(1,3)]
names(tt5Bm) <- c("ki","st")
NonNAindex <- which(is.na(tt5Bm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt5Bm[NonNAindex,]$st = 0
}
tt5Bm <- data.table(tt5Bm)
tt5Bm <- tt5Bm[ ,lapply(.SD, sum), by = "ki"]
tt5Bm$momm <- tt5Bm$st  
for(i in 2:dim(tt5Bm)[1]-1) {
  tt5Bm$momm[i+1]<-(tt5Bm$mom[i]+tt5Bm$st[i])*exp(C)
}  

tt6Bm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt6Bm) <- c("min","st")
tt6B <- tt6B[,c("min","st")]
tt6Bm <- merge(tt6Bm, tt6B, all = TRUE,by = c('min'))
tt6Bm <- tt6Bm[,c(1,3)]
names(tt6Bm) <- c("ki","st")
NonNAindex <- which(is.na(tt6Bm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt6Bm[NonNAindex,]$st = 0
}
tt6Bm <- data.table(tt6Bm)
tt6Bm <- tt6Bm[ ,lapply(.SD, sum), by = "ki"]
tt6Bm$momm <- tt6Bm$st  
for(i in 2:dim(tt6Bm)[1]-1) {
  tt6Bm$momm[i+1]<-(tt6Bm$mom[i]+tt6Bm$st[i])*exp(C)
}  

tt7Bm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt7Bm) <- c("min","st")
tt7B <- tt7B[,c("min","st")]
tt7Bm <- merge(tt7Bm, tt7B, all = TRUE,by = c('min'))
tt7Bm <- tt7Bm[,c(1,3)]
names(tt7Bm) <- c("ki","st")
NonNAindex <- which(is.na(tt7Bm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt7Bm[NonNAindex,]$st = 0
}
tt7Bm <- data.table(tt7Bm)
tt7Bm <- tt7Bm[ ,lapply(.SD, sum), by = "ki"]
tt7Bm$momm <- tt7Bm$st  
for(i in 2:dim(tt7Bm)[1]-1) {
  tt7Bm$momm[i+1]<-(tt7Bm$mom[i]+tt7Bm$st[i])*exp(C)
}  

tt1Cm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt1Cm) <- c("min","st")
tt1C <- tt1C[,c("min","st")]
tt1Cm <- merge(tt1Cm, tt1C, all = TRUE,by = c('min'))
tt1Cm <- tt1Cm[,c(1,3)]
names(tt1Cm) <- c("ki","st")
NonNAindex <- which(is.na(tt1Cm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt1Cm[NonNAindex,]$st = 0
}
tt1Cm <- data.table(tt1Cm)
tt1Cm <- tt1Cm[ ,lapply(.SD, sum), by = "ki"]
tt1Cm$momm <- tt1Cm$st  
for(i in 2:dim(tt1Cm)[1]-1) {
  tt1Cm$momm[i+1]<-(tt1Cm$mom[i]+tt1Cm$st[i])*exp(C)
}  

tt1Am <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt1Am) <- c("min","st")
tt1A <- tt1A[,c("min","st")]
tt1Am <- merge(tt1Am, tt1A, all = TRUE,by = c('min'))
tt1Am <- tt1Am[,c(1,3)]
names(tt1Am) <- c("ki","st")
NonNAindex <- which(is.na(tt1Am$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt1Am[NonNAindex,]$st = 0
}
tt1Am <- data.table(tt1Am)
tt1Am <- tt1Am[ ,lapply(.SD, sum), by = "ki"]
tt1Am$momm <- tt1Am$st  
for(i in 2:dim(tt1Am)[1]-1) {
  tt1Am$momm[i+1]<-(tt1Am$mom[i]+tt1Am$st[i])*exp(C)
}  


tt2Am <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt2Am) <- c("min","st")
tt2A <- tt2A[,c("min","st")]
tt2Am <- merge(tt2Am, tt2A, all = TRUE,by = c('min'))
tt2Am <- tt2Am[,c(1,3)]
names(tt2Am) <- c("ki","st")
NonNAindex <- which(is.na(tt2Am$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt2Am[NonNAindex,]$st = 0
}
tt2Am <- data.table(tt2Am)
tt2Am <- tt2Am[ ,lapply(.SD, sum), by = "ki"]
tt2Am$momm <- tt2Am$st  
for(i in 2:dim(tt2Am)[1]-1) {
  tt2Am$momm[i+1]<-(tt2Am$mom[i]+tt2Am$st[i])*exp(C)
}  

tt3Am <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt3Am) <- c("min","st")
tt3A <- tt3A[,c("min","st")]
tt3Am <- merge(tt3Am, tt3A, all = TRUE,by = c('min'))
tt3Am <- tt3Am[,c(1,3)]
names(tt3Am) <- c("ki","st")
NonNAindex <- which(is.na(tt3Am$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt3Am[NonNAindex,]$st = 0
}
tt3Am <- data.table(tt3Am)
tt3Am <- tt3Am[ ,lapply(.SD, sum), by = "ki"]
tt3Am$momm <- tt3Am$st  
for(i in 2:dim(tt3Am)[1]-1) {
  tt3Am$momm[i+1]<-(tt3Am$mom[i]+tt3Am$st[i])*exp(C)
}  

tt4Am <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt4Am) <- c("min","st")
tt4A <- tt4A[,c("min","st")]
tt4Am <- merge(tt4Am, tt4A, all = TRUE,by = c('min'))
tt4Am <- tt4Am[,c(1,3)]
names(tt4Am) <- c("ki","st")
NonNAindex <- which(is.na(tt4Am$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt4Am[NonNAindex,]$st = 0
}
tt4Am <- data.table(tt4Am)
tt4Am <- tt4Am[ ,lapply(.SD, sum), by = "ki"]
tt4Am$momm <- tt4Am$st  
for(i in 2:dim(tt4Am)[1]-1) {
  tt4Am$momm[i+1]<-(tt4Am$mom[i]+tt4Am$st[i])*exp(C)
}  

tt5Am <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt5Am) <- c("min","st")
tt5A <- tt5A[,c("min","st")]
tt5Am <- merge(tt5Am, tt5A, all = TRUE,by = c('min'))
tt5Am <- tt5Am[,c(1,3)]
names(tt5Am) <- c("ki","st")
NonNAindex <- which(is.na(tt5Am$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt5Am[NonNAindex,]$st = 0
}
tt5Am <- data.table(tt5Am)
tt5Am <- tt5Am[ ,lapply(.SD, sum), by = "ki"]
tt5Am$momm <- tt5Am$st  
for(i in 2:dim(tt5Am)[1]-1) {
  tt5Am$momm[i+1]<-(tt5Am$mom[i]+tt5Am$st[i])*exp(C)
}  

tt6Am <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt6Am) <- c("min","st")
tt6A <- tt6A[,c("min","st")]
tt6Am <- merge(tt6Am, tt6A, all = TRUE,by = c('min'))
tt6Am <- tt6Am[,c(1,3)]
names(tt6Am) <- c("ki","st")
NonNAindex <- which(is.na(tt6Am$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt6Am[NonNAindex,]$st = 0
}
tt6Am <- data.table(tt6Am)
tt6Am <- tt6Am[ ,lapply(.SD, sum), by = "ki"]
tt6Am$momm <- tt6Am$st  
for(i in 2:dim(tt6Am)[1]-1) {
  tt6Am$momm[i+1]<-(tt6Am$mom[i]+tt6Am$st[i])*exp(C)
}  

tt7Am <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt7Am) <- c("min","st")
tt7A <- tt7A[,c("min","st")]
tt7Am <- merge(tt7Am, tt7A, all = TRUE,by = c('min'))
tt7Am <- tt7Am[,c(1,3)]
names(tt7Am) <- c("ki","st")
NonNAindex <- which(is.na(tt7Am$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt7Am[NonNAindex,]$st = 0
}
tt7Am <- data.table(tt7Am)
tt7Am <- tt7Am[ ,lapply(.SD, sum), by = "ki"]
tt7Am$momm <- tt7Am$st  
for(i in 2:dim(tt7Am)[1]-1) {
  tt7Am$momm[i+1]<-(tt7Am$mom[i]+tt7Am$st[i])*exp(C)
}  

tt1Bm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt1Bm) <- c("min","st")
tt1B <- tt1B[,c("min","st")]
tt1Bm <- merge(tt1Bm, tt1B, all = TRUE,by = c('min'))
tt1Bm <- tt1Bm[,c(1,3)]
names(tt1Bm) <- c("ki","st")
NonNAindex <- which(is.na(tt1Bm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt1Bm[NonNAindex,]$st = 0
}
tt1Bm <- data.table(tt1Bm)
tt1Bm <- tt1Bm[ ,lapply(.SD, sum), by = "ki"]
tt1Bm$momm <- tt1Bm$st
for(i in 2:dim(tt1Bm)[1]-1) {
  tt1Bm$momm[i+1]<-(tt1Bm$mom[i]+tt1Bm$st[i])*exp(C)
}  

tt2Dm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt2Dm) <- c("min","st")
tt2D <- tt2D[,c("min","st")]
tt2Dm <- merge(tt2Dm, tt2D, all = TRUE,by = c('min'))
tt2Dm <- tt2Dm[,c(1,3)]
names(tt2Dm) <- c("ki","st")
NonNAindex <- which(is.na(tt2Dm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt2Dm[NonNAindex,]$st = 0
}
tt2Dm <- data.table(tt2Dm)
tt2Dm <- tt2Dm[ ,lapply(.SD, sum), by = "ki"]
tt2Dm$momm <- tt2Dm$st  
for(i in 2:dim(tt2Dm)[1]-1) {
  tt2Dm$momm[i+1]<-(tt2Dm$mom[i]+tt2Dm$st[i])*exp(C)
}  

tt3Dm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt3Dm) <- c("min","st")
tt3D <- tt3D[,c("min","st")]
tt3Dm <- merge(tt3Dm, tt3D, all = TRUE,by = c('min'))
tt3Dm <- tt3Dm[,c(1,3)]
names(tt3Dm) <- c("ki","st")
NonNAindex <- which(is.na(tt3Dm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt3Dm[NonNAindex,]$st = 0
}
tt3Dm <- data.table(tt3Dm)
tt3Dm <- tt3Dm[ ,lapply(.SD, sum), by = "ki"]
tt3Dm$momm <- tt3Dm$st  
for(i in 2:dim(tt3Dm)[1]-1) {
  tt3Dm$momm[i+1]<-(tt3Dm$mom[i]+tt3Dm$st[i])*exp(C)
}  


tt4Dm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt4Dm) <- c("min","st")
tt4D <- tt4D[,c("min","st")]
tt4Dm <- merge(tt4Dm, tt4D, all = TRUE,by = c('min'))
tt4Dm <- tt4Dm[,c(1,3)]
names(tt4Dm) <- c("ki","st")
NonNAindex <- which(is.na(tt4Dm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt4Dm[NonNAindex,]$st = 0
}
tt4Dm <- data.table(tt4Dm)
tt4Dm <- tt4Dm[ ,lapply(.SD, sum), by = "ki"]
tt4Dm$momm <- tt4Dm$st  
for(i in 2:dim(tt4Dm)[1]-1) {
  tt4Dm$momm[i+1]<-(tt4Dm$mom[i]+tt4Dm$st[i])*exp(C)
}  

tt5Dm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt5Dm) <- c("min","st")
tt5D <- tt5D[,c("min","st")]
tt5Dm <- merge(tt5Dm, tt5D, all = TRUE,by = c('min'))
tt5Dm <- tt5Dm[,c(1,3)]
names(tt5Dm) <- c("ki","st")
NonNAindex <- which(is.na(tt5Dm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt5Dm[NonNAindex,]$st = 0
}
tt5Dm <- data.table(tt5Dm)
tt5Dm <- tt5Dm[ ,lapply(.SD, sum), by = "ki"]
tt5Dm$momm <- tt5Dm$st  
for(i in 2:dim(tt5Dm)[1]-1) {
  tt5Dm$momm[i+1]<-(tt5Dm$mom[i]+tt5Dm$st[i])*exp(C)
}  


tt7Dm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt7Dm) <- c("min","st")
tt7D <- tt7D[,c("min","st")]
tt7Dm <- merge(tt7Dm, tt7D, all = TRUE,by = c('min'))
tt7Dm <- tt7Dm[,c(1,3)]
names(tt7Dm) <- c("ki","st")
NonNAindex <- which(is.na(tt7Dm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt7Dm[NonNAindex,]$st = 0
}
tt7Dm <- data.table(tt7Dm)
tt7Dm <- tt7Dm[ ,lapply(.SD, sum), by = "ki"]
tt7Dm$momm <- tt7Dm$st  
for(i in 2:dim(tt7Dm)[1]-1) {
  tt7Dm$momm[i+1]<-(tt7Dm$mom[i]+tt7Dm$st[i])*exp(C)
}  


tt6Dm <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
names(tt6Dm) <- c("min","st")
tt6D <- tt6D[,c("min","st")]
tt6Dm <- merge(tt6Dm, tt6D, all = TRUE,by = c('min'))
tt6Dm <- tt6Dm[,c(1,3)]
names(tt6Dm) <- c("ki","st")
NonNAindex <- which(is.na(tt6Dm$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  tt6Dm[NonNAindex,]$st = 0
}
tt6Dm <- data.table(tt6Dm)
tt6Dm <- tt6Dm[ ,lapply(.SD, sum), by = "ki"]
tt6Dm$momm <- tt6Dm$st  
for(i in 2:dim(tt6Dm)[1]-1) {
  tt6Dm$momm[i+1]<-(tt6Dm$mom[i]+tt6Dm$st[i])*exp(C)
}  











nonzero = 0
if (nonzero == 1){
  if (min(tt2C$momm) < 0){tt2C[which(tt2C$momm < 0),]$momm = 0}
  if (min(tt3C$momm) < 0){tt3C[which(tt3C$momm < 0),]$momm = 0}
  if (min(tt4C$momm) < 0){tt4C[which(tt4C$momm < 0),]$momm = 0}
  if (min(tt5C$momm) < 0){tt5C[which(tt5C$momm < 0),]$momm = 0}
  if (min(tt6C$momm) < 0){tt6C[which(tt6C$momm < 0),]$momm = 0}
  if (min(tt7C$momm) < 0){tt7C[which(tt7C$momm < 0),]$momm = 0}
  if (min(tt1D$momm) < 0){tt1D[which(tt1D$momm < 0),]$momm = 0}
  if (min(tt2D$momm) < 0){tt2D[which(tt2D$momm < 0),]$momm = 0}
  if (min(tt3D$momm) < 0){tt3D[which(tt3D$momm < 0),]$momm = 0}
  if (min(tt4D$momm) < 0){tt4D[which(tt4D$momm < 0),]$momm = 0}
  if (min(tt5D$momm) < 0){tt5D[which(tt5D$momm < 0),]$momm = 0}
  if (min(tt6D$momm) < 0){tt6D[which(tt6D$momm < 0),]$momm = 0}
  if (min(tt7D$momm) < 0){tt7D[which(tt7D$momm < 0),]$momm = 0}
  if (min(tt2B$momm) < 0){tt2B[which(tt2B$momm < 0),]$momm = 0}
  if (min(tt3B$momm) < 0){tt3B[which(tt3B$momm < 0),]$momm = 0}
  if (min(tt4B$momm) < 0){tt4B[which(tt4B$momm < 0),]$momm = 0}
  if (min(tt5B$momm) < 0){tt5B[which(tt5B$momm < 0),]$momm = 0}
  if (min(tt6B$momm) < 0){tt6B[which(tt6B$momm < 0),]$momm = 0}
  if (min(tt7B$momm) < 0){tt7B[which(tt7B$momm < 0),]$momm = 0}
  if (min(tt1C$momm) < 0){tt1C[which(tt1C$momm < 0),]$momm = 0}
  if (min(tt1A$momm) < 0){tt1A[which(tt1A$momm < 0),]$momm = 0}
  if (min(tt2A$momm) < 0){tt2A[which(tt2A$momm < 0),]$momm = 0}
  if (min(tt3A$momm) < 0){tt3A[which(tt3A$momm < 0),]$momm = 0}
  if (min(tt4A$momm) < 0){tt4A[which(tt4A$momm < 0),]$momm = 0}
  if (min(tt5A$momm) < 0){tt5A[which(tt5A$momm < 0),]$momm = 0}
  if (min(tt6A$momm) < 0){tt6A[which(tt6A$momm < 0),]$momm = 0}
  if (min(tt7A$momm) < 0){tt7A[which(tt7A$momm < 0),]$momm = 0}
  if (min(tt1B$momm) < 0){tt1B[which(tt1B$momm < 0),]$momm = 0}}


momm <- c(tt1Am$momm[length(tt1Am$momm)],tt2Am$momm[length(tt2Am$momm)],tt3Am$momm[length(tt3Am$momm)],tt4Am$momm[length(tt4Am$momm)],tt5Am$momm[length(tt5Am$momm)],tt6Am$momm[length(tt6Am$momm)],tt7Am$momm[length(tt7Am$momm)],tt1Bm$momm[length(tt1Bm$momm)],
          tt2Bm$momm[length(tt2Bm$momm)],tt3Bm$momm[length(tt3Bm$momm)],tt4Bm$momm[length(tt4Bm$momm)],tt5Bm$momm[length(tt5Bm$momm)],tt6Bm$momm[length(tt6Bm$momm)],tt7Bm$momm[length(tt7Bm$momm)],tt1Cm$momm[length(tt1Cm$momm)],
          tt2Cm$momm[length(tt2Cm$momm)],tt3Cm$momm[length(tt3Cm$momm)],tt4Cm$momm[length(tt4Cm$momm)],tt5Cm$momm[length(tt5Cm$momm)],tt6Cm$momm[length(tt6Cm$momm)],tt7Cm$momm[length(tt7Cm$momm)],tt1Dm$momm[length(tt1Dm$momm)],
          tt2Dm$momm[length(tt2Dm$momm)],tt3Dm$momm[length(tt3Dm$momm)],tt4Dm$momm[length(tt4Dm$momm)],tt5Dm$momm[length(tt5Dm$momm)],tt6Dm$momm[length(tt6Dm$momm)],tt7Dm$momm[length(tt7Dm$momm)])


ki <-  c("1A","2A","3A","4A","5A","6A","7A","1B","2B","3B","4B","5B","6B","7B","1C","2C","3C","4C","5C","6C","7C","1D","2D","3D","4D","5D","6D","7D")
count <- data.frame(ki,momm)


#### MOMENTUM KAN LET FINDES HER!!!!!!!!!!

t1 <- c("/?mode=momentum&1A=",as.character(count[which(count$ki == "1A"),2]))    
t2 <- c("&2A=",as.character(count[which(count$ki == "2A"),2]))    
t3 <- c("&3A=",as.character(count[which(count$ki == "3A"),2]))    
t4 <- c("&4A=",as.character(count[which(count$ki == "4A"),2]))    
t5 <- c("&5A=",as.character(count[which(count$ki == "5A"),2]))    
t6 <- c("&6A=",as.character(count[which(count$ki == "6A"),2]))    
t7 <- c("&7A=",as.character(count[which(count$ki == "7A"),2]))    
t8 <- c("&1B=",as.character(count[which(count$ki == "1B"),2]))    
t9 <- c("&2B=",as.character(count[which(count$ki == "2B"),2]))    
t10 <- c("&3B=",as.character(count[which(count$ki == "3B"),2]))    
t11 <- c("&4B=",as.character(count[which(count$ki == "4B"),2]))    
t12 <- c("&5B=",as.character(count[which(count$ki == "5B"),2]))    
t13 <- c("&6B=",as.character(count[which(count$ki == "6B"),2]))    
t14 <- c("&7B=",as.character(count[which(count$ki == "7B"),2]))    
t15 <- c("&1C=",as.character(count[which(count$ki == "1C"),2]))
t16 <- c("&2C=",as.character(count[which(count$ki == "2C"),2]))    
t17 <- c("&3C=",as.character(count[which(count$ki == "3C"),2]))    
t18 <- c("&4C=",as.character(count[which(count$ki == "4C"),2]))    
t19 <- c("&5C=",as.character(count[which(count$ki == "5C"),2]))    
t20 <- c("&6C=",as.character(count[which(count$ki == "6C"),2]))    
t21 <- c("&7C=",as.character(count[which(count$ki == "7C"),2]))    
t22 <- c("&1D=",as.character(count[which(count$ki == "1D"),2]))    
t23 <- c("&2D=",as.character(count[which(count$ki == "2D"),2]))    
t24 <- c("&3D=",as.character(count[which(count$ki == "3D"),2]))    
t25 <- c("&4D=",as.character(count[which(count$ki == "4D"),2]))    
t26 <- c("&5D=",as.character(count[which(count$ki == "5D"),2]))    
t27 <- c("&6D=",as.character(count[which(count$ki == "6D"),2]))    
t28 <- c("&7D=",as.character(count[which(count$ki == "7D"),2]))    

tt1 <- paste(t1,collapse="");
tt2<-paste(t2,collapse="");
tt3<-paste(t3,collapse="");
tt4<-paste(t4,collapse="");
tt5<-paste(t5,collapse="");
tt6<-paste(t6,collapse="");
tt7<-paste(t7,collapse="");
tt8<-paste(t8,collapse="");
tt9<-paste(t9,collapse="");
tt10<-paste(t10,collapse="");
tt11<-paste(t11,collapse="");
tt12<-paste(t12,collapse="");
tt13<-paste(t13,collapse="");
tt14<-paste(t14,collapse="");
tt15<-paste(t15,collapse="");
tt16<-paste(t16,collapse="");
tt17<-paste(t17,collapse="");
tt18<-paste(t18,collapse="");
tt19<-paste(t19,collapse="");
tt20<-paste(t20,collapse="");
tt21<-paste(t21,collapse="");
tt22<-paste(t22,collapse="");
tt23<-paste(t23,collapse="");
tt24<-paste(t24,collapse="");
tt25<-paste(t25,collapse="");
tt26<-paste(t26,collapse="");
tt27<-paste(t27,collapse="");
tt28<-paste(t28,collapse="");

JC2 <- paste(c(tt1,tt2,tt3,tt4,tt5,tt6,tt7,tt8,tt9,tt10,tt11,tt12,tt13,tt14,tt15,tt16,tt17,tt18,tt19,tt20,tt21,tt22,tt23,tt24,tt25,tt26,tt27,tt28),collapse="");
getToHost("127.0.0.1",JC2,"", port=8081)





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
  datam$momm[i+1]<-(datam$mom[i]+datam$st[i])*exp(C)
}  


if (print == 1){png(file = "display/plots/plot9.png",res=reso)}
par(mar=c(5, 4.5, 4, 2) + 0.1)
plot(datam$min,datam$momm,type="l",col="blue",xaxt="n",las=1,ylab="",xlab="")
abline(v=c(540,540+24*60,540+24*60+24*60),lty=2,col="black")
if(max(data$min) < 1440) {
  hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
  axis(1, at=seq(0,2880,60), labels=hours,las = 2)
} else if (max(data$min) > 1440){
  hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
  axis(1, at=seq(0,2880+1440,120), labels=hours,las = 2)
}
title(main="Hamringmomentum* for daempning \n paa 2.34% per minut",mgp=c(4,1,0), font.main = 1)
title(ylab="Hamringmomentum",mgp=c(3.0,1,0))
title(mgp=c(3.5,1,0),xlab="*En form for 'hamrings-hastigheds-speedometer'")
if (print == 1){dev.off()}


ki <-  c("1A","2A","3A","4A","5A","6A","7A","1B","2B","3B","4B","5B","6B","7B","1C","2C","3C","4C","5C","6C","7C","1D","2D","3D","4D","5D","6D","7D")
st <- rep(0,28)
tabtemp <- data.frame(ki,st)
countiner <- count


count <- data.table(data[,c("ki","st")])
count <- count[ ,lapply(.SD, sum), by = ki]
count <- data.frame(count)
names(tabtemp) <- c("ki","st")
count <- merge(count, tabtemp, all = TRUE,by = c('ki'))
count <- count[,1:2]
names(count) <- c("ki","st")
NonNAindex <- which(is.na(count$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  count[NonNAindex,]$st = 0
}
count <- count[order(count[,"ki"]),] # Sorting
tableprint <- merge(count, countiner, all = TRUE,by = c('ki'))
names(tableprint) <- c("Koekken","Streger","momentum")




et1 <- data[which(data$min < 540),]
ki <-ki <- c("1A","2A","3A","4A","5A","6A","7A","1B","2B","3B","4B","5B","6B","7B","1C","2C","3C","4C","5C","6C","7C","1D","2D","3D","4D","5D","6D","7D")
st <- rep(0,28)
tabtemp <- data.frame(ki,st)

et1 <- data.table(et1[,1:2])
et1 <- et1[ ,lapply(.SD, sum), by = ki]
et1 <- et1[order(et1[,ki]),] # Sorting
et1 <- data.frame(et1)
et1 <- merge(et1, tabtemp, all = TRUE,by = c('ki'))
et1 <- et1[,1:2]
names(et1) <- c("ki","st")
NonNAindex <- which(is.na(et1$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  et1[NonNAindex,]$st = 0
}
names(et1) <- c("Koekken","Streger")
tableprint <- merge(tableprint,et1, all = TRUE,by = c('Koekken'))
names(tableprint) <- c("Koekken","Streger","momentum","Etape 1")


et2 <- data[which(data$min > 540 & data$min < 1980),]
ki <-  c("1A","2A","3A","4A","5A","6A","7A","1B","2B","3B","4B","5B","6B","7B","1C","2C","3C","4C","5C","6C","7C","1D","2D","3D","4D","5D","6D","7D")
st <- rep(0,28)
tabtemp <- data.frame(ki,st)
et2 <- data.table(et2[,1:2])
et2 <- et2[ ,lapply(.SD, sum), by = ki]
et2 <- et2[order(et2[,ki]),] # Sorting
et2 <- data.frame(et2)
et2 <- merge(et2, tabtemp, all = TRUE,by = c('ki'))
et2 <- et2[,1:2]
names(et2) <- c("ki","st")
NonNAindex <- which(is.na(et2$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  et2[NonNAindex,]$st = 0
}
names(et2) <- c("Koekken","Streger")
tableprint <- merge(tableprint,et2, all = TRUE,by = c('Koekken'))
names(tableprint) <- c("Koekken","Streger","momentum","Etape 1","Etape 2")



et3 <- data[which(data$min > 1980),]
ki <-  c("1A","2A","3A","4A","5A","6A","7A","1B","2B","3B","4B","5B","6B","7B","1C","2C","3C","4C","5C","6C","7C","1D","2D","3D","4D","5D","6D","7D")
st <- rep(0,28)
tabtemp <- data.frame(ki,st)
et3 <- data.table(et3[,1:2])
et3 <- et3[ ,lapply(.SD, sum), by = ki]
et3 <- et3[order(et3[,ki]),] # Sorting
et3 <- data.frame(et3)
et3 <- merge(et3, tabtemp, all = TRUE,by = c('ki'))
et3 <- et3[,1:2]
names(et3) <- c("ki","st")
NonNAindex <- which(is.na(et3$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  et3[NonNAindex,]$st = 0
}
names(et3) <- c("Koekken","Streger")
tableprint <- merge(tableprint,et3, all = TRUE,by = c('Koekken'))
names(tableprint) <- c("Koekken","Streger","Momentum","Torsd.","Fred.","Lord.")
tableprint$Momentum <- round(tableprint$Momentum)





# if (print == 1){png(file = "display/plots/plot10.png",res=reso-22)}
# grid.newpage() 
# pushViewport(viewport(layout.pos.col=2, layout.pos.row=2, clip="on"))
# grid.draw(tableGrob(tableprint, gp=gpar(fontsize=12, lwd=.5)))
# popViewport()
# if (print == 1){dev.off()}

temp2 <- max(c(tt2Cm$momm,tt3Cm$momm,tt4Cm$momm,tt5Cm$momm,tt6Cm$momm,tt7Cm$momm,tt1Dm$momm,tt2Dm$momm,tt3Dm$momm,tt4Dm$momm,tt5Dm$momm,tt6Dm$momm,tt7Dm$momm,tt2Bm$momm,tt3Bm$momm,tt4Bm$momm,tt5Bm$momm,tt6Bm$momm,tt7Bm$momm,tt1Cm$momm,tt1Am$momm,tt2Am$momm,tt3Am$momm,tt4Am$momm,tt5Am$momm,tt6Am$momm,tt7Am$momm,tt1Bm$momm))

if (print == 1){png(file = "display/plots/plot11.png",res=reso)}
par(mar=c(6, 5, 4, 2) + 0.1)
plot(tt2Cm$ki,tt2Cm$momm,type="l",col=1,xaxt="n",las=1,ylab="",xlab="",ylim = c(0,temp2),xlim=c(0,max(data$min)))
points(tt3Cm$ki,tt3Cm$momm,type="l",col=2,xaxt="n",las=1,ylab="",xlab="")
points(tt4Cm$ki,tt4Cm$momm,type="l",col=3,xaxt="n",las=1,ylab="",xlab="")
points(tt5Cm$ki,tt5Cm$momm,type="l",col=4,xaxt="n",las=1,ylab="",xlab="")
points(tt6Cm$ki,tt6Cm$momm,type="l",col=5,xaxt="n",las=1,ylab="",xlab="")
points(tt7Cm$ki,tt7Cm$momm,type="l",col=6,xaxt="n",las=1,ylab="",xlab="")
points(tt1Dm$ki,tt1Dm$momm,type="l",col=7,xaxt="n",las=1,ylab="",xlab="")
points(tt2Bm$ki,tt2Bm$momm,type="l",lty=2,col=1,xaxt="n",las=1,ylab="",xlab="")
points(tt3Bm$ki,tt3Bm$momm,type="l",lty=2,col=2,xaxt="n",las=1,ylab="",xlab="")
points(tt4Bm$ki,tt4Bm$momm,type="l",lty=2,col=3,xaxt="n",las=1,ylab="",xlab="")
points(tt5Bm$ki,tt5Bm$momm,type="l",lty=2,col=4,xaxt="n",las=1,ylab="",xlab="")
points(tt6Bm$ki,tt6Bm$momm,type="l",lty=2,col=5,xaxt="n",las=1,ylab="",xlab="")
points(tt7Bm$ki,tt7Bm$momm,type="l",lty=2,col=6,xaxt="n",las=1,ylab="",xlab="")
points(tt1Cm$ki,tt1Cm$momm,type="l",lty=2,col=7,xaxt="n",las=1,ylab="",xlab="")
points(tt1Am$ki,tt1Am$momm,type="l",lty=4,col=1,xaxt="n",las=1,ylab="",xlab="")
points(tt2Am$ki,tt2Am$momm,type="l",lty=4,col=2,xaxt="n",las=1,ylab="",xlab="")
points(tt3Am$ki,tt3Am$momm,type="l",lty=4,col=3,xaxt="n",las=1,ylab="",xlab="")
points(tt4Am$ki,tt4Am$momm,type="l",lty=4,col=4,xaxt="n",las=1,ylab="",xlab="")
points(tt5Am$ki,tt5Am$momm,type="l",lty=4,col=5,xaxt="n",las=1,ylab="",xlab="")
points(tt6Am$ki,tt6Am$momm,type="l",lty=4,col=6,xaxt="n",las=1,ylab="",xlab="")
points(tt7Am$ki,tt7Am$momm,type="l",lty=4,col=7,xaxt="n",las=1,ylab="",xlab="")
points(tt1Bm$ki,tt1Bm$momm,type="l",lty=4,col=8,xaxt="n",las=1,ylab="",xlab="")
points(tt2Dm$ki,tt2Dm$momm,type="l",col=2,xaxt="n",las=1,ylab="",xlab="")
points(tt3Dm$ki,tt3Dm$momm,type="l",col=3,xaxt="n",las=1,ylab="",xlab="")
points(tt4Dm$ki,tt4Dm$momm,type="l",col=4,xaxt="n",las=1,ylab="",xlab="")
points(tt5Dm$ki,tt5Dm$momm,type="l",col=5,xaxt="n",las=1,ylab="",xlab="")
points(tt6Dm$ki,tt6Dm$momm,type="l",col=6,xaxt="n",las=1,ylab="",xlab="")
points(tt7Dm$ki,tt7Dm$momm,type="l",col=7,xaxt="n",las=1,ylab="",xlab="")
abline(v=c(540,540+24*60,540+24*60+24*60),lty=2,col="black")
leg <- ki <- c("1A","2A","3A","4A","5A","6A","7A","1B","2B","3B","4B","5B","6B","7B","1C","2C","3C","4C","5C","6C","7C","1D","2D","3D","4D","5D","6D","7D")
legend("topleft", inset=.05,leg, horiz=FALSE,lty=c(rep(1,7),rep(2,7),rep(4,8)),col=c(1:7,1:7,1:8,1:7),ncol=3, cex=.8,lwd=1,bg="transparent")
if(max(data$yhour) < 24) {
  hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
  axis(1, at=seq(0,2880,60), labels=hours,las = 2)
} else if (max(data$yhour) > 24){
  hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
  axis(1, at=seq(0,2880+1440,120), labels=hours,las = 2)
}
title(main="Hamringmomentum med daemping \n paa 2.34% for hele marathon-baren",xlab="Tidspunkt",mgp=c(4,1,0), font.main = 1)
title(ylab="Momentum [steger/sekund]",mgp=c(3.5,1,0))
text(c(270,1440/2+540,2430,2430+1500),c(10,10,10,10), c("torsdag","fredag","loerdag","soendag"))
if (print == 1){dev.off()}



x <- tt1Am$ki
y <- tt1Am$momm
id <- order(x)
gron <- data.frame(max(tt1Am$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt1Atroje <- data.frame("1A",gron,prik)
names(tt1Atroje) <- c("Kokken","gron","prik")

x <- tt2Am$ki
y <- tt2Am$momm
id <- order(x)
gron <- data.frame(max(tt2Am$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt2Atroje <- data.frame("2A",gron,prik)
names(tt2Atroje) <- c("Kokken","gron","prik")

x <- tt3Am$ki
y <- tt3Am$momm
id <- order(x)
gron <- data.frame(max(tt3Am$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt3Atroje <- data.frame("3A",gron,prik)
names(tt3Atroje) <- c("Kokken","gron","prik")

x <- tt4Am$ki
y <- tt4Am$momm
id <- order(x)
gron <- data.frame(max(tt4Am$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt4Atroje <- data.frame("4A",gron,prik)
names(tt4Atroje) <- c("Kokken","gron","prik")

x <- tt5Am$ki
y <- tt5Am$momm
id <- order(x)
gron <- data.frame(max(tt5Am$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt5Atroje <- data.frame("5A",gron,prik)
names(tt5Atroje) <- c("Kokken","gron","prik")

x <- tt6Am$ki
y <- tt6Am$momm
id <- order(x)
gron <- data.frame(max(tt6Am$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt6Atroje <- data.frame("6A",gron,prik)
names(tt6Atroje) <- c("Kokken","gron","prik")

x <- tt7Am$ki
y <- tt7Am$momm
id <- order(x)
gron <- data.frame(max(tt7Am$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt7Atroje <- data.frame("7A",gron,prik)
names(tt7Atroje) <- c("Kokken","gron","prik")

x <- tt1Bm$ki
y <- tt1Bm$momm
id <- order(x)
gron <- data.frame(max(tt1Bm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt1Btroje <- data.frame("1B",gron,prik)
names(tt1Btroje) <- c("Kokken","gron","prik")

x <- tt2Bm$ki
y <- tt2Bm$momm
id <- order(x)
gron <- data.frame(max(tt2Bm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt2Btroje <- data.frame("2B",gron,prik)
names(tt2Btroje) <- c("Kokken","gron","prik")

x <- tt3Bm$ki
y <- tt3Bm$momm
id <- order(x)
gron <- data.frame(max(tt3Bm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt3Btroje <- data.frame("3B",gron,prik)
names(tt3Btroje) <- c("Kokken","gron","prik")

x <- tt4Bm$ki
y <- tt4Bm$momm
id <- order(x)
gron <- data.frame(max(tt4Bm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt4Btroje <- data.frame("4B",gron,prik)
names(tt4Btroje) <- c("Kokken","gron","prik")

x <- tt5Bm$ki
y <- tt5Bm$momm
id <- order(x)
gron <- data.frame(max(tt5Bm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt5Btroje <- data.frame("5B",gron,prik)
names(tt5Btroje) <- c("Kokken","gron","prik")

x <- tt6Bm$ki
y <- tt6Bm$momm
id <- order(x)
gron <- data.frame(max(tt6Bm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt6Btroje <- data.frame("6B",gron,prik)
names(tt6Btroje) <- c("Kokken","gron","prik")

x <- tt7Bm$ki
y <- tt7Bm$momm
id <- order(x)
gron <- data.frame(max(tt7Bm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt7Btroje <- data.frame("7B",gron,prik)
names(tt7Btroje) <- c("Kokken","gron","prik")

x <- tt1Cm$ki
y <- tt1Cm$momm
id <- order(x)
gron <- data.frame(max(tt1Cm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt1Ctroje <- data.frame("1C",gron,prik)
names(tt1Ctroje) <- c("Kokken","gron","prik")

x <- tt2Cm$ki
y <- tt2Cm$momm
id <- order(x)
gron <- data.frame(max(tt2Cm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt2Ctroje <- data.frame("2C",gron,prik)
names(tt2Ctroje) <- c("Kokken","gron","prik")

x <- tt3Bm$ki
y <- tt3Bm$momm
id <- order(x)
gron <- data.frame(max(tt3Bm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt3Btroje <- data.frame("3B",gron,prik)
names(tt3Btroje) <- c("Kokken","gron","prik")

x <- tt3Cm$ki
y <- tt3Cm$momm
id <- order(x)
gron <- data.frame(max(tt3Cm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt3Ctroje <- data.frame("3C",gron,prik)
names(tt3Ctroje) <- c("Kokken","gron","prik")

x <- tt4Cm$ki
y <- tt4Cm$momm
id <- order(x)
gron <- data.frame(max(tt4Cm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt4Ctroje <- data.frame("4C",gron,prik)
names(tt4Ctroje) <- c("Kokken","gron","prik")

x <- tt5Cm$ki
y <- tt5Cm$momm
id <- order(x)
gron <- data.frame(max(tt5Cm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt5Ctroje <- data.frame("5C",gron,prik)
names(tt5Ctroje) <- c("Kokken","gron","prik")

x <- tt6Cm$ki
y <- tt6Cm$momm
id <- order(x)
gron <- data.frame(max(tt6Cm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt6Ctroje <- data.frame("6C",gron,prik)
names(tt6Ctroje) <- c("Kokken","gron","prik")

x <- tt7Cm$ki
y <- tt7Cm$momm
id <- order(x)
gron <- data.frame(max(tt7Cm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt7Ctroje <- data.frame("7C",gron,prik)
names(tt7Ctroje) <- c("Kokken","gron","prik")

x <- tt1Dm$ki
y <- tt1Dm$momm
id <- order(x)
gron <- data.frame(max(tt1Dm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt1Dtroje <- data.frame("1D",gron,prik)
names(tt1Dtroje) <- c("Kokken","gron","prik")

x <- tt2Dm$ki
y <- tt2Dm$momm
id <- order(x)
gron <- data.frame(max(tt2Dm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt2Dtroje <- data.frame("2D",gron,prik)
names(tt2Dtroje) <- c("Kokken","gron","prik")


x <- tt3Dm$ki
y <- tt3Dm$momm
id <- order(x)
gron <- data.frame(max(tt3Dm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt3Dtroje <- data.frame("3D",gron,prik)
names(tt3Dtroje) <- c("Kokken","gron","prik")

x <- tt4Dm$ki
y <- tt4Dm$momm
id <- order(x)
gron <- data.frame(max(tt4Dm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt4Dtroje <- data.frame("4D",gron,prik)
names(tt4Dtroje) <- c("Kokken","gron","prik")

x <- tt5Dm$ki
y <- tt5Dm$momm
id <- order(x)
gron <- data.frame(max(tt5Dm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt5Dtroje <- data.frame("5D",gron,prik)
names(tt5Dtroje) <- c("Kokken","gron","prik")

x <- tt6Dm$ki
y <- tt6Dm$momm
id <- order(x)
gron <- data.frame(max(tt6Dm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt6Dtroje <- data.frame("6D",gron,prik)
names(tt6Dtroje) <- c("Kokken","gron","prik")

x <- tt7Dm$ki
y <- tt7Dm$momm
id <- order(x)
gron <- data.frame(max(tt7Dm$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
tt7Dtroje <- data.frame("7D",gron,prik)
names(tt7Dtroje) <- c("Kokken","gron","prik")


troje <- rbind(tt1Atroje,tt2Atroje,tt3Atroje,tt4Atroje,tt5Atroje,tt6Atroje,tt7Atroje,tt1Btroje,tt2Btroje,tt3Btroje,tt4Btroje,tt5Btroje,tt6Btroje,tt7Btroje,tt1Ctroje,tt2Ctroje,tt3Ctroje,tt4Ctroje,tt5Ctroje,tt6Ctroje,tt7Ctroje,tt1Dtroje,tt2Dtroje,tt3Dtroje,tt4Dtroje,tt5Dtroje,tt6Dtroje,tt7Dtroje)


trojeprik <- troje[order(troje[,"prik"]),] # Sorting
trojegron <- troje[order(troje[,"gron"]),] # Sorting

if (print == 1){png(file = "display/plots/plot12.png",res=reso)}
barplot(troje$gron,xlab="*det stoerst opnaaede momentum",ylab="Maximalt opnaaet momentum",names.arg = troje[1:28,1],las=2,col=rainbow(29),main="Pointstillingen i konkurrencen \n om den groenne troeje*", font.main = 1)
if (print == 1){dev.off()}

if (print == 1){png(file = "display/plots/plot13.png",res=reso)}
barplot(troje$prik/1e4,xlab="*stoerste areal under momentumkurven",ylab="Areal under momentum kurve (*1e4)",names.arg = troje[1:28,1],las=2,col=rainbow(29),main="Pointstillingen i konkurrencen \n om den prikkede troeje*", font.main = 1)
if (print == 1){dev.off()}

a<-c("/?mode=jersey&yellow=",as.character(count[which(count$st == max(count$st)),1]))
b<-c("&green=",as.character(troje[which(troje$gron == max(troje$gron)),1]))
c<-c("&dotted=",as.character(troje[which(troje$prik == max(troje$prik)),1]))
a<-paste(a,collapse="");b<-paste(b,collapse="");c<-paste(c,collapse="")
JC<-paste(c(a,b),collapse="");JC<-paste(c(JC,c),collapse="")


getToHost("127.0.0.1",JC,"", port=8081)

