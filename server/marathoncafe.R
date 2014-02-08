rm(list = ls()) # Clear workspace
install_packages = 0
load_packages = 1
print = 1
database = "CSV" # "SQLite"
reso <- 91
udkomt <- 0 # 0 = udkommenteret
# setwd("~/Dropbox/Marathoncafe")

# Packages:
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
  library(ggplot2) 
  library(zoo) 
  library(lattice)
  library(grid)
  library(gridExtra)
  library(RSQLite)
  library(data.table) 
  library(plotrix)
  library(httpRequest)  
}

if (database == "CSV"){
  print("Remember to set file location")  
  data <- read.table("data.csv", sep=";", quote="\"")    
} else if (database == "SQLite") {
  con <- dbConnect(drv="SQLite", dbname="db.sqlite")
  tables <- dbListTables(con)
  tables <- tables[tables != "sqlite_sequence"]
  lDataFrames <- vector("list", length=length(tables))
  for (i in seq(along=tables)) {
    lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
  }
  data <- data.frame(lDataFrames)
}

names(data) <- c("ki","st","utime")
starttime = data$utime[1]
data$ti <- as.POSIXct(data[,3], origin="1970-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S")
endtime = data$utime[nrow(data)]


#####
# Streger per k??kken
ki <- c("GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8","ML2","ML3","ML4","ML5","ML6","ML7","ML8","NY2","NY3","NY4","NY5","NY6","NY7","NY8")
st <- rep(0,22)
tabtemp <- data.frame(ki,st)

# if (udkomt == 1){
# count <- data.table(data[,1:2])
# count <- count[ ,lapply(.SD, sum), by = ki]
# count <- count[order(count[,ki]),] # Sorting
# count <- data.frame(count)
# count <- merge(count, tabtemp, all = TRUE,by = c('ki'))
# count <- count[,1:2]
# names(count) <- c("ki","st")
# NonNAindex <- which(is.na(count$st))
# if (length(NonNAindex) == 0){  
#   print("Do nothing")
# } else if (length(NonNAindex) > 0){
#   count[NonNAindex,]$st = 0
# }
# if (print == udkomt){png(file = "display/plots/plot1.png",res=reso)}
# barplot(count$st,xlab="Koekken",ylab="Antal streger",names.arg = count[1:22,1],las=2,col=rainbow(23),main="Antal streger fordelt paa koekkener")
# if (print == udkomt){dev.off()}
# }


#####
# Streger per k??kken 2
count <- data.table(data[,1:2])
count <- count[ ,lapply(.SD, sum), by = ki]
count <- data.frame(count)
count <- merge(count, tabtemp, all = TRUE,by = c('ki'))
count <- count[,1:2]
names(count) <- c("ki","st")
NonNAindex <- which(is.na(count$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  count[NonNAindex,]$st = 0
}
count <- count[order(count[,"st"]),] # Sorting
if (print == 1){png(file = "display/plots/plot1.png",res=reso)}
barplot(count$st,xlab="Koekken",ylab="Antal streger",names.arg = count[1:22,1],las=2,col=rainbow(23),main="Antal streger fordelt \n paa koekkener")
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
par(mar=c(6, 6, 4, 2) + 0.1)
plot(data$min,data$cumst,type="l",col="blue",xaxt="n",las=1,ylab="",xlab="",cex=0.9)
abline(v=c(540,540+24*60),lty=2,col="red")
if(max(data$yhour) < 24) {
  hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
  axis(1, at=seq(0,2880,60), labels=hours,las = 2)
}else{
  hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
  axis(1, at=seq(0,2880,120), labels=hours,las = 2)
}
title(main="Kumulerede antal streger paa minut- \nbasis under hele marathoncafeen",xlab="Tidspunkt",mgp=c(4,1,0))
title(ylab="Antal streger",mgp=c(5,1,0))
text(c(270,1440/2+540,2430),c(10,10,10), c("fredag","loerdag","soendag"))
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
# title(main="Kumulerede antal streger for alle koekkener \n for hele marathoncafeen fordelt paa timer",xlab="Tidspunkt",mgp=c(4,1,0))
# title(ylab="Antal streger",mgp=c(5,1,0))
# text(c(4.5,9+12,42),c(10,10,10), c("fredag","loerdag","soendag"))
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

if (print == 1){png(file = "display/plots/plot3.png",res=reso)}
par(mar=c(5, 4.5, 4, 2) + 0.1)
plot(count$st,type="l",col="blue",xaxt="n",las=1,ylab="",xlab="")
abline(v=c(9,9+24),lty=2,col="red")
  hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
  axis(1, at=seq(0,48,2), labels=hours,las = 2)
title(main="Antal streger fordelt paa timer for alle \n koekkener for hele marathoncafeen",xlab="Tidspunkt",mgp=c(4,1,0))
title(ylab="Antal streger",mgp=c(3.5,1,0))
text(c(4.5,9+12,42),c(10,10,10), c("fredag","loerdag","soendag"))
if (print == 1){dev.off()}


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
# title(main="Antal streger per minut for alle koekkener \n for hele marathoncafeen",xlab="Tidspunkt",mgp=c(4,1,0))
# title(ylab="Antal streger",mgp=c(3.5,1,0))
# text(c(270,1440/2+540,2430),c(10,10,10), c("fredag","loerdag","soendag"),col="black")
# if (print == udkomt){dev.off()}
# }

if (print == 1){png(file = "display/plots/plot4.png",res=reso)}
par(mar=c(5, 4.5, 4, 2) + 0.1)
plot(count$st,type="l",col="blue",xaxt="n",las=1,ylab="",xlab="",xlim=c(max(data$min)-120,max(data$min)))
abline(v=c(540,540+24*60),lty=2,col="red")
hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
axis(1, at=seq(0,2880,60), labels=hours,las = 2)
title(main="Antal streger for alle koekkkener \n per minut de seneste to timer",xlab="Tidspunkt",mgp=c(4,1,0))
title(ylab="Antal streger",mgp=c(3.5,1,0))
text(c(270,1440/2+540,2430),c(10,10,10), c("fredag","loerdag","soendag"))
if (print == 1){dev.off()}


##
# Barvagt plot - antal streger p?? egent k??kken. 
barvagt <- c("Personalet","G1","M7","M4","N5","G8","G3","N8","N7","G6","Festival","G5","M6","N6","G4","G1","Revyen","Cafeen")
tidspunkt <- c(4*60,3*60,2.5*60,2.5*60,2.5*60,2.5*60,3*60,3*60,120,120,120,180,2.5*60,2.5*60,2.5*60,2.5*60,180,180)
tidspunkt <- cumsum(tidspunkt)
vagt <- data.frame(tidspunkt=tidspunkt,barvagt=barvagt)
data$vagt <- 0
if (length(which(data$ki == "GL1" & data$min >= 240 & data$min < 420))>0){data[which(data$ki == "GL1" & data$min >= 240 & data$min < 420),]$vagt = "GL1"}
if (length(which(data$ki == "ML7" & data$min >= 420 & data$min < 570))>0){data[which(data$ki == "ML7" & data$min >= 420 & data$min < 570),]$vagt = "ML7"}
if (length(which(data$ki == "ML3" & data$min >= 420 & data$min < 570))>0){data[which(data$ki == "ML3" & data$min >= 420 & data$min < 570),]$vagt = "ML3"} # Forhalsbar
if (length(which(data$ki == "ML4" & data$min >= 570 & data$min < 720))>0){data[which(data$ki == "ML4" & data$min >= 570 & data$min < 720),]$vagt = "ML4"}
if (length(which(data$ki == "NY4" & data$min >= 570 & data$min < 720))>0){data[which(data$ki == "NY4" & data$min >= 570 & data$min < 720),]$vagt = "NY4"} # Forhalsbar
if (length(which(data$ki == "NY5" & data$min >= 720 & data$min < 870) )>0){data[which(data$ki == "NY5" & data$min >= 720 & data$min < 870),]$vagt = "NY5"}
if (length(which(data$ki == "GL8" & data$min >= 870 & data$min < 1020) )>0){data[which(data$ki == "GL8" & data$min >= 870 & data$min < 1020),]$vagt = "GL8"}
if (length(which(data$ki == "GL3" & data$min >= 1020 & data$min < 1200) )>0){data[which(data$ki == "GL3" & data$min >= 1020 & data$min < 1200),]$vagt = "GL3"}
if (length(which(data$ki == "NY8" & data$min >= 1200 & data$min < 1380) )>0){data[which(data$ki == "NY8" & data$min >= 1200 & data$min < 1380),]$vagt = "NY8"}
if (length(which(data$ki == "NY7" & data$min >= 1380 & data$min < 1500) )>0){data[which(data$ki == "NY7" & data$min >= 1380 & data$min < 1500),]$vagt = "NY7"}
if (length(which(data$ki == "GL6" & data$min >= 1500 & data$min < 1620) )>0){data[which(data$ki == "GL6" & data$min >= 1500 & data$min < 1620),]$vagt = "GL6"}
# data[which(data$ki == "GL1" & data$min >= 1620 & data$min < 1740),]$vagt = "Festival"
if (length(which(data$ki == "GL5" & data$min >= 1740 & data$min < 1920) )>0){data[which(data$ki == "GL5" & data$min >= 1740 & data$min < 1920),]$vagt = "GL5"}
if (length(which(data$ki == "ML6" & data$min >= 1920 & data$min < 2070) )>0){data[which(data$ki == "ML6" & data$min >= 1920 & data$min < 2070),]$vagt = "ML6"}
if (length(which(data$ki == "ML5" & data$min >= 2010 & data$min < 2160) )>0){data[which(data$ki == "ML5" & data$min >= 2010 & data$min < 2160),]$vagt = "ML5"} # Forhalsbar
if (length(which(data$ki == "NY6" & data$min >= 2070 & data$min < 2220) )>0){data[which(data$ki == "NY6" & data$min >= 2070 & data$min < 2220),]$vagt = "NY6"}
if (length(which(data$ki == "GL4" & data$min >= 2220 & data$min < 2370) )>0){data[which(data$ki == "GL4" & data$min >= 2220 & data$min < 2370),]$vagt = "GL4"}
if (length(which(data$ki == "GL1" & data$min >= 2370 & data$min < 2520) )>0){data[which(data$ki == "GL1" & data$min >= 2370 & data$min < 2520),]$vagt = "GL1"}
#data[which(data$ki == "GL1" & data$min >= 2520 & data$min < 2700),]$vagt = "Revyen"
#data[which(data$ki == "GL1" & data$min >= 2700 & data$min < 2880),]$vagt = "Cafeen"

vagt <- c("GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8","ML2","ML3","ML4","ML5","ML6","ML7","ML8","NY2","NY3","NY4","NY5","NY6","NY7","NY8")
st <- rep(0,22)
count <- data.table(data[,c("vagt","st")])
count <- count[ ,lapply(.SD, sum), by = vagt]
count <- count[order(count[,vagt]),] # Sorting
count <- data.frame(count)
count <- count[-1,]
tabtemp <- data.frame(vagt,st)
count <- merge(count, tabtemp, all = TRUE,by = c('vagt'))
count <- count[,1:2]
cheats <- count
names(cheats) <- c("ki","vagt")
NonNAindex <- which(is.na(cheats$vagt))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  cheats[NonNAindex,]$vagt = 0
}


if (print == 1){png(file = "display/plots/plot5.png",res=reso)}       
if (sum(cheats$vagt)==0){
  plot(0,col="white",xlab="",ylab="",xaxt="n",yaxt="n")  
  text(1,0,c("*This plot will be generated when data is available* \n (Antal streger sat paa det koekken med barvagten)"),cex=0.85)
} else {
  barplot(cheats$vagt,xlab="Koekken",ylab="Antal streger",names.arg = cheats[1:22,1],las=3,col=rainbow(23),main="Antal streger sat paa det koekken med barvagten")}
if (print == 1){dev.off()}

start <- read.table("start.csv", sep=";", quote="\"") 
start$V3 <- starttime
names(start) <- c("ki","st","utime")

####
# Kommuleret antal streger fordelt p?? minutter og k??kkener

data$cumst<-cumsum(data$st)
start[4:10] <- data[1,4:10,]  
start$cumst <- 0
datac <- rbind(start,data)

NY2 <- (datac[which(datac$ki == "NY2"),])
NY3 <- (datac[which(datac$ki == "NY3"),])
NY4 <- (datac[which(datac$ki == "NY4"),])
NY5 <- (datac[which(datac$ki == "NY5"),])
NY6 <- (datac[which(datac$ki == "NY6"),])
NY7 <- (datac[which(datac$ki == "NY7"),])
NY8 <- (datac[which(datac$ki == "NY8"),])
ML2 <- (datac[which(datac$ki == "ML2"),])
ML3 <- (datac[which(datac$ki == "ML3"),])
ML4 <- (datac[which(datac$ki == "ML4"),])
ML5 <- (datac[which(datac$ki == "ML5"),])
ML6 <- (datac[which(datac$ki == "ML6"),])
ML7 <- (datac[which(datac$ki == "ML7"),])
ML8 <- (datac[which(datac$ki == "ML8"),])
GL1 <- (datac[which(datac$ki == "GL1"),])
GL2 <- (datac[which(datac$ki == "GL2"),])
GL3 <- (datac[which(datac$ki == "GL3"),])
GL4 <- (datac[which(datac$ki == "GL4"),])
GL5 <- (datac[which(datac$ki == "GL5"),])
GL6 <- (datac[which(datac$ki == "GL6"),])
GL7 <- (datac[which(datac$ki == "GL7"),])
GL8 <- (datac[which(datac$ki == "GL8"),])
NY2$cumst <- cumsum(NY2$st)
NY3$cumst <- cumsum(NY3$st)
NY4$cumst <- cumsum(NY4$st)
NY5$cumst <- cumsum(NY5$st)
NY6$cumst <- cumsum(NY6$st)
NY7$cumst <- cumsum(NY7$st)
NY8$cumst <- cumsum(NY8$st)
ML2$cumst <- cumsum(ML2$st)
ML3$cumst <- cumsum(ML3$st)
ML4$cumst <- cumsum(ML4$st)
ML5$cumst <- cumsum(ML5$st)
ML6$cumst <- cumsum(ML6$st)
ML7$cumst <- cumsum(ML7$st)
ML8$cumst <- cumsum(ML8$st)
GL1$cumst <- cumsum(GL1$st)
GL2$cumst <- cumsum(GL2$st)
GL3$cumst <- cumsum(GL3$st)
GL4$cumst <- cumsum(GL4$st)
GL5$cumst <- cumsum(GL5$st)
GL6$cumst <- cumsum(GL6$st)
GL7$cumst <- cumsum(GL7$st)
GL8$cumst <- cumsum(GL8$st)

temporary <- c(NY2$cumst,NY3$cumst,NY4$cumst,NY5$cumst,NY6$cumst,NY7$cumst,NY8$cumst,ML2$cumst,ML3$cumst,ML4$cumst,ML5$cumst,ML6$cumst,ML7$cumst,ML8$cumst,GL1$cumst,GL2$cumst,GL3$cumst,GL4$cumst,GL5$cumst,GL6$cumst,GL7$cumst,GL8$cumst)
temporary <- max(temporary)

if (data$min[length(data$min)] > 30){
if (print == 1){png(file = "display/plots/plot6.png",res=reso)}
par(mar=c(6, 5, 4, 2) + 0.1)
plot(NY2$min,NY2$cumst,type="l",col=1,xaxt="n",las=1,ylab="",xlab="",ylim = c(0,temporary))
points(NY3$min,NY3$cumst,type="l",col=2,xaxt="n",las=1,ylab="",xlab="")
points(NY4$min,NY4$cumst,type="l",col=3,xaxt="n",las=1,ylab="",xlab="")
points(NY5$min,NY5$cumst,type="l",col=4,xaxt="n",las=1,ylab="",xlab="")
points(NY6$min,NY6$cumst,type="l",col=5,xaxt="n",las=1,ylab="",xlab="")
points(NY7$min,NY7$cumst,type="l",col=6,xaxt="n",las=1,ylab="",xlab="")
points(NY8$min,NY8$cumst,type="l",col=7,xaxt="n",las=1,ylab="",xlab="")
points(ML2$min,ML2$cumst,type="l",lty=2,col=1,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(ML3$min,ML3$cumst,type="l",lty=2,col=2,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(ML4$min,ML4$cumst,type="l",lty=2,col=3,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(ML5$min,ML5$cumst,type="l",lty=2,col=4,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(ML6$min,ML6$cumst,type="l",lty=2,col=5,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(ML7$min,ML7$cumst,type="l",lty=2,col=6,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(ML8$min,ML8$cumst,type="l",lty=2,col=7,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL1$min,GL1$cumst,type="l",lty=3,col=1,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL2$min,GL2$cumst,type="l",lty=4,col=1,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL3$min,GL3$cumst,type="l",lty=4,col=2,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL4$min,GL4$cumst,type="l",lty=4,col=3,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL5$min,GL5$cumst,type="l",lty=4,col=4,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL6$min,GL6$cumst,type="l",lty=4,col=5,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL7$min,GL7$cumst,type="l",lty=4,col=6,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL8$min,GL8$cumst,type="l",lty=4,col=7,xaxt="n",las=1,ylab="",xlab="",lwd=2)
abline(v=c(540,540+24*60),lty=1,col="black")
leg <- c("ML2","ML3","ML4","ML5","ML6","ML7","ML8","GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8")
legend("topleft", inset=.05,leg, horiz=FALSE,lty=c(rep(2,7),rep(3,7)),col=c(1:7,1:7),ncol=2, cex=.75,lwd=2,bg="transparent")
leg <- c("NY2","NY3","NY4","NY5","NY6","NY7","NY8")
legend("bottomright", inset=.05,leg, horiz=FALSE,lty=c(rep(1,8)),col=c(1:8),ncol=3, cex=.75,bg="transparent")
if(max(data$yhour) < 24) {
  hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
  axis(1, at=seq(0,2880,60), labels=hours,las = 2)
} else if (max(data$yhour) > 24){
  hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
  axis(1, at=seq(0,2880,120), labels=hours,las = 2)
}
title(main="Kumulerede antal streger fordelt paa \n koekkener for hele marathoncafeen",mgp=c(4,1,0),cex=0.9)
title(ylab="Antal streger",xlab="Tidspunkt",mgp=c(3.5,1,0))
text(c(270,1440/2+540,2430),c(10,10,10), c("fredag","loerdag","soendag"))
if (print == 1){dev.off()}
} else if (data$min[length(data$min)] <= 30){
  if (print == 1){png(file = "display/plots/plot6.png",res=reso)}
  plot(0,col="white",xlab="",ylab="",xaxt="n",yaxt="n")  
  text(1,0,c("This plot will be generated when \n after 30 minutes"))
  if (print == 1){dev.off()}
}

count <- data.table(datac[,1:2])
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
count <- count[order(count[,"st"]),] # Sorting

if (dim(count)[1] >= 3){
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
  par(mar=c(6, 5, 4, 2) + 0.1)
  plot(datanr1$min,datanr1$cumst,type="l",col=1,xaxt="n",las=1,ylab="",xlab="",ylim = c(0,temporary))
  points(datanr2$min,datanr2$cumst,type="l",col=2,xaxt="n",las=1,ylab="",xlab="")
  points(datanr3$min,datanr3$cumst,type="l",col=3,xaxt="n",las=1,ylab="",xlab="")  
  leg <- c(nr1,nr2,nr3)
  abline(v=c(540,540+24*60),lty=1,col="black")
  legend("topleft", inset=.05,leg, horiz=FALSE,lty=1,col=c(1:3),ncol=1, cex=.90,lwd=1,bg="transparent")
  if(max(data$yhour) < 24) {
    hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
    axis(1, at=seq(0,2880,60), labels=hours,las = 2)
  } else if (max(data$yhour) > 24){
    hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
    axis(1, at=seq(0,2880,120), labels=hours,las = 2)
  }
  title(main="Kumulerede antal streger per minut for de tre \n foerende koekkener for hele marathoncafeen",xlab="Tidspunkt",mgp=c(4,1,0))
  title(ylab="Antal streger",mgp=c(3.5,1,0))
  text(c(270,1440/2+540,2430),c(10,10,10), c("fredag","loerdag","soendag"))
  if (print == 1){dev.off()}  
} else if (dim(count)[1] < 3){
  if (print == 1){png(file = "display/plots/plot7.png",res=reso)}  
  plot(0,col="white",xlab="",ylab="",xaxt="n",yaxt="n")  
  text(1,0,c("This plot will be generated when \n data is available"))
  if (print == 1){dev.off()}
}



# Implement hamring momentuma
NY2 <- NY2[,c("min","st","cumst")]
NY3 <- NY3[,c("min","st","cumst")]
NY4 <- NY4[,c("min","st","cumst")]
NY5 <- NY5[,c("min","st","cumst")]
NY6 <- NY6[,c("min","st","cumst")]
NY7 <- NY7[,c("min","st","cumst")]
NY8 <- NY8[,c("min","st","cumst")]
ML2 <- ML2[,c("min","st","cumst")]
ML3 <- ML3[,c("min","st","cumst")]
ML4 <- ML4[,c("min","st","cumst")]
ML5 <- ML5[,c("min","st","cumst")]
ML6 <- ML6[,c("min","st","cumst")]
ML7 <- ML7[,c("min","st","cumst")]
ML8 <- ML8[,c("min","st","cumst")]
GL1 <- GL1[,c("min","st","cumst")]
GL2 <- GL2[,c("min","st","cumst")]
GL3 <- GL3[,c("min","st","cumst")]
GL4 <- GL4[,c("min","st","cumst")]
GL5 <- GL5[,c("min","st","cumst")]
GL6 <- GL6[,c("min","st","cumst")]
GL7 <- GL7[,c("min","st","cumst")]
GL8 <- GL8[,c("min","st","cumst")]


if (length(GL1$min)==1){
  GL1=data.frame(t(matrix(rep(0,8),4)))
  names(GL1)=c("min","st","cumst","momm")
}
if (length(GL2$min)==1){
  GL2=data.frame(t(matrix(rep(0,8),4)))
  names(GL2)=c("min","st","cumst","momm")
}
if (length(GL3$min)==1){
  GL3=data.frame(t(matrix(rep(0,8),4)))
  names(GL3)=c("min","st","cumst","momm")
}
if (length(GL4$min)==1){
  GL4=data.frame(t(matrix(rep(0,8),4)))
  names(GL4)=c("min","st","cumst","momm")
}
if (length(GL5$min)==1){
  GL5=data.frame(t(matrix(rep(0,8),4)))
  names(GL5)=c("min","st","cumst","momm")
}
if (length(GL6$min)==1){
  GL6<-data.frame(t(matrix(rep(0,8),4)))
  names(GL6)=c("min","st","cumst","momm")
}
if (length(GL7$min)==1){
  GL7=data.frame(t(matrix(rep(0,8),4)))
  names(GL7)=c("min","st","cumst","momm")
}
if (length(GL8$min)==1){
  GL8=data.frame(t(matrix(rep(0,8),4)))
  names(GL8)=c("min","st","cumst","momm")
}
if (length(ML2$min)==1){
  ML2=data.frame(t(matrix(rep(0,8),4)))
  names(ML2)=c("min","st","cumst","momm")
}
if (length(ML3$min)==1){
  ML3=data.frame(t(matrix(rep(0,8),4)))
  names(ML3)=c("min","st","cumst","momm")
}
if (length(ML4$min)==1){
  ML4=data.frame(t(matrix(rep(0,8),4)))
  names(ML4)=c("min","st","cumst","momm")
}
if (length(ML5$min)==1){
  ML5=data.frame(t(matrix(rep(0,8),4)))
  names(ML5)=c("min","st","cumst","momm")
}
if (length(ML6$min)==1){
  ML6=data.frame(t(matrix(rep(0,8),4)))
  names(ML6)=c("min","st","cumst","momm")
}
if (length(ML7$min)==1){
  ML7=data.frame(t(matrix(rep(0,8),4)))
  names(ML7)=c("min","st","cumst","momm")
}
if (length(ML8$min)==1){
  ML8=data.frame(t(matrix(rep(0,8),4)))
  names(ML8)=c("min","st","cumst","momm")
}
if (length(NY2$min)==1){
  NY2=data.frame(t(matrix(rep(0,8),4)))
  names(NY2)=c("min","st","cumst","momm")
}
if (length(NY3$min)==1){
  NY3=data.frame(t(matrix(rep(0,8),4)))
  names(NY3)=c("min","st","cumst","momm")
}
if (length(NY4$min)==1){
  NY4=data.frame(t(matrix(rep(0,8),4)))
  names(NY4)=c("min","st","cumst","momm")
}
if (length(NY5$min)==1){
  NY5=data.frame(t(matrix(rep(0,8),4)))
  names(NY5)=c("min","st","cumst","momm")
}
if (length(NY6$min)==1){
  NY6=data.frame(t(matrix(rep(0,8),4)))
  names(NY6)=c("min","st","cumst","momm")
}
if (length(NY7$min)==1){
  NY7=data.frame(t(matrix(rep(0,8),4)))
  names(NY7)=c("min","st","cumst","momm")
}
if (length(NY8$min)==1){
  NY8=data.frame(t(matrix(rep(0,8),4)))
  names(NY8)=c("min","st","cumst","momm")
}

C <- -0.0234
NY2m <- data.frame(seq(0,dim(NY2)[1]-1,1),rep(0,dim(NY2)[1]))
names(NY2m) <- c("min","st")
NY2 <- NY2[,c("min","st")]
NY2m <- merge(NY2m, NY2, all = TRUE,by = c('min'))
NY2m <- NY2m[,c(1,3)]
names(NY2m) <- c("ki","st")
NonNAindex <- which(is.na(NY2m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  NY2m[NonNAindex,]$st = 0
}
NY2m <- data.table(NY2m)
NY2m <- NY2m[ ,lapply(.SD, sum), by = "ki"]
NY2m$momm <- NY2m$st  
for(i in 2:dim(NY2m)[1]-1) {
  NY2m$momm[i+1]<-(NY2m$mom[i]+NY2m$st[i])*exp(C)
}  

NY3m <- data.frame(seq(0,dim(NY3)[1]-1,1),rep(0,dim(NY3)[1]))
names(NY3m) <- c("min","st")
NY3 <- NY3[,c("min","st")]
NY3m <- merge(NY3m, NY3, all = TRUE,by = c('min'))
NY3m <- NY3m[,c(1,3)]
names(NY3m) <- c("ki","st")
NonNAindex <- which(is.na(NY3m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  NY3m[NonNAindex,]$st = 0
}
NY3m <- data.table(NY3m)
NY3m <- NY3m[ ,lapply(.SD, sum), by = "ki"]
NY3m$momm <- NY3m$st  
for(i in 2:dim(NY3m)[1]-1) {
NY3m$momm[i+1]<-(NY3m$mom[i]+NY3m$st[i])*exp(C)
}  

NY4m <- data.frame(seq(0,dim(NY4)[1]-1,1),rep(0,dim(NY4)[1]))
names(NY4m) <- c("min","st")
NY4 <- NY4[,c("min","st")]
NY4m <- merge(NY4m, NY4, all = TRUE,by = c('min'))
NY4m <- NY4m[,c(1,3)]
names(NY4m) <- c("ki","st")
NonNAindex <- which(is.na(NY4m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  NY4m[NonNAindex,]$st = 0
}
NY4m <- data.table(NY4m)
NY4m <- NY4m[ ,lapply(.SD, sum), by = "ki"]
NY4m$momm <- NY4m$st  
for(i in 2:dim(NY4m)[1]-1) {
  NY4m$momm[i+1]<-(NY4m$mom[i]+NY4m$st[i])*exp(C)
}  

NY5m <- data.frame(seq(0,dim(NY5)[1]-1,1),rep(0,dim(NY5)[1]))
names(NY5m) <- c("min","st")
NY5 <- NY5[,c("min","st")]
NY5m <- merge(NY5m, NY5, all = TRUE,by = c('min'))
NY5m <- NY5m[,c(1,3)]
names(NY5m) <- c("ki","st")
NonNAindex <- which(is.na(NY5m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  NY5m[NonNAindex,]$st = 0
}
NY5m <- data.table(NY5m)
NY5m <- NY5m[ ,lapply(.SD, sum), by = "ki"]
NY5m$momm <- NY5m$st  
for(i in 2:dim(NY5m)[1]-1) {
  NY5m$momm[i+1]<-(NY5m$mom[i]+NY5m$st[i])*exp(C)
}  

NY6m <- data.frame(seq(0,dim(NY6)[1]-1,1),rep(0,dim(NY6)[1]))
names(NY6m) <- c("min","st")
NY6 <- NY6[,c("min","st")]
NY6m <- merge(NY6m, NY6, all = TRUE,by = c('min'))
NY6m <- NY6m[,c(1,3)]
names(NY6m) <- c("ki","st")
NonNAindex <- which(is.na(NY6m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  NY6m[NonNAindex,]$st = 0
}
NY6m <- data.table(NY6m)
NY6m <- NY6m[ ,lapply(.SD, sum), by = "ki"]
NY6m$momm <- NY6m$st  
for(i in 2:dim(NY6m)[1]-1) {
  NY6m$momm[i+1]<-(NY6m$mom[i]+NY6m$st[i])*exp(C)
}  

NY7m <- data.frame(seq(0,dim(NY7)[1]-1,1),rep(0,dim(NY7)[1]))
names(NY7m) <- c("min","st")
NY7 <- NY7[,c("min","st")]
NY7m <- merge(NY7m, NY7, all = TRUE,by = c('min'))
NY7m <- NY7m[,c(1,3)]
names(NY7m) <- c("ki","st")
NonNAindex <- which(is.na(NY7m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  NY7m[NonNAindex,]$st = 0
}
NY7m <- data.table(NY7m)
NY7m <- NY7m[ ,lapply(.SD, sum), by = "ki"]
NY7m$momm <- NY7m$st  
for(i in 2:dim(NY7m)[1]-1) {
  NY7m$momm[i+1]<-(NY7m$mom[i]+NY7m$st[i])*exp(C)
}  

NY8m <- data.frame(seq(0,dim(NY8)[1]-1,1),rep(0,dim(NY8)[1]))
names(NY8m) <- c("min","st")
NY8 <- NY8[,c("min","st")]
NY8m <- merge(NY8m, NY8, all = TRUE,by = c('min'))
NY8m <- NY8m[,c(1,3)]
names(NY8m) <- c("ki","st")
NonNAindex <- which(is.na(NY8m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  NY8m[NonNAindex,]$st = 0
}
NY8m <- data.table(NY8m)
NY8m <- NY8m[ ,lapply(.SD, sum), by = "ki"]
NY8m$momm <- NY8m$st  
for(i in 2:dim(NY8m)[1]-1) {
  NY8m$momm[i+1]<-(NY8m$mom[i]+NY8m$st[i])*exp(C)
}  

ML2m <- data.frame(seq(0,dim(ML2)[1]-1,1),rep(0,dim(ML2)[1]))
names(ML2m) <- c("min","st")
ML2 <- ML2[,c("min","st")]
ML2m <- merge(ML2m, ML2, all = TRUE,by = c('min'))
ML2m <- ML2m[,c(1,3)]
names(ML2m) <- c("ki","st")
NonNAindex <- which(is.na(ML2m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  ML2m[NonNAindex,]$st = 0
}
ML2m <- data.table(ML2m)
ML2m <- ML2m[ ,lapply(.SD, sum), by = "ki"]
ML2m$momm <- ML2m$st  
for(i in 2:dim(ML2m)[1]-1) {
  ML2m$momm[i+1]<-(ML2m$mom[i]+ML2m$st[i])*exp(C)
}  

ML3m <- data.frame(seq(0,dim(ML3)[1]-1,1),rep(0,dim(ML3)[1]))
names(ML3m) <- c("min","st")
ML3 <- ML3[,c("min","st")]
ML3m <- merge(ML3m, ML3, all = TRUE,by = c('min'))
ML3m <- ML3m[,c(1,3)]
names(ML3m) <- c("ki","st")
NonNAindex <- which(is.na(ML3m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  ML3m[NonNAindex,]$st = 0
}
ML3m <- data.table(ML3m)
ML3m <- ML3m[ ,lapply(.SD, sum), by = "ki"]
ML3m$momm <- ML3m$st  
for(i in 2:dim(ML3m)[1]-1) {
  ML3m$momm[i+1]<-(ML3m$mom[i]+ML3m$st[i])*exp(C)
}  

ML4m <- data.frame(seq(0,dim(ML4)[1]-1,1),rep(0,dim(ML4)[1]))
names(ML4m) <- c("min","st")
ML4 <- ML4[,c("min","st")]
ML4m <- merge(ML4m, ML4, all = TRUE,by = c('min'))
ML4m <- ML4m[,c(1,3)]
names(ML4m) <- c("ki","st")
NonNAindex <- which(is.na(ML4m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  ML4m[NonNAindex,]$st = 0
}
ML4m <- data.table(ML4m)
ML4m <- ML4m[ ,lapply(.SD, sum), by = "ki"]
ML4m$momm <- ML4m$st  
for(i in 2:dim(ML4m)[1]-1) {
  ML4m$momm[i+1]<-(ML4m$mom[i]+ML4m$st[i])*exp(C)
}  

ML5m <- data.frame(seq(0,dim(ML5)[1]-1,1),rep(0,dim(ML5)[1]))
names(ML5m) <- c("min","st")
ML5 <- ML5[,c("min","st")]
ML5m <- merge(ML5m, ML5, all = TRUE,by = c('min'))
ML5m <- ML5m[,c(1,3)]
names(ML5m) <- c("ki","st")
NonNAindex <- which(is.na(ML5m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  ML5m[NonNAindex,]$st = 0
}
ML5m <- data.table(ML5m)
ML5m <- ML5m[ ,lapply(.SD, sum), by = "ki"]
ML5m$momm <- ML5m$st  
for(i in 2:dim(ML5m)[1]-1) {
  ML5m$momm[i+1]<-(ML5m$mom[i]+ML5m$st[i])*exp(C)
}  

ML6m <- data.frame(seq(0,dim(ML6)[1]-1,1),rep(0,dim(ML6)[1]))
names(ML6m) <- c("min","st")
ML6 <- ML6[,c("min","st")]
ML6m <- merge(ML6m, ML6, all = TRUE,by = c('min'))
ML6m <- ML6m[,c(1,3)]
names(ML6m) <- c("ki","st")
NonNAindex <- which(is.na(ML6m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  ML6m[NonNAindex,]$st = 0
}
ML6m <- data.table(ML6m)
ML6m <- ML6m[ ,lapply(.SD, sum), by = "ki"]
ML6m$momm <- ML6m$st  
for(i in 2:dim(ML6m)[1]-1) {
  ML6m$momm[i+1]<-(ML6m$mom[i]+ML6m$st[i])*exp(C)
}  

ML7m <- data.frame(seq(0,dim(ML7)[1]-1,1),rep(0,dim(ML7)[1]))
names(ML7m) <- c("min","st")
ML7 <- ML7[,c("min","st")]
ML7m <- merge(ML7m, ML7, all = TRUE,by = c('min'))
ML7m <- ML7m[,c(1,3)]
names(ML7m) <- c("ki","st")
NonNAindex <- which(is.na(ML7m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  ML7m[NonNAindex,]$st = 0
}
ML7m <- data.table(ML7m)
ML7m <- ML7m[ ,lapply(.SD, sum), by = "ki"]
ML7m$momm <- ML7m$st  
for(i in 2:dim(ML7m)[1]-1) {
  ML7m$momm[i+1]<-(ML7m$mom[i]+ML7m$st[i])*exp(C)
}  

ML8m <- data.frame(seq(0,dim(ML8)[1]-1,1),rep(0,dim(ML8)[1]))
names(ML8m) <- c("min","st")
ML8 <- ML8[,c("min","st")]
ML8m <- merge(ML8m, ML8, all = TRUE,by = c('min'))
ML8m <- ML8m[,c(1,3)]
names(ML8m) <- c("ki","st")
NonNAindex <- which(is.na(ML8m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  ML8m[NonNAindex,]$st = 0
}
ML8m <- data.table(ML8m)
ML8m <- ML8m[ ,lapply(.SD, sum), by = "ki"]
ML8m$momm <- ML8m$st  
for(i in 2:dim(ML8m)[1]-1) {
  ML8m$momm[i+1]<-(ML8m$mom[i]+ML8m$st[i])*exp(C)
}  

GL1m <- data.frame(seq(0,dim(GL1)[1]-1,1),rep(0,dim(GL1)[1]))
names(GL1m) <- c("min","st")
GL1 <- GL1[,c("min","st")]
GL1m <- merge(GL1m, GL1, all = TRUE,by = c('min'))
GL1m <- GL1m[,c(1,3)]
names(GL1m) <- c("ki","st")
NonNAindex <- which(is.na(GL1m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  GL1m[NonNAindex,]$st = 0
}
GL1m <- data.table(GL1m)
GL1m <- GL1m[ ,lapply(.SD, sum), by = "ki"]
GL1m$momm <- GL1m$st  
for(i in 2:dim(GL1m)[1]-1) {
  GL1m$momm[i+1]<-(GL1m$mom[i]+GL1m$st[i])*exp(C)
}  


GL2m <- data.frame(seq(0,dim(GL2)[1]-1,1),rep(0,dim(GL2)[1]))
names(GL2m) <- c("min","st")
GL2 <- GL2[,c("min","st")]
GL2m <- merge(GL2m, GL2, all = TRUE,by = c('min'))
GL2m <- GL2m[,c(1,3)]
names(GL2m) <- c("ki","st")
NonNAindex <- which(is.na(GL2m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  GL2m[NonNAindex,]$st = 0
}
GL2m <- data.table(GL2m)
GL2m <- GL2m[ ,lapply(.SD, sum), by = "ki"]
GL2m$momm <- GL2m$st  
for(i in 2:dim(GL2m)[1]-1) {
  GL2m$momm[i+1]<-(GL2m$mom[i]+GL2m$st[i])*exp(C)
}  

GL3m <- data.frame(seq(0,dim(GL3)[1]-1,1),rep(0,dim(GL3)[1]))
names(GL3m) <- c("min","st")
GL3 <- GL3[,c("min","st")]
GL3m <- merge(GL3m, GL3, all = TRUE,by = c('min'))
GL3m <- GL3m[,c(1,3)]
names(GL3m) <- c("ki","st")
NonNAindex <- which(is.na(GL3m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  GL3m[NonNAindex,]$st = 0
}
GL3m <- data.table(GL3m)
GL3m <- GL3m[ ,lapply(.SD, sum), by = "ki"]
GL3m$momm <- GL3m$st  
for(i in 2:dim(GL3m)[1]-1) {
  GL3m$momm[i+1]<-(GL3m$mom[i]+GL3m$st[i])*exp(C)
}  

GL4m <- data.frame(seq(0,dim(GL4)[1]-1,1),rep(0,dim(GL4)[1]))
names(GL4m) <- c("min","st")
GL4 <- GL4[,c("min","st")]
GL4m <- merge(GL4m, GL4, all = TRUE,by = c('min'))
GL4m <- GL4m[,c(1,3)]
names(GL4m) <- c("ki","st")
NonNAindex <- which(is.na(GL4m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  GL4m[NonNAindex,]$st = 0
}
GL4m <- data.table(GL4m)
GL4m <- GL4m[ ,lapply(.SD, sum), by = "ki"]
GL4m$momm <- GL4m$st  
for(i in 2:dim(GL4m)[1]-1) {
  GL4m$momm[i+1]<-(GL4m$mom[i]+GL4m$st[i])*exp(C)
}  

GL5m <- data.frame(seq(0,dim(GL5)[1]-1,1),rep(0,dim(GL5)[1]))
names(GL5m) <- c("min","st")
GL5 <- GL5[,c("min","st")]
GL5m <- merge(GL5m, GL5, all = TRUE,by = c('min'))
GL5m <- GL5m[,c(1,3)]
names(GL5m) <- c("ki","st")
NonNAindex <- which(is.na(GL5m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  GL5m[NonNAindex,]$st = 0
}
GL5m <- data.table(GL5m)
GL5m <- GL5m[ ,lapply(.SD, sum), by = "ki"]
GL5m$momm <- GL5m$st  
for(i in 2:dim(GL5m)[1]-1) {
  GL5m$momm[i+1]<-(GL5m$mom[i]+GL5m$st[i])*exp(C)
}  

GL6m <- data.frame(seq(0,dim(GL6)[1]-1,1),rep(0,dim(GL6)[1]))
names(GL6m) <- c("min","st")
GL6 <- GL6[,c("min","st")]
GL6m <- merge(GL6m, GL6, all = TRUE,by = c('min'))
GL6m <- GL6m[,c(1,3)]
names(GL6m) <- c("ki","st")
NonNAindex <- which(is.na(GL6m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  GL6m[NonNAindex,]$st = 0
}
GL6m <- data.table(GL6m)
GL6m <- GL6m[ ,lapply(.SD, sum), by = "ki"]
GL6m$momm <- GL6m$st  
for(i in 2:dim(GL6m)[1]-1) {
  GL6m$momm[i+1]<-(GL6m$mom[i]+GL6m$st[i])*exp(C)
}  

GL7m <- data.frame(seq(0,dim(GL7)[1]-1,1),rep(0,dim(GL7)[1]))
names(GL7m) <- c("min","st")
GL7 <- GL7[,c("min","st")]
GL7m <- merge(GL7m, GL7, all = TRUE,by = c('min'))
GL7m <- GL7m[,c(1,3)]
names(GL7m) <- c("ki","st")
NonNAindex <- which(is.na(GL7m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  GL7m[NonNAindex,]$st = 0
}
GL7m <- data.table(GL7m)
GL7m <- GL7m[ ,lapply(.SD, sum), by = "ki"]
GL7m$momm <- GL7m$st  
for(i in 2:dim(GL7m)[1]-1) {
  GL7m$momm[i+1]<-(GL7m$mom[i]+GL7m$st[i])*exp(C)
}  

GL8m <- data.frame(seq(0,dim(GL8)[1]-1,1),rep(0,dim(GL8)[1]))
names(GL8m) <- c("min","st")
GL8 <- GL8[,c("min","st")]
GL8m <- merge(GL8m, GL8, all = TRUE,by = c('min'))
GL8m <- GL8m[,c(1,3)]
names(GL8m) <- c("ki","st")
NonNAindex <- which(is.na(GL8m$st))
if (length(NonNAindex) == 0){  
  print("Do nothing")
} else if (length(NonNAindex) > 0){
  GL8m[NonNAindex,]$st = 0
}
GL8m <- data.table(GL8m)
GL8m <- GL8m[ ,lapply(.SD, sum), by = "ki"]
GL8m$momm <- GL8m$st
for(i in 2:dim(GL8m)[1]-1) {
  GL8m$momm[i+1]<-(GL8m$mom[i]+GL8m$st[i])*exp(C)
}  


nonzero = 0
if (nonzero == 1){
if (min(NY2$momm) < 0){NY2[which(NY2$momm < 0),]$momm = 0}
if (min(NY3$momm) < 0){NY3[which(NY3$momm < 0),]$momm = 0}
if (min(NY4$momm) < 0){NY4[which(NY4$momm < 0),]$momm = 0}
if (min(NY5$momm) < 0){NY5[which(NY5$momm < 0),]$momm = 0}
if (min(NY6$momm) < 0){NY6[which(NY6$momm < 0),]$momm = 0}
if (min(NY7$momm) < 0){NY7[which(NY7$momm < 0),]$momm = 0}
if (min(NY8$momm) < 0){NY8[which(NY8$momm < 0),]$momm = 0}
if (min(ML2$momm) < 0){ML2[which(ML2$momm < 0),]$momm = 0}
if (min(ML3$momm) < 0){ML3[which(ML3$momm < 0),]$momm = 0}
if (min(ML4$momm) < 0){ML4[which(ML4$momm < 0),]$momm = 0}
if (min(ML5$momm) < 0){ML5[which(ML5$momm < 0),]$momm = 0}
if (min(ML6$momm) < 0){ML6[which(ML6$momm < 0),]$momm = 0}
if (min(ML7$momm) < 0){ML7[which(ML7$momm < 0),]$momm = 0}
if (min(ML8$momm) < 0){ML8[which(ML8$momm < 0),]$momm = 0}
if (min(GL1$momm) < 0){GL1[which(GL1$momm < 0),]$momm = 0}
if (min(GL2$momm) < 0){GL2[which(GL2$momm < 0),]$momm = 0}
if (min(GL3$momm) < 0){GL3[which(GL3$momm < 0),]$momm = 0}
if (min(GL4$momm) < 0){GL4[which(GL4$momm < 0),]$momm = 0}
if (min(GL5$momm) < 0){GL5[which(GL5$momm < 0),]$momm = 0}
if (min(GL6$momm) < 0){GL6[which(GL6$momm < 0),]$momm = 0}
if (min(GL7$momm) < 0){GL7[which(GL7$momm < 0),]$momm = 0}
if (min(GL8$momm) < 0){GL8[which(GL8$momm < 0),]$momm = 0}}

momm <- c(GL1m$momm[length(GL1m$momm)],GL2m$momm[length(GL2m$momm)],GL3m$momm[length(GL3m$momm)],GL4m$momm[length(GL4m$momm)],GL5m$momm[length(GL5m$momm)],GL6m$momm[length(GL6m$momm)],GL7m$momm[length(GL7m$momm)],GL8m$momm[length(GL8m$momm)],
          ML2m$momm[length(ML2m$momm)],ML3m$momm[length(ML3m$momm)],ML4m$momm[length(ML4m$momm)],ML5m$momm[length(ML5m$momm)],ML6m$momm[length(ML6m$momm)],ML7m$momm[length(ML7m$momm)],ML8m$momm[length(ML8m$momm)],
          NY2m$momm[length(NY2m$momm)],NY3m$momm[length(NY3m$momm)],NY4m$momm[length(NY4m$momm)],NY5m$momm[length(NY5m$momm)],NY6m$momm[length(NY6m$momm)],NY7m$momm[length(NY7m$momm)],NY8m$momm[length(NY8m$momm)])

ki <- c("GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8","ML2","ML3","ML4","ML5","ML6","ML7","ML8","NY2","NY3","NY4","NY5","NY6","NY7","NY8")
count <- data.frame(ki,momm)
if (print == 1){png(file = "display/plots/plot8.png",res=reso)}
barplot(count$momm,xlab="*En form for 'hamrings-hastigheds-speedometer'",ylab="Hamringsmomentum",names.arg = count[1:22,1],las=2,col=rainbow(23),main="Hamringmomentum* paa koekkenerbasis \n med daempning paa 2.34% per minut")
if (print == 1){dev.off()}

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
abline(v=c(540,540+24*60),lty=1,col="black")
if(max(data$min) < 1440) {
  hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
  axis(1, at=seq(0,2880,60), labels=hours,las = 2)
} else if (max(data$min) > 1440){
  hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
  axis(1, at=seq(0,2880,120), labels=hours,las = 2)
}
title(main="Hamringmomentum* for daemp- \n ning paa 2.34% per minut",mgp=c(4,1,0))
title(ylab="Hamringmomentum",mgp=c(3.5,1,0),xlab="*En form for 'hamrings-hastigheds-speedometer'")
if (print == 1){dev.off()}


ki <- c("GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8","ML2","ML3","ML4","ML5","ML6","ML7","ML8","NY2","NY3","NY4","NY5","NY6","NY7","NY8")
st <- rep(0,22)
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
ki <- c("GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8","ML2","ML3","ML4","ML5","ML6","ML7","ML8","NY2","NY3","NY4","NY5","NY6","NY7","NY8")
st <- rep(0,22)
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
ki <- c("GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8","ML2","ML3","ML4","ML5","ML6","ML7","ML8","NY2","NY3","NY4","NY5","NY6","NY7","NY8")
st <- rep(0,22)
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
ki <- c("GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8","ML2","ML3","ML4","ML5","ML6","ML7","ML8","NY2","NY3","NY4","NY5","NY6","NY7","NY8")
st <- rep(0,22)
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





if (print == 1){png(file = "display/plots/plot10.png",res=reso-21)}
grid.newpage() 
pushViewport(viewport(layout.pos.col=2, layout.pos.row=2, clip="on"))
grid.draw(tableGrob(tableprint, gp=gpar(fontsize=12, lwd=.5)))
popViewport()
if (print == 1){dev.off()}

temp2 <- max(c(NY2m$momm,NY3m$momm,NY4m$momm,NY5m$momm,NY6m$momm,NY7m$momm,NY8m$momm,ML2m$momm,ML3m$momm,ML4m$momm,ML5m$momm,ML6m$momm,ML7m$momm,ML8m$momm,GL1m$momm,GL2m$momm,GL3m$momm,GL4m$momm,GL5m$momm,GL6m$momm,GL7m$momm,GL8m$momm))

if (print == 1){png(file = "display/plots/plot11.png",res=reso)}
par(mar=c(6, 5, 4, 2) + 0.1)
plot(NY2m$ki,NY2m$momm,type="l",col=1,xaxt="n",las=1,ylab="",xlab="",ylim = c(0,temp2),xlim=c(0,max(data$min)))
points(NY3m$ki,NY3m$momm,type="l",col=2,xaxt="n",las=1,ylab="",xlab="")
points(NY4m$ki,NY4m$momm,type="l",col=3,xaxt="n",las=1,ylab="",xlab="")
points(NY5m$ki,NY5m$momm,type="l",col=4,xaxt="n",las=1,ylab="",xlab="")
points(NY6m$ki,NY6m$momm,type="l",col=5,xaxt="n",las=1,ylab="",xlab="")
points(NY7m$ki,NY7m$momm,type="l",col=6,xaxt="n",las=1,ylab="",xlab="")
points(NY8m$ki,NY8m$momm,type="l",col=7,xaxt="n",las=1,ylab="",xlab="")
points(ML2m$ki,ML2m$momm,type="l",lty=2,col=1,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(ML3m$ki,ML3m$momm,type="l",lty=2,col=2,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(ML4m$ki,ML4m$momm,type="l",lty=2,col=3,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(ML5m$ki,ML5m$momm,type="l",lty=2,col=4,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(ML6m$ki,ML6m$momm,type="l",lty=2,col=5,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(ML7m$ki,ML7m$momm,type="l",lty=2,col=6,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(ML8m$ki,ML8m$momm,type="l",lty=2,col=7,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL1m$ki,GL1m$momm,type="l",lty=3,col=1,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL2m$ki,GL2m$momm,type="l",lty=4,col=1,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL3m$ki,GL3m$momm,type="l",lty=4,col=2,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL4m$ki,GL4m$momm,type="l",lty=4,col=3,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL5m$ki,GL5m$momm,type="l",lty=4,col=4,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL6m$ki,GL6m$momm,type="l",lty=4,col=5,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL7m$ki,GL7m$momm,type="l",lty=4,col=6,xaxt="n",las=1,ylab="",xlab="",lwd=2)
points(GL8m$ki,GL8m$momm,type="l",lty=4,col=7,xaxt="n",las=1,ylab="",xlab="",lwd=2)
abline(v=c(540,540+24*60),lty=1,col="black")
leg <- c("ML2","ML3","ML4","ML5","ML6","ML7","ML8","GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8","NY2","NY3","NY4","NY5","NY6","NY7","NY8")
legend("topleft", inset=.05,leg, horiz=FALSE,lty=c(rep(2,7),rep(3,7),rep(1,8)),col=c(1:7,1:7,1:8),ncol=3, cex=.8,lwd=2,bg="transparent")
#leg <- c()
#legend("bottomright", inset=.05,leg, horiz=FALSE,lty=c(rep(1,8)),col=c(1:8),ncol=3, cex=.75,bg="transparent")
if(max(data$yhour) < 24) {
  hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
  axis(1, at=seq(0,2880,60), labels=hours,las = 2)
} else if (max(data$yhour) > 24){
  hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
  axis(1, at=seq(0,2880,120), labels=hours,las = 2)
}
title(main="Hamringmomentum med daemping \n paa 2.34% for hele marathoncafeen",xlab="Tidspunkt",mgp=c(4,1,0))
title(ylab="Antal streger",mgp=c(3.5,1,0))
text(c(270,1440/2+540,2430),c(10,10,10), c("fredag","loerdag","soendag"))
if (print == 1){dev.off()}







x <- GL1m$ki
y <- GL1m$momm
id <- order(x)
gron <- data.frame(max(GL1m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
GL1troje <- data.frame("GL1",gron,prik)
names(GL1troje) <- c("Kokken","gron","prik")

x <- GL2m$ki
y <- GL2m$momm
id <- order(x)
gron <- data.frame(max(GL2m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
GL2troje <- data.frame("GL2",gron,prik)
names(GL2troje) <- c("Kokken","gron","prik")

x <- GL3m$ki
y <- GL3m$momm
id <- order(x)
gron <- data.frame(max(GL3m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
GL3troje <- data.frame("GL3",gron,prik)
names(GL3troje) <- c("Kokken","gron","prik")

x <- GL4m$ki
y <- GL4m$momm
id <- order(x)
gron <- data.frame(max(GL4m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
GL4troje <- data.frame("GL4",gron,prik)
names(GL4troje) <- c("Kokken","gron","prik")

x <- GL5m$ki
y <- GL5m$momm
id <- order(x)
gron <- data.frame(max(GL5m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
GL5troje <- data.frame("GL5",gron,prik)
names(GL5troje) <- c("Kokken","gron","prik")

x <- GL6m$ki
y <- GL6m$momm
id <- order(x)
gron <- data.frame(max(GL6m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
GL6troje <- data.frame("GL6",gron,prik)
names(GL6troje) <- c("Kokken","gron","prik")

x <- GL7m$ki
y <- GL7m$momm
id <- order(x)
gron <- data.frame(max(GL7m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
GL7troje <- data.frame("GL7",gron,prik)
names(GL7troje) <- c("Kokken","gron","prik")

x <- GL8m$ki
y <- GL8m$momm
id <- order(x)
gron <- data.frame(max(GL8m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
GL8troje <- data.frame("GL8",gron,prik)
names(GL8troje) <- c("Kokken","gron","prik")

x <- ML2m$ki
y <- ML2m$momm
id <- order(x)
gron <- data.frame(max(ML2m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
ML2troje <- data.frame("ML2",gron,prik)
names(ML2troje) <- c("Kokken","gron","prik")

x <- ML3m$ki
y <- ML3m$momm
id <- order(x)
gron <- data.frame(max(ML3m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
ML3troje <- data.frame("ML3",gron,prik)
names(ML3troje) <- c("Kokken","gron","prik")

x <- ML4m$ki
y <- ML4m$momm
id <- order(x)
gron <- data.frame(max(ML4m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
ML4troje <- data.frame("ML4",gron,prik)
names(ML4troje) <- c("Kokken","gron","prik")

x <- ML5m$ki
y <- ML5m$momm
id <- order(x)
gron <- data.frame(max(ML5m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
ML5troje <- data.frame("ML5",gron,prik)
names(ML5troje) <- c("Kokken","gron","prik")

x <- ML6m$ki
y <- ML6m$momm
id <- order(x)
gron <- data.frame(max(ML6m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
ML6troje <- data.frame("ML6",gron,prik)
names(ML6troje) <- c("Kokken","gron","prik")

x <- ML7m$ki
y <- ML7m$momm
id <- order(x)
gron <- data.frame(max(ML7m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
ML7troje <- data.frame("ML7",gron,prik)
names(ML7troje) <- c("Kokken","gron","prik")

x <- ML8m$ki
y <- ML8m$momm
id <- order(x)
gron <- data.frame(max(ML8m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
ML8troje <- data.frame("ML8",gron,prik)
names(ML8troje) <- c("Kokken","gron","prik")

x <- NY2m$ki
y <- NY2m$momm
id <- order(x)
gron <- data.frame(max(NY2m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
NY2troje <- data.frame("NY2",gron,prik)
names(NY2troje) <- c("Kokken","gron","prik")

x <- ML3m$ki
y <- ML3m$momm
id <- order(x)
gron <- data.frame(max(ML3m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
ML3troje <- data.frame("ML3",gron,prik)
names(ML3troje) <- c("Kokken","gron","prik")

x <- NY3m$ki
y <- NY3m$momm
id <- order(x)
gron <- data.frame(max(NY3m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
NY3troje <- data.frame("NY3",gron,prik)
names(NY3troje) <- c("Kokken","gron","prik")

x <- NY4m$ki
y <- NY4m$momm
id <- order(x)
gron <- data.frame(max(NY4m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
NY4troje <- data.frame("NY4",gron,prik)
names(NY4troje) <- c("Kokken","gron","prik")

x <- NY5m$ki
y <- NY5m$momm
id <- order(x)
gron <- data.frame(max(NY5m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
NY5troje <- data.frame("NY5",gron,prik)
names(NY5troje) <- c("Kokken","gron","prik")

x <- NY6m$ki
y <- NY6m$momm
id <- order(x)
gron <- data.frame(max(NY6m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
NY6troje <- data.frame("NY6",gron,prik)
names(NY6troje) <- c("Kokken","gron","prik")

x <- NY7m$ki
y <- NY7m$momm
id <- order(x)
gron <- data.frame(max(NY7m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
NY7troje <- data.frame("NY7",gron,prik)
names(NY7troje) <- c("Kokken","gron","prik")

x <- NY8m$ki
y <- NY8m$momm
id <- order(x)
gron <- data.frame(max(NY8m$momm))
prik <- sum(diff(x[id])*rollmean(y[id],2))
NY8troje <- data.frame("NY8",gron,prik)
names(NY8troje) <- c("Kokken","gron","prik")

troje <- rbind(GL1troje,GL2troje,GL3troje,GL4troje,GL5troje,GL6troje,GL7troje,GL8troje,ML2troje,ML3troje,ML4troje,ML5troje,ML6troje,ML7troje,ML8troje,NY2troje,NY3troje,NY4troje,NY5troje,NY6troje,NY7troje,NY8troje)

trojeprik <- troje[order(troje[,"prik"]),] # Sorting
trojegron <- troje[order(troje[,"gron"]),] # Sorting

if (print == 1){png(file = "display/plots/plot12.png",res=reso)}
barplot(troje$gron,xlab="*det stoerst opnaaede momentum",ylab="Maximalt opnaaet momentum",names.arg = troje[1:22,1],las=2,col=rainbow(23),main="Pointstillingen i konkurrencen \n om den groenne troeje*")
if (print == 1){dev.off()}

if (print == 1){png(file = "display/plots/plot13.png",res=reso)}
barplot(troje$gron,xlab="*stoerste areal under momentumkurven",ylab="Areal under momentum kurve",names.arg = troje[1:22,1],las=2,col=rainbow(23),main="Pointstillingen i konkurrencen \n om den prikkede troeje*")
if (print == 1){dev.off()}

a<-c("/?mode=jersey&yellow=",as.character(count[which(count$st == max(count$st)),1]))
b<-c("&green=",as.character(troje[which(troje$gron == max(troje$gron)),1]))
c<-c("&dotted=",as.character(troje[which(troje$prik == max(troje$prik)),1]))
a<-paste(a,collapse="");b<-paste(b,collapse="");c<-paste(c,collapse="")
JC<-paste(c(a,b),collapse="");JC<-paste(c(JC,c),collapse="")


getToHost("127.0.0.1",JC,"", port=8081)


