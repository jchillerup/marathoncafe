Beregn_Kumuleret_Antal_Streger_For_Foerende_5 <- function(data){


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
temporary2 <- c(NY2$min,NY3$min,NY4$min,NY5$min,NY6$min,NY7$min,NY8$min,ML2$min,ML3$min,ML4$min,ML5$min,ML6$min,ML7$min,ML8$min,GL1$min,GL2$min,GL3$min,GL4$min,GL5$min,GL6$min,GL7$min,GL8$min)
temporary2 <- max(temporary2)

# if (data$min[length(data$min)] > 30){
# if (print == 1){png(file = "display/plots/plot6.png",res=reso)}
# par(mar=c(6, 5, 4, 2) + 0.1)
# plot(NY2$min,NY2$cumst,type="l",col=1,xaxt="n",las=1,ylab="",xlab="",ylim = c(0,temporary),xlim=c(0,temporary2))
# points(NY3$min,NY3$cumst,type="l",col=2,xaxt="n",las=1,ylab="",xlab="")
# points(NY4$min,NY4$cumst,type="l",col=3,xaxt="n",las=1,ylab="",xlab="")
# points(NY5$min,NY5$cumst,type="l",col=4,xaxt="n",las=1,ylab="",xlab="")
# points(NY6$min,NY6$cumst,type="l",col=5,xaxt="n",las=1,ylab="",xlab="")
# points(NY7$min,NY7$cumst,type="l",col=6,xaxt="n",las=1,ylab="",xlab="")
# points(NY8$min,NY8$cumst,type="l",col=7,xaxt="n",las=1,ylab="",xlab="")
# points(ML2$min,ML2$cumst,type="l",lty=2,col=1,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(ML3$min,ML3$cumst,type="l",lty=2,col=2,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(ML4$min,ML4$cumst,type="l",lty=2,col=3,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(ML5$min,ML5$cumst,type="l",lty=2,col=4,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(ML6$min,ML6$cumst,type="l",lty=2,col=5,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(ML7$min,ML7$cumst,type="l",lty=2,col=6,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(ML8$min,ML8$cumst,type="l",lty=2,col=7,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(GL1$min,GL1$cumst,type="l",lty=4,col=1,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(GL2$min,GL2$cumst,type="l",lty=4,col=2,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(GL3$min,GL3$cumst,type="l",lty=4,col=3,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(GL4$min,GL4$cumst,type="l",lty=4,col=4,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(GL5$min,GL5$cumst,type="l",lty=4,col=5,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(GL6$min,GL6$cumst,type="l",lty=4,col=6,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(GL7$min,GL7$cumst,type="l",lty=4,col=7,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# points(GL8$min,GL8$cumst,type="l",lty=4,col=8,xaxt="n",las=1,ylab="",xlab="",lwd=2)
# abline(v=c(540,540+24*60),lty=1,col="black")
# leg <- c("NY2","NY3","NY4","NY5","NY6","NY7","NY8","ML2","ML3","ML4","ML5","ML6","ML7","ML8","GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8")
# legend("topleft", inset=.05,leg, horiz=FALSE,lty=c(rep(1,7),rep(2,7),rep(4,8)),col=c(1:7,1:7,1:8),ncol=3, cex=.75,lwd=2,bg="transparent")
# if(max(data$yhour) < 24) {
#   hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
#   axis(1, at=seq(0,2880,60), labels=hours,las = 2)
# } else if (max(data$yhour) > 24){
#   hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
#   axis(1, at=seq(0,2880,120), labels=hours,las = 2)
# }
# title(main="Kumulerede antal streger fordelt paa \n koekkener for hele marathoncafeen",mgp=c(4,1,0),cex=0.8,font.main=1)
# title(ylab="Antal streger")
# text(c(270,1440/2+540,2430),c(10,10,10), c("fredag","loerdag","soendag"))
# if (print == 1){dev.off()}
# } else if (data$min[length(data$min)] <= 30){
#   if (print == 1){png(file = "display/plots/plot6.png",res=reso)}
#   plot(0,col="white",xlab="",ylab="",xaxt="n",yaxt="n")  
#   text(1,0,c("This plot will be generated \n after 30 minutes"))
#   text(1,-0.5,paste("Present plots were generated: ",Sys.time()),cex=0.85)
#   if (print == 1){dev.off()}
# }

count <- data.table(datac[,1:2])
count <- count[ ,lapply(.SD, sum), by = ki]
count <- data.frame(count)
ki <- c("GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8","ML2","ML3","ML4","ML5","ML6","ML7","ML8","NY2","NY3","NY4","NY5","NY6","NY7","NY8")
st <- rep(0,22)
tabtemp <- data.frame(ki,st)
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

# if (dim(count)[1] >= 5){
#   if (print == 1){png(file = "display/plots/plot7.png",res=reso)}  
#   nr1 <- count[dim(count)[1],]
#   nr1 <- data.frame(lapply(nr1, as.character), stringsAsFactors=FALSE)[1]
#   nr1 <- as.character(nr1)
#   datanr1 <- datac[which(datac$ki == nr1,),]
#   datanr1$cumst <- cumsum(datanr1$st)
#   nr2 <- count[dim(count)[1]-1,]
#   nr2 <- data.frame(lapply(nr2, as.character), stringsAsFactors=FALSE)[1]
#   nr2 <- as.character(nr2)
#   datanr2 <- datac[which(datac$ki == nr2,),]
#   datanr2$cumst <- cumsum(datanr2$st)
#   nr3 <- count[dim(count)[1]-2,]  
#   nr3 <- data.frame(lapply(nr3, as.character), stringsAsFactors=FALSE)[1]
#   nr3 <- as.character(nr3)  
#   datanr3 <- datac[which(datac$ki == nr3,),]
#   datanr3$cumst <- cumsum(datanr3$st)
#   nr4 <- count[dim(count)[1]-3,]
#   nr4 <- data.frame(lapply(nr4, as.character), stringsAsFactors=FALSE)[1]
#   nr4 <- as.character(nr4)
#   datanr4 <- datac[which(datac$ki == nr4,),]
#   datanr4$cumst <- cumsum(datanr4$st)
#   nr5 <- count[dim(count)[1]-4,]
#   nr5 <- data.frame(lapply(nr5, as.character), stringsAsFactors=FALSE)[1]
#   nr5 <- as.character(nr5)
#   datanr5 <- datac[which(datac$ki == nr5,),]
#   datanr5$cumst <- cumsum(datanr5$st)
#   par(mar=c(5, 5, 4, 2) + 0.1)
#   plot(datanr1$min,datanr1$cumst,type="l",col=1,xaxt="n",las=1,ylab="",xlab="",ylim = c(0,temporary)) 
#   points(datanr2$min,datanr2$cumst,type="l",col=2,xaxt="n",las=1,ylab="",xlab="")
#   points(datanr3$min,datanr3$cumst,type="l",col=3,xaxt="n",las=1,ylab="",xlab="")  
#   points(datanr4$min,datanr4$cumst,type="l",col=4,xaxt="n",las=1,ylab="",xlab="")  
#   points(datanr5$min,datanr5$cumst,type="l",col=5,xaxt="n",las=1,ylab="",xlab="")  
#   leg <- c(nr1,nr2,nr3,nr4,nr5)
#   abline(v=c(540,540+24*60),lty=1,col="black")
#   legend("topleft", inset=.05,leg, horiz=FALSE,lty=1,col=c(1:5),ncol=2, cex=.90,lwd=1,bg="transparent")
#   if(max(data$yhour) < 24) {
#     hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
#     axis(1, at=seq(0,2880,60), labels=hours,las = 2)
#   } else if (max(data$yhour) > 24){
#     hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
#     axis(1, at=seq(0,2880,120), labels=hours,las = 2)
#   }
#   title(main="Kumulerede antal streger per minut for de tre \n foerende koekkener for hele marathoncafeen",mgp=c(3,1,0), font.main = 1)
#   title(ylab="Antal streger",mgp=c(3.1,1,0))
#   text(c(270,1440/2+540,2430),c(10,10,10), c("fredag","loerdag","soendag"))
#   if (print == 1){dev.off()}  
# } else if (dim(count)[1] < 3){
#   if (print == 1){png(file = "display/plots/plot7.png",res=reso)}  
#   plot(0,col="white",xlab="",ylab="",xaxt="n",yaxt="n")  
#   text(1,0,c("This plot will be generated when \n data is available"))
#   text(1,-0.5,paste("Present plots were generated: ",Sys.time()),cex=0.85)
#   if (print == 1){dev.off()}
# }

return(datac)


## Her skal man retunere noget med HTTL og noget andet.
}
