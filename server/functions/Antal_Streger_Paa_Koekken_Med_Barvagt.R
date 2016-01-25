Antal_Streger_Paa_Koekken_Med_Barvagt <- function(data){

barvagt <- c("Personalet","NY5","ML7","Prof","NY5","GL2","GL5","NY7","GL1","ML3","ML6","Prof","ML8","xx","xx","Cafeen")
tidspunkt <- c(4*30,5*30,5*30,16*30,4*30,6*30,4*30,4*30,4*30,5*30,5*30,20*30,4*30,5*30,5*30,5*30)
tidspunkt <- cumsum(tidspunkt)
vagt <- data.frame(tidspunkt=tidspunkt,barvagt=barvagt)
data$vagt <- 0

if (length(which(data$ki == "NY5" & data$min >= 120 & data$min < 270))>0){data[which(data$ki == "NY5" & data$min >= 120 & data$min < 270),]$vagt = "NY5"}
if (length(which(data$ki == "ML7" & data$min >= 270 & data$min < 420))>0){data[which(data$ki == "ML7" & data$min >= 270 & data$min < 420),]$vagt = "ML7"}
if (length(which(data$ki == "NY5" & data$min >= 900 & data$min < 1020))>0){data[which(data$ki == "NY5" & data$min >= 900 & data$min < 1020),]$vagt = "NY5"} # Forhalsbar

if (length(which(data$ki == "GL2" & data$min >= 1020 & data$min < 1200))>0){data[which(data$ki == "GL2" & data$min >= 1020 & data$min < 1200),]$vagt = "GL2"} # Forhalsbar
if (length(which(data$ki == "GL5" & data$min >= 1200 & data$min < 1320))>0){data[which(data$ki == "GL5" & data$min >= 1200 & data$min < 1320),]$vagt = "GL5"}

if (length(which(data$ki == "NY7" & data$min >= 1320 & data$min < 1440))>0){data[which(data$ki == "NY7" & data$min >= 1320 & data$min < 1440),]$vagt = "NY7"} # Forhalsbar
if (length(which(data$ki == "GL1" & data$min >= 1440 & data$min < 1560) )>0){data[which(data$ki == "GL1" & data$min >= 1440 & data$min < 1560),]$vagt = "GL1"}
if (length(which(data$ki == "ML3" & data$min >= 1560 & data$min < 1710) )>0){data[which(data$ki == "ML3" & data$min >= 1560 & data$min < 1710),]$vagt = "ML3"}

if (length(which(data$ki == "ML6" & data$min >= 1710 & data$min < 1860) )>0){data[which(data$ki == "ML6" & data$min >= 1710 & data$min < 1860),]$vagt = "ML6"}
if (length(which(data$ki == "ML8" & data$min >= 2340 & data$min < 2460) )>0){data[which(data$ki == "ML8" & data$min >= 2340 & data$min < 2460),]$vagt = "ML8"}

# if (length(which(data$ki == "NY7" & data$min >= 1380 & data$min < 1500) )>0){data[which(data$ki == "NY7" & data$min >= 1380 & data$min < 1500),]$vagt = "NY7"}
# if (length(which(data$ki == "GL6" & data$min >= 1500 & data$min < 1620) )>0){data[which(data$ki == "GL6" & data$min >= 1500 & data$min < 1620),]$vagt = "GL6"}
# if (length(which(data$ki == "GL5" & data$min >= 1740 & data$min < 1920) )>0){data[which(data$ki == "GL5" & data$min >= 1740 & data$min < 1920),]$vagt = "GL5"}
# if (length(which(data$ki == "ML6" & data$min >= 1920 & data$min < 2070) )>0){data[which(data$ki == "ML6" & data$min >= 1920 & data$min < 2070),]$vagt = "ML6"}
# if (length(which(data$ki == "ML5" & data$min >= 2010 & data$min < 2160) )>0){data[which(data$ki == "ML5" & data$min >= 2010 & data$min < 2160),]$vagt = "ML5"} # Forhalsbar
# if (length(which(data$ki == "NY6" & data$min >= 2070 & data$min < 2220) )>0){data[which(data$ki == "NY6" & data$min >= 2070 & data$min < 2220),]$vagt = "NY6"}
# if (length(which(data$ki == "GL4" & data$min >= 2220 & data$min < 2370) )>0){data[which(data$ki == "GL4" & data$min >= 2220 & data$min < 2370),]$vagt = "GL4"}
# if (length(which(data$ki == "GL1" & data$min >= 2370 & data$min < 2520) )>0){data[which(data$ki == "GL1" & data$min >= 2370 & data$min < 2520),]$vagt = "GL1"}

vagt <- c("GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8","ML2","ML3","ML4","ML5","ML6","ML7","ML8","NY2","NY3","NY4","NY5","NY6","NY7","NY8")
st <- rep(0,22)
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


# 
# if (print == 1){png(file = "display/plots/plot5.png",res=reso)}       
# if (sum(cheats$vagt)==0){
#   plot(0,col="white",xlab="",ylab="",xaxt="n",yaxt="n")  
#   text(1,0,c("This plot will be generated when data is available \n Antal streger sat paa det koekken med barvagten"),cex=0.85)
#   text(1,-0.5,paste("Present plots were generated: ",Sys.time()),cex=0.85)
# } else {
#   barplot(cheats$vagt,ylab="Antal streger",names.arg = cheats[1:22,1],las=3,col=rainbow(23),main="Antal streger sat paa \n det koekken med barvagten", font.main = 1)
#   title(xlab=paste("Plots generated: ",Sys.time(),sep=""))
# }
# if (print == 1){dev.off()}
# return(cheats)
#


}




