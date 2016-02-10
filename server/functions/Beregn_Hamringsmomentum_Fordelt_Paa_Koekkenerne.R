Beregn_Hamringsmomentum_Fordelt_Paa_Koekkenerne <- function(datac,Antal_Streger_Paa_Hvert_Koekken,C,print,reso){


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


NY2m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(NY2m$mom[i]*1/(22*C))
  CC <- NY2m$mom[i]/C
  # CC <- datam$mom[i]/C
  NY2m$momm[i+1]<-(NY2m$mom[i]+NY2m$st[i])*(1-CC)
}  





NY3m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(NY3m$mom[i]*1/(22*C))
  CC <- NY3m$mom[i]/C
  NY3m$momm[i+1]<-(NY3m$mom[i]+NY3m$st[i])*(1-CC)
}  





NY4m <-data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(NY4m$mom[i]*1/(22*C))
  CC <- NY4m$mom[i]/C
  NY4m$momm[i+1]<-(NY4m$mom[i]+NY4m$st[i])*(1-CC)
}  





NY5m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(NY5m$mom[i]*1/(22*C))
  CC <- NY5m$mom[i]/C
  NY5m$momm[i+1]<-(NY5m$mom[i]+NY5m$st[i])*(1-CC)
}  






NY6m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(NY6m$mom[i]*1/(22*C))
  CC <- NY6m$mom[i]/C
  NY6m$momm[i+1]<-(NY6m$mom[i]+NY6m$st[i])*(1-CC)
}  






NY7m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(NY7m$mom[i]*1/(22*C))
  CC <- NY7m$mom[i]/C
  NY7m$momm[i+1]<-(NY7m$mom[i]+NY7m$st[i])*(1-CC)
}  






NY8m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(NY8m$mom[i]*1/(22*C))
  CC <- NY8m$mom[i]/C
  NY8m$momm[i+1]<-(NY8m$mom[i]+NY8m$st[i])*(1-CC)
}  






ML2m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(ML2m$mom[i]*1/(22*C))
  CC <- ML2m$mom[i]/C
  ML2m$momm[i+1]<-(ML2m$mom[i]+ML2m$st[i])*(1-CC)
}  







ML3m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
 #  CC <- tanh(ML3m$mom[i]*1/(22*C))
  CC <- ML3m$mom[i]/C
  ML3m$momm[i+1]<-(ML3m$mom[i]+ML3m$st[i])*(1-CC)
}  





ML4m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(ML4m$mom[i]*1/(22*C))
  CC <- ML4m$mom[i]/C
  ML4m$momm[i+1]<-(ML4m$mom[i]+ML4m$st[i])*(1-CC)
}  







ML5m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(ML5m$mom[i]*1/(22*C))
  CC <- ML5m$mom[i]/C
  ML5m$momm[i+1]<-(ML5m$mom[i]+ML5m$st[i])*(1-CC)
}  







ML6m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(ML6m$mom[i]*1/(22*C))
  CC <- ML6m$mom[i]/C
  ML6m$momm[i+1]<-(ML6m$mom[i]+ML6m$st[i])*(1-CC)
}  







ML7m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(ML7m$mom[i]*1/(22*C))
  CC <- ML7m$mom[i]/C
  ML7m$momm[i+1]<-(ML7m$mom[i]+ML7m$st[i])*(1-CC)
}  






ML8m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(ML8m$mom[i]*1/(22*C))
  CC <- ML8m$mom[i]/C
  ML8m$momm[i+1]<-(ML8m$mom[i]+ML8m$st[i])*(1-CC)
}  






GL1m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(GL1m$mom[i]*1/(22*C))
  CC <- GL1m$mom[i]/C
  GL1m$momm[i+1]<-(GL1m$mom[i]+GL1m$st[i])*(1-CC)
}  







GL2m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(GL2m$mom[i]*1/(22*C))
  CC <- GL2m$mom[i]/C
  GL2m$momm[i+1]<-(GL2m$mom[i]+GL2m$st[i])*(1-CC)
}  






GL3m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(GL3m$mom[i]*1/(22*C))
  CC <- GL3m$mom[i]/C
  
  GL3m$momm[i+1]<-(GL3m$mom[i]+GL3m$st[i])*(1-CC)
}  






GL4m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(GL4m$mom[i]*1/(22*C))
  CC <- GL4m$mom[i]/C
  GL4m$momm[i+1]<-(GL4m$mom[i]+GL4m$st[i])*(1-CC)
}  





GL5m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(GL5m$mom[i]*1/(22*C))
  CC <- GL5m$mom[i]/C
  GL5m$momm[i+1]<-(GL5m$mom[i]+GL5m$st[i])*(1-CC)
}  






GL6m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(GL6m$mom[i]*1/(22*C))
  CC <- GL6m$mom[i]/C
  GL6m$momm[i+1]<-(GL6m$mom[i]+GL6m$st[i])*(1-CC)
}  






GL7m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(GL7m$mom[i]*1/(22*C))
  CC <- GL7m$mom[i]/C
  GL7m$momm[i+1]<-(GL7m$mom[i]+GL7m$st[i])*(1-CC)
}  





GL8m <- data.frame(seq(0,max(data$min)),rep(0,max(data$min)+1))
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
  # CC <- tanh(GL8m$mom[i]*1/(22*C))
  CC <- GL8m$mom[i]/C
  GL8m$momm[i+1]<-(GL8m$mom[i]+GL8m$st[i])*(1-CC)
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

count <- count[order(count[,"momm"]),] # Sorting  



#### MOMENTUM KAN LET FINDES HER!!!!!!!!!!

t1 <- c("/?mode=momentum&GL1=",as.character(count[which(count$ki == "GL1"),2]))    
t2 <- c("&GL2=",as.character(count[which(count$ki == "GL2"),2]))    
t3 <- c("&GL3=",as.character(count[which(count$ki == "GL3"),2]))    
t4 <- c("&GL4=",as.character(count[which(count$ki == "GL4"),2]))    
t5 <- c("&GL5=",as.character(count[which(count$ki == "GL5"),2]))    
t6 <- c("&GL6=",as.character(count[which(count$ki == "GL6"),2]))    
t7 <- c("&GL7=",as.character(count[which(count$ki == "GL7"),2]))    
t8 <- c("&GL8=",as.character(count[which(count$ki == "GL8"),2]))    
t9 <- c("&ML2=",as.character(count[which(count$ki == "ML2"),2]))    
t10 <- c("&ML3=",as.character(count[which(count$ki == "ML3"),2]))    
t11 <- c("&ML4=",as.character(count[which(count$ki == "ML4"),2]))    
t12 <- c("&ML5=",as.character(count[which(count$ki == "ML5"),2]))    
t13 <- c("&ML6=",as.character(count[which(count$ki == "ML6"),2]))    
t14 <- c("&ML7=",as.character(count[which(count$ki == "ML7"),2]))    
t15 <- c("&ML8=",as.character(count[which(count$ki == "ML8"),2]))
t16 <- c("&NY2=",as.character(count[which(count$ki == "NY2"),2]))    
t17 <- c("&NY3=",as.character(count[which(count$ki == "NY3"),2]))    
t18 <- c("&NY4=",as.character(count[which(count$ki == "NY4"),2]))    
t19 <- c("&NY5=",as.character(count[which(count$ki == "NY5"),2]))    
t20 <- c("&NY6=",as.character(count[which(count$ki == "NY6"),2]))    
t21 <- c("&NY7=",as.character(count[which(count$ki == "NY7"),2]))    
t22 <- c("&NY8=",as.character(count[which(count$ki == "NY8"),2]))    


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

JC2 <- paste(c(tt1,tt2,tt3,tt4,tt5,tt6,tt7,tt8,tt9,tt10,tt11,tt12,tt13,tt14,tt15,tt16,tt17,tt18,tt19,tt20,tt21,tt22),collapse="");
getToHost("127.0.0.1",JC2,"", port=8081)





# 
# if (print == 1){png(file = "display/plots/plot10.png",res=reso-22)}
# grid.newpage() 
# pushViewport(viewport(layout.pos.col=2, layout.pos.row=2, clip="on"))
# grid.draw(tableGrob(tableprint, gp=gpar(fontsize=12, lwd=.5)))
# popViewport()
# if (print == 1){dev.off()}
# 
# temp2 <- max(c(NY2m$momm,NY3m$momm,NY4m$momm,NY5m$momm,NY6m$momm,NY7m$momm,NY8m$momm,ML2m$momm,ML3m$momm,ML4m$momm,ML5m$momm,ML6m$momm,ML7m$momm,ML8m$momm,GL1m$momm,GL2m$momm,GL3m$momm,GL4m$momm,GL5m$momm,GL6m$momm,GL7m$momm,GL8m$momm))
# 
# if (print == 1){png(file = "display/plots/plot11.png",res=reso)}
# par(mar=c(6, 5, 4, 2) + 0.1)
# plot(NY2m$ki,NY2m$momm,type="l",col=1,xaxt="n",las=1,ylab="",xlab="",xlim=c(0,max(data$min)),ylim = c(0,temp2)) # 
# points(NY3m$ki,NY3m$momm,type="l",col=2,xaxt="n",las=1,ylab="",xlab="")
# points(NY4m$ki,NY4m$momm,type="l",col=3,xaxt="n",las=1,ylab="",xlab="")
# points(NY5m$ki,NY5m$momm,type="l",col=4,xaxt="n",las=1,ylab="",xlab="")
# points(NY6m$ki,NY6m$momm,type="l",col=5,xaxt="n",las=1,ylab="",xlab="")
# points(NY7m$ki,NY7m$momm,type="l",col=6,xaxt="n",las=1,ylab="",xlab="")
# points(NY8m$ki,NY8m$momm,type="l",col=7,xaxt="n",las=1,ylab="",xlab="")
# points(ML2m$ki,ML2m$momm,type="l",lty=2,col=1,xaxt="n",las=1,ylab="",xlab="")
# points(ML3m$ki,ML3m$momm,type="l",lty=2,col=2,xaxt="n",las=1,ylab="",xlab="")
# points(ML4m$ki,ML4m$momm,type="l",lty=2,col=3,xaxt="n",las=1,ylab="",xlab="")
# points(ML5m$ki,ML5m$momm,type="l",lty=2,col=4,xaxt="n",las=1,ylab="",xlab="")
# points(ML6m$ki,ML6m$momm,type="l",lty=2,col=5,xaxt="n",las=1,ylab="",xlab="")
# points(ML7m$ki,ML7m$momm,type="l",lty=2,col=6,xaxt="n",las=1,ylab="",xlab="")
# points(ML8m$ki,ML8m$momm,type="l",lty=2,col=7,xaxt="n",las=1,ylab="",xlab="")
# points(GL1m$ki,GL1m$momm,type="l",lty=4,col=1,xaxt="n",las=1,ylab="",xlab="")
# points(GL2m$ki,GL2m$momm,type="l",lty=4,col=2,xaxt="n",las=1,ylab="",xlab="")
# points(GL3m$ki,GL3m$momm,type="l",lty=4,col=3,xaxt="n",las=1,ylab="",xlab="")
# points(GL4m$ki,GL4m$momm,type="l",lty=4,col=4,xaxt="n",las=1,ylab="",xlab="")
# points(GL5m$ki,GL5m$momm,type="l",lty=4,col=5,xaxt="n",las=1,ylab="",xlab="")
# points(GL6m$ki,GL6m$momm,type="l",lty=4,col=6,xaxt="n",las=1,ylab="",xlab="")
# points(GL7m$ki,GL7m$momm,type="l",lty=4,col=7,xaxt="n",las=1,ylab="",xlab="")
# points(GL8m$ki,GL8m$momm,type="l",lty=4,col=8,xaxt="n",las=1,ylab="",xlab="")
# abline(v=c(540,540+24*60),lty=1,col="black")
# leg <- c("NY2","NY3","NY4","NY5","NY6","NY7","NY8","ML2","ML3","ML4","ML5","ML6","ML7","ML8","GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8")
# legend("topleft", inset=.05,leg, horiz=FALSE,lty=c(rep(1,7),rep(2,7),rep(4,8)),col=c(1:7,1:7,1:8),ncol=3, cex=.8,lwd=1,bg="transparent")
# if(max(data$yhour) < 24) {
#   hours <- c("15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00" ,"16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00")
#   axis(1, at=seq(0,2880,60), labels=hours,las = 2)
# } else if (max(data$yhour) > 24){
#   hours <- c("15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00","17:00","19:00","21:00","23:00","01:00","03:00","05:00","07:00","09:00","11:00","13:00","15:00")
#   axis(1, at=seq(0,2880,120), labels=hours,las = 2)
# }
# title(main="Hamringmomentum med daemping \n paa 2.34% for hele marathoncafeen",xlab="Tidspunkt",mgp=c(4,1,0), font.main = 1)
# title(ylab="Momentum [steger/sekund]",mgp=c(3.5,1,0))
# text(c(270,1440/2+540,2430),c(10,10,10), c("fredag","loerdag","soendag"))
# if (print == 1){dev.off()}





GT <- data.frame(GL1m$momm,GL2m$momm,GL3m$momm,GL4m$momm,GL5m$momm,GL6m$momm,GL7m$momm,GL8m$momm,
                 ML2m$momm,ML3m$momm,ML4m$momm,ML5m$momm,ML6m$momm,ML7m$momm,ML8m$momm,
                 NY2m$momm,NY3m$momm,NY4m$momm,NY5m$momm,NY6m$momm,NY7m$momm,NY8m$momm
                )


momm_max = 1;
for (i in 1:dim(GT)[2]){
  momm_max[i] <- max(GT[,i])
}


ki <- c("GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8",
        "ML2","ML3","ML4","ML5","ML6","ML7","ML8",
        "NY2","NY3","NY4","NY5","NY6","NY7","NY8")

momm_max <- data.frame(ki,momm_max)
momm_max <- momm_max[order(momm_max[,"momm_max"]),]
momm_max <- data.frame(momm_max,1:22)


momm_now = 1;
momm_now_tmp <- GT[dim(GT)[1],]
for (i in 1:dim(GT)[2]){
  momm_now[i] = max(momm_now_tmp[i])
}

momm_now <- data.frame(ki,momm_now)
momm_now <- momm_now[order(momm_now[,"momm_now"]),]
momm_now <- data.frame(momm_now,1:22)






lw = 1
md = 1
up = 1
for (i in 1:dim(GT)[2]){
lw[i] <- max(GT[1:round(dim(GT)[1]/3),i])
md[i] <- max(GT[round(dim(GT)[1]/3):round(2*dim(GT)[1]/3),i])
up[i] <- max(GT[round(2*dim(GT)[1]/3):round(3*dim(GT)[1]/3),i])
}

momm_stg <- data.frame(ki,lw)
momm_stg <- momm_stg[order(momm_stg[,"lw"]),]
momm_stg_lw <- data.frame(momm_stg,1:22)

momm_stg <- data.frame(ki,md)
momm_stg <- momm_stg[order(momm_stg[,"md"]),]
momm_stg_md <- data.frame(momm_stg,1:22)

momm_stg <- data.frame(ki,up)
momm_stg <- momm_stg[order(momm_stg[,"up"]),]
momm_stg_up <- data.frame(momm_stg,1:22)

momm_stg_tmp <- merge(momm_stg_lw, momm_stg_md, all = TRUE,by = c('ki'))
momm_stg <- merge(momm_stg_tmp, momm_stg_up, all = TRUE,by = c('ki'))
momm_stg <- momm_stg[,c(1,3,5,7)]

momm_stg$point <- momm_stg[,2] + momm_stg[,3] + momm_stg[,4]
momm_stg <- momm_stg[,c(1,5)]


momm_stg <- momm_stg[order(momm_stg[,"point"]),] 
momm_stg <- data.frame(momm_stg,1:22)
momm_stg <- momm_stg[,c(1,3)]


groen <- merge(momm_max, momm_now, all = TRUE,by = c('ki'))
groen <- merge(groen, momm_stg, all = TRUE,by = c('ki'))
groen <- groen[,c(1,3,5,6)]
colnames(groen) <- c("ki","momm_max","momm_now","momm_stg")

groen$point <- 0.1*groen$momm_max + 0.4*groen$momm_now + 0.5*groen$momm_stg 
groen <- groen[,c(1,5)]




################################################ 
#### BEREGNER HVEM DER HAR DE FORSKELLIGE TR??JER
################################################

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

# if (print == 1){png(file = "display/plots/plot12.png",res=reso)}
# barplot(troje$gron,xlab="*det stoerst opnaaede momentum",ylab="Maximalt opnaaet momentum",names.arg = troje[1:22,1],las=2,col=rainbow(23),main="Pointstillingen i konkurrencen \n om den groenne troeje*", font.main = 1)
# if (print == 1){dev.off()}
# 
# if (print == 1){png(file = "display/plots/plot13.png",res=reso)}
# barplot(troje$prik/1e4,xlab="*stoerste areal under momentumkurven",ylab="Areal under momentum kurve (*1e4)",names.arg = troje[1:22,1],las=2,col=rainbow(23),main="Pointstillingen i konkurrencen \n om den prikkede troeje*", font.main = 1)
# if (print == 1){dev.off()}



prik = trojeprik[,c(1,3)]
colnames(prik) <- c("ki","point")
groen
troejer <- merge(prik, groen, all = TRUE, by = c('ki'))
colnames(troejer) <- c("ki","prik","groen")



troejer$prik <- troejer$prik/1000


a<-c("/?mode=jersey&yellow=",as.character(count[which(Antal_Streger_Paa_Hvert_Koekken$st == max(Antal_Streger_Paa_Hvert_Koekken$st)),1]))
b<-c("&green=",as.character(troje[which(troejer$groen == max(troejer$groen)),1]))
c<-c("&dotted=",as.character(troje[which(troejer$prik == max(troejer$prik)),1]))
a<-paste(a,collapse="");b<-paste(b,collapse="");c<-paste(c,collapse="")
JC<-paste(c(a,b),collapse="");JC<-paste(c(JC,c),collapse="")
getToHost("127.0.0.1",JC,"", port=8081)


troejer <- merge(troejer, Antal_Streger_Paa_Hvert_Koekken, all = TRUE, by = c('ki'))
colnames(troejer) <- c("ki","prik","groen","gul")

return(troejer)
}

