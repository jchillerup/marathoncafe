
Beregn_Antal_Streger <- function(data){
 
  ki <- c("GL1","GL2","GL3","GL4","GL5","GL6","GL7","GL8","ML2","ML3","ML4","ML5","ML6","ML7","ML8","NY2","NY3","NY4","NY5","NY6","NY7","NY8")
  st <- rep(0,22)
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
  
#   if (print == 1){png(file = "display/plots/plot1.png",res=reso)}
#   barplot(count$st,xlab="",ylab="Antal streger",names.arg = count[1:22,1],las=2,col=rainbow(23),main="Antal streger fordelt paa koekkener", font.main = 1)
#   title(xlab=paste("Present plots were generated: ",Sys.time()))
#   if (print == 1){dev.off()}
  
  return(count)
}