Load_Data <- function(){
  data <- read.table("data.csv", sep=";", quote="\"")    
  names(data) <- c("ki","st","utime")
  starttime = data$utime[1]
  data$ti <- as.POSIXct(data[,3], origin="1970-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S")
  endtime = data$utime[nrow(data)]
  return(data)
}
