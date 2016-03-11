pollutantmean <- function (directory, pollutant, id = 1:332) {
  setwd(directory)
  avg <- 0
  intermediate_count <- 0
  intermediate_sum <- 0
  for (i in id){
    # Get a proper File name with leading 0
    if (i <10){
      filename <- paste0("00",i,".csv",collapse="")
    } else if (i < 100) {
      filename <- paste0("0",i,".csv",collapse="")
    } else {
      filename <- paste0(i,".csv",collapse="")
    }
    df <- read.csv(filename)
    good <- complete.cases(df[[pollutant]])
    poll <- df[[pollutant]][good]
    intermediate_sum <- intermediate_sum + sum (poll)
    intermediate_count <- intermediate_count + length(poll)
    intermediate_avg <- intermediate_sum/intermediate_count
#    print (paste("Monitor Average",sum(poll)/length(poll)))
  }
  avg <- intermediate_sum/intermediate_count
  avg
}


