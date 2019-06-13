
### Source functions for Density maps shiny app

ddmm2Julian <- function(string){
  
  ## TODO: Add in a stopifnot()
  # string <- "01-02" ## TEST
  
  ## input is "mm-dd". Add year = 2001
  new.date <- as.Date(paste0(string, "-2001"), format="%d-%m-%Y")
  ref.date <- as.Date("2000-12-31", format("%Y-%m-%d"))
  
  ## new format "yyyy-mm-dd"
  new.date.julian <- as.double(new.date) - as.double(ref.date)
  
  ## output as index of days since Jan 1st
  new.date.julian
}

ddmm2Display <- function(julian){
  
  ## turn julian day into e.g. "Jan-01"

  date.full <- as.Date(julian, origin = "2000-12-31")
  
  date.new <- format(date.full, "%d-%b")
  
  date.new
}



phenTable1 <- function(laydate_string, inc_length_days, brood_length_days, post_length_days){
  
  ## laydate is a character string of format "mm-dd" - convert to number
  laydate <- ddmm2Julian(laydate_string)
  
  output <- data.frame(matrix(ncol=3, nrow=4))
  colnames(output) <- c("stage", "start", "length")
  output[ ,1] <- c("incubation", "brood-guard", "post-brood", "end post-brood")
  output[1,2] <- ddmm2Display(laydate)
  output[1,3] <- inc_length_days
  output[2,2] <- ddmm2Display(laydate + inc_length_days)
  output[2,3] <- brood_length_days
  output[3,2] <- ddmm2Display(laydate + inc_length_days + brood_length_days)
  output[3,3] <- post_length_days
  output[4,2] <- ddmm2Display(laydate + inc_length_days + brood_length_days + post_length_days)
  
  output
}

phenTable1("01-01", 10, 10, 10)













