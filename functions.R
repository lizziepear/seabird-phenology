## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Source functions for Density maps shiny app
## Lizzie Pearmain & Ana Carneiro
## June 2019
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###############################################################################
######## SECTION 1 - mini phenology table #####################################
###############################################################################

#### 1.0 Helper function 1: convert laydate input to julian day ----
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

#### 1.0 Helper function 2: convert julian day to a pretty data display format ----
ddmm2Display <- function(julian){
  ## turn julian day into e.g. "Jan-01"

  date.full <- as.Date(julian, origin = "2000-12-31")
  
  date.new <- format(date.full, "%d-%b")
  
  date.new
}


#### 1.1 Main function: create summary phenology table ----
phenTable1 <- function(laydate_string, prelay_length_days, inc_length_days, brood_length_days, post_length_days){ ## TODO: add in , sp_type
  
  ## laydate is a character string of format "mm-dd" - convert to number
  laydate <- ddmm2Julian(laydate_string)
  
  ## sp_type is either "annual" or "biennial" and this affects the 'total non-breeding' row of the table
  
  output <- data.frame(matrix(ncol=3, nrow=6))
  colnames(output) <- c("stage", "start", "length")
  output[ ,1] <- c("pre-laying", "incubation", "brood-guard", "post-brood", "End breeding", "Non-breeding")
  output[1,2] <- ddmm2Display(laydate - prelay_length_days)
  output[1,3] <- prelay_length_days
  output[2,2] <- ddmm2Display(laydate)
  output[2,3] <- inc_length_days
  output[3,2] <- ddmm2Display(laydate + inc_length_days)
  output[3,3] <- brood_length_days
  output[4,2] <- ddmm2Display(laydate + inc_length_days + brood_length_days)
  output[4,3] <- post_length_days
  output[5,2] <- ddmm2Display(laydate + inc_length_days + brood_length_days + post_length_days)
  output[5,3] <- sum(prelay_length_days, inc_length_days, brood_length_days, post_length_days)
  output[6,2] <- ddmm2Display(laydate + inc_length_days + brood_length_days + post_length_days)
  output[6,3] <- 365 - sum(prelay_length_days, inc_length_days, brood_length_days, post_length_days)
  
  output$length <- as.integer(output$length)
  output
}

## test
table <- phenTable1("01-01", 0, 10, 20, 30)
table

###############################################################################
######## SECTION 2 - large phenology table ####################################
###############################################################################

#### 2.1 Helper function 1: calculate number of days in intersect of two intervals ----
intersectDays <- function(int1, int2){
  output <- as.numeric(as.period(intersect(int1, int2), "days"), "days")
  output
  ## TODO: if would rather have 0 than NAs, add a line to replace with 0 if output is NA
  ## TODO: edit to include the start day of the interval
}

#### 2.2 Helper function 2: split a given interval (breed stage) into number of days per month ----
## NB. does not count the first day of the interval
## => need to backdate the start of the input interval by 1
interval2Months <- function(input) {
  stopifnot(class(input) == "Interval") ## check the input is actually an interval!
  
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ints <- c(interval("2000-12-31", "2001-01-31"), interval("2001-01-31", "2001-02-28"),
            interval("2001-02-28", "2001-03-31"), interval("2001-03-31", "2001-04-30"),
            interval("2001-04-30", "2001-05-31"), interval("2001-05-31", "2001-06-30"),
            interval("2001-06-30", "2001-07-31"), interval("2001-07-31", "2001-08-31"),
            interval("2001-08-31", "2001-09-30"), interval("2001-09-30", "2001-10-31"),
            interval("2001-10-31", "2001-11-30"), interval("2001-11-30", "2001-12-31"))
  ovls <- vector(mode="numeric", length=12)
  
  ovls <- sapply(ints, intersectDays, int2=input)
  # reslist <- list(months, ints, ovls) ## outpu the entire list
  ovls ## output just the numeric vector of overlaps
}

# test <- interval("2001-01-01", "2001-01-31")
# interval2Months(test)

#### 2.3 Helper function 3: create raster equation from phenTableBeta ----
## input: prl, inc, brg, psb, nbr number of days in given month

createEquation <- function(vec){
  
  stopifnot(length(vec)==6)
  
  n_days <- vec[1]
  prl <- vec[2]
  inc <- vec[3]
  brg <- vec[4]
  psb <- vec[5]
  nbr <- vec[6]
  
  ## testing:
  # n_days <- 28
  # prl <- NA
  # inc <- 4
  # brg <- 5
  # psb <- 19
  # nbr <- NA
  
  if (prl > 0){
    str_prl <- paste0("(", prl, "/", n_days, " * pre-laying)")
  } else {str_prl <- NA}
  if (inc > 0){
    str_inc <- paste0("(", inc, "/", n_days, " * incubation)")
  } else {str_inc <- NA}
  if (brg > 0){
    str_brg <- paste0("(", brg, "/", n_days, " * brood_guard)")
  } else {str_brg <- NA}
  if (psb > 0){
    str_psb <- paste0("(", psb, "/", n_days, " * post_brood)")
  } else {str_psb <- NA}
  if ( nbr > 0){
    str_nbr <- paste0("(", nbr, "/", n_days, " * non-breeding)")
  } else {str_nbr <- NA}
  
  toPaste <- c(str_prl, str_inc, str_brg, str_psb, str_nbr)
  string <- paste(toPaste[!is.na(toPaste)], collapse=" + ")
  string
}

# vec <- c(28, 0, 4, 5, 19, 0)
# createEquation(n_days=28, prl=0, inc=4, brg=5, psb=19, nbr=0)
# test
# createEquation(vec)

#### 2.4 Main function: monthly phenolog for BETA (successful breeders) ----

## input: phenTable1
# table <- phenTable1("01-01", 0, 35, 5, 45)
# table

phenTableBeta <- function(table){
  
  ## Define the intervals for each breed stage based on phenTable1 ----
  prl_interval <- interval(as.Date(paste0(table[1,2], "-2001"), format="%d-%b-%Y")-1, as.Date(paste0(table[2,2], "-2001"), format="%d-%b-%Y")-1)
  inc_interval <- interval(as.Date(paste0(table[2,2], "-2001"), format="%d-%b-%Y")-1, as.Date(paste0(table[3,2], "-2001"), format="%d-%b-%Y")-1)
  brg_interval <- interval(as.Date(paste0(table[3,2], "-2001"), format="%d-%b-%Y")-1, as.Date(paste0(table[4,2], "-2001"), format="%d-%b-%Y")-1)
  psb_interval <- interval(as.Date(paste0(table[4,2], "-2001"), format="%d-%b-%Y")-1, as.Date(paste0(table[5,2], "-2001"), format="%d-%b-%Y")-1)
  ## TODO: add in nbr_interval
  
  ### TESTING THE HELPER FUNCTIONS:
  
  # prl_interval
  # intersectDays(prl_interval, interval("2001-01-01", "2001-01-31"))
  # 
  # inc_interval
  # # jan_interval <- interval("2001-01-01", "2001-01-31")
  # jan_interval <- interval("2000-12-31", "2001-01-31")
  # jan_interval
  # intersectDays(inc_interval, jan_interval)
  # 
  # inc_interval
  # feb_interval <- interval("2001-01-31", "2001-02-28")
  # feb_interval
  # intersectDays(inc_interval, feb_interval)
  # 
  # brg_interval
  # psb_interval
  
  ## define output data frame ----
  df <- data.frame(matrix(nrow=12, ncol=7))
  colnames(df) <- c("month", "n_days", "pre_laying", "incubation", "brood_guard","post_brood","non_breeding")
  df[ ,1] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  df[ ,2] <- as.integer(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  
  df$pre_laying <- as.integer(replace(interval2Months(prl_interval), is.na(interval2Months(prl_interval)), 0))
  df$incubation <- as.integer(replace(interval2Months(inc_interval), is.na(interval2Months(inc_interval)), 0))
  df$brood_guard <- as.integer(replace(interval2Months(brg_interval), is.na(interval2Months(brg_interval)), 0))
  df$post_brood <- as.integer(replace(interval2Months(psb_interval), is.na(interval2Months(psb_interval)), 0))
  
  df
  ## replace NA with 0
  df[is.na(df)] <- 0
  df
  
  ### Add in equation column for how to combine distributions per month ----
  all_list <- list(as.integer(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)), 
                   df$pre_laying, df$incubation,
                   df$brood_guard, df$post_brood, df$non_breeding)
  all_list
  length(all_list[[6]])
  all_list[1]
  
  eqs <- vector(mode="character", length=12)
  for (i in 1:12){
    # i <- 1
    args <- as.integer(paste(lapply(all_list, `[[`, i)))
    eqs[i] <- createEquation(args)
  }
  df$equation_to_combine_distributions <- eqs
  
  ## output result
  df
}

# table <- phenTable1("01-01", 0, 35, 5, 45)
# table
# phenTableBeta(table)

## TODO: potential problem: if inc starts in Dec and brood starts in Jan need to find a way to 
## make sure the date is read in as the FOLLOWING January.




