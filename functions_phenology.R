## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Phenology functions for Shiny app to explore the methods in the density maps paper
## Copyright (C) 2019 Lizzie Pearmain & Ana Carneiro
## This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
## This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
## You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


###############################################################################
######## SECTION 1 - mini phenology table #####################################
###############################################################################


#### 1.1 Helper function 1: convert laydate input to julian day ----
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

#### 1.2 Helper function 2: convert julian day to a pretty data display format ----
ddmm2Display <- function(julian){
  ## turn julian day into e.g. "Jan-01"
  
  date.full <- as.Date(julian, origin = "2000-12-31")
  
  date.new <- format(date.full, "%d-%b")
  
  date.new
}


#### 1.3 Main function: create summary phenology table for SUCCESSFUL BREEDERS ----
phenTable1Succ <- function(laydate_string, prelay_length_days, inc_length_days, brood_length_days, post_length_days, species_type){
  
  ## TESTING WITH PARAMS
  # laydate_string <- "01-01" ## "29-07"
  # prelay_length_days <- 26 ## 10
  # inc_length_days <- 79 ## 70
  # brood_length_days <- 31 ## 60
  # post_length_days <- 240 ## 40
  # species_type <- "annual"
  
  ## laydate is a character string of format "mm-dd" - convert to number
  laydate <- ddmm2Julian(laydate_string)
  
  ## FIND START DATE OF WHOLE BREEDING CYCLE = pre-laying start date
  start_date_julian <- laydate - prelay_length_days
  
  if (species_type == "annual"){
    
    ## make full data frame for results
    df <- data.frame(matrix(ncol=10, nrow=5))
    colnames(df) <- c("stage", "length", "start_jul_1", "end_jul_1", "start_jul_real", "end_jul_real", "start_date", "end_date", "start_display", "end_display")
    df[ ,1] <- c("pre-laying", "incubation", "brood-guard", "post-brood", "non-breeding")
    df
    
    ## fill in the length values
    df[1,2] <- prelay_length_days
    df[2,2] <- inc_length_days
    df[3,2] <- brood_length_days
    if (sum(prelay_length_days, inc_length_days, brood_length_days, post_length_days) > 365){ ## CHOP POST-GUARD IF BREEDING CYCLE > 365 DAYS
      post_length_days <- post_length_days - (sum(prelay_length_days, inc_length_days, brood_length_days, post_length_days)-365)
    }
    df[4,2] <- post_length_days
    df[5,2] <- 365 - sum(prelay_length_days, inc_length_days, brood_length_days, post_length_days)
    df
    
    ## fill in the table of START julian values from 1
    df[1,3] <- 1
    for (k in 1:4){
      df[k+1,3] <- df[k,3] + df[k,2]
    }
    df
    
    ## fill in the table of END julian values
    df[1,4] <- df[1,2]
    for (m in 1:4){
      df[m+1,4] <- df[m,4] + df[m+1,2]
    }
    df
    
    ## fill in the table of real julian values - just add (start_date_julian -1) to each start_jul_1 value.
    df$start_jul_real <- df$start_jul_1 + start_date_julian - 1
    df
    df$end_jul_real <- df$end_jul_1 + start_date_julian - 1
    df
    
    #### Part 2 - convert Julian dates to real dates over 2001 to 2002 by adding julian number to "2000-12-31"
    df$start_date <- as.Date("2000-12-31", format="%Y-%m-%d") + days(df$start_jul_real)
    df$end_date <- as.Date("2000-12-31", format="%Y-%m-%d") + days(df$end_jul_real)
    
    #### Part 3 - make pretty versions for start and end date for displaying in the table
    df$start_display <- as.character(format(df$start_date, "%d-%b"))
    df$end_display <- as.character(format(df$end_date, "%d-%b"))
    
  } else if (species_type == "biennial"){
    
    ## make full data frame for results, with extra row for non-breeding 2
    df <- data.frame(matrix(ncol=10, nrow=6))
    colnames(df) <- c("stage", "length", "start_jul_1", "end_jul_1", "start_jul_real", "end_jul_real", "start_date", "end_date", "start_display", "end_display")
    df[ ,1] <- c("pre-laying", "incubation", "brood-guard", "post-brood", "non-breeding year 1", "non-breeding year 2")
    df
    
    ## fill in the length values
    df[1,2] <- prelay_length_days
    df[2,2] <- inc_length_days
    df[3,2] <- brood_length_days
    df[4,2] <- post_length_days
    if (sum(prelay_length_days, inc_length_days, brood_length_days, post_length_days) > 365){
      df[5,2] <- 0
      df[6,2] <- 365 - (sum(prelay_length_days, inc_length_days, brood_length_days, post_length_days)-365)
    } else if (sum(prelay_length_days, inc_length_days, brood_length_days, post_length_days) <= 365){
      df[5,2] <- 365 - sum(prelay_length_days, inc_length_days, brood_length_days, post_length_days)
      df[6,2] <- 365
    }
    df
    
    ## fill in the table of START julian values from 1
    df[1,3] <- 1
    for (k in 1:5){ ## changed from (k in 1:4)
      df[k+1,3] <- df[k,3] + df[k,2]
    }
    df
    
    ## fill in the table of END julian values
    df[1,4] <- df[1,2]
    for (m in 1:5){ ## changed from (m in 1:4)
      df[m+1,4] <- df[m,4] + df[m+1,2]
    }
    df
    
    ## fill in the table of real julian values - just add (start_date_julian -1) to each start_jul_1 value.
    df$start_jul_real <- df$start_jul_1 + start_date_julian - 1
    df
    df$end_jul_real <- df$end_jul_1 + start_date_julian - 1
    df
    
    #### Part 2 - convert Julian dates to real dates over 2001 to 2002 by adding julian number to "2000-12-31"
    df$start_date <- as.Date("2000-12-31", format="%Y-%m-%d") + days(df$start_jul_real)
    df$end_date <- as.Date("2000-12-31", format="%Y-%m-%d") + days(df$end_jul_real)
    
    #### Part 3 - make pretty versions for start and end date for displaying in the table
    df$start_display <- as.character(format(df$start_date, "%d-%b"))
    df$end_display <- as.character(format(df$end_date, "%d-%b"))
    
  }
  
  df
}

#### 1.4 Helper function 3: find the fail date based on table of SUCC breeders NB. MIGHT NOT NEED THIS ----
findFailDate <- function(table){
  breed_interval <- interval(table[2,7], table[4,8]) ## interval from start of INC to end of PSB
  fail_date <- breed_interval@start + as.duration(breed_interval)/2 + days(1) ## find failure date as mean of breed_interval: ex@start + as.duration(ex)/2
  fail_date <- as.character(format(fail_date, "%d-%b"))
  return(fail_date)
}

#### 1.4. Helper function 4: convert inc_length_days, brood_length_days and post_length_days for fail breeders ----
convertToFail <- function(vec, fail_day){
  stopifnot(length(vec)==3) ## must be vector of length 3
  xs <- cumsum(vec)
  x2 <- xs ## create copy to work on
  y <- fail_day
  
  if (y <= xs[1]){                                  ## fail during incubation
    x2[1] <- y # x2[1] <- y - 1                     ## end inc on the day before failing
    x2[2] <- 0                                      ## brg does not happen
    x2[3] <- 0                                      ## psb does not happen
  } else if (y > xs[1] & y <= xs[2]){               ## fail during brood-guard
    x2[2] <- y - x2[1] # x2[2] <- y - 1 - x2[1]     ## end brg on the day before failing
    x2[3] <- 0                                      ## psb does not happen
  } else if (y > xs[2] & y <= xs[3]){               ## fail during post-guard
    x2[3] <- y - x2[2] # x2[3] <- y - 1 - x2[2      ## end psb on the day before failing
    x2[2] <- x2[2] - x2[1]                          ## convert brg back to brg rather than cumsum(inc, brg)
  } else if (y > xs[3]){
    stop("Fail date is not between start of incubation and end of post-brood.")
  }
  return(x2)
}

#### 1.5 Main function create summary phenology table for FAIL BREEDERS ----
phenTable1Fail <- function(laydate_string, prelay_length_days, inc_length_days, brood_length_days, post_length_days){
  
  ## TESTING WITH PARAMS
  # laydate_string <- "27-10" #"29-07"
  # prelay_length_days <- 22 #10
  # inc_length_days <- 68 #70
  # brood_length_days <- 22 #60
  # post_length_days <- 94 #40
  
  ## laydate is a character string of format "mm-dd" - convert to number
  laydate <- ddmm2Julian(laydate_string)
  
  ## FIND START DATE OF WHOLE BREEDING CYCLE = pre-laying start date
  start_date_julian <- laydate - prelay_length_days
  
  #### EXTRA SECTION FOR FAILS: REPLACE inc, brood, post WITH NEW VALUES AFTER FAILING HALFWAY THROUGH.
  breed_length_days <- inc_length_days + brood_length_days + post_length_days
  fail_day <- ceiling(breed_length_days/2) ## round up for consistency, and START NON-BREED ON THIS DAY NOW.
  x <- c(inc_length_days, brood_length_days, post_length_days)
  xf <- convertToFail(x, fail_day)
  ## replace inc, brood and post with the new lengths
  inc_length_days <- xf[1]
  brood_length_days <- xf[2]
  post_length_days <- xf[3]
  rm(breed_length_days, fail_day, x, xf)
  #### END EXTRA SECTION
  
  ## Create output data frame
  df <- data.frame(matrix(ncol=10, nrow=5))
  colnames(df) <- c("stage", "length", "start_jul_1", "end_jul_1", "start_jul_real", "end_jul_real", "start_date", "end_date", "start_display", "end_display")
  df[ ,1] <- c("pre-laying", "incubation", "brood-guard", "post-brood", "non-breeding")
  df
  
  ## fill in the length values
  df[1,2] <- prelay_length_days
  df[2,2] <- inc_length_days
  df[3,2] <- brood_length_days
  df[4,2] <- post_length_days
  df[5,2] <- 365 - sum(prelay_length_days, inc_length_days, brood_length_days, post_length_days)
  df
  
  ## fill in the table of START julian values from 1
  df[1,3] <- 1
  for (k in 1:4){
    df[k+1,3] <- df[k,3] + df[k,2]
  }
  df
  
  ## fill in the table of END julian values
  df[1,4] <- df[1,2]
  for (m in 1:4){
    df[m+1,4] <- df[m,4] + df[m+1,2]
  }
  df
  
  ## fill in the table of real julian values - just add (start_date_julian -1) to each start_jul_1 value.
  df$start_jul_real <- df$start_jul_1 + start_date_julian - 1
  df
  df$end_jul_real <- df$end_jul_1 + start_date_julian - 1
  df
  
  #### Part 2 - convert Julian dates to real dates over 2001 to 2002 by adding julian number to "2000-12-31"
  df$start_date <- as.Date("2000-12-31", format="%Y-%m-%d") + df$start_jul_real
  df$end_date <- as.Date("2000-12-31", format="%Y-%m-%d") + df$end_jul_real
  
  #### Part 3 - make pretty versions for start and end date for displaying in the table
  df$start_display <- as.character(format(df$start_date, "%d-%b"))
  df$end_display <- as.character(format(df$end_date, "%d-%b"))
  
  df
}

#### 1.6 Helper function: prettify the phenTable1 output for display ----

prettyPhenTable1 <- function(table){
  output <- data.frame(matrix(ncol=4, nrow=nrow(table)))
  colnames(output) <- c("stage", "start", "length", "end")
  output$stage <- table$stage
  output$start <- table$start_display
  output$length <- as.integer(table$length)
  output$end <- table$end_display
  ## only display end if length > 0
  for (i in 1:nrow(output)) {
    if (output$length[i] == 0) {
      output$end[i] <- NA
    }
  }
  output
}

# ## test
# # table <- phenTable1Succ("01-12", 0, 70, 60, 40)
# table <- phenTable1Succ("27-10", 22, 68, 22, 94)
# table
# # tablef <- phenTable1Fail("01-12", 0, 70, 60, 40)
# tablef <- phenTable1Fail("27-10", 22, 68, 22, 94)
# tablef
# 
# prettyPhenTable1(table)
# prettyPhenTable1(tablef)
# findFailDate(table)

# table <- phenTable1Succ("01-01", 26, 79, 31, 240, "biennial")
# table
# prettyPhenTable1(table)

#### 1.7 Main funciton: plot the breeding cyle to show graphically ----
plotCycle <- function(table, species_type) {
  
  mycols <- c("#ffd900", "#ffa53f", "#ff727f", "#c772f1", "#0084ff")
  
  if(species_type == "annual"){
    m <- matrix(table$length)
    xmax <- 365
    
  } else if(species_type == "biennial"){
    lens <- table$length
    lens[5] <- lens[5]+lens[6]
    m <- matrix(lens[1:5])
    
    xmax <- 730
  }
  
  par(mar = c(5,2,1,2))
  barplot(m, horiz=T,
          xlim=c(0, xmax), ylim=c(0,2), width=0.5,
          xlab = paste0("days from ", table$start_display[1]), col = mycols)
  text(as.numeric(cumsum(m)-m/2), 0.6, 
       labels=as.character(m),
       pos = 3, xpd=TRUE)
  legend(x="topleft", fill=mycols, legend=c("pre-laying","incubation","brood-guard","post-guard","non-breeding"))
}


###############################################################################
######## SECTION 2 - large phenology tables ###################################
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
  
  ##testing
  # input <- interval("2000-12-16", "2001-01-20")
  # year(int_start(input))
  # int_end(input)
  # c(year(int_start(input)), year(int_end(input)))
  
  ## set up results:
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  ovls2000 <- vector(mode="numeric", length=12) ## these all initialise with zeros, this is v useful!
  ovls2001 <- vector(mode="numeric", length=12)
  ovls2002 <- vector(mode="numeric", length=12)
  ovls <- vector(mode="numeric", length=12)
  
  ## run overlap ONLY for years in the interval
  if (2000 %in% c(year(int_start(input)), year(int_end(input)))){
    ints2000 <- c(interval("1999-12-31", "2000-01-31"), interval("2000-01-31", "2000-02-28"),
                  interval("2000-02-28", "2000-03-31"), interval("2000-03-31", "2000-04-30"),
                  interval("2000-04-30", "2000-05-31"), interval("2000-05-31", "2000-06-30"),
                  interval("2000-06-30", "2000-07-31"), interval("2000-07-31", "2000-08-31"),
                  interval("2000-08-31", "2000-09-30"), interval("2000-09-30", "2000-10-31"),
                  interval("2000-10-31", "2000-11-30"), interval("2000-11-30", "2000-12-31"))
    ovls2000 <- replace(sapply(ints2000, intersectDays, int2=input), is.na(sapply(ints2000, intersectDays, int2=input)), 0)
  }
  if (2001 %in% c(year(int_start(input)), year(int_end(input)))){
    ints2001 <- c(interval("2000-12-31", "2001-01-31"), interval("2001-01-31", "2001-02-28"),
                  interval("2001-02-28", "2001-03-31"), interval("2001-03-31", "2001-04-30"),
                  interval("2001-04-30", "2001-05-31"), interval("2001-05-31", "2001-06-30"),
                  interval("2001-06-30", "2001-07-31"), interval("2001-07-31", "2001-08-31"),
                  interval("2001-08-31", "2001-09-30"), interval("2001-09-30", "2001-10-31"),
                  interval("2001-10-31", "2001-11-30"), interval("2001-11-30", "2001-12-31"))
    ovls2001 <- replace(sapply(ints2001, intersectDays, int2=input), is.na(sapply(ints2001, intersectDays, int2=input)), 0)
  }
  if (2002 %in% c(year(int_start(input)), year(int_end(input)))){
    ints2002 <- c(interval("2001-12-31", "2002-01-31"), interval("2002-01-31", "2002-02-28"),
                  interval("2002-02-28", "2002-03-31"), interval("2002-03-31", "2002-04-30"),
                  interval("2002-04-30", "2002-05-31"), interval("2002-05-31", "2002-06-30"),
                  interval("2002-06-30", "2002-07-31"), interval("2002-07-31", "2002-08-31"),
                  interval("2002-08-31", "2002-09-30"), interval("2002-09-30", "2002-10-31"),
                  interval("2002-10-31", "2002-11-30"), interval("2002-11-30", "2002-12-31"))
    ovls2002 <- replace(sapply(ints2002, intersectDays, int2=input), is.na(sapply(ints2002, intersectDays, int2=input)), 0)
  }
  
  ## sum the results
  ovls[1:12] <- ovls2000[1:12] + ovls2001[1:12] + ovls2002[1:12]
  
  # reslist <- list(months, ints, ovls) ## output the entire list
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



###############################################################################
#### 2.4 Main function: monthly phenology for BREEDERS (succ and fail) ---- ###
###############################################################################

## input: phenTable1
# table <- phenTable1("01-01", 0, 35, 5, 45)
# table

phenTableBreed <- function(table, updateProgress=NULL){
  
  ## Define the intervals for each breed stage based on phenTable1 ----
  prl_interval <- interval(table[1,7]-1, table[1,8])
  inc_interval <- interval(table[2,7]-1, table[2,8])
  brg_interval <- interval(table[3,7]-1, table[3,8])
  psb_interval <- interval(table[4,7]-1, table[4,8])
  nbr_interval <- interval(table[5,7]-1, table[5,8])

  
  if (is.function(updateProgress)) {
    text <- "part 1"
    updateProgress(detail = text)
  }
  
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
  
  if (is.function(updateProgress)) {
    text <- "pre-laying"
    updateProgress(detail = text)
  }
  df$pre_laying <- as.integer(replace(interval2Months(prl_interval), is.na(interval2Months(prl_interval)), 0)) ## replacing any NAs with 0s
  
  if (is.function(updateProgress)) {
    text <- "incubation"
    updateProgress(detail = text)
  }
  df$incubation <- as.integer(replace(interval2Months(inc_interval), is.na(interval2Months(inc_interval)), 0))
  
  if (is.function(updateProgress)) {
    text <- "brood-guard"
    updateProgress(detail = text)
  }
  df$brood_guard <- as.integer(replace(interval2Months(brg_interval), is.na(interval2Months(brg_interval)), 0))
  
  if (is.function(updateProgress)) {
    text <- "post-brood"
    updateProgress(detail = text)
  }
  df$post_brood <- as.integer(replace(interval2Months(psb_interval), is.na(interval2Months(psb_interval)), 0))
  
  if (is.function(updateProgress)) {
    text <- "non-breeding"
    updateProgress(detail = text)
  }
  df$non_breeding <- as.integer(replace(interval2Months(nbr_interval), is.na(interval2Months(nbr_interval)), 0))
  
  df
  ## replace NA with 0
  df[is.na(df)] <- 0
  df
  
  if (is.function(updateProgress)) {
    text <- "part 3"
    updateProgress(detail = text)
  }
  
  ### Add in equation column for how to combine distributions per month ----
  all_list <- list(as.integer(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)), 
                   df$pre_laying, df$incubation,
                   df$brood_guard, df$post_brood, df$non_breeding)
  all_list
  length(all_list[[6]])
  all_list[1]
  
  
  if (is.function(updateProgress)) {
    text <- "part 4"
    updateProgress(detail = text)
  }
  
  eqs <- vector(mode="character", length=12)
  for (i in 1:12){
    # i <- 1
    args <- as.integer(paste(lapply(all_list, `[[`, i)))
    eqs[i] <- createEquation(args)
  }
  df$equation_to_combine_distributions <- eqs
  
  #### REORDER TABLE to start with the month in which pre-laying starts ----
  
  if (is.function(updateProgress)) {
    text <- "Done!"
    updateProgress(detail = text)
  }
  
  ## output result
  df
}

# table <- phenTable1("01-01", 0, 35, 5, 45)
# table
# phenTableBeta(table)

# ### TESTING FUNCTIONS ----
# ## phenology table 1
# table <- phenTable1("29-07", 10, 70, 60, 40)
# table
# prettyPhenTable1(table)
# 
# # phenologytable beta
# phenTableBeta(table)

# interval2Months(prl_interval)
# input <- prl_interval

###############################################################################
#### 2.5-7 Main function: monthly phenology for NON- and PRE-BREEDERS ---- ####
###############################################################################

#### 2.5 Main function: Monthly phenology table for Adult non-breeders (Sabbaticals)
phenTableDelta <- function() { ## has no args
  ## Define output data frame
  df <- data.frame(matrix(nrow=12, ncol=7))
  colnames(df) <- c("month", "n_days", "pre_laying", "incubation", "brood_guard","post_brood","non_breeding")
  df[ ,1] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  df[ ,2] <- as.integer(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  df
  
  ## replace NA with 0
  df[is.na(df)] <- as.integer(0)
  # df
  
  ## fill in non-breeding column with the full number of days per month as they are always non-breeding ----
  df$non_breeding <- as.integer(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  # df
  
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
  
  ## output
  df
}

#### 2.6 Main function: Monthly phenology table for Immatures
phenTableTheta <- function() { ## has no args
  ## Define output data frame
  df <- data.frame(matrix(nrow=12, ncol=4))
  colnames(df) <- c("month", "n_days", "immature_distribution", "equation_to_combine_distributions")
  df[ ,1] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  df[ ,2] <- as.integer(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  df[ ,3] <- as.integer(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  df[1:3, 4] <- "immature_Q1"
  df[4:6, 4] <- "immature_Q2"
  df[7:9, 4] <- "immature_Q3"
  df[10:12, 4] <- "immature_Q4"
  df
}

#### 2.7 Main function: Monthly phenology table for Juveniles
phenTableZeta <- function() { ## has no args
  ## Define output data frame
  df <- data.frame(matrix(nrow=12, ncol=4))
  colnames(df) <- c("month", "n_days", "juvenile_distribution", "equation_to_combine_distributions")
  df[ ,1] <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  df[ ,2] <- as.integer(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  df[ ,3] <- as.integer(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  df[1:3, 4] <- "juvenile_Q1"
  df[4:6, 4] <- "juvenile_Q2"
  df[7:9, 4] <- "juvenile_Q3"
  df[10:12, 4] <- "juvenile_Q4"
  df
}





