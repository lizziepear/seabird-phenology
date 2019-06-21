## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Source functions for Density maps shiny app
## Part 2 - DEMOGRAPHY
## Lizzie Pearmain & Ana Carneiro
## June 2019
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###############################################################################
######## SECTION 1 ############################################################
###############################################################################


##### The following code is directly copied from script 01_demography.R - need to edit it
## function calcDem() which produces DEM_RESULTS
## Might need to edit the output further
## function plotDem() which takes the output of calcDem() and plots it.

#### Function 1 - run the demography model and output the results as proportion of the population

popStrat <- function(afb, Sa, Sj, BS, BF){
  
  ## args <- c(afb, Sa, Sj, BS, BF)

  
  
  
}


#####################################################
############# START LOOP OVER EACH SPECIES ##########
#####################################################

# DEM_RESULTS <- data.frame()
# 
# for (i in 1:nrow(ddem)){
#   
#   # i <- 1 ## testing
#   ddem_spp <- ddem[i,]
#   
#   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   #### a. DEFINE THE PARAMETERS FOR THIS SPECIES ----
#   pop.size <- as.numeric(as.character(ddem_spp$PopEstimate))              ### population size in breeding pairs
#   Sa <- as.numeric(as.character(ddem_spp$AnnualAdultSurvProb))            ### survival of adults
#   Sj <- as.numeric(as.character(ddem_spp$AnnualJuveSurvProb))             ### survival of juveniles (up to a.f.b)
#   BS <- as.numeric(as.character(ddem_spp$BreedingSuccess))                ### breeding success
#   BF <- as.numeric(as.character(ddem_spp$BreedingFrequency))              ### breeding frequency
#   afb <- round(as.numeric(as.character(ddem_spp$AgeFirstBreeding)), 0)    ### age at first breeding AS AN INTEGER (i.e. round to 0 decimal places)
#   
#   #### b. SET UP TABLE OF DEMOGRAPHY PARAMETERS ----
#   ### if testing multiple params, use this table to loop over
#   simul_in <- expand.grid(pop.size, Sa, Sj, BS, BF, afb)
#   dim(simul_in)
#   names(simul_in) <- c('pop.size','Sa','Sj','BS','BF','afb')
#   # simul_in ## view the table to check
#   SIM_OUT <- data.frame() ## create results data frame for simulating stable state for this species
#   
#   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   #### c. DEFINE THE MATRIX FOR THIS SPECIES ----
#   ## Model follows Abraham et al. 2016 - see diagram in manuscript.
#   ## Age classes are Adult, age 1, age 2, ..., age a.f.b - 1.
#   ## Modelling a population of females (= half of the total species population)
#   
#   dim1 <- afb      ## number of age classes = a.f.b.
#   dim2 <- dim1^2   ## dimensions of matrix are a.f.b. * a.f.b.
#   
#   species.matrix <- (rep.int(0, dim2))                        ## construct Leslie matrix, filled with 0s
#   species.matrix[0*dim1 + 1] <- expression(Sa)                ## row 1  , col 1    : adults in year t surviving to be adults in year t+1
#   species.matrix[1*dim1 + 1] <- expression(BF*BS*0.5)         ## row 2  , col 1    : production of first year juveniles in year t+1 from adults in year t
#   species.matrix[0*dim1 + dim1] <- expression(Sj)             ## row 1  , col dim1 (last column): immatures of age a.f.b.-1 in year t surviving to become adults in year t+1
#   
#   for (k in 2:(dim1-1)){
#     species.matrix[k*dim1 + k] <- expression(Sj)              ## row k+1, col k    : fill in the off-diagonal Sj terms - juveniles/immatures surviving from year t to t+1
#   }
#   
#   # species.matrix ## View the matrix to check (in the form of a 1-dimensional list for now)
#   
#   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   #### d. CALCULATING STABLE AGE DISTRIBUTION ----
#   
#   #### Create Leslie matrix using the species' vital rates
#   seabird.vr <- list(Sa=simul_in[1,2], Sj=simul_in[1,3],
#                      BS=simul_in[1,4], BF=simul_in[1,5],
#                      afb=simul_in[1,6]) ## extract parameters for this simulation from the table of input params
#   
#   A <- matrix(sapply(species.matrix, eval, seabird.vr, NULL), nrow=sqrt(length(species.matrix)), byrow=TRUE) ## create Leslie matrix
#   x <- stable.stage(A) ## find stable stage distribution
#   # A ## View matrix to check
#   # x ## View stable stage results
#   
#   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   #### e. SUM INTO THE DIFFERENT AGE CLASSES ----
#   out <- data.frame(matrix(ncol=9, nrow=1))               ## create data frame to output the age classes
#   colnames(out) <- c("TaxonOrder","CommonName","IslandGroup","pop.size","Sa","Sj","BS","BF","afb")
#   out[1, 1:3] <- c(ddem_spp$TaxonOrder[1],ddem_spp$CommonName[1],ddem_spp$IslandGroup[1])            ## Fill in info on species / island group
#   out[1, 4:9] <- c(as.numeric(simul_in[[1]]),as.numeric(simul_in[[2]]),as.numeric(simul_in[[3]]),
#                    as.numeric(simul_in[[4]]),as.numeric(simul_in[[5]]),as.numeric(simul_in[[6]]))    ## Fill in in demography params used
#   
#   out
#   str(out)
#   
#   ### e.1: keep as proportions:
#   out$Prop_AdBreed <- x[1]*BF                             ## Calculate number of adults breeders from number of adults * breeding frequency
#   out$Prop_AdNonbreed <- x[1]*(1-BF)                      ## Calculate number of adults breeders from number of adults * (1 - breeding frequency)
#   out$Prop_Juvenile <- x[2]                               ## Number of juveniles = number of birds aged 1 year
#   out$Prop_Immature <- sum(x[3:(afb)])                    ## Number of immatures = number of birds aged 2 to a.f.b.-1 inclusive (between juvenile and adult)
#   # out                                                     ## View to check
#   # sum(c(out$Prop_AdBreed, out$Prop_AdNonbreed,
#   #       out$Prop_Juvenile, out$Prop_Immature))            ## Check that these proportions sum to 1
#   
#   ### e.2: include number of birds:
#   ## NB. pop.size is in units of breeding pairs = number of breeding females = PropAdBreed
#   out$TotalPopNum <- out$pop.size/out$Prop_AdBreed*2      ## total population = Number of breeding females / PropAdBreed. Multiply by 2 to get number of both sexes.
#   out$AdBreed <- out$Prop_AdBreed*out$TotalPopNum         ## Calculate number of adult breeders from proportion of pop * total population number
#   out$AdNonbreed <- out$Prop_AdNonbreed*out$TotalPopNum   ## Calculate number of adult non-breeders
#   out$Juvenile <- out$Prop_Juvenile*out$TotalPopNum       ## Calculate number of juveniles
#   out$Immature <- out$Prop_Immature*out$TotalPopNum       ## Calculate number of immatures
#   # out                                                     ## View to check that out$AdBreed (number of adult breeders) = out$pop.size * 2
#   # sum(c(out$AdBreed, out$AdNonbreed,
#   #       out$Juvenile, out$Immature))                      ## Check that these values sum to out$TotalPopNum
#   
#   ### e.3. add into DEM_RESULTS data frame
#   SIM_OUT <- out
#   DEM_RESULTS <- rbind(DEM_RESULTS, as.data.frame(SIM_OUT))
#   
# }

