## Seabird phenology

R Shiny app to create monthly phenology tables for each pool of birds in a population, and create equations to combine distribution rasters for each breeding stage with the correct weightings to produce an average monthly distribution raster for each group.

Inputs:
- Average laying date
- Average length in days of the different stages of the breeding cycle: pre-laying, incubation, brood-guard, post-guard.

Outputs:
- Summary phenology table showing dates and duration of each stage of the breeding cycle
- Monthly phenology table showing the number of days per month in each breeding stage for the different groups of birds in the population:
  - Adult successful breeders
  - Adult fail breeders
  - Adult non-breeders (sabbaticals)
  - Immatures
  - Juveniles
- Downloadable csv file of phenology metadata for input into the R codes developed by Ana Carneiro & Lizzie Pearmain as part of the methods paper Carneiro *et al*. (in prep) *Mapping the global distribution of seabird populations: a framework for integrating tracking, demographic and phenological datasets*.
