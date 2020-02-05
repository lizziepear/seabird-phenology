## Seabird phenology

R Shiny app to create monthly phenology tables for each pool of individuals in a population, and create equations to combine distribution rasters for each breeding stage with the correct weightings to produce an average monthly distribution raster for each group. 

This is part of the framework developed by Ana Carneiro & Lizzie Pearmain as part of the methods paper Carneiro, A.P.B. *et al*. (2020) '**A framework for mapping the distribution of seabirds by integrating tracking, demography and phenology**', *Journal of Applied Ecology*. The paper can be found [here](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2664.13568) and R scripts for the rest of the framework are available at [Ana's GitHub page](https://github.com/anacarneiro/DensityMaps).

### Inputs:
- Average egg-laying date for the population;
- Average duration in days of the different stages of the breeding cycle: pre-laying, incubation, brood-guard, post-guard.

### Outputs:
* Summary phenology table showing dates and duration of each stage of the breeding cycle
* Monthly phenology table showing the number of days per month in each breeding stage for the different groups of birds in the population:
    * Adult breeders (adults attempting to breed in a given year)
        * Successful breeders (complete the breeding cycle through to chick fledging)
        * Fail breeders (fail partway through the breeding cycle)
    * Adult non-breeders (sabbatical adults, or adults deferring breeding for a given year)
    * Immatures (birds from second year at sea until the age of first breeding)
    * Juveniles (fledged chicks in their first year at sea)
* Downloadable csv file of phenology metadata for input into the rest of the R scripts for the analysis framework.

## How to run this app

### Option 1: run the online version
* Go to my [shinyapps.io](https://lizziepear.shinyapps.io/seabird-phenology/) page to run the online version of the app.
    * Please note: I only have 25 active hours per month for this app, so if it will not load it means the hours have run out. You will need to download the app and run it locally instead.

### Option 2: run locally
1. Download the source code from this repository;
1. Ensure you have an updated version of R and RStudio installed;
1. Open the file `app.R` in RStudio;
1. Ensure you have the required packages installed and loaded: `shiny`, `shinydashboard`, `lubridate`;
1. Change the working directory in line 15 to your working directory;
1. Click 'Run App' in the top right-hand corner of RStudio's code editor;
1. Click 'Open in browser' in the top menu of the app to open in the browser of your choice (or, use it in the RStudio browser).

If you have trouble loading the app, please use the [resources](https://shiny.rstudio.com/tutorial/) on the R Shiny website to troubleshoot.

## License information
This software is licensed under the GNU General Public License version 3 (see LICENSE.md).
Please note that this licence does not apply to any logos, emblems and trade marks on the app or to the app’s design elements or to any photography or imagery. Those specific items may not be re-used without express permission.
