# Simulation framework: Collected data by onboard observers

This is the code used in Correa et al. (XXXX) to evaluate the impacts 
of onboard observers sampling strategies on collected data. This code 
can be applied to any stock and fishery worldwide. 

There are six main R files in this repository:

-   `runSimulationParallel.R`: run the simulation framework in parallel (Windows users)
-   `1_parameters.R`: contain the parameters of the simulation. This is the file that the user needs to modify to apply this code to other case studies. 
-   `2_spatial_variability.R`: simulates the Gaussian random field for fish abundance
-   `3_population_fishery_model.R`: simulates the dynamics of the population and fishery
-   `4_sampling.R`: evaluate different sampling strategies by onboard observers
-   `analyze_results.R`: analyze the simulated data and produces figures and tables

Feel free to modify any part of this repository to apply it to specific case studies. 

The most important file is `1_parameters.R`, which contains the parameters for this simulation framework. It is divided in four sections:

Simulation parameters
---------------------

### Population model

-   `maxAge`: age plus grop.
-   `minAge`: minimum age. Should always be 0.
-   `maxLen`: maximum length (cm)
-   `minLen`: minimum length (cm)
-   `lenBin`: length bin size (cm)
-   `meanRec`: mean recruitment (number of fish)
-   `sigmaR`: variability of recruitment
-   `femFrac`: fraction of females in the population
-   `A1_par`: reference age
-   `Linf`: asymptotic length (cm) (females,males)
-   `K_par`: growth rate (1/year) (females,males)
-   `M_par`: natural mortality (1/year) (females,males)
-   `L1_par`: mean length at age `A1_par` (females,males)
-   `SD1`: standard deviation of lengths at `minAge` (females,males)
-   `SD2`: standard deviation of lengths at `maxAge` (females,males)
-   `par_a`: parameter length-weight relationship (females,males)
-   `par_b`: parameter length-weight relationship (females,males)

### Spatial field

-   `gridSize`: fraction of a grade.
-   `SpatialScale`: spatial scale
-   `SD_O`: spatial variance
-   `NuMat`: nu parameter Matern function
-   `xMin`: minimum longitude (define the study area)
-   `xMax`: maximum longitude (define the study area)
-   `yMin`: minimum latitude (define the study area)
-   `yMax`: maximum latitude (define the study area)

### Fishery model

-   `beginFish`: fraction of year between spawning season and beginning of fishing season
-   `F_par`: Apical fishing mortality
-   `areaSwept`: area swept by a fishing set (in km2)
-   `nVessels`: number of vessels in the fishery
-   `timeUnits`: time units (day = 1)
-   `nTUnitsSeason`: number of time units during the fishing season
-   `full_H`: fraction of the hold capacity full in a fishing trip to return to port
-   `nTimesTravelArea`: number of time steps to travel from the port to fishing area
-   `mean_holdCapacity`: mean vessels hold capacity in the fishery
-   `max_nTimesTrip`: maximum number of time steps in a fishing trip
-   `radiusTrip`: maximum distance between fishing sets
-   `sigmaM`: variability in catches

Also, a vector of selectivity at length is required: `selexAtLength` (same length as the total number of length bins). Here, we use a logistic function to get this vector:

``` r
beta1 = 55 # selectivity parameter 1
beta2 = 2 # selectivity parameter 2
allLens = seq(from = minLen, to = maxLen, by = lenBin)
selexAtLength = 1/(1 + exp(-log(19)*(allLens-beta1)/beta2)) 
```

Any functional form can be used.

### Observer scenarios

-   `nObservers`: vector of the number of observers to be evaluated (one observer per vessel)
-   `precisionScenarios`: data.frame with the precision scenarios to be evaluated. Columns should be: `$name` (character), `$len` (numeric), `$sex` (numeric)
-   `nFishSampled`: vector of the number of fish sampled in a fishing set

Finally, folders where the simulated data and figures are saved need to be specified:

-   `folder_figures`: folder name to save figures
-   `folder_outputs`: folder name to save simulated data
