rm(list = ls())

library(foreach)
library(doParallel)
library(doSNOW)

setwd('C:/Users/moroncog/Documents/GitHub/SimulationSamplingObservers/Code')
# Number of replicates
nSim = 10

cores = detectCores()
cl = makeCluster(cores[1] - 3)
registerDoSNOW(cl)

foreach(ix = 1:nSim) %dopar% {

	# Required libraries:
  require(dplyr)
  require(RandomFields)
  require(geosphere)
  require(tidyr)
  require(ggplot2)

	# parameters for the simulation:
	source('1_parameters.R', local = TRUE)

	# simulate Random Fields:
	source('2_spatial_variability.R', local = TRUE)

  # population and fishery model:
  source('3_population_fishery_model.R', local = TRUE)
  
	# sampling:
	source('4_sampling.R', local = TRUE) 

}

stopCluster(cl)