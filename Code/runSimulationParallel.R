rm(list = ls())

library(foreach)
library(doParallel)
library(doSNOW)

setwd('C:/Users/moroncog/Documents/Cousteau_Group/CamerasWWFProject/Code')
# Number of replicates
nSim = 100

cores = detectCores()
cl = makeCluster(cores[1] - 3)
registerDoSNOW(cl)

foreach(ix = 1:nSim) %dopar% {

	# Required libraries:
	require(sp)
	library(gstat)
	require(BBmisc)
	require(ggplot2)
	#require(ALKr)
	require(reshape2)
	library(mgcv)
	library(mapdata)
	library(grid)
	library(RColorBrewer)
	require(geoR)
	require(RandomFields)
	require(reshape)
	require(reshape2)
	require(gridExtra)
	require(statmod)
	require(fields)
	require(plyr)
	require(dplyr)
  require(spate)
  library(mapdata)
  require(ggplot2)
  require(RandomFields)

	# call aux functions needed for the simulation:
	source('auxFunctionsSimulation.R', local = TRUE)

	# parameters for the simulation and estimation step:
	source('parametersSimulation.R', local = TRUE)

	# simulate Random Fields for recruitment
	source('simulateRandomFields.R', local = TRUE)

	# main code for simulation (population and sampling):
	source('mainSimulation3.R', local = TRUE) # simulation1 is length stratified. simulation2 is random sampling

}

stopCluster(cl)