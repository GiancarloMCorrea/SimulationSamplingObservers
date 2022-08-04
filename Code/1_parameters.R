
# Population model

maxAge = 3
minAge = 0
maxLen = 170
minLen = 1
lenBin = 1
meanRec = 27e06 # initial recruitment in the entire stock area
sigmaR = 0.6 # for recruitment
femFrac = 0.66 # fraction of females in the population
A1_par = 0.5 # A1
Linf = c(128, 147) # Asymptotic length (females, males)
K_par = c(0.893, 0.893) # Growth rate (females, males)
M_par = c(1, 1) # Natural mortality (females, males)
L1_par = c(50, 50) # Mean length at age A1_par (females, males)
SD1 = c(7, 7) # Standard deviations of lengths at age 0 (females, males)
SD2 = c(15, 15) # Standard deviations of lengths at age plus group (females, males)
par_a = c(3.67e-05, 1.28e-05)
par_b = c(2.62, 2.89)

# Spatial model

gridSize = 0.05 # gridSize is a fraction of a grade (e.g. 0.05 = 0.05*1 grade = 0.05*60nm = 3 mn)
SpatialScale = 0.5 # st value = 0.5
SD_O = 0.5 # st value = 0.5
NuMat = 1 # st value = 1
xMin = -80
xMax = -75
yMin = -10
yMax = -5

# Fishery model

beginFish = 0.75 # initial dT from rec season (Jan 1st) to init of fishing (Oct 1st)
F_par = 0.8 # Apical F
areaSwept = 0.1 # per set, in km2 
nVessels = 100
timeUnits = 1 # in days
nTUnitsSeason = 180
nTimesTravelArea = 4 # number of time units to travel to the fishing area
mean_holdCapacity = 20
max_nTimesTrip = 25
radiusTrip = 20 # in nautical miles
beta1 = 55 # selectivity parameter 1
beta2 = 2 # selectivity parameter 2
sigmaM = 1 # for simulated catches. For lognormal catch

# Observer scenarios:

nObservers = 11 # number of vessels with cameras
precisionScenarios = data.frame(name = c('Low', 'Medium', 'High'),
						   		len = c(5, 3, 1),
						   		sex = c(0.8, 0.9, 1))
nFishSampled = c(250, 200, 150, 100, 50)


# Folder names:

folder_figures = 'my_figures'
folder_outputs = 'my_outputs'