
# Population model

maxAge = 3
minAge = 0
maxLen = 170
minLen = 1
lenBin = 1
meanRec = 24000e03 # initial recruitment in the entire stock area
sigmaR = 0.6 # for recruitment
femFrac = 0.66 # fraction of females in the population
A1_par = 0.5 # A1
Linf = c(147, 128) # Aymptotic length (females, males)
K_par = c(0.893, 0.893) # Growth rate (females, males)
M_par = c(1, 1) # Natural mortality (females, males)
L1_par = c(50, 50) # Mean length at age A1_par (females, males)
SD1 = c(5, 5) # Standard deviations of lengths at age 1 (females, males)
SD2 = c(15, 15) # Standard deviations of lengths at age plus group (females, males)

# Spatial model

gridSize = 0.1 # gridSize is a fraction of a grade (e.g. 0.25 = 0.25*1 = 0.25*60nm = 15 mn)
SpatialScale = 0.5 # st value = 0.5
SD_O = 0.5 # st value = 0.5
NuMat = 1 # st value = 1
xMin = -80
xMax = -75
yMin = -10
yMax = -5

# Fishery model

dT = 0.75 # initial dT from rec season (Jan 1st) to init of fishing (Oct 1st)
F_par = 0.8 # Apical F
areaSwept = 2.5 # per set, in km2 
nVessels = 100
nSetsPerTrip = rpois(n = 1, lambda = 8)
nDaysPerTrip = rpois(n = 1, lambda = 15)
propPositiveCatch = 0.85 # probability positive catch
beta1 = 55 # selectivity parameter 1
beta2 = 2 # selectivity parameter 2
meanCatchSet = 160
sigmaM = 0.4 # for simulated catches. For lognormal catch
agesFullSelex = c(1,2,3)

# Observer scenarios

nObserversVector = 1:11 # number of vessels with cameras
precisionScenarios = data.frame(name = c('Low', 'Med', 'Hig'),
						   		len = c(5, 3, 1),
						   		sex = c(0.8, 0.9, 1))
samplingStrategies = data.frame(type = c('Max', 'Max', 'Max', 'Prop', 'Prop', 'Prop'),
								max = c(200, 125, 50, NA, NA, NA),
								prop = c(NA, NA, NA, 1, 0.75, 0.5))

# -----------------------------------------------------

# Grid size:

# Internal calculations:
gridSizeKm = gridSize * 111 # this is grid size in km
GridArea = gridSizeKm*gridSizeKm # in km2
if(nGridPerTrip == 0) nSetsPerTrip = 1


# Read stock grids
#predictGrid2 = read.csv('allGrids.csv') # this is the stock standard area
# Peru grids
#peruGrids = read.csv('peruGrids.csv') # this is the Peruvian area

# Rec density:
#StudyArea = nrow(peruGrids)*dsGrid*dsGrid*3.43 # in km2 
#StudyArea = 181046 # calculated based on assumed sphere 

# These parameters will be used later:






# Derived quantities:
allAges = seq(from = minAge, to = maxAge, by = 1)
nAges = length(allAges)
allLens = seq(from = minLen, to = maxLen, by = lenBin)
SelecFish = 1/(1 + exp(-log(19)*(allLens-beta1)/beta2)) 
SelecFishAge = c(1, 1, 1, 1) # This only applies to fishing season.
# Since ind age 0 are selected, selectivity = 1 for all ages

# Here simulate fishing trips:
#daysPerTrip = vector(mode = "list", length = nCameras) # here save days per trip
#minDay = 6 # min number of days per trip
#maxDay = 13 # max number of days per trip
#nDaysFSseason = ceiling(365/2) # number of days in fishing season 
nT = 8 # time steps in fishing season
deltaT = 22 # number of days in a time step
nGridPerTrip = rpois(n = 1, lambda = 8)
if(nGridPerTrip == 0) nGridPerTrip = 1
nTripSeason = rpois(n = 1, lambda = 6000) # n trips per season in total

#nGridsPerTrip = 3 # number of grids explored per trip
#gridsPerTrip = vector(mode = "list", length = nCameras) # here save days per trip
#names(gridsPerTrip) = paste0('C', 1:nCameras)

# Mean catch per grid: (only for rmultinorm)


allSets = 1:(nTripSeason*nGridPerTrip) # all sets 
splitSets = chunk(allSets, nT)

# Define square area:


# --------------------------------------------------------------
# Plot of random field spatial:

# if(ix == 1){

# ak = map_data('worldHires','USA:Alaska')
# ak = ak[ak$long < 0, ]

# bitmap(paste0('RandomField_K_', scenarioName, '.tiff'), height = 65, width = 130, units = 'mm', res = 900)
# print(map.heatmap(lat = yy2@coords[,2], lon = yy2@coords[,1], yy2@data,
#               color_low = "blue", color_high = "red", zeroiswhite = TRUE, xlim = c(-179,-158), ylim = c(54,62.5)) +
# 			  geom_polygon(data = ak, aes(long, lat, group = group), 
# 			  fill = 8, color="black") +
# 			  xlab('longitude') +
# 			  ylab('latitude') +
# 			  #xlim(-180,-156) +
# 			  theme(legend.position = c(0.15, 0.15), plot.margin = unit(c(0,0,0,0),"cm"), legend.key.width = unit(0.5, "cm")))
# dev.off()  

# }