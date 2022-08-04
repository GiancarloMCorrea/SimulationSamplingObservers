# --------------------------------------------------------------
# Create randon numbers for recruitment
# Space and spatiotemporal components

# For movement:
x = seq(from = xMin, to = xMax, by = gridSize)
y = seq(from = yMax, to = yMin, by = -gridSize)
basedat = expand.grid(x = x, y = y)
nGrids = length(x) # should be the same as length(y)

# Simualte RF
model_O = RandomFields::RMmatern(nu = NuMat, var=SD_O^2, scale=SpatialScale) # spatial model 

# Simulate Omega for all ages:
Omega = RandomFields::RFsimulate(model = model_O, x=as.matrix(basedat), grid = FALSE)

saveOmega = as.data.frame(Omega@data)
colnames(saveOmega) = 'Devs'
saveOmega$lon = basedat$x
saveOmega$lat = basedat$y
saveOmega$prob = saveOmega$Devs + abs(min(saveOmega$Devs)) # equal or larger than 0
saveOmega$prob = saveOmega$prob/sum(saveOmega$prob) # to sum 1
