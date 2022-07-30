# --------------------------------------------------------------
# Create randon numbers for recruitment
# Space and spatiotemporal components

# For movement:
x = seq(from = xMin, to = xMax, by = gridSize)
y = seq(from = yMax, to = yMin, by = -gridSize)
basedat = expand.grid(x = x, y = y)
nGrids = length(x) # should be the same as length(y)

# Calculate initial conditions (warm-up period)
# StartVal = rep(0, nGrids^2)
# StartVal[130 * nGrids + 10] = 1000
# par = c(rho0 = 0.001, sigma2 = 0.0025^2, zeta = -log(0.99), rho1 = 0.02, gamma = 2, 
#          alpha = -pi/2.9, muX = 0.025, muY = -0.035, tau2 = 0.00001)
# spateSim = spate.sim(par = par, n = nGrids, T = 11, StartVal = StartVal, seed = 1)
# plot(spateSim, mfrow = c(3, 4), mar = c(2, 2, 2, 2), indScale = TRUE, 
#      cex.axis = 1.5, cex.main = 2)
# saveInitial = spateSim$xi[11,]
#save(saveInitial, file = 'saveInitial.RData')

# Load what was created in previous step
load('saveInitial.RData')
# Calculate final values for grids
StartVal = saveInitial
par = c(rho0 = 0.001, sigma2 = 0.002^2, zeta = -log(0.999), rho1 = 0.01, gamma = 2, 
        alpha = -pi/2.9, muX = 0.035, muY = -0.055, tau2 = 0.00001) # muY = -0.02
spateSim = spate.sim(par = par, n = nGrids, T = nT, StartVal = StartVal, seed = 1)


dir.create(path = 'RandomField_Recs')
# Simualte RF
model_O = RandomFields::RMmatern(nu = NuMat, var=SD_O^2, scale=SpatialScale) # spatial model 

saveOmega = matrix(NA, ncol = nAges, nrow = nrow(basedat))
# Simulate Omega for all ages:
for(i in seq_along(allAges)) {
	Omega = RandomFields::RFsimulate(model = model_O, x=as.matrix(basedat), grid = FALSE)
	saveOmega[,i] = Omega@data[,1]
}

saveOmega = as.data.frame(saveOmega)
colnames(saveOmega) = paste0('Age', allAges)
saveOmega$lon = basedat$x
saveOmega$lat = basedat$y

if(ix == 1){

	png('RandomField_Recs/RandomField_Rec_Omega.png', width = 180, height = 170, units = 'mm', res = 500)
	print(ggplot(data = saveOmega, aes(x = lon, y = lat)) + 
		geom_point(aes(color = Age0)) +
		scale_colour_gradient2() +
		theme_bw())
	dev.off()

}
