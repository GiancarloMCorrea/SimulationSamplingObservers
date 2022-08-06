# Internal calculations --------------------------------------------------------------

# General information:
allAges = seq(from = minAge, to = maxAge, by = 1)
nAges = length(allAges)
allLens = seq(from = minLen, to = maxLen, by = lenBin)
nLens = length(allLens)
propSex = c(femFrac, 1-femFrac)

# Growth model:
Lminp = minLen - lenBin*0.5 
bpar = (L1_par - Lminp)/A1_par
a_new = maxAge:(2*maxAge)

# Simulation --------------------------------------------------------------

# Population model ------------------------------------------------------

 # Initial recruitment:
rRecTemp = rnorm(n = maxAge+1, mean = -(sigmaR^2)/2, sd = sigmaR)
 R0year = meanRec*exp(rRecTemp)

# Create inital conditions:
iniNs = matrix(NA, ncol = maxAge + 1, nrow = 2)
iniLens = matrix(NA, ncol = maxAge + 1, nrow = 2)
sdLen = matrix(NA, ncol = maxAge + 1, nrow = 2)
for(i in 1:2) {
  	recSex = R0year * propSex[i] # recruitment by gender
  	iniNs[i,] = recSex*exp(-M_par[i]*(allAges+beginFish)) # numbers-at-age by the beginning of the fishing season

  	# Create inital length for all ages:
  	iniLens_tmp = ifelse((allAges+beginFish) <= A1_par, Lminp + (bpar*(allAges+beginFish)), 
  	                 		Linf[i]+(L1_par[i]-Linf[i])*exp(-(K_par[i])*((allAges+beginFish) - A1_par)))
  	iniLens[i,] = iniLens_tmp

  	sdLen[i,] = SD1[i] + ((iniLens[i,] - L1_par[i])/(Linf[i] - L1_par[i]))*(SD2[i]-SD1[i])
}

numbersGender = array(NA, dim = c(nTUnitsSeason, 2, nAges))
SelecFishAge = array(NA, dim = c(nTUnitsSeason, 2, nAges))
AgeLenMatrixProp = array(NA, dim = c(nTUnitsSeason, 2, nLens, nAges))
# Time loop:
for(t in 1:nTUnitsSeason) {
	
	# Time step:
	multT = (t - timeUnits*0.5)*(timeUnits/365) # at the middle of the time step

	# Calculate length at the middle of the time step:
	LensMid = iniLens + (iniLens-Linf)*(exp(-multT*(K_par)) - 1)

	# Calculate sd length:
	sdLen = SD1 + ((LensMid - L1_par)/(Linf - L1_par))*(SD2-SD1)
	
	# Calculate transition matrix:
  	for(s in 1:2) {
	  	for(i in 1:nLens){
	  	  
	  	  if(i == 1){
	  	    
	  	    #Lminp = minLen - lenBin*0.5 # 0.5 because I am working with 1 cm bin
	  	    Fac1 = (Lminp - LensMid[s,])/sdLen[s,]
	  	    AgeLenMatrixProp[t,s,i,] = pnorm(Fac1)
	  	    
	  	  }
	  	  if(i == length(allLens)){
	  	    
	  	    Lmaxp = maxLen - lenBin*0.5
	  	    Fac1 = (Lmaxp - LensMid[s,])/sdLen[s,]
	  	    AgeLenMatrixProp[t,s,i, ] = 1 - pnorm(Fac1)
	  	    
	  	  } else {
	  	    
	  	    Ll1p = allLens[i] + lenBin*0.5
	  	    Llp = allLens[i] - lenBin*0.5
	  	    Fac1 = (Ll1p - LensMid[s,])/sdLen[s,]
	  	    Fac2 = (Llp - LensMid[s,])/sdLen[s,]
	  	    AgeLenMatrixProp[t,s,i, ] = pnorm(Fac1) - pnorm(Fac2)
	  	    
	  	  }
	  	  
	  	}
	}

  SelecFishAge[t, 1,] = colSums(sweep(AgeLenMatrixProp[t,1,,], MARGIN=1, selexAtLength, `*`)) # females
  SelecFishAge[t, 2,] = colSums(sweep(AgeLenMatrixProp[t,2,,], MARGIN=1, selexAtLength, `*`)) # males

	# Numbers-at-age:
	Z_par = M_par + (F_par*SelecFishAge[t,,])
	numbersGender[t,,] = iniNs*exp(-multT*Z_par)

} # time loop

saveInformation = list()
countList = 1
# Begin fishery:
for(k in 1:nVessels) {

	holdCapacity = rpois(n = 1, lambda = mean_holdCapacity) # in metric tons
	countTripTimes = 1
	countTrip = 1
	totCatch = 0
	countSet = 1

	# Time loop:
	for(t in 1:nTUnitsSeason) {
		
		#print(countTripTimes)
	  if(countTripTimes == max_nTimesTrip) { 
	    countTripTimes = 1
	    countTrip = countTrip + 1
	    #print(totCatch)
	    totCatch = 0
	    countSet = 1
	    next
	  }
	  if(countTripTimes >= (max_nTimesTrip - nTimesTravelArea) & countTripTimes < max_nTimesTrip) { 
	    countTripTimes = countTripTimes + 1
	    next
	  }
	  
		if(countTripTimes <= nTimesTravelArea) {
		  countTripTimes = countTripTimes + 1
		  next
    }
		if(countTripTimes > nTimesTravelArea & countTripTimes < (max_nTimesTrip - nTimesTravelArea)) {

				if(totCatch < full_H*holdCapacity) {

					if(countSet == 1) {
						selGrid = sample(x = rownames(saveOmega), size = 1, prob = saveOmega$prob)
						this_row = which(as.numeric(rownames(saveOmega)) == selGrid)
						this_lon = saveOmega$lon[this_row]
						this_lat = saveOmega$lat[this_row]
						numbersGenderGrid = numbersGender[t,,]*saveOmega$prob[this_row]
					} else {
						distGrids = geosphere::distm(x = saveOmega[selGrid, c('lon', 'lat')], y = saveOmega[, c('lon', 'lat')])[1,]/1852 # in nautical miles
						newOmega = saveOmega[which(distGrids < radiusTrip), ] # for next set
						selGrid = sample(x = rownames(newOmega), size = 1, prob = newOmega$prob)
						this_row = which(as.numeric(rownames(newOmega)) == selGrid)
						this_lon = newOmega$lon[this_row]
						this_lat = newOmega$lat[this_row]
						numbersGenderGrid = numbersGender[t,,]*newOmega$prob[this_row]
					}
					
					numbersGenderLenGrid = matrix(NA, nrow = 2, ncol = nLens)
					numbersGenderLenGrid[1, ] = rowSums(sweep(AgeLenMatrixProp[t,1,,], MARGIN=2, numbersGenderGrid[1,], `*`)) # females
					numbersGenderLenGrid[2, ] = rowSums(sweep(AgeLenMatrixProp[t,2,,], MARGIN=2, numbersGenderGrid[2,], `*`)) # males
					selNumbersGenderLenGrid =	sweep(numbersGenderLenGrid, MARGIN=2, selexAtLength, `*`)
					
					pL = 1 - exp(-areaSwept*selNumbersGenderLenGrid)
					pLsim = structure(vapply(pL, rbinom, numeric(1), n = 1, size = 1), dim=dim(pL))
					rL = (areaSwept*selNumbersGenderLenGrid)/pL # as poisson delta link model. add over pL if it is necessary.
					findNAN = which(is.nan(rL)|is.infinite(rL))
					rL[findNAN] = 1 # to avoid warnings
					randomNumbers = structure(rnorm(nrow(rL)*ncol(rL), mean = 0, sd = sigmaM), dim=dim(rL))
					rLsim = structure(vapply(rL*exp(randomNumbers), rpois, numeric(1), n = 1), dim=dim(rL)) # poisson
					roundLenSampled = pLsim*rLsim # get round sampled matrix
					catchStation = sum(roundLenSampled)

					catchWgtSet = sum(roundLenSampled[1,]*par_a[1]*(allLens)^par_b[1] + roundLenSampled[2,]*par_a[2]*(allLens)^par_b[2])

					saveCatchSet = as.data.frame(t(roundLenSampled))
					colnames(saveCatchSet) = c('female', 'male')
					saveCatchSet$fish_lengths = allLens
					saveCatchSet$vessel = k
					saveCatchSet$set = countSet
					saveCatchSet$trip = countTrip
					saveCatchSet$lon = this_lon
					saveCatchSet$lat = this_lat
					saveCatchSet$iteration = ix
					tempData = tidyr::gather(saveCatchSet, 'gender', 'n', 1:2)
					tempData$gender2 = ifelse(test = tempData$gender == 'female', yes = 0, no = 1)
					saveInformation[[countList]] = tempData
					
					countSet = countSet + 1
					countList = countList + 1
					totCatch = totCatch + catchWgtSet*1e-03 # in mt
					countTripTimes = countTripTimes + 1
					next

				} else {
					countTripTimes = max_nTimesTrip - nTimesTravelArea # to terminate loop
					next
				} 
    } # end if set positive

	} # time loop

} # vessel loop


allData =	dplyr::bind_rows(saveInformation)
trueLenComp = allData %>% 
  dplyr::group_by(gender2, fish_lengths) %>%
  dplyr::summarise(n = sum(n), .groups = 'drop')

trueLenComp2 = trueLenComp %>% 
  dplyr::group_by(gender2) %>% 
  dplyr::mutate(comps = n/sum(n))

setLocations = allData %>%
            group_by(trip, set, vessel, iteration) %>%
            summarise(lon = unique(lon), lat = unique(lat), .groups = 'drop')

write.csv(setLocations, file.path(folder_outputs, paste0('setLocationsAll_', ix, '.csv')))


if(ix == 1){
  
  ggplot(trueLenComp2, aes(fish_lengths, comps)) +
    geom_line(aes(color = factor(gender2))) +
    xlab('Length (cm)') +
    ylab('Proportion') +
    theme_bw() +
    scale_color_manual(values = c('red', 'blue')) +
    theme(legend.position = 'none',  legend.background = element_rect(fill = "transparent")) 
  ggsave(filename = file.path(folder_figures, 'true_lencomp_season.png'), width = 90, 
         height = 90, units = 'mm', dpi = 500)
  
  pointsPlot = allData %>%
                filter(vessel == 1) %>%
                group_by(trip, set) %>%
                summarise(lon = unique(lon), lat = unique(lat), .groups = 'drop')

  ggplot(data = saveOmega, aes(x = lon, y = lat)) + 
          geom_point(aes(color = Devs), size = 0.7) +
          geom_point(data = pointsPlot, aes(x = lon, y = lat), size = 0.7) +
          scale_colour_gradient2() +
          theme_bw() +
          xlab('Longitude') +
          ylab('Latitude') +
          theme(legend.position = 'none')
  ggsave(filename = file.path(folder_figures, 'map_randomfield_sets.png'), width = 90, 
         height = 90, units = 'mm', dpi = 500)
  
}
