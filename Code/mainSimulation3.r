# Save strategy:
if(Strategy == 'Max') FacStr = MaxnSamCam
if(Strategy == 'Prop') FacStr = propSamCam*100 # in %

#L'min in SS equations:
Lminp = minLen - lenBin*0.5 # 0.5 because I am working with cm
bpar = (L1_par-Lminp)/A1_par
a_new = maxAge:(2*maxAge)

# Simulation --------------------------------------------------------------
  rRecTemp = rnorm(n = maxAge+1, mean = -(sigmaR^2)/2, sd = sigmaR)
  
	# Rec for this year (in numbers):
	R0year = meanRec*exp(rRecTemp)
	
	# Rec in density terms
	#R0inidengrid = R0year/StudyArea # Nfish/km2: mean density 
	R0inidengrid = R0year # Nfish: abundance 
	R0grid = R0inidengrid # add spatial and spatiotemporal structure
	
	# Create inital conditions:
	iniNs = numeric(maxAge + 1)
	iniLens = numeric(maxAge + 1)
	iniLensMal = numeric(maxAge + 1)
	for(i in seq_along(allAges)){
	  # Create inital densities for each age class
	  if(i == 1) { 
	    Z_par = M_par[i] 
  	  iniNs_tmp   = R0grid[i]*exp(-Z_par*(allAges+dT)) # take care: Z mortality
  	  iniNs[i] = iniNs_tmp[i]
	  } else {
	    Z_par = M_par[i] + (F_par*SelecFishAge)
	    iniNs_tmp   = R0grid[i]*exp(-Z_par*(allAges+0.25)) # previous fishing season
	    iniNs_tmp   = iniNs_tmp*exp(-M_par[i]*(dT-0.25)) # previous no-fishing season
	    iniNs[i] = iniNs_tmp[i]
	  }
  	# Create inital length for each age class (Females)
  	iniLens_tmp = ifelse((allAges+dT) <= A1_par, Lminp + (bpar*(allAges+dT)), 
  	                 Linf[i]+(L1_par-Linf[i])*exp(-(K_par[i])*((allAges+dT) - A1_par)))
  	iniLens_tmp[length(iniLens_tmp)] = sum(exp(-0.2*(a_new - maxAge))*(iniLens_tmp[length(iniLens_tmp)] + ((a_new - maxAge)/maxAge)*(Linf[i]-iniLens_tmp[length(iniLens_tmp)])))/sum(exp(-0.2*(a_new-maxAge)))
  	iniLens[i] = iniLens_tmp[i]

  	 # Create inital length for each age class (Males)
  	iniLens_tmpMal = ifelse((allAges+dT) <= A1_par, Lminp + (bpar*(allAges+dT)), 
  	                 LinfMal[i]+(L1_par-LinfMal[i])*exp(-(K_par[i])*((allAges+dT) - A1_par)))
  	iniLens_tmpMal[length(iniLens_tmpMal)] = sum(exp(-0.2*(a_new - maxAge))*(iniLens_tmpMal[length(iniLens_tmpMal)] + ((a_new - maxAge)/maxAge)*(LinfMal[i]-iniLens_tmpMal[length(iniLens_tmpMal)])))/sum(exp(-0.2*(a_new-maxAge)))
  	iniLensMal[i] = iniLens_tmpMal[i]
  	
	}
	
	# High-level loop: nCameras:
	save_map = NULL
	for(nC in seq_along(nObserversVector)){

		nCameras = nObserversVector[nC]

	# A partir de aqui a pasos de 25 dias:
	
	# Save results:
	save_df = NULL # save final data frame
	save_catch = NULL # save catch values
	save_sex = NULL # save sex prop
	save_MSE = NULL
	save_MRE = NULL
	tmp_spSampled = numeric(nSpAll)
	for(k in 1:nT){

		# prob positive sets
		setPosProb = rnorm(n = 1, mean = propPositiveCatch, sd = 0.03) # positive sets for mahimahi
		if(setPosProb > 1) setPosProb = 1
	
	  # Calculate movement matrix * spatial prob:
	  spatialProb = spateSim$xi[k,]/sum(spateSim$xi[k,])
	  spatialProb[spatialProb < th] = 0 # no negative probability
	  spatialProb[spatialProb >= th] = 1 # no negative probability
	  saveDispAdv = data.frame(lon = basedat$x, lat = rev(basedat$y),
	                           disp = spatialProb)
	  saveDispAdv2 = saveDispAdv[saveDispAdv$disp == 1, ]
	  posDataComb$probAge0 = posData$Omega0*saveDispAdv$disp
	  posDataComb$probAge0 = posDataComb$probAge0/sum(posDataComb$probAge0)
	  posDataComb$probAge1 = posData$Omega1*saveDispAdv$disp
	  posDataComb$probAge1 = posDataComb$probAge1/sum(posDataComb$probAge1)
	  posDataComb$probAge2 = posData$Omega2*saveDispAdv$disp
	  posDataComb$probAge2 = posDataComb$probAge2/sum(posDataComb$probAge2)
	  posDataComb$probAge3 = posData$Omega3*saveDispAdv$disp
	  posDataComb$probAge3 = posDataComb$probAge3/sum(posDataComb$probAge3)
	 
	  if(nC == 1){
	  	posDataComb$time = k
	  	save_map = rbind(save_map, posDataComb)
	  }

	  # Get sampled grid during time t
  	sampledStations = sample(x = 1:nrow(posDataComb), size = nCameras*nGridPerTrip, 
  	                  prob = posDataComb$probAge0, replace = FALSE)

		# For bycatch: sampled sets  	
  	sampledSets = lapply(X = splitSets, FUN = sample, size = length(sampledStations)) # grid sampled
  	sampledSets_t = sampledSets[[k]]
  	for(iSp in 1:nSpAll) tmp_spSampled[iSp] = ifelse(test = sum(nSpSet[[iSp]][sampledSets_t]) > 0, yes = 1, no = 0)
  	saveSpe[k, ] = tmp_spSampled # this is the species sampled

  	# Save state of the population at time t
  	nTmp = iniNs
  	lTmp = iniLens
  	
  	# First half: Natural mortality: time step
  	multT = (k*deltaT/365)*0.5 # at the middle of the time step
  	Z_par = M_par + (F_par*SelecFishAge)
  	nTmp2	= nTmp*exp(-multT*Z_par)
  	# Individual growth: time step t1*dT
  	lTmp2 = lTmp + (lTmp-Linf)*(exp(-multT*(K_par)) - 1)
  	lTmp2Mal = lTmp + (lTmp-LinfMal)*(exp(-multT*(K_par)) - 1)
  	
  	# SD len after first half: 
  	sdTmp2 = numeric(length(lTmp2))
  	sdTmp2Mal = numeric(length(lTmp2))
  	sdTmp2 = SD1 + ((lTmp2 - L1_par)/(Linf[1] - L1_par))*(SD2-SD1) # only first Linf
  	sdTmp2Mal = SD1 + ((lTmp2 - L1_par)/(LinfMal[1] - L1_par))*(SD2-SD1) # only first Linf
  	
  	# For females:
  	AgeLenMatrixProp = matrix(NA, ncol = length(allAges), nrow = length(allLens))
  	for(i in 1:nrow(AgeLenMatrixProp)){
  	  
  	  if(i == 1){
  	    
  	    #Lminp = minLen - lenBin*0.5 # 0.5 because I am working with 1 cm bin
  	    Fac1 = (Lminp - lTmp2)/sdTmp2
  	    AgeLenMatrixProp[i, ] = pnorm(Fac1)
  	    
  	  }
  	  if(i == length(allLens)){
  	    
  	    Lmaxp = maxLen - lenBin*0.5
  	    Fac1 = (Lmaxp - lTmp2)/sdTmp2
  	    AgeLenMatrixProp[i, ] = 1 - pnorm(Fac1)
  	    
  	  } else {
  	    
  	    Ll1p = allLens[i] + lenBin*0.5
  	    Llp = allLens[i] - lenBin*0.5
  	    Fac1 = (Ll1p - lTmp2)/sdTmp2
  	    Fac2 = (Llp - lTmp2)/sdTmp2
  	    AgeLenMatrixProp[i, ] = pnorm(Fac1) - pnorm(Fac2)
  	    
  	  }
  	  
  	}

  	AgeLenMatrixTotFem = sweep(AgeLenMatrixProp, MARGIN=2, nTmp2*femFrac, `*`)

  	# For males:
  	AgeLenMatrixPropMal = matrix(NA, ncol = length(allAges), nrow = length(allLens))
  	for(i in 1:nrow(AgeLenMatrixPropMal)){
  	  
  	  if(i == 1){
  	    
  	    #Lminp = minLen - lenBin*0.5 # 0.5 because I am working with 1 cm bin
  	    Fac1 = (Lminp - lTmp2)/sdTmp2Mal
  	    AgeLenMatrixPropMal[i, ] = pnorm(Fac1)
  	    
  	  }
  	  if(i == length(allLens)){
  	    
  	    Lmaxp = maxLen - lenBin*0.5
  	    Fac1 = (Lmaxp - lTmp2Mal)/sdTmp2Mal
  	    AgeLenMatrixPropMal[i, ] = 1 - pnorm(Fac1)
  	    
  	  } else {
  	    
  	    Ll1p = allLens[i] + lenBin*0.5
  	    Llp = allLens[i] - lenBin*0.5
  	    Fac1 = (Ll1p - lTmp2Mal)/sdTmp2Mal
  	    Fac2 = (Llp - lTmp2Mal)/sdTmp2Mal
  	    AgeLenMatrixPropMal[i, ] = pnorm(Fac1) - pnorm(Fac2)
  	    
  	  }
  	  
  	}

  	AgeLenMatrixTotMal = sweep(AgeLenMatrixPropMal, MARGIN=2, nTmp2*(1-femFrac), `*`)

  	AgeLenMatrixTot = AgeLenMatrixTotFem + AgeLenMatrixTotMal
  	LenMatrixTot = sweep(AgeLenMatrixTot, MARGIN=1, SelecFish, `*`)
  	fishTotPopStr = rowSums(LenMatrixTot)/sum(LenMatrixTot)

  	# Here vary nTemp2:
  	for(j in seq_along(sampledStations)){
  	  
  	  nTmp3 = nTmp2* c(posDataComb$probAge0[sampledStations[j]], 
          	           posDataComb$probAge1[sampledStations[j]], 
          	           posDataComb$probAge2[sampledStations[j]], 
          	           posDataComb$probAge3[sampledStations[j]])
  	  
  	  # For females:
  	  AgeLenMatrix = sweep(AgeLenMatrixProp, MARGIN=2, nTmp3*femFrac, `*`) 
   	  SelAbun = sweep(AgeLenMatrix, MARGIN=1, SelecFish, `*`)
  	  SelAbun2Fem = rowSums(SelAbun) # Keep as abundance for multinom. Change as density (/GridArea) for Thorson et al 2019. Same for males.

  	  # For males:
  	  AgeLenMatrix = sweep(AgeLenMatrixPropMal, MARGIN=2, nTmp3*(1-femFrac), `*`) 
   	  SelAbun = sweep(AgeLenMatrix, MARGIN=1, SelecFish, `*`)
  	  SelAbun2Mal = rowSums(SelAbun) # again as N/km2

  	  # Model binomial response (just once for males and females)
  	  if(nC & k & j == 1) { # For Enrique data
  	  	randomCatchBin = 1 
  	  } else {
  	  	randomCatchBin = rbinom(n = 1, size = 1, prob = setPosProb)
  	  }	
  	  # Model positive response (just once for males and females)
  	  randomCatchPos = rlnorm(n = 1, meanlog = log(meanCatchSet), sdlog = sigmaM)
  	  # Final Catch:
  	  randomCatch = ceiling(randomCatchPos) # the pos/neg of bin will be applied at the moment of saving data

		  # simulate sex of ind caught:
		  popSexVec = c(rep(x = 0, times = sum(SelAbun2Fem)), rep(x = 1, times = sum(SelAbun2Mal)))
		  catchSexVec = sample(x = popSexVec, size = randomCatch, replace = FALSE)

		  	  # Catch per grid as multinom: (females)
		  	  randomCatch = sum(catchSexVec == 0)
					nFishLenSampledFem = as.vector(rmultinom(n = 1, size = ceiling(randomCatch), prob = SelAbun2Fem/sum(SelAbun2Fem)))
		  	  catchStationFem = sum(nFishLenSampledFem)

		  	  # Catch per grid as Thorson et al 2019:
		  	  # pL = 1 - exp(-areaSwept*SelAbun2Fem)
		  	  # pLsim = structure(vapply(pL, rbinom, numeric(1), n = 1, size = 1), dim=dim(pL))
		  	  # rL = (areaSwept*SelAbun2Fem)/pL 
		  	  # findNAN = which(is.nan(rL)|is.infinite(rL))
		  	  # rL[findNAN] = 1 # to avoid warnings
		  	  # randomNumbers = structure(rnorm(length(rL), mean = 0, sd = sigmaM), dim=dim(rL))
		  	  # rLsim = structure(vapply(rL*exp(randomNumbers), rpois, numeric(1), n = 1), dim=dim(rL))
		  	  # nFishLenSampledFem = pLsim*rLsim # get round sampled matrix
		  	  # catchStationFem = sum(nFishLenSampledFem)

		  	  # Catch per grid as multinom: (males)
		  	  randomCatch = sum(catchSexVec == 1)
					nFishLenSampledMal = as.vector(rmultinom(n = 1, size = ceiling(randomCatch), prob = SelAbun2Mal/sum(SelAbun2Mal)))
		  	  catchStationMal = sum(nFishLenSampledMal)
		  	  
		  	  # Catch per grid as Thorson et al 2019:
		  	  # pL = 1 - exp(-areaSwept*SelAbun2Mal)
		  	  # pLsim = structure(vapply(pL, rbinom, numeric(1), n = 1, size = 1), dim=dim(pL))
		  	  # rL = (areaSwept*SelAbun2Mal)/pL 
		  	  # findNAN = which(is.nan(rL)|is.infinite(rL))
		  	  # rL[findNAN] = 1 # to avoid warnings
		  	  # randomNumbers = structure(rnorm(length(rL), mean = 0, sd = sigmaM), dim=dim(rL))
		  	  # rLsim = structure(vapply(rL*exp(randomNumbers), rpois, numeric(1), n = 1), dim=dim(rL))
		  	  # nFishLenSampledMal = pLsim*rLsim # get round sampled matrix
		  	  # catchStationMal = sum(nFishLenSampledMal)

		  	  # Summing males and females:
		  	  SelAbun2 = SelAbun2Fem + SelAbun2Mal
		  	  fishPopLenStructure = SelAbun2/sum(SelAbun2) # for population
		  	  nFishLenSampled = nFishLenSampledFem + nFishLenSampledMal # ind caught per len bin
		  	  catchStation = catchStationFem + catchStationMal # ind caught total

		  	  # Save here for enrique:
		  	  if(nC & k & j == 1) {
		  	  	save_catch_at_len_ind = NULL
		  	  	save_catch_sex_ind = NULL
		  	  }

		  	  for(nS in seq_along(PrecVec)){

		  	  	Prec = PrecVec[nS]

							# Precision:
							if(Prec == 'H') {
								  PrecFac = 0 # 0, 1, or 2
							  	PrecProb = 1
							}
							if(Prec == 'M') {
								  PrecFac = 1 # 0, 1, or 2
							  	PrecProb = c(1/3, 1/3, 1/3)
							}
							if(Prec == 'L') {
								  PrecFac = 2 # 0, 1, or 2
							  	PrecProb = c(1/5, 1/5, 1/5, 1/5, 1/5)
							}

			  	  # Sampling strategy
			  	  if(Strategy == 'Max'){
			  	  	nSamCam = MaxnSamCam
							if(catchStation < MaxnSamCam) nSamCam = catchStation
						}
						if(Strategy == 'Prop'){
							nSamCam = round(catchStation*propSamCam)
						}
			  	  # Here calculate precision:
			  	  new_nFishLenSampled = numeric(length(allLens))
			  	  # For females:
			  	  indLenFem = rep(x = allLens, times = nFishLenSampledFem)
			  	  sexVecFem = rep(x = 0, times = length(indLenFem))
			  	  # For males:
						indLenMal = rep(x = allLens, times = nFishLenSampledMal)
			  	  sexVecMal = rep(x = 1, times = length(indLenMal))
			  	  # Merge female and male vectors:
			  	  indLen = c(indLenFem, indLenMal)
			  	  sexVec = c(sexVecFem, sexVecMal)
			  	  # Get sampling:
			  	  sampInd = sort(sample(1:length(indLen), size = nSamCam, replace = FALSE))
			  	  indLen = indLen[sampInd] # Here the number of ind sampled by cameras
			  	  sexVec = sexVec[sampInd]

			  	  # Introduce sex error: (only for H size precision)
			  	  if(Prec == 'H') {

				  	  for(nSex in seq_along(sexPrecVec)) {
				  	  	sexPrec = sexPrecVec[nSex]
					  	  new_sexVec = numeric(length(sexVec))
					  	  for(r in seq_along(sexVec)){
					  	  	if(sexVec[r] == 0) {
					  	  		femPr = sexPrec
					  	  		malPr = 1 - sexPrec
					  	  	} else {
					  	  		femPr = 1 - sexPrec
					  	  		malPr = sexPrec
					  	  	}
					  	  	new_sexVec[r] = sample(x = c(0, 1), size = 1, prob = c(femPr, malPr))
					  		}
					  	  #Estimated prop sex:
					  	  estSexProp = 1 - mean(new_sexVec) # for females
					  	  if(randomCatchBin == 0) estSexProp = NA
					  	  # Save sex:
					      tmp_sex = data.frame(prop = estSexProp, PrecSex = sexPrec*100)
					      save_sex = rbind(save_sex, tmp_sex)

					      # For Enrique:
					  	  if(nC & k & j == 1) {
					  	  	tmp_catch_sex = data.frame(ID = 1:length(new_sexVec), sex = new_sexVec, PrecSex = sexPrec*100, Strategy = paste0(Strategy, FacStr), iter = ix)
					  	  	save_catch_sex_ind = rbind(save_catch_sex_ind, tmp_catch_sex)
					  		}

				    	} # Sex Prec

			    	}

			  	  # Continue as sex combined:
			  	  PrecVector = seq(from = PrecFac*-1, to = PrecFac, by = 1)
			  	  newIndLen = indLen + sample(x = PrecVector, size = length(indLen), replace = TRUE, prob = PrecProb)
			  	  if(max(newIndLen) > max(allLens)) newIndLen[which(newIndLen > max(allLens))] = max(allLens)
			  	  new_nFishLenSampled[match(as.numeric(names(table(newIndLen))), allLens)] = as.vector(table(newIndLen))

	  	  # Save results:
	  	  tmp_df = data.frame(len = allLens, 
	  	                      fishPopSt = fishTotPopStr, #fishPopLenStructure for population at grid and fishTotPopStr for total population
	  	                      fishFreq = new_nFishLenSampled*randomCatchBin, # HERE WE APPLY BIN RESPONSE
	  	                      set = j, 
	  	                      trip = k, iter = ix) # trip same as time step here
	  	  tmp_df$Prec = Prec
				tmp_df$NCam = nCameras
	  	  save_df = rbind(save_df, tmp_df)

	  	  # For Enrique:
	  	  if(nC & k & j == 1) {
	  	  	tmp_catch_len = data.frame(len = allLens, fishFreq = new_nFishLenSampled, Prec = Prec, Strategy = paste0(Strategy, FacStr), iter = ix)
	  	  	save_catch_at_len_ind = rbind(save_catch_at_len_ind, tmp_catch_len)
	  		}


    	} # Size precision

    	# For Enrique:
    	if(nC & k & j == 1) {
    		write.csv(save_catch_at_len_ind, paste0('catch_len_output/catch_len_Strat_', Strategy, FacStr, '_', ix, '.csv'), row.names = FALSE)
    		write.csv(save_catch_sex_ind, paste0('catch_sex_output/catch_sex_Strat_', Strategy, FacStr, '_', ix, '.csv'), row.names = FALSE)
    	}

    	tmp_catch = data.frame(catch = catchStation*randomCatchBin, set = j, 
	  	                      		trip = k, iter = ix)
	    save_catch = rbind(save_catch, tmp_catch)


  	} # Grid
  	
	
	} # Time

# Write final df catch:
if(ix == 1) write.csv(save_catch, file.path('catch_output', paste0('catchData_iter',  ix,'.csv')), row.names = FALSE)
# Save map data:
if(ix == 1) write.csv(save_map, file.path('RandomField_Recs', 'mapData_iter.csv'), row.names = FALSE)

# Save bycatch:
out_bycatch = data.frame(iter = ix, NCam = nCameras, NspeSeason = nSpTot, NspeSeasonSampled = sum(colSums(saveSpe) > 0))
write.csv(out_bycatch, file.path('bycatch', paste0('bycatch_iter',  ix, '_nCam', nCameras, '.csv')), row.names = FALSE)

# ------------------------------------------------------------------
# Order output files. MAKE THIS WHEN fishPopSt IS POPULATION AT GRID:
				# Save len structures:
				# save_df$lenProp = ave(x = save_df$fishFreq, list(save_df$set, save_df$trip, save_df$Prec), FUN=function(x) x/sum(x)) 
				
				# #Save MSE:
	   #    save_df$fishPopSt[which(save_df$fishPopSt < 1e-07)] = 0 # replace with zeros very small values
				# # Calculate MSE:
				# save_df$MSE = (save_df$fishPopSt - save_df$lenProp)^2
				# tmpMSE = aggregate(save_df$MSE, list(Prec = save_df$Prec, len = save_df$len), FUN = median) 
				# allMSE = data.frame(MSE = tmpMSE$x, Prec = tmpMSE$Prec, len = tmpMSE$len, iter = ix, NCam = nCameras, Strategy = Strategy)

				# # Save MRE:
				# # Calculate MRE:
				# save_df$MRE = (save_df$lenProp - save_df$fishPopSt)/save_df$fishPopSt
				# tmpMRE = aggregate(save_df$MRE, list(Prec = save_df$Prec, len = save_df$len), FUN = median, na.rm = TRUE) 
				# allMRE = data.frame(MRE = tmpMRE$x, Prec = tmpMRE$Prec, len = tmpMRE$len, iter = ix, NCam = nCameras, Strategy = Strategy)

				# alldata = save_df
				#alldata$Strategy = Strategy
				#alldata$FacStr = FacStr
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# Order output files. MAKE THIS WHEN fishPopSt IS TOTAL POPULATION:
				# Save len structures:
				catchLenData = aggregate(save_df$fishFreq, list(len = save_df$len, time = save_df$trip, ncam = save_df$NCam, prec = save_df$Prec),
				                        FUN = sum)
				catchLenData$lenProp = ave(x = catchLenData$x, list(catchLenData$time, catchLenData$prec), FUN=function(x) x/sum(x)) 
				catchLenData$type = 'catch'

				popLenData = aggregate(save_df$fishPopSt, list(len = save_df$len, time = save_df$trip, ncam = save_df$NCam, prec = save_df$Prec),
					                         FUN = mean)
				colnames(popLenData) = c('len', 'time', 'ncam', 'prec', 'lenProp')
				popLenData$type = 'population'
					# Order data frames:
				catchLenData = catchLenData[order(catchLenData$len, catchLenData$time, catchLenData$ncam, catchLenData$prec), ]
				popLenData = popLenData[order(popLenData$len, popLenData$time, popLenData$ncam, popLenData$prec), ]

				alldata = rbind(catchLenData[,c('len', 'time', 'lenProp', 'type', 'ncam', 'prec')],
				                popLenData[,c('len', 'time', 'lenProp', 'type', 'ncam', 'prec')])
				colnames(alldata) = c('len', 'time', 'lenProp', 'type', 'NCam', 'Prec')
				alldata$iter = ix
				alldata$Strategy = paste0(Strategy, FacStr)

				#Save MSE:
	      popLenData$lenProp[which(popLenData$lenProp < 1e-07)] = 0 # replace with zeros very small values
				# Calculate MSE:
				MSE_vec = (popLenData$lenProp - catchLenData$lenProp)^2
				tmpMSE = aggregate(MSE_vec, list(Prec = popLenData$prec, len = popLenData$len), FUN = median) 
				allMSE = data.frame(MSE = tmpMSE$x, Prec = tmpMSE$Prec, len = tmpMSE$len, iter = ix, NCam = nCameras, Strategy = paste0(Strategy, FacStr))

				# Save MRE:
				# Calculate MRE:
				MRE_vec = (catchLenData$lenProp - popLenData$lenProp)/popLenData$lenProp
				tmpMRE = aggregate(MRE_vec, list(Prec = popLenData$prec, len = popLenData$len), FUN = median, na.rm = TRUE) 
				allMRE = data.frame(MRE = tmpMRE$x, Prec = tmpMRE$Prec, len = tmpMRE$len, iter = ix, NCam = nCameras, Strategy = paste0(Strategy, FacStr))

# ------------------------------------------------------------------

write.csv(alldata, file.path('output', paste0('output_iter', ix, '_nCam', nCameras, '_Strat', Strategy, FacStr, '.csv')), row.names = FALSE)

write.csv(allMSE, file.path('MSE', paste0('MSE_iter', ix, '_nCam', nCameras, '_Strat', Strategy, FacStr, '.csv')), row.names = FALSE)

write.csv(allMRE, file.path('MRE', paste0('MRE_iter', ix, '_nCam', nCameras, '_Strat', Strategy, FacStr, '.csv')), row.names = FALSE)

# Calculate sex proportion:
save_sex$NCam = nCameras
save_sex$Strategy = paste0(Strategy, FacStr) 
save_sex$iter = ix
write.csv(save_sex, file.path('sexProp', paste0('sexProp_iter', ix, '_nCam', nCameras, '_Strat', Strategy, FacStr, '.csv')), 
					row.names = FALSE)

} # NCam
