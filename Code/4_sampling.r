# -------------------------------------------------------------------------
# Begin sampling:

templateData = expand.grid(fish_lengths = allLens, gender2 = c(0,1),
                           precision = precisionScenarios$name,
                           n_sample = nFishSampled)

saveEstInformation = list()
saveIndTotal = list()
saveIndLen = list()
countList = 1
for(j in 1:nObservers) {

  vesselSel = sample(x = 1:nVessels, size = j)
  vesselData = allData[allData$vessel %in% vesselSel, ]
  idVesselData = vesselData %>% 
                    slice(rep(1:nrow(vesselData), vesselData$n))
  
  for(i in seq_along(nFishSampled)) {
    
    # sample fish:
    sampledData = idVesselData %>% 
                    dplyr::group_by(trip, set) %>%
                    dplyr::slice(sample(n(), min(nFishSampled[i], n()))) 
    
    id_len = sampledData$fish_lengths
    id_sex = sampledData$gender2
    
    for(k in 1:nrow(precisionScenarios)) {
    
      # sex precision:
      prec_sex = precisionScenarios$sex[k]
      idNew_sex = numeric(length(id_sex))
      for(r in seq_along(id_sex)){
        if(id_sex[r] == 0) {
          femPr = prec_sex
          malPr = 1 - prec_sex
        } else {
          femPr = 1 - prec_sex
          malPr = prec_sex
        }
        idNew_sex[r] = sample(x = c(0, 1), size = 1, prob = c(femPr, malPr))
      }
      
      # len precision:
      prec_len = precisionScenarios$len[k]
      PrecVector = seq(from = prec_len*-1, to = prec_len, by = 1)
      idNew_len = id_len + sample(x = PrecVector, size = length(id_len), replace = TRUE)
      if(max(idNew_len) > max(allLens)) idNew_len[which(idNew_len > max(allLens))] = max(allLens) 
      if(max(idNew_len) < min(allLens)) idNew_len[which(idNew_len < min(allLens))] = min(allLens) 
      
      tempData = data.frame(fish_lengths = idNew_len , gender2 = idNew_sex)
      tempData = tempData %>% count(fish_lengths, gender2)
      tempData$precision = precisionScenarios$name[k]
      tempData$n_sample = nFishSampled[i]
      tempData$vessel = j
      
      saveEstInformation[[countList]] = tempData
      countList = countList + 1
    } # precision loop
  
  } # number of fish sampled loop

  # Save data for all observers:
  estimatedData =	dplyr::bind_rows(saveEstInformation)
  
  estLenComp = estimatedData %>% 
    dplyr::group_by(gender2, fish_lengths, precision, n_sample) %>%
    dplyr::summarise(n = sum(n), .groups = 'drop') # here do loop number of observers
  estLenComp2 = dplyr::left_join(templateData, estLenComp)
  estLenComp2$n[is.na(estLenComp2$n)] = 0
  
  estLenComp3 = estLenComp2 %>% 
    dplyr::group_by(gender2, precision, n_sample) %>% 
    dplyr::mutate(comps = n/sum(n))
  estLenComp3$precision = factor(estLenComp3$precision, levels = precisionScenarios$name)
  
  errorData = dplyr::left_join(x = estLenComp3, y = trueLenComp2, by = c('gender2', 'fish_lengths'))
  errorData$error = ((errorData$comps.x - errorData$comps.y)^2)*1e+06
  
  errorData$bias = ifelse(test = errorData$comps.x == 0 & errorData$comps.y == 0, yes = NA, 
                          no = ((errorData$comps.x - errorData$comps.y)/(errorData$comps.y + 1e-09))*100)
  errorData$bias[is.nan(errorData$bias)] = NA
  
  errorData$nObservers = j
  errorData$iteration = ix
  saveIndLen[[j]] = errorData
  
  summaryErrorData = errorData %>%
    dplyr::group_by(precision, n_sample) %>%
    dplyr::summarise(error = median(error), bias = median(bias, na.rm = TRUE), .groups = 'drop')
  summaryErrorData$nObservers = j
  summaryErrorData$iteration = ix
  saveIndTotal[[j]] = summaryErrorData
  
  # if(ix == 1) {
  #   ggplot(data = estLenComp3, aes(x = fish_lengths, y = comps)) +
  #     geom_line(aes(color = factor(gender2))) +
  #     theme_bw() +
  #     scale_color_manual(values = c('red', 'blue')) +
  #     theme(legend.position = 'none') +
  #     xlab('Length (cm)') +
  #     ylab('Proportion') +
  #     facet_grid(factor(n_sample) ~ precision)
  #   ggsave(filename = file.path(folder_figures, paste0('estcomps_nObs_', j, '.png')), 
  #          width = 190, height = 230, units = 'mm', dpi = 500)
  #   
  #   ggplot(data = errorData, aes(x = fish_lengths, y = error)) +
  #     geom_line(aes(color = factor(gender2))) +
  #     theme_bw() +
  #     scale_color_manual(values = c('red', 'blue')) +
  #     theme(legend.position = 'none') +
  #     xlab('Length (cm)') +
  #     ylab('Error') +
  #     facet_grid(factor(n_sample) ~ precision)
  #   ggsave(filename = file.path(folder_figures, paste0('error_nObs_', j, '.png')), 
  #          width = 190, height = 230, units = 'mm', dpi = 500)
  #   
  #   ggplot(data = errorData, aes(x = fish_lengths, y = bias)) +
  #     geom_line(aes(color = factor(gender2))) +
  #     geom_hline(yintercept = 0, linetype = 'dashed', color = 'black') +
  #     theme_bw() +
  #     coord_cartesian(ylim = c(-100, 100)) +
  #     scale_color_manual(values = c('red', 'blue')) +
  #     theme(legend.position = 'none') +
  #     xlab('Length (cm)') +
  #     ylab('Bias (%)') +
  #     facet_grid(factor(n_sample) ~ precision)
  #   ggsave(filename = file.path(folder_figures, paste0('bias_nObs_', j, '.png')), 
  #          width = 190, height = 230, units = 'mm', dpi = 500)
  # }
  
   
} # number of observers loop
  

indTotData = dplyr::bind_rows(saveIndTotal)
indLenData = dplyr::bind_rows(saveIndLen)

write.csv(indTotData, file.path(folder_outputs, paste0('indTotData_', ix, '.csv')))
write.csv(indLenData, file.path(folder_outputs, paste0('indLenData_', ix, '.csv')))


# if(ix == 1) {
#   
#   ggplot(data = indTotData, aes(x=nObservers, y=error)) +
#     geom_line(aes(color=factor(n_sample))) +
#     geom_point(aes(color=factor(n_sample))) +
#     xlab('Number of observers') +
#     ylab('Error') +
#     theme_bw() +
#     theme(legend.position = c(0.8, 0.8),  legend.background = element_rect(fill = "transparent")) +
#     guides(color=guide_legend(title="Number fish sampled")) +
#     facet_wrap(. ~ precision)
#   ggsave(filename = file.path(folder_figures, 'error_total_nObs.png'), 
#          width = 190, height = 100, units = 'mm', dpi = 500)
#   
#   
#   ggplot(data = indTotData, aes(x=nObservers, y=bias)) +
#     geom_line(aes(color=factor(n_sample))) +
#     geom_point(aes(color=factor(n_sample))) +
#     xlab('Number of observers') +
#     ylab('Bias (%)') +
#     theme_bw() +
#     scale_x_continuous(breaks = 1:nObservers) +
#     theme(legend.position = c(0.85, 0.2), legend.background = element_rect(fill = "transparent")) +
#     guides(color=guide_legend(title="Number fish sampled")) +
#     facet_wrap(. ~ precision)
#   ggsave(filename = file.path(folder_figures, 'bias_total_nObs.png'), 
#          width = 190, height = 100, units = 'mm', dpi = 500)
# 
# }

