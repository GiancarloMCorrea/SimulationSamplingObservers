# Set wd:
setwd('C:/Users/moroncog/Documents/Cousteau_Group/CamerasWWFProject/Code')
require(dplyr)
require(ggplot2)
require(tidyr)
library(scales)
require(mapdata)
nCamerasVec = 1:11
StrategySel = 'Max900'
nStrategy = 6
FemFrac = 0.66 # true in the population

# ----------------------------------------------------------
# Compare len dist:
PrecSel = 'L'
for(i in seq_along(nCamerasVec)){

  base_scalar = read.csv(file.path('output', paste0('output_iter1_nCam', nCamerasVec[i], '_Strat', StrategySel, '.csv')))

  # Make plot MSE: (only precision size = H):
  base_plot = base_scalar[base_scalar$Prec == PrecSel & base_scalar$NCam == nCamerasVec[i], ]
  png(file.path('figures/compareLens', paste0('compareLen_Prec', PrecSel, '_nCam', nCamerasVec[i], '_Strat', StrategySel, '.png')), 
         width = 180, height = 120, units = 'mm', res = 500)

    print(ggplot(data = base_plot) +
      geom_line(aes(x = len, y = lenProp, color = type)) +
      theme_bw() +
      ylab('Proporcion') +
      xlab('Longitud (cm)') +
      scale_color_discrete(name = "Fuente: ", labels = c("Camara", "Poblacion")) +
      theme(legend.position = 'top', legend.background = element_rect(fill = "transparent")) +
      facet_wrap(. ~ time))

  dev.off()

}

# ----------------------------------------------------------
# Make MAP plot:
# Read map of Peru:
peruMap = map_data('worldHires', region = 'Peru')
mapData = read.csv('RandomField_Recs/mapData_iter.csv')
mapData = mapData[mapData$probAge0 > 0, ] # Clean zero values
colnames(mapData) = c('Longitud', 'Latitud', 'Edad0', 'Edad1', 'Edad2', 'Edad3', 'Time')
newmapData = mapData %>% gather(Age, Prob, Edad0:Edad3)

  png('RandomField_Recs/RandomField_Movement_Final.png', width = 160, height = 210, units = 'mm', res = 500)
    ggplot(data = newmapData) + 
      geom_point(aes(Longitud, Latitud, color = Prob), size = 0.1) +
      geom_polygon(data = peruMap, aes(long, lat, group = group), 
          fill = 8, color="black") +
      scale_colour_gradient2(low = muted("blue"), mid = "white", high = muted("red"), midpoint = 0.0015) +
      theme_bw() +
      theme(legend.position = "none") +
      facet_grid(factor(Time) ~ Age)
  dev.off()
  

# COMPARE SIZE PRECISION:
# ----------------------------------------------------------
# ----------------------------------------------------------
# ----------------------------------------------------------
# Read MSE files:
list_files = list.files(file.path('MSE'))
scalar_files = paste0('MSE', '/', list_files[grep(pattern = 'MSE', list_files)])
all_scalar_dfs = lapply(scalar_files, read.csv)
base_scalar = bind_rows(all_scalar_dfs)
base_scalar$MSE = base_scalar$MSE*1e06

# For next plots:
base_plot_1 = aggregate(base_scalar$MSE, list(Prec = base_scalar$Prec, Strategy = base_scalar$Strategy, 
                      iter = base_scalar$iter, NCam = base_scalar$NCam), FUN = median)
# Make plot MSE: (only precision size = H):
base_plot = base_plot_1[base_plot_1$Prec == 'H' & base_plot_1$Strategy == StrategySel, ]

png(file.path('figures', paste0('compareMSEPrecH_Strat', StrategySel,'.png')), 
     width = 90, height = 70, units = 'mm', res = 500)
ggplot(base_plot, aes(x=factor(NCam), y=x)) +
  geom_boxplot(fill='#F8766D', color='#F8766D', alpha = 0.2) +
  xlab('Numero de camaras') +
  ylab('MSE (e-06)') +
  theme_bw()
dev.off()

# ----------------------------------------------------------
# Make plot MSE: Including precision size levels:
base_plot = base_plot_1[base_plot_1$Strategy == StrategySel, ]
base_plot$Prec = factor(base_plot$Prec, levels = c('H', "M", "L"))
png(file.path('figures', paste0('compareMSE_Strat', StrategySel,'.png')), 
     width = 180, height = 90, units = 'mm', res = 500)
ggplot(base_plot, aes(x=factor(NCam), y=x, fill = factor(Prec), color = factor(Prec))) +
  geom_boxplot(alpha = 0.2) +
  xlab('Numero de camaras') +
  ylab('MSE (e-06)') +
  labs(col = "Precision", fill = "Precision") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8), legend.background = element_rect(fill = "transparent")) 
dev.off()

# ----------------------------------------------------------
# Make plot MSE by len and by precision level:
MSE50 = aggregate(base_scalar$MSE, list(len = base_scalar$len, Prec = base_scalar$Prec, 
                    NCam = base_scalar$NCam, Strategy = base_scalar$Strategy), FUN = median)
MSE025 = aggregate(base_scalar$MSE, list(len = base_scalar$len, Prec = base_scalar$Prec, 
                    NCam = base_scalar$NCam, Strategy = base_scalar$Strategy), FUN = quantile, probs = 0.025, na.rm = TRUE)
MSE975 = aggregate(base_scalar$MSE, list(len = base_scalar$len, Prec = base_scalar$Prec, 
                    NCam = base_scalar$NCam, Strategy = base_scalar$Strategy), FUN = quantile, probs = 0.975, na.rm = TRUE)

nCamInd = 9 # here define ncam
MSEdf = data.frame(len = MSE50$len, Prec = MSE50$Prec, NCam = MSE50$NCam, Strategy = MSE50$Strategy,
                   q50 = MSE50$x, q5 = MSE025$x, q95 = MSE975$x)

base_plot = MSEdf[MSEdf$Prec == 'H' & MSEdf$NCam == nCamInd & MSEdf$Strategy == StrategySel, ]

png(file.path('figures', paste0('compareLenMSENCam', nCamInd, '_PrecH_Strat', StrategySel,'.png')), 
     width = 180, height = 120, units = 'mm', res = 500)
ggplot(base_plot, aes(x = len)) +
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = factor(Prec)),  alpha=0.2) +
  geom_line(aes(y = q50), colour = '#F8766D') +
  xlab("Longitud (cm)") + 
  ylab("MSE (e-06)") +
  theme_bw()  +
  theme(legend.position = 'none')
dev.off()

base_plot = MSEdf[MSEdf$Strategy == StrategySel, ]
base_plot$Prec = factor(base_plot$Prec, levels = c('H', "M", "L"))

png(file.path('figures', paste0('compareLenMSEByNCam_ByPrec_Strat', StrategySel,'.png')), 
     width = 180, height = 120, units = 'mm', res = 500)
ggplot(base_plot, aes(x = len)) +
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = Prec), alpha=0.2) +
  geom_line(aes(y = q50, colour = Prec)) +
  xlab("Longitud (cm)") + 
  ylab("MSE (e-06)") +
  theme_bw() +
  theme(legend.position = c(0.9, 0.075), legend.background = element_rect(fill = "transparent")) +
  labs(col = "Precision", fill = "Precision") +
  facet_wrap( ~ NCam, scales = 'free_y', ncol = 4)
dev.off()


# ----------------------------------------------------------
# ----------------------------------------------------------
# ----------------------------------------------------------
# Read MRE files:
list_files = list.files(file.path('MRE'))
scalar_files = paste0('MRE', '/', list_files[grep(pattern = 'MRE', list_files)])
all_scalar_dfs = lapply(scalar_files, read.csv)
base_scalar = bind_rows(all_scalar_dfs)

# For next plots:
base_plot_1 = aggregate(base_scalar$MRE, list(Prec = base_scalar$Prec, Strategy = base_scalar$Strategy, 
                      iter = base_scalar$iter, NCam = base_scalar$NCam), FUN = median, na.rm = TRUE)
# Make plot MRE: (only precision size = H):
base_plot = base_plot_1[base_plot_1$Prec == 'H' & base_plot_1$Strategy == StrategySel, ]

png(file.path('figures', paste0('compareMREPrecH_Strat', StrategySel, '.png')), 
     width = 90, height = 70, units = 'mm', res = 500)
ggplot(base_plot, aes(x=factor(NCam), y=x)) +
  geom_boxplot(fill='#F8766D', color="#F8766D", alpha = 0.2) +
  xlab('Numero de camaras') +
  ylab('MRE') +
  theme_bw()
dev.off()

# ----------------------------------------------------------
# Make plot MRE: Including precision size levels:
base_plot = base_plot_1[base_plot_1$Strategy == StrategySel, ]
base_plot$Prec = factor(base_plot$Prec, levels = c('H', "M", "L"))
png(file.path('figures', paste0('compareMRE_Strat', StrategySel,'.png')), 
     width = 180, height = 90, units = 'mm', res = 500)
ggplot(base_plot, aes(x=factor(NCam), y=x, fill = factor(Prec), color = factor(Prec))) +
  geom_boxplot(alpha = 0.2) +
  xlab('Numero de camaras') +
  ylab('MRE') +
  labs(fill = "Precision", color = "Precision") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2), legend.background = element_rect(fill = "transparent")) 
dev.off()

# ----------------------------------------------------------
# Make plot MRE by len and by precision level:
MRE50 = aggregate(base_scalar$MRE, list(len = base_scalar$len, Prec = base_scalar$Prec, 
                    NCam = base_scalar$NCam, Strategy = base_scalar$Strategy), FUN = median, na.rm = TRUE)
MRE025 = aggregate(base_scalar$MRE, list(len = base_scalar$len, Prec = base_scalar$Prec, 
                    NCam = base_scalar$NCam, Strategy = base_scalar$Strategy), FUN = quantile, probs = 0.025, na.rm = TRUE)
MRE975 = aggregate(base_scalar$MRE, list(len = base_scalar$len, Prec = base_scalar$Prec, 
                    NCam = base_scalar$NCam, Strategy = base_scalar$Strategy), FUN = quantile, probs = 0.975, na.rm = TRUE)

MREdf = data.frame(len = MRE50$len, Prec = MRE50$Prec, NCam = MRE50$NCam, Strategy = MRE50$Strategy,
                   q50 = MRE50$x, q5 = MRE025$x, q95 = MRE975$x)

nCamInd = 9
base_plot = MREdf[MREdf$Prec == 'H' & MREdf$NCam == nCamInd & MREdf$Strategy == StrategySel, ]

png(file.path('figures', paste0('compareLenMRENCam', nCamInd,'_PrecH_Strat', StrategySel,'.png')), 
     width = 180, height = 120, units = 'mm', res = 500)
ggplot(base_plot, aes(x = len)) +
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = factor(Prec)),  alpha=0.2) +
  geom_line(aes(y = q50), colour = '#F8766D') +
  xlab("Longitud (cm)") + 
  ylab("MRE") +
  theme_bw()  +
  theme(legend.position = 'none')
dev.off()

base_plot = MREdf[MREdf$Strategy == StrategySel, ]
base_plot$Prec = factor(base_plot$Prec, levels = c('H', "M", "L"))

png(file.path('figures', paste0('compareLenMREByNCam_ByPrec_Strat', StrategySel,'.png')), 
     width = 180, height = 120, units = 'mm', res = 500)
ggplot(base_plot, aes(x = len)) +
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = Prec), alpha=0.2) +
  geom_line(aes(y = q50, colour = Prec)) +
  xlab("Longitud (cm)") + 
  ylab("MRE") +
  theme_bw() +
  theme(legend.position = c(0.9, 0.075), legend.background = element_rect(fill = "transparent")) +
  scale_y_continuous(breaks=c(-1, 0, 1)) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(col = "Precision", fill = "Precision") +
  facet_wrap( ~ NCam, ncol = 4)
dev.off()

# COMPARE SAMPLING STRATEGIES: SIZE
# ----------------------------------------------------------
# ----------------------------------------------------------
# ----------------------------------------------------------
library(RColorBrewer)
ColPalStr = brewer.pal(n = nStrategy, 'Set1')  # number of strategies

PrecSel = 'H'

# MSE:
list_files = list.files(file.path('MSE'))
scalar_files = paste0('MSE', '/', list_files[grep(pattern = 'MSE', list_files)])
all_scalar_dfs = lapply(scalar_files, read.csv)
base_scalar = bind_rows(all_scalar_dfs)
base_scalar$MSE = base_scalar$MSE*1e06

# For next plots:
base_plot_1 = aggregate(base_scalar$MSE, list(Prec = base_scalar$Prec, Strategy = base_scalar$Strategy, 
                      iter = base_scalar$iter, NCam = base_scalar$NCam), FUN = median)
# Make plot MSE: (only precision size = H):
base_plot = base_plot_1[base_plot_1$Prec == PrecSel, ]
base_plot$Strategy = factor(base_plot$Strategy, levels = c('Max900', "Max200", "Max125", "Max50", 'Prop75', 'Prop50'))

png(file.path('figures', paste0('compareMSE_ByNCam_ByStrat_Prec', PrecSel,'.png')), 
     width = 180, height = 90, units = 'mm', res = 500)
ggplot(base_plot, aes(x=factor(NCam), y=x, fill = factor(Strategy), color = factor(Strategy))) +
  geom_boxplot(alpha = 0.2) +
  xlab('Numero de camaras') +
  ylab('MSE (e-06)') +
  labs(fill = "Estrategia", color = "Estrategia") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.7), legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(labels = c("All", "Max 200", "Max 125", "Max 50", "Prop 75%", "Prop 50%"), values = ColPalStr) +
  scale_fill_manual(labels = c("All", "Max 200", "Max 125", "Max 50", "Prop 75%", "Prop 50%"), values = ColPalStr)
dev.off()

# ----------------------------------------------------------
# Make plot MSE by len and by Strategy level:
MSE50 = aggregate(base_scalar$MSE, list(len = base_scalar$len, Prec = base_scalar$Prec, 
                    NCam = base_scalar$NCam, Strategy = base_scalar$Strategy), FUN = median)
MSE025 = aggregate(base_scalar$MSE, list(len = base_scalar$len, Prec = base_scalar$Prec, 
                    NCam = base_scalar$NCam, Strategy = base_scalar$Strategy), FUN = quantile, probs = 0.025, na.rm = TRUE)
MSE975 = aggregate(base_scalar$MSE, list(len = base_scalar$len, Prec = base_scalar$Prec, 
                    NCam = base_scalar$NCam, Strategy = base_scalar$Strategy), FUN = quantile, probs = 0.975, na.rm = TRUE)

MSEdf = data.frame(len = MSE50$len, Prec = MSE50$Prec, NCam = MSE50$NCam, Strategy = MSE50$Strategy,
                   q50 = MSE50$x, q5 = MSE025$x, q95 = MSE975$x)

base_plot = MSEdf[MSEdf$Prec == PrecSel, ]
base_plot$Strategy = factor(base_plot$Strategy, levels = c('Max900', "Max200", "Max125",  "Max50", 'Prop75', 'Prop50'))

png(file.path('figures', paste0('compareLenMSE_ByNCam_Prec', PrecSel,'_ByStrat.png')), 
     width = 180, height = 120, units = 'mm', res = 500)
ggplot(base_plot, aes(x = len)) +
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = Strategy), alpha=0.2) +
  geom_line(aes(y = q50, colour = Strategy)) +
  xlab("Longitud (cm)") + 
  ylab("MSE (e-06)") +
  theme_bw() +
  theme(legend.position = c(0.9, 0.1), legend.background = element_rect(fill = "transparent")) +
  labs(col = "Estrategia", fill = "Estrategia") +
  scale_color_manual(labels = c("All", "Max 200", "Max 125",  "Max 50", "Prop 75%", "Prop 50%"), values = ColPalStr) +
  scale_fill_manual(labels = c("All", "Max 200", "Max 125",  "Max 50", "Prop 75%", "Prop 50%"), values = ColPalStr) +
  facet_wrap( ~ NCam, scales = 'free_y', ncol = 4)
dev.off()

# ------------------------------------
# MRE files:
list_files = list.files(file.path('MRE'))
scalar_files = paste0('MRE', '/', list_files[grep(pattern = 'MRE', list_files)])
all_scalar_dfs = lapply(scalar_files, read.csv)
base_scalar = bind_rows(all_scalar_dfs)

# For next plots:
base_plot_1 = aggregate(base_scalar$MRE, list(Prec = base_scalar$Prec, Strategy = base_scalar$Strategy, 
                      iter = base_scalar$iter, NCam = base_scalar$NCam), FUN = median, na.rm = TRUE)
# Make plot MSE: (only precision size = H):
base_plot = base_plot_1[base_plot_1$Prec == PrecSel, ]
base_plot$Strategy = factor(base_plot$Strategy, levels = c('Max900', "Max200", "Max125",  "Max50", 'Prop75', 'Prop50'))

png(file.path('figures', paste0('compareMRE_ByNCam_ByStrat_Prec', PrecSel,'.png')), 
     width = 180, height = 90, units = 'mm', res = 500)
ggplot(base_plot, aes(x=factor(NCam), y=x, fill = factor(Strategy), color = factor(Strategy))) +
  geom_boxplot(alpha = 0.2) +
  xlab('Numero de camaras') +
  ylab('MRE') +
  labs(fill = "Estrategia", color = "Estrategia") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.3), legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(labels = c("All", "Max 200", "Max 125",  "Max 50", "Prop 75%", "Prop 50%"), values = ColPalStr) +
  scale_fill_manual(labels = c("All", "Max 200", "Max 125",  "Max 50", "Prop 75%", "Prop 50%"), values = ColPalStr)
dev.off()

# ----------------------------------------------------------
# Make plot MRE by len and by precision level:
MRE50 = aggregate(base_scalar$MRE, list(len = base_scalar$len, Prec = base_scalar$Prec, 
                    NCam = base_scalar$NCam, Strategy = base_scalar$Strategy), FUN = median, na.rm = TRUE)
MRE025 = aggregate(base_scalar$MRE, list(len = base_scalar$len, Prec = base_scalar$Prec, 
                    NCam = base_scalar$NCam, Strategy = base_scalar$Strategy), FUN = quantile, probs = 0.025, na.rm = TRUE)
MRE975 = aggregate(base_scalar$MRE, list(len = base_scalar$len, Prec = base_scalar$Prec, 
                    NCam = base_scalar$NCam, Strategy = base_scalar$Strategy), FUN = quantile, probs = 0.975, na.rm = TRUE)

MREdf = data.frame(len = MRE50$len, Prec = MRE50$Prec, NCam = MRE50$NCam, Strategy = MRE50$Strategy,
                   q50 = MRE50$x, q5 = MRE025$x, q95 = MRE975$x)

base_plot = MREdf[MREdf$Prec == PrecSel, ]
base_plot$Strategy = factor(base_plot$Strategy, levels = c('Max900', "Max200", "Max125",  "Max50", 'Prop75', 'Prop50'))

png(file.path('figures', paste0('compareLenMRE_ByNCam_Prec', PrecSel,'_ByStrat.png')), 
     width = 180, height = 120, units = 'mm', res = 500)
ggplot(base_plot, aes(x = len)) +
  geom_ribbon(aes(ymin = q5, ymax = q95, fill = Strategy), alpha=0.2) +
  geom_line(aes(y = q50, colour = Strategy)) +
  xlab("Longitud (cm)") + 
  ylab("MRE") +
  theme_bw() +
  theme(legend.position = c(0.9, 0.1), legend.background = element_rect(fill = "transparent")) +
  scale_y_continuous(breaks=c(-1, 0, 1)) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(col = "Precision", fill = "Precision") +
  scale_color_manual(labels = c("All", "Max 200", "Max 125",  "Max 50", "Prop 75%", "Prop 50%"), values = ColPalStr) +
  scale_fill_manual(labels = c("All", "Max 200", "Max 125",  "Max 50", "Prop 75%", "Prop 50%"), values = ColPalStr) +
  facet_wrap( ~ NCam, ncol = 4)
dev.off()


# COMPARE SEX PRECISION
# ----------------------------------------------------------
# ----------------------------------------------------------
# ----------------------------------------------------------
# Read sex Prop files:
list_files = list.files(file.path('sexProp'))
scalar_files = paste0('sexProp', '/', list_files[grep(pattern = 'sexProp', list_files)])
all_scalar_dfs = lapply(scalar_files, read.csv)
base_scalar = bind_rows(all_scalar_dfs)

# ----------------------------------------------------------
# MSE figures:
# Select df:
base_plot_1 = base_scalar[base_scalar$PrecSex == 100 & base_scalar$Strategy == StrategySel, ]

# Aggregate over fishing operations:
base_plot = aggregate(base_plot_1$prop, list(PrecSex = base_plot_1$PrecSex, Strategy = base_plot_1$Strategy, 
                                             iter = base_plot_1$iter, NCam = base_plot_1$NCam), FUN = mean, na.rm = TRUE)
base_plot$MSE = (FemFrac - base_plot$x)^2 # calculate MSE. FemFrac because it is the true prop in the population
base_plot$MSE = base_plot$MSE*1e03

# Make plot:
png(file.path('figures', paste0('compareSexPropMSE_PrecSex100_ByNCam_Strat', StrategySel,'.png')), 
     width = 90, height = 70, units = 'mm', res = 500)
ggplot(base_plot, aes(x=factor(NCam), y=MSE)) +
  geom_boxplot(fill='#F8766D', color="#F8766D", alpha = 0.2) +
  xlab('Numero de camaras') +
  ylab('MSE (e-03)') +
  theme_bw()
dev.off()

# Make plot Sex by precision sex
base_plot_1 = base_scalar[base_scalar$Strategy == StrategySel, ]

# Aggregate over fishing operations:
base_plot = aggregate(base_plot_1$prop, list(PrecSex = base_plot_1$PrecSex, Strategy = base_plot_1$Strategy, 
                                             iter = base_plot_1$iter, NCam = base_plot_1$NCam), FUN = mean, na.rm = TRUE)
base_plot$MSE = (FemFrac - base_plot$x)^2 # calculate MSE. FemFrac because it is the true prop in the population
base_plot$MSE = base_plot$MSE*1e03
base_plot$PrecSex = paste0(base_plot$PrecSex, '%')
base_plot$PrecSex = factor(base_plot$PrecSex, levels = c('100%', "90%", "80%"))

png(file.path('figures', paste0('compareSexPropMSE_ByNCam_ByPrecSex_Strat', StrategySel,'.png')), 
     width = 180, height = 90, units = 'mm', res = 500)
ggplot(base_plot, aes(x=factor(NCam), y=MSE, fill = factor(PrecSex), color = factor(PrecSex))) +
  geom_boxplot(alpha = 0.2) +
  xlab('Numero de camaras') +
  ylab('MSE (e-03)') +
  labs(fill = "Precision", color = "Precision") +
  theme_bw() +
  theme(legend.position = c(0.75, 0.9), legend.background = element_rect(fill = "transparent"),
        legend.direction = "horizontal") 
dev.off()

# ----------------------------------------------------------
# MRE figures:
# Select df:
base_plot_1 = base_scalar[base_scalar$PrecSex == 100 & base_scalar$Strategy == StrategySel, ]

# Aggregate over fishing operations:
base_plot = aggregate(base_plot_1$prop, list(PrecSex = base_plot_1$PrecSex, Strategy = base_plot_1$Strategy, 
                                             iter = base_plot_1$iter, NCam = base_plot_1$NCam), FUN = mean, na.rm = TRUE)
base_plot$MRE = (base_plot$x-FemFrac)/FemFrac # calculate MRE. FemFrac because it is the true prop in the population

# Make plot:
png(file.path('figures', paste0('compareSexPropMRE_PrecSex100_ByNCam_Strat', StrategySel,'.png')), 
     width = 90, height = 70, units = 'mm', res = 500)
ggplot(base_plot, aes(x=factor(NCam), y=MRE)) +
  geom_boxplot(fill='#F8766D', color="#F8766D", alpha = 0.2) +
  xlab('Numero de camaras') +
  ylab('MRE') +
  theme_bw()
dev.off()

# Make plot Sex by precision sex
base_plot_1 = base_scalar[base_scalar$Strategy == StrategySel, ]

# Aggregate over fishing operations:
base_plot = aggregate(base_plot_1$prop, list(PrecSex = base_plot_1$PrecSex, Strategy = base_plot_1$Strategy, 
                                             iter = base_plot_1$iter, NCam = base_plot_1$NCam), FUN = mean, na.rm = TRUE)
base_plot$MRE = (base_plot$x-FemFrac)/FemFrac # calculate MRE. FemFrac because it is the true prop in the population
base_plot$PrecSex = paste0(base_plot$PrecSex, '%')
base_plot$PrecSex = factor(base_plot$PrecSex, levels = c('100%', "90%", "80%"))

png(file.path('figures', paste0('compareSexPropMRE_ByNCam_ByPrecSex_Strat', StrategySel,'.png')), 
     width = 180, height = 90, units = 'mm', res = 500)
ggplot(base_plot, aes(x=factor(NCam), y=MRE, fill = factor(PrecSex), color = factor(PrecSex))) +
  geom_boxplot(alpha = 0.2) +
  xlab('Numero de camaras') +
  ylab('MRE') +
  labs(fill = "Precision", color = "Precision") +
  theme_bw() +
  theme(legend.position = c(0.75, 0.1), legend.background = element_rect(fill = "transparent"),
        legend.direction = "horizontal") 
dev.off()


# COMPARE SAMPLING STRATEGIES: SEX PROP
# ----------------------------------------------------------
# ----------------------------------------------------------
# ----------------------------------------------------------
library(RColorBrewer)
ColPalStr = brewer.pal(n = nStrategy, 'Set1')
PrecSel = '100'

# MSE:
base_plot_1 = base_scalar[base_scalar$PrecSex == PrecSel, ]

# Aggregate over fishing operations:
base_plot = aggregate(base_plot_1$prop, list(PrecSex = base_plot_1$PrecSex, Strategy = base_plot_1$Strategy, 
                                             iter = base_plot_1$iter, NCam = base_plot_1$NCam), FUN = mean, na.rm = TRUE)
base_plot$MSE = (FemFrac - base_plot$x)^2 # calculate MSE. FemFrac because it is the true prop in the population
base_plot$MSE = base_plot$MSE*1e03
base_plot$Strategy = factor(base_plot$Strategy, levels = c('Max900', "Max200", "Max125",  "Max50", 'Prop75', 'Prop50'))

png(file.path('figures', paste0('compareSexPropMSE_ByNCam_ByStrat_Prec', PrecSel,'.png')), 
     width = 180, height = 90, units = 'mm', res = 500)
ggplot(base_plot, aes(x=factor(NCam), y=MSE, fill = factor(Strategy), color = factor(Strategy))) +
  geom_boxplot(alpha = 0.2) +
  xlab('Numero de camaras') +
  ylab('MSE (e-03)') +
  labs(fill = "Estrategia", color = "Estrategia") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.7), legend.background = element_rect(fill = "transparent")) +
  scale_color_manual(labels = c("All", "Max 200", "Max 125",  "Max 50", "Prop 75%", "Prop 50%"), values = ColPalStr) +
  scale_fill_manual(labels = c("All", "Max 200", "Max 125",  "Max 50", "Prop 75%", "Prop 50%"), values = ColPalStr)
dev.off()


# MRE:
base_plot_1 = base_scalar[base_scalar$PrecSex == PrecSel, ]

# Aggregate over fishing operations:
base_plot = aggregate(base_plot_1$prop, list(PrecSex = base_plot_1$PrecSex, Strategy = base_plot_1$Strategy, 
                                             iter = base_plot_1$iter, NCam = base_plot_1$NCam), FUN = mean, na.rm = TRUE)
base_plot$MRE = (base_plot$x - FemFrac)/FemFrac # calculate MSE. FemFrac because it is the true prop in the population
base_plot$Strategy = factor(base_plot$Strategy, levels = c('Max900', "Max200", "Max125",  "Max50", 'Prop75', 'Prop50'))

png(file.path('figures', paste0('compareSexPropMRE_ByNCam_ByStrat_Prec', PrecSel,'.png')), 
     width = 180, height = 90, units = 'mm', res = 500)
ggplot(base_plot, aes(x=factor(NCam), y=MRE, fill = factor(Strategy), color = factor(Strategy))) +
  geom_boxplot(alpha = 0.2) +
  xlab('Numero de camaras') +
  ylab('MRE') +
  labs(fill = "Estrategia", color = "Estrategia") +
  #coord_cartesian(ylim = c(-0.2, 0.2)) +
  theme_bw() +
  theme(legend.position = c(0.6, 0.1), legend.background = element_rect(fill = "transparent"),
        legend.direction = "horizontal") +
  scale_color_manual(labels = c("All", "Max 200", "Max 125",  "Max 50", "Prop 75%", "Prop 50%"), values = ColPalStr) +
  scale_fill_manual(labels = c("All", "Max 200", "Max 125",  "Max 50", "Prop 75%", "Prop 50%"), values = ColPalStr)
dev.off()


# ----------------------------------------------------------
# ----------------------------------------------------------
# ----------------------------------------------------------
# Read catch files:
list_files = list.files(file.path('catch_output'))
scalar_files = paste0('catch_output', '/', list_files[grep(pattern = 'catchData', list_files)])
all_scalar_dfs = lapply(scalar_files, read.csv)
base_scalar = bind_rows(all_scalar_dfs)

# Make plot:
png(file.path('figures', 'catchDistribution.png'), 
     width = 90, height = 80, units = 'mm', res = 500)
ggplot(base_scalar, aes(x=catch)) +
  geom_histogram(color = 'black', fill = 'gray80') +
  xlab('Captura por operacion') +
  ylab('Frecuencia') +
  theme_bw()
dev.off()


# ----------------------------------------------------------
# ----------------------------------------------------------
# ----------------------------------------------------------
# Read bycatch files:
list_files = list.files(file.path('bycatch'))
scalar_files = paste0('bycatch', '/', list_files[grep(pattern = 'bycatch', list_files)])
all_scalar_dfs = lapply(scalar_files, read.csv)
base_scalar = bind_rows(all_scalar_dfs)

# Make plot:
base_scalar$MRE = (base_scalar$NspeSeasonSampled - base_scalar$NspeSeason)/base_scalar$NspeSeason

png(file.path('figures', 'compareBycatchMRE_ByNCam.png'), 
     width = 90, height = 80, units = 'mm', res = 500)
ggplot(base_scalar, aes(x=factor(NCam), y=MRE)) +
  geom_boxplot(alpha = 0.2, fill = 'gray50') +
  xlab('Numero de camaras') +
  ylab('MRE') +
  #coord_cartesian(ylim = c(-0.2, 0.2)) +
  theme_bw() 
dev.off()
