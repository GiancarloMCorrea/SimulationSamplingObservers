
# Multinomial dist --------------------------------------------------------


require(ggplot2)

output = NULL
randomCatch = 180
for(i in 1:100){

poplen = rnorm(n = 5000, mean = 40, sd = 4)
#hist(poplen)

poplen = round(poplen)
#hist(poplen)

df = data.frame(len = 1:100, prop = 0)
df$prop[match(as.numeric(names(table(poplen))), df$len)] = as.vector(table(poplen))
df$prop = df$prop/sum(df$prop)
df$type = 'pop'

# Precision VH:
samlen1 = rmultinom(n = 1, size = randomCatch, prob = df$prop)
#hist(samlen1)

df1 = data.frame(len = 1:100, prop = as.vector(samlen1))
df1$prop = df1$prop/sum(df1$prop)
df1$type = 'VH'


# Precision VL:
samlen2 = rep(x = 1:100, times = as.vector(samlen1))
samlen2 = samlen2 + sample(x = -4:4, size = length(samlen2), replace = TRUE)
#hist(samlen2)

df2 = data.frame(len = 1:100, prop = 0)
df2$prop[match(as.numeric(names(table(samlen2))), df2$len)] = as.vector(table(samlen2))
df2$prop = df2$prop/sum(df2$prop)
df2$type = 'VL'


# Plot:
alldf = rbind(df, df1, df2)

# ggplot(data = alldf, aes(x = len, y = prop)) +
#   geom_line(aes(color = factor(type))) +
#   theme_bw()

# MSE:
tmp1 = data.frame(len = 1:100, MSE = (df1$prop - df$prop)^2, type = 'VH')
tmp2 = data.frame(len = 1:100, MSE = (df2$prop - df$prop)^2, type = 'VL')
tmp = rbind(tmp1, tmp2)
output = rbind(output, tmp)

}

toPlot = aggregate(output$MSE, list(len = output$len, type = output$type), FUN = median)

ggplot(data = toPlot, aes(x = len, y = x)) +
  geom_line(aes(color = factor(type))) +
  theme_bw()


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Poisson link delta model ------------------------------------------------


areaSwept = 3
sigmaM = 0.7
require(ggplot2)

output = NULL
for(i in 1:100){
  
  poplen = rnorm(n = 5000, mean = 40, sd = 4)
  #hist(poplen)
  
  poplen = round(poplen)
  #hist(poplen)
  
  df = data.frame(len = 1:100, prop = 0)
  df$prop[match(as.numeric(names(table(poplen))), df$len)] = as.vector(table(poplen))
  SelAbun2 = df$prop/100
  df$prop = df$prop/sum(df$prop)
  df$type = 'pop'
  
  # Precision VH:
  
  pL = 1 - exp(-areaSwept*SelAbun2)
  pLsim = structure(vapply(pL, rbinom, numeric(1), n = 1, size = 1), dim=dim(pL))
  rL = (areaSwept*SelAbun2)/pL
  findNAN = which(is.nan(rL)|is.infinite(rL))
  rL[findNAN] = 1 # to avoid warnings
  randomNumbers = structure(rnorm(length(rL), mean = 0, sd = sigmaM), dim=dim(rL))
  rLsim = structure(vapply(rL*exp(randomNumbers), rpois, numeric(1), n = 1), dim=dim(rL))
  nFishLenSampled = pLsim*rLsim # get round sampled matrix
  
  #hist(samlen1)
  
  df1 = data.frame(len = 1:100, prop = nFishLenSampled)
  df1$prop = df1$prop/sum(df1$prop)
  df1$type = 'VH'
  
  
  # Precision VL:
  samlen2 = rep(x = 1:100, times = nFishLenSampled)
  samlen2 = samlen2 + sample(x = -4:4, size = length(samlen2), replace = TRUE)
  #hist(samlen2)
  
  df2 = data.frame(len = 1:100, prop = 0)
  df2$prop[match(as.numeric(names(table(samlen2))), df2$len)] = as.vector(table(samlen2))
  df2$prop = df2$prop/sum(df2$prop)
  df2$type = 'VL'
  
  
  # Plot:
  alldf = rbind(df, df1, df2)
  
  # ggplot(data = alldf, aes(x = len, y = prop)) +
  #   geom_line(aes(color = factor(type))) +
  #   theme_bw()
  # 
  # MSE:
  tmp1 = data.frame(len = 1:100, MSE = (df1$prop - df$prop)^2, type = 'VH')
  tmp2 = data.frame(len = 1:100, MSE = (df2$prop - df$prop)^2, type = 'VL')
  tmp = rbind(tmp1, tmp2)
  output = rbind(output, tmp)
  
}

toPlot = aggregate(output$MSE, list(len = output$len, type = output$type), FUN = median)

ggplot(data = toPlot, aes(x = len, y = x)) +
  geom_line(aes(color = factor(type))) +
  theme_bw()