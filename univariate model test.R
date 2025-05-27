phenotypicdata <- read.csv("./gryphon.csv")
pedigreedata <- read.csv("./gryphonped.csv")

library(MCMCglmm)

inverseAmatrix <- inverseA(pedigreedata)$Ainv


model1.1 <- MCMCglmm(birth_weight ~ 1, #Response and Fixed effect formula
                     random = ~id, # Random effect formula
                     ginverse = list(id = inverseAmatrix), # correlations among random effect levels (here breeding values)
                     data = phenotypicdata,
                     burnin = 10000, nitt = 30000, thin = 20)# data set
