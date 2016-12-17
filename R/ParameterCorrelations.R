##### Checking correlation of bootstrapped parameters ######
# Date: 12-16-16
# Last updated: 12-16-16 by IAN McC
############################################################

#### Set ups ####
setwd('H:/Ian_GIS/gleon/SOS')
nruns = 1000
runs = paste0(rep('run',nruns),seq(1,nruns,1))
params = c('DOCR_RespParam','DOCL_RespParam','R_auto','BurialFactor_R','BurialFactor_L',
           'POC_lcR','POC_lcL','ObservedMAR_oc','converge')

######################## Main program ######################
#### Monona ####
Monona = as.data.frame(t(read.csv('MononaLake/Results/Monona_boostrapResults1000.csv')))
rownames(Monona) = runs
colnames(Monona) = params
cor(Monona)

#### Vanern ####
Vanern = as.data.frame(t(read.csv('VanernLake/Results/Vanern_boostrapResults1000.csv')))
rownames(Vanern) = runs
colnames(Vanern) = params
cor(Vanern)

#### Trout ####
Trout = as.data.frame(t(read.csv('TroutLake/Results/Trout_boostrapResults1000.csv')))
rownames(Trout) = runs
colnames(Trout) = params
cor(Trout)

#### Toolik ####
Toolik = as.data.frame(t(read.csv('ToolikLake/Results/Toolik_boostrapResults1000.csv')))
rownames(Toolik) = runs
colnames(Toolik) = params
cor(Toolik)

#### Harp ####
Harp = as.data.frame(t(read.csv('HarpLake/Results/Harp_boostrapResults1000.csv')))
rownames(Harp) = runs
colnames(Harp) = params
cor(Harp)
