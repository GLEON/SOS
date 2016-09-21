##This program is to calculate the long-term mean allochthony, autochthony, R, S, units in g

#User input lakename
LakeName = 'Toolik'

#Read in results data from SOS Carbon Flux Model
DOC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOC_Results.csv',sep='')
POC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_POC_Results.csv',sep='')
SOS_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_SOS_Results.csv',sep='')

DOC_df <- read.csv(DOC_results_filename)
POC_df <- read.csv(POC_results_filename)
SOS <- read.csv(SOS_results_filename)

ParameterFile <- read.csv(paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep=''),sep='\t')
volume = ParameterFile[ParameterFile$Parameter == 'LakeVolume',]$Value
area = ParameterFile[ParameterFile$Parameter == 'LakeArea',]$Value

alloch<-POC_df$POC_in_alloch_g+DOC_df$DOC_in_alloch_g
autoch<-POC_df$POC_in_autoch_g+DOC_df$DOC_in_autoch_g

R<-DOC_df$respOut_gm2y*area/365

S<-SOS$Sink

fOut<-(POC_df$leachOut_gm2y*area/365 + POC_df$FlowOut_gm2y*area/365 + DOC_df$DOC_out_g - DOC_df$respOut_gm2y*area/365)

alloch<-mean(alloch,na.rm=T)
autoch<-mean(autoch,na.rm=T)
R<-mean(R,na.rm=T)
S<-mean(S,na.rm=T)
fOut<-mean(fOut,na.rm=T)
#print("alloch=",alloch,", autoch=", autoch,", R=", R, ", S=",S,", Out", out)
