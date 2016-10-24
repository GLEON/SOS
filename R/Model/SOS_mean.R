##This program is to calculate the long-term mean allochthony, autochthony, R, S, units in g

#User input lakename
LakeName = 'Annie'

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

R<-DOC_df$respOut_gm2y

S<-POC_df$sedOut_gm2y

fOut<-(POC_df$leachOut_gm2y + POC_df$FlowOut_gm2y+ DOC_df$FlowOut_gm2y)

alloch<-DOC_df$FlowIn_gm2y+DOC_df$leachIn_gm2y+POC_df$FlowIn_gm2y
autoch<-POC_df$NPPin_gm2y+DOC_df$NPPin_gm2y

alloch<-mean(alloch,rm=T)
autoch<-mean(autoch,rm=T)
R<-mean(R,na.rm=T)
S<-mean(S,na.rm=T)
fOut<-mean(fOut,na.rm=T)
#print("alloch=",alloch,", autoch=", autoch,", R=", R, ", S=",S,", Out", out)
