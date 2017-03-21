##This program is to calculate the long-term mean allochthony, autochthony, R, S, units in g

SOS_mean = function(LakeName){
  #Lakename = lake name in quotes
  #Read in results data from SOS Carbon Flux Model
  DOC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOC_Results.csv',sep='')
  POC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_POC_Results.csv',sep='')
  SOS_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_SOS_Results.csv',sep='')
  
  DOC_df <- read.csv(DOC_results_filename)
  POC_df <- read.csv(POC_results_filename)
  #SOS <- read.csv(SOS_results_filename)
  
  #ParameterFile <- read.csv(paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep=''),sep='\t')
  ParameterFile <- paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep='')
  parameters <- read.table(file = ParameterFile,header=TRUE,comment.char="#",stringsAsFactors = F)
  for (i in 1:nrow(parameters)){ # assign parameters
    assign(parameters[i,1],parameters[i,2])
  }
  
  # IM: was getting error about atomic vectors, so reverted to reading of params
  # used in optim6
  #volume = ParameterFile[ParameterFile$Parameter == 'LakeVolume',]$Value
  #area = ParameterFile[ParameterFile$Parameter == 'LakeArea',]$Value
  volume = LakeVolume
  area = LakeArea
  
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
  summary = data.frame(Alloch_gm2y=alloch,Autoch_gm2y=autoch,R_gm2y=R,S_gm2y=S,Out_gm2y=fOut)
  return(summary)
}

Harp = SOS_mean('Harp')
Monona = SOS_mean('Monona')
Toolik = SOS_mean('Toolik')
Trout = SOS_mean('Trout')
Vanern = SOS_mean('Vanern')

SOS_lakes = rbind.data.frame(Harp, Monona, Toolik, Trout, Vanern)
rownames(SOS_lakes) = c('Harp', 'Monona', 'Toolik', 'Trout', 'Vanern')
write.csv(SOS_lakes, file='SOS_mean.csv')
