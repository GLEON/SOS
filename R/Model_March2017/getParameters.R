getParameters <- function(lakenames) {
  
  df = data.frame(lakename=lakenames,DOCR_RespParam=NA,DOCL_RespParam=NA,
                  BurialFactor_R=NA,BurialFactor_L=NA)  
  
  for (n in 1:length(lakenames)) {
    LakeName = lakenames[n]
    
    ParameterFile <- paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep='')
    ##### READ PARAMETER FILE ##################
    parameters <- read.table(file = ParameterFile,header=TRUE,comment.char="#",stringsAsFactors = F)
    for (i in 1:nrow(parameters)){ # assign parameters
      assign(parameters[i,1],parameters[i,2])
    }
    
    df[n,-1] = c(DOCR_RespParam,DOCL_RespParam,BurialFactor_R,BurialFactor_L)  
  }
  return(df)
}

gP = getParameters(c('Monona','Trout','Harp','Vanern','Toolik'))
write.table(gP, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)


getParameters2 <- function(lakenames) {
  df = data.frame(lakename=NA,DOCR_RespParam=NA,DOCL_RespParam=NA,
                  BurialFactor_R=NA,BurialFactor_L=NA)  
  
  for (n in 1:length(lakenames)) {
    LakeName = lakenames[n]
    ##### READ PARAMETER FILE ##################
    for (i in 1){ # assign parameters
      if (i == 1){ParameterFile <- paste('./R/FMEresults/',LakeName,'_fitpars.csv',sep='')}
      if (i == 2) {ParameterFile <- paste('./R/FMEresults/',LakeName,'_fitpars_Burial0.csv',sep='')}
      if (i == 3) {ParameterFile <- paste('./R/FMEresults/',LakeName,'_fitpars_Burial1.csv',sep='')}
      parameters <- read.table(file = ParameterFile,header=TRUE,comment.char="#",stringsAsFactors = F)
      df = rbind(df,c(LakeName,round(parameters[,1],4)))
    }
  }
  return(df)
}
gP = getParameters2(c('Monona','Trout','Harp','Vanern','Toolik'))
write.table(gP, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)
