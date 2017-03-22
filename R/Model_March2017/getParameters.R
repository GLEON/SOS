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
