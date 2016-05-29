#### The legendary beard plot ####
# Warning: things could get hairy


#### Set your own working directory #####
setwd("C:/Users/Ian/Desktop/GLEON/SOS/")
#setwd(paste0(mywd,lakename,'Lake','/','Results'))

# Vanern
Vanern_DOC = read.csv('VanernLake/Results/Vanern_DOC_Results.csv')
cc = which(complete.cases(Vanern_DOC))
Vanern_DOC = Vanern_DOC[cc[1]:tail(cc,1),]
Vanern_POC = read.csv('VanernLake/Results/Vanern_POC_Results.csv')
cc = which(complete.cases(Vanern_POC))
Vanern_POC = Vanern_POC[cc[1]:tail(cc,1),]

# Toolik
Toolik_DOC = read.csv('ToolikLake/Results/Toolik_DOC_Results.csv')
cc = which(complete.cases(Toolik_DOC))
Toolik_DOC = Toolik_DOC[cc[1]:tail(cc,1),]
Toolik_POC = read.csv('ToolikLake/Results/Toolik_POC_Results.csv')
cc = which(complete.cases(Toolik_POC))
Toolik_POC = Toolik_POC[cc[1]:tail(cc,1),]
Toolik_POC = Toolik_POC[-c(913), ]

# Trout
Trout_DOC = read.csv('TroutLake/Results/Trout_DOC_Results.csv')
cc = which(complete.cases(Trout_DOC))
Trout_DOC = Trout_DOC[cc[1]:tail(cc,1),]
Trout_POC = read.csv('TroutLake/Results/Trout_POC_Results.csv')
cc = which(complete.cases(Trout_POC))
Trout_POC = Trout_POC[cc[1]:tail(cc,1),]

# Mendota
Mendota_DOC = read.csv('MendotaLake/Results/Mendota_DOC_Results.csv')
cc = which(complete.cases(Mendota_DOC))
Mendota_DOC = Mendota_DOC[cc[1]:tail(cc,1),]
Mendota_POC = read.csv('MendotaLake/Results/Mendota_POC_Results.csv')
cc = which(complete.cases(Mendota_POC))
Mendota_POC = Mendota_POC[cc[1]:tail(cc,1),]

# Harp
Harp_DOC = read.csv('HarpLake/Results/Harp_DOC_Results.csv')
cc = which(complete.cases(Harp_DOC))
Harp_DOC = Harp_DOC[cc[1]:tail(cc,1),]
Harp_POC = read.csv('HarpLake/Results/Harp_POC_Results.csv')
cc = which(complete.cases(Harp_POC))
Harp_POC = Harp_POC[cc[1]:tail(cc,1),]

# Annie
Annie_DOC = read.csv('AnnieLake/Results/Annie_DOC_Results.csv')
cc = which(complete.cases(Annie_DOC))
Annie_DOC = Annie_DOC[cc[1]:tail(cc,1),]
Annie_POC = read.csv('AnnieLake/Results/Annie_POC_Results.csv')
cc = which(complete.cases(Annie_POC))
Annie_POC = Annie_POC[cc[1]:tail(cc,1),]

#### Calculate respiration/sedimentation and autochthonous/allochthonous ratio

# Calculate resipration/sedimentation ratio (RS)
Vanern_RS = Vanern_DOC$respOut_gm2y/Vanern_POC$sedOut_gm2y
Toolik_RS = Toolik_DOC$respOut_gm2y/Toolik_POC$sedOut_gm2y
Trout_RS = Trout_DOC$respOut_gm2y/Trout_POC$sedOut_gm2y
Mendota_RS = Mendota_DOC$respOut_gm2y/Mendota_POC$sedOut_gm2y
Harp_RS = Harp_DOC$respOut_gm2y/Harp_POC$sedOut_gm2y
Annie_RS = Annie_DOC$respOut_gm2y/Annie_POC$sedOut_gm2y

# Calculate autochthonous/allochthonous ratio (AA)
Vanern_AA = Vanern_DOC$DOC_in_autoch_g/Vanern_DOC$DOC_in_alloch_g
Toolik_AA = Toolik_DOC$DOC_in_autoch_g/Toolik_DOC$DOC_in_alloch_g
Trout_AA = Trout_DOC$DOC_in_autoch_g/Trout_DOC$DOC_in_alloch_g
Mendota_AA = Mendota_DOC$DOC_in_autoch_g/Mendota_DOC$DOC_in_alloch_g
Harp_AA = Harp_DOC$DOC_in_autoch_g/Harp_DOC$DOC_in_alloch_g
Annie_AA = Annie_DOC$DOC_in_autoch_g/Annie_DOC$DOC_in_alloch_g

# Combine into single data frame
Vanern_df = data.frame(Date = Vanern_DOC$Date, RS = Vanern_RS, AA = Vanern_AA)
Toolik_df = data.frame(Date = Toolik_DOC$Date, RS = Toolik_RS, AA = Toolik_AA)
Trout_df = data.frame(Date = Trout_DOC$Date, RS = Trout_RS, AA = Trout_AA)
Mendota_df = data.frame(Date = Mendota_DOC$Date, RS = Mendota_RS, AA = Mendota_AA)
Harp_df = data.frame(Date = Harp_DOC$Date, RS = Harp_RS, AA = Harp_AA)
Annie_df = data.frame(Date = Annie_DOC$Date, RS = Annie_RS, AA = Annie_AA)

# plot
par(mfrow=c(2,3))
ylab = 'Respiration/Sedimentation Ratio'
xlab = 'Autochthonous/Allochthonous Ratio'

plot(RS ~ AA, Vanern_df, xlab = xlab, ylab = ylab, main='Vanern')
plot(RS ~ AA, Toolik_df, xlab = xlab, ylab = ylab, main='Toolik')
plot(RS ~ AA, Trout_df, xlab = xlab, ylab = ylab, main='Trout')
plot(RS ~ AA, Mendota_df, xlab = xlab, ylab = ylab, main='Mendota')
plot(RS ~ AA, Harp_df, xlab = xlab, ylab = ylab, main='Harp')
plot(RS ~ AA, Annie_df, xlab = xlab, ylab = ylab, main='Annie')

