#### The legendary beard plot ####
# Warning: things could get hairy

##### Testing this on Trout Lake only #####
lakename = 'Trout'
mywd = "C:/Users/Ian/Desktop/GLEON/SOS/"
setwd(paste0(mywd,lakename,'Lake','/','Results'))

DOC = read.csv('Trout_DOC_Results.csv')
names(DOC)
POC = read.csv('Trout_POC_Results.csv')
names(POC)
SOS = read.csv('Trout_SOS_Results.csv')
names(SOS)

## Ian 5-15-16: I am not sure if I am using the correct output columns

# Calculate resipration/sedimentation ratio
RS_ratio = DOC$respOut_gm2y/POC$sedOut_gm2y

# Calculate autochthonous/allochthonous ratio
AA_ratio = DOC$DOCautoch_g/DOC$DOCalloch_g

# Combine into single data frame
beard_df = data.frame(Date = DOC$Date, RS = RS_ratio, AA = AA_ratio)

# plot
plot(RS ~ AA, beard_df, xlab = 'Autochthonous/Allochthonous Ratio (g)',
     ylab = 'Respiration/Sedimentation Ratio (g/m2/yr)', main=lakename)
