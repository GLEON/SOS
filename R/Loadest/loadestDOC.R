library(loadflex)

# DOC
harp = read.csv('C:/Users/hdugan/Documents/Rpackages/SOS/HarpLake/StagingFiles/DOC_Sw.csv',stringsAsFactors = F)
head(harp)
harp$sampledate = as.POSIXct(strptime(harp$sampledate,'%m/%d/%Y'))
plot(harp$sampledate,harp$doc)

# Inflow
inflow = read.csv('C:/Users/hdugan/Documents/Rpackages/SOS/HarpLake/StagingFiles/Harp_inflow.csv',stringsAsFactors = F)
head(inflow)
inflow$SDATE = as.POSIXct(strptime(inflow$SDATE,'%m/%d/%Y'))

# Site 3
h3 = harp %>% dplyr::filter(site_name == 'HARP INFLOW #3') %>%
  dplyr::select(Dates = sampledate,Solute = doc)

i3 = inflow %>% dplyr::filter(INFO == 'HARP INFLOW #3') %>%
  dplyr::select(Dates = SDATE,Flow = DISCHARGE) %>%
  left_join(h3,'Dates') %>%
  filter(!is.na(Flow)) %>%
  filter(Flow > 0) %>%
  arrange(Dates)

# As Dates 
d3 = i3 %>% mutate(Dates = as.Date(Dates))

# Remove NA
s3 = i3 %>% filter(!is.na(Solute)) %>%
  mutate(Dates = as.Date(Dates))

a = selBestModel("Solute", data = s3, flow = "Flow",
                 dates = "Dates", conc.units="mg/L")

print(paste0('best model = ',(a$model.no)))

# Models 
lr9 <- loadReg2(loadReg(Solute ~ model(9), data = s3, flow = "Flow",
               dates = "Dates",conc.units="mg/L"))
lrC <- loadComp(reg.model = lr9, interp.format = "conc",
                interp.data = s3)

# Summaries 
summary(getFittedModel(lr9))
qplot(x=Date, y=Resid, data=getResiduals(lr9, newdata=s3))
residDurbinWatson(lr9, "conc", newdata=s3, irreg=TRUE)
estimateRho(lr9, "conc", newdata=s3, irreg=TRUE)$rhoestimateRho(no3_reg2, "conc", newdata=intdat, irreg=TRUE)$rho
getCorrectionFraction(lrC, "flux", newdat=s3)

# Make predictions
# ----------------------
p.lr = predictSolute(lr9, "conc", d3, se.pred=TRUE, date=TRUE)
p.lc = predictSolute(lrC, "conc", d3, se.pred=TRUE, date=TRUE)


# Plot
png('loadestDOC_HARP.png',width = 7,height = 5,units = 'in',res = 300)
  par(mar=c(3,3,3,1),mgp=c(1.5,0.5,0))
  plot(d3$Dates,d3$Solute,type='o',col='red3',pch=16,
       xlab = 'Date',ylab = 'DOC (mg/L)',main = 'Harp Lake Site #3')
  lines(p.lr$date,p.lr$fit,type='o',col='navy',pch=16,cex=0.2)
  lines(p.lc$date,p.lc$fit,type='o',col='seagreen',pch=16,cex=0.2)
  legend('topleft',fill = c('red3','navy','seagreen'),
         legend = c('Observed','Model - Reg','Model - Comp'),
         bty='n')
dev.off()

write.csv(p.lr,'loadestReg_Harp3.csv',row.names = F)
write.csv(p.lc,'loadestComp_Harp3.csv',row.names = F)
write.csv(d3,'observed_Harp3.csv',row.names = F)