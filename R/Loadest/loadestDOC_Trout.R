library(loadflex)

# DOC
trout = read.csv('../../TroutLake/Trout_inflow_DOC_noInterp.csv',stringsAsFactors = F)
head(trout)
trout$datetime = as.POSIXct(strptime(trout$datetime,'%Y-%m-%d'))
plot(trout$datetime,trout$SW_DOC)

# Inflow
inflow = read.csv('../../TroutLake/Trout_inflow_volume_noInterp.csv',stringsAsFactors = F)
head(inflow)
inflow$datetime = as.POSIXct(strptime(inflow$datetime,'%Y-%m-%d'))


inflow.doc = function(lakename){
  # Site 3
  h3 = trout %>% 
    dplyr::select(Dates = datetime,Solute = SW_DOC)
  
  i3 = inflow %>% 
    dplyr::select(Dates = datetime,Flow = FlowIn) %>%
    left_join(h3,'Dates') %>%
    dplyr::filter(!duplicated(Dates)) %>%
    dplyr::filter(!is.na(Flow)) %>%
    dplyr::filter(Flow > 0) %>%
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
  # summary(getFittedModel(lr9))
  # qplot(x=Date, y=Resid, data=getResiduals(lr9, newdata=s3))
  # residDurbinWatson(lr9, "conc", newdata=s3, irreg=TRUE)
  # estimateRho(lr9, "conc", newdata=s3, irreg=TRUE)$rhoestimateRho(no3_reg2, "conc", newdata=intdat, irreg=TRUE)$rho
  getCorrectionFraction(lrC, "flux", newdat=s3)
  
  # Make predictions
  # ----------------------
  p.lr = predictSolute(lr9, "conc", d3, se.pred=TRUE, date=TRUE)
  p.lc = predictSolute(lrC, "conc", d3, se.pred=TRUE, date=TRUE)
  
  
  # Plot
  png(paste0(lakename,'/fig_',which.site,'.png'),
      width = 7,height = 5,units = 'in',res = 300)
    par(mar=c(3,3,3,1),mgp=c(1.5,0.5,0))
    plot(d3$Dates,d3$Solute,type='o',col='red3',pch=16,
         xlab = 'Date',ylab = 'DOC (mg/L)',main = which.site)
    lines(p.lr$date,p.lr$fit,type='o',col='navy',pch=16,cex=0.2)
    lines(p.lc$date,p.lc$fit,type='o',col='seagreen',pch=16,cex=0.2)
    legend('topleft',fill = c('red3','navy','seagreen'),
           legend = c('Observed','Model - Reg','Model - Comp'),
           bty='n')
  dev.off()
  
  
  write.csv(p.lr,paste0(lakename,'/loadestReg_',which.site,'.csv'),row.names = F)
  write.csv(p.lc,paste0(lakename,'/loadestComp_',which.site,'.csv'),row.names = F)
  write.csv(d3,paste0(lakename,'/observed_',which.site,'.csv'),row.names = F)
}

sites = unique(harp$site_name)
for (i in 1:length(sites)){
  inflow.doc('Harp',sites[i])
}
