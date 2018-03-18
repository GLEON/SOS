library(loadflex)
library(dplyr)

# DOC
harp = read.csv('../../HarpLake/StagingFiles/DOC_Sw.csv',stringsAsFactors = F)
head(harp)
harp$sampledate = as.POSIXct(strptime(harp$sampledate,'%m/%d/%Y'))
plot(harp$sampledate,harp$doc)

# Inflow
inflow = read.csv('../../HarpLake/StagingFiles/Harp_inflow.csv',stringsAsFactors = F)
head(inflow)
inflow$SDATE = as.POSIXct(strptime(inflow$SDATE,'%m/%d/%Y'))

startDate = as.Date('1991-01-01')
endDate = as.Date('2001-12-31')

which.site = 'HARP INFLOW #3'


library(tidyr)
winflow = spread(inflow,key = INFO,value = DISCHARGE)
icor = cor(winflow[,2:7])[3,] #correlation table with inflow #4

sites = unique(harp$site_name)
for (i in sites){
  useCor = icor[names(icor) == i]
  a = winflow[,names(winflow) == i]
  b = winflow[,names(winflow) == 'HARP INFLOW #4']
  indx = which(is.na(a) | a<=0)
  a[indx] = b[indx] * useCor
  winflow[,names(winflow) == i] = a
}
newinflow = gather(winflow,key = SDATE,value = DISCHARGE)
names(newinflow) = c('SDATE','INFO','DISCHARGE')

inflow.doc = function(lakename,which.site){
  # Site 3
  h3 = harp %>% dplyr::filter(site_name == which.site) %>%
    dplyr::select(Dates = sampledate,Solute = doc)
  
  i3 = newinflow %>% dplyr::filter(INFO == which.site) %>%
    dplyr::select(Dates = SDATE,Flow = DISCHARGE) %>%
    left_join(h3,'Dates') %>%
    dplyr::filter(!duplicated(Dates)) %>%
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
  # summary(getFittedModel(lr9))
  # qplot(x=Date, y=Resid, data=getResiduals(lr9, newdata=s3))
  # residDurbinWatson(lr9, "conc", newdata=s3, irreg=TRUE)
  # estimateRho(lr9, "conc", newdata=s3, irreg=TRUE)$rhoestimateRho(no3_reg2, "conc", newdata=intdat, irreg=TRUE)$rho
  getCorrectionFraction(lrC, "flux", newdat=s3)
  
  # Make predictions
  # ----------------------
  p.lr = predictSolute(lr9, "conc", d3, se.pred=TRUE, date=TRUE)
  p.lc = predictSolute(lrC, "conc", d3, se.pred=TRUE, date=TRUE)
  
  p.lr$Flow = d3$Flow
  p.lc$Flow = d3$Flow
  
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


#### Join Harp Inflows ####
library(dplyr)
lakename = 'Harp'
sites = unique(harp$site_name)
a = list(); b=list(); c=list()
for (i in 1:length(sites)) {
  
  a[[i]] = read.csv(paste0(lakename,'/loadestReg_',sites[i],'.csv'),stringsAsFactors = F)
  a[[i]]$date = as.Date(a[[i]]$date)
  b[[i]] = read.csv(paste0(lakename,'/loadestComp_',sites[i],'.csv'),stringsAsFactors = F)
  b[[i]]$date = as.Date(b[[i]]$date)
  c[[i]] = read.csv(paste0(lakename,'/observed_',sites[i],'.csv'),stringsAsFactors = F)
  c[[i]]$Dates = as.Date(c[[i]]$Dates)
  
  a[[i]]$Mass = a[[i]]$fit * a[[i]]$Flow
  b[[i]]$Mass = b[[i]]$fit * b[[i]]$Flow
  c[[i]]$Mass = c[[i]]$Solute * b[[i]]$Flow
}


# REGRESSION MODEL: total of inflows
d = lapply(a, `[`, 5)
df <- data.frame(matrix(unlist(d), nrow = nrow(a[[1]]), byrow=F))
mass = rowSums(df) #g/sec
totInflow = rowSums(winflow[,2:7])
p.lr = data.frame("date" = winflow$SDATE,"fit" = mass/totInflow,"se.pred" = NA)


# COMPOSITE MODEL: total of inflows
d = lapply(b, `[`, 5)
df <- data.frame(matrix(unlist(d), nrow = nrow(b[[1]]), byrow=F))
mass = rowSums(df) #g/sec
totInflow = rowSums(winflow[,2:7])
p.lc = data.frame("date" = winflow$SDATE,"fit" = mass/totInflow,"se.pred" = NA)


# OBSERVATIONS: total of inflows
d = lapply(c, `[`, 2)
df <- data.frame(matrix(unlist(d), nrow = nrow(c[[1]]), byrow=F))
totInflow = rowSums(df) #g/sec

e = lapply(c, `[`, 4)
df <- data.frame(matrix(unlist(e), nrow = nrow(c[[1]]), byrow=F))
mass = rowSums(df) #g/sec

d3 = data.frame("Dates" = c[[1]]$Dates,"Flow" = totInflow,"Solute" = mass/totInflow)


write.csv(p.lr,paste0(lakename,'/loadestReg.csv'),row.names = F)
write.csv(p.lc,paste0(lakename,'/loadestComp.csv'),row.names = F)
write.csv(d3,paste0(lakename,'/observed.csv'),row.names = F)
