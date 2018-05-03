# function for fitting distributions to time series data
# Paul C. Hanson, July 2015

Fit.Distributions <- function(working.data,WhichModels,TestValue, Plot,VarName){
  # InputData is a data.frame with columns 'Index' and 'Response'
  # If InputData=NULL, then a test distribution is generated and analyzed
  
  # WhichModels, Models: 1=Means, 2=Linear, 3=Gaussian, 4=Gamma, 5=GEV, 6=Weibull, 7=logNormal
  # Test value is used in 1-p(TestValue|FittedModel)
  
  # Plot is boolean
  
  if (!is.null(working.data)){
    colnames(working.data) = c('Index','Response')
    # Plot the data
    if(Plot){plot(density(working.data$Response),lwd=3,main = paste('Fit.Dist...',VarName))}
   }
      
  library(likelihood)
  library(MASS)
  nModels = length(WhichModels)
  
  # initialize the output
  # Output = data.frame(matrix(,nModels,3))
  # names(Output) = c('Model','AIC_corr','R2')
  Output = list()
  
  # Model fitting constants
  maxIterations = 1000
  
mCounter = 0 # counter for models  
for (i in WhichModels){
  mCounter = mCounter + 1
    if (i==1){
      #************************************************** 
      ## MEANS MODEL
      ThisModel = 'Means'
      
      # If InputData is null, then generate this test data set
      if (is.null(working.data)){
        # Generate random data as a test
        x = 1:1000
        y = rnorm(1000, mean = 4.32, sd = 0.234)
        working.data = data.frame(x,y)
        names(working.data) = c('Index','Response')
        plot(density(working.data$Response),main = paste('Fit.Dist...',VarName))
      }
      
      means.model <- function(a) { a }
      par<-list(a = 0.5, sd = 1) 
      var <- list(X1 = "Response")
      par_lo<-list(a = -1000000,  sd = 0.00001)
      par_hi<-list(a = 1000000, sd = 1000000) 
      var$x<-"Response"
      var$mean<-"predicted" 
      var$log<-TRUE 
      myResults<-anneal(means.model,par,var,working.data,par_lo,par_hi,dnorm,"Response",hessian = T, max_iter=maxIterations, show_display = FALSE)
    }
  
    if (i==2){
      ## ******************************************************************************
      ## LINEAR MODEL
      ThisModel = 'Linear'
      
      # If InputData is null, then generate this test data set
      if (is.null(working.data)){
        # Generate random data as a test
        x = 1:1000
        y = rnorm(1000, mean = 4.32, sd = 0.234)
        y = x*0.05+y
        working.data = data.frame(x,y)
        names(working.data) = c('Index','Response')
        plot(density(working.data$Response),main = paste('Fit.Dist...',VarName))
      }
      
      linear.model <- function(a,b,X1) { a + b*X1 }
      par<-list(a = 0.5, b = 0, sd = 1) 
      var <- list(X1 ="Index")
      par_lo<-list(a = -1000000, b = -100000, sd = 0.000001)
      par_hi<-list(a = 1000000, b = 100000, sd = 1000000) 
      var$x<-"Response"
      var$mean<-"predicted" 
      var$log<-TRUE 
      ##  now call the annealing algorithm, choosing which model to use
      myResults<-anneal(linear.model,par,var,working.data,par_lo,par_hi,dnorm,"Response",hessian = T, max_iter=maxIterations, show_display = FALSE)
    }
    
    if (i==3){
      #########################################################
      #GAUSSIAN MODEL
      ThisModel = 'Gaussian'
      
      # If InputData is null, then generate this test data set
      if (is.null(working.data)){
        # Generate random data as a test
        x = 1:1000
        y = rnorm(1000, mean = 4.32, sd = 0.234)
        working.data = data.frame(x,y)
        names(working.data) = c('Index','Response')
        plot(density(working.data$Response),main = paste('Fit.Dist...',VarName))
      }
      
      myResults<-fitdistr(working.data$Response,"normal")
      myProb = 1 - pnorm(TestValue, mean = myResults$estimate[1], sd = myResults$estimate[2])
      x.Pred = rnorm(1000,mean=myResults$estimate[1],sd=myResults$estimate[2])
      if(Plot){lines(density(x.Pred),col=2,lwd=mCounter,main = paste('Fit.Dist...',VarName))}
    }

  if (i==4){
    #########################################################
    #GAMMA MODEL, using MASS fitdistr
    ThisModel = 'Gamma'
    
    # If InputData is null, then generate this test data set
    if (is.null(working.data)){
      # Gamma test data set
      y<-rgamma(1000,rate=2,shape=15) 
      x = 1:1000
      working.data = data.frame(x,y)
      names(working.data) = c('Index','Response')
      plot(density(working.data$Response),main = paste('Fit.Dist...',VarName))
      # End Gamma test data set
    }

    myResults<-fitdistr(working.data$Response,"gamma")
    myProb = 1 - pgamma(TestValue, shape = myResults$estimate[1], rate = myResults$estimate[2])
    # Now sample, given the new parameters
    x.Pred = rgamma(1000,shape=myResults$estimate[1], rate=myResults$estimate[2])
    if(Plot){lines(density(x.Pred),col=2,lwd=mCounter,main = paste('Fit.Dist...',VarName))}
  }
  
  
  if (i==5){
    #########################################################
    #GEV
    ThisModel = 'EVD'
    library(evd)
    
    # If InputData is null, then generate this test data set
    if (is.null(working.data)){
      # Gamma test data set
      y<-rgev(100, loc=1, scale=0.5, shape=0.8) 
      x = 1:100
      working.data = data.frame(x,y)
      names(working.data) = c('Index','Response')
      plot(density(working.data$Response),main = paste('Fit.Dist...',VarName))
      # End Gamma test data set
    }
    
    # Fit model
    myResults = fgev(working.data$Response,std.err = FALSE)
    myProb = 1 - pgamma(TestValue, shape = myResults$estimate[1], rate = myResults$estimate[2])
    # Now sample, given the new parameters
    x.Pred = rgev(100,loc=myResults$estimate[1], scale=myResults$estimate[2], shape=myResults$estimate[3])
    if(Plot){lines(density(x.Pred),col=2,lwd=mCounter,main = paste('Fit.Dist...',VarName))}

  }
  
  if (i==6){
    #########################################################
    #WEIBULL MODEL, using MASS fitdistr
    ThisModel = 'Weibull'
    
    # If InputData is null, then generate this test data set
    if (is.null(working.data)){
      # Gamma test data set
      y<-rweibull(1000,scale=1,shape=15) 
      x = 1:1000
      working.data = data.frame(x,y)
      names(working.data) = c('Index','Response')
      plot(density(working.data$Response),main = paste('Fit.Dist...',VarName))
      # End Gamma test data set
    }
    
    myResults<-fitdistr(working.data$Response,"weibull")
    myProb = 1 - pweibull(TestValue, shape = myResults$estimate[1], scale = myResults$estimate[2])
    # Now sample, given the new parameters
    x.Pred = rweibull(1000,shape=myResults$estimate[1], scale=myResults$estimate[2])
    if(Plot){lines(density(x.Pred),col=2,lwd=mCounter,main = paste('Fit.Dist...',VarName))}

  }
  
  if (i==7){
    #########################################################
    #LogNormal MODEL, using MASS fitdistr
    ThisModel = 'logNormal'
    library(stats)
    # If InputData is null, then generate this test data set
    if (is.null(working.data)){
      # Gamma test data set
      y<-rlnorm(1000, meanlog = 3, sdlog = 1) 
      x = 1:1000
      working.data = data.frame(x,y)
      names(working.data) = c('Index','Response')
      plot(density(working.data$Response),main = paste('Fit.Dist...',VarName))
      # End Gamma test data set
    }
    
    myResults<-fitdistr(working.data$Response,"lognormal")
    myProb = 1 - plnorm(TestValue, meanlog = myResults$estimate[1], sdlog = myResults$estimate[2])
    # Now sample, given the new parameters
    x.Pred = rlnorm(1000,meanlog=myResults$estimate[1], sdlog=myResults$estimate[2])
    if(Plot){lines(density(x.Pred),col=2,lwd=mCounter,main = paste('Fit.Dist...',VarName))}
    
  }
  Output[[mCounter]] = myResults
  Output[[mCounter]]$TestProb = myProb
  Output[[mCounter]]$ModelName = ThisModel
  Output[[mCounter]]$ModelAIC = AIC(myResults)
}

  return(Output)
  
}