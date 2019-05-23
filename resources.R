# *************************************************************** #
# Financial Volatility          : Group Project                   #
# Code File                     : Resources                       #
# *************************************************************** #

# This file contains useful functions that will be sourced by the main file.










# We define a functin that allows us to estimate a DCC and reports the results in a concise way. 
# This function will be used later in order to find the best model

fit_dcc_quick <- function(x = data, var_order = 10, uni_models = c(1,1,1), orders = list(c(1,1),c(1,1),c(1,1)), 
                           innovations = "norm", full_out = F){
  
  #this function takes a numeric code for the univariate models and uses the univariate models as input for the dcc. The numeric model code is 
  #accoridng to the following vector:
  models <- c("sGARCH","gjrGARCH")
  
  #specify model for the mean: 
  varfit = varxfit(X=x, p=var_order, exogen = NULL, robust = F, postpad = "constant")
  
  #convert numers to classes
  mod1 <- models[uni_models[1]];  mod2 <- models[uni_models[2]]; mod3 <- models[uni_models[3]]
  
  #specify univariate models
  unispec1 = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = 
                                            FALSE), variance.model = list(model = mod1, garchOrder = orders[[1]], submodel = "ALLGARCH"), 
                        distribution.model = innovations) 
  #specify univariate models
  unispec2 = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = 
                                            FALSE), variance.model = list(model = mod2, garchOrder = orders[[2]], submodel = "ALLGARCH"), 
                        distribution.model = innovations) 
  #specify univariate models
  unispec3 = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = 
                                            FALSE), variance.model = list(model = mod3, garchOrder = orders[[3]], submodel = "ALLGARCH"), 
                        distribution.model = innovations) 
  
  #plug models together in list in order of the series
  spec = dccspec(uspec = multispec( list(unispec1, unispec2 ,unispec3)), VAR = T, 
                 lag = 1, dccOrder = c(1,1), distribution = "mvnorm") 
  
  
  #fit the dcc model with the specified univar models and the var 10
  fit = dccfit(spec, data = x, fit.control = list(eval.se=TRUE), 
               VAR.fit = varfit) 
  
  #for model selection, we have thge option to only output the AIC
  if(full_out ==F)  return(infocriteria(fit)[1] %>% set_names(paste(mod1,mod2,mod3)))
  
  #for model fittig we can output the whole dccfit object
  if(full_out ==T)  return(fit)
}


#We define a function that will run a model selection on the desired data, evaluating a specified range of models.
select_dcc <- function(data = data, var_order = 10, 
                       uni_range = list(1:2,1:2,1:2), #which models should be tested? needs to have as many elements as data?
                       data_full = data, 
                       order_range = list(c(1,1),c(1,1),c(1,1)), #which orders should be tested? needs to have as many elements as data?
                       innovations = "norm", cluster = F, full_out = F){
  
  #fit the desired models
  evaluated <- apply(expand.grid(uni_range), 1, function(x) try(fit_dcc_quick(uni_models = x, full_out = full_out, 
                                                                      data_full = data_full, orders = order_range, innovations = innovations)))
  
  #choose the winner
  winner <- expand.grid(uni_range)[which(evaluated == min(evaluated)),]
  
  if(full_out == T) return(evaluated) #if benchmarking is desired, put out all models
  
  return(list("Chosen" = c(winner) %>% do.call(what = cbind), "AIC" = evaluated[which(evaluated == min(evaluated))]))
}


# From above we get a fit. This function extracts the specs of the chosen model. We need this spec to fit th GARCH model on the training set
extract_spec <- function(fit){
  gdpspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean =
                                           FALSE), variance.model = list(model = fit@model[["umodel"]][["modeldesc"]][["vmodel"]][1], garchOrder = c(1,1)), #fit@model[["umodel"]][["modeldesc"]][["vmodel"]] extracts GARCH type
                       distribution.model = "norm")
  
  #regular garch(1,1) for exports since we found no significant threshold
  expspec <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean =
                                            FALSE), variance.model = list(model = fit@model[["umodel"]][["modeldesc"]][["vmodel"]][2], garchOrder = c(1,1)),
                        distribution.model = "norm")
  
  #regular garch(1,1) for exports since we found no significant threshold
  consspec <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean =
                                             FALSE), variance.model = list(model = fit@model[["umodel"]][["modeldesc"]][["vmodel"]][3], garchOrder = c(1,1)),
                         distribution.model = "norm")
  
  #plug models together in list in order of the series: gdp,export, consumption <- gjr, garch, gjr
  spec = dccspec(uspec = multispec( list(gdpspec,expspec ,consspec)), VAR = T,
                 lag = 1, dccOrder = c(1,1), distribution = "mvnorm")
  return(spec)
}



conf_plot <- function(fit, horizon, level){   
  
  
  # Splitting into test and training samples
  train <-  head(data, -horizon)
  test. <-  tail(data, horizon)
  
  # Extracting Spec
  spec  <- extract_spec(fit)
  
  # Fitting VAR and DCC model to training dataset
  varfit_train <- varxfit(X=train, p=10, exogen = NULL, robust = F, postpad = "constant")
  dccfit_train <- dccfit(spec, data = train, fit.control = list(eval.se=TRUE), # Spec is used here
                         VAR.fit = varfit_train) 
  
  # Forecasting which serve as input for plots
  dccforecast_train <- dccforecast(dccfit_train, n.ahead = horizon)
  
  # Plot function
  plot_vol_forecast(dccforecast_train, level=level, test=test.)
  
}
plot_vol_forecast = function(x, series = c(1, 2, 3), level, data.=data, ...) {
  .divisortable = function(n)
  {
    z=matrix(c(1,1,1,
               2,2,1,
               3,2,2,
               4,2,2,
               5,2,3,
               6,2,3,
               7,2,4,
               8,2,4,
               9,3,3,
               10,3,4,
               11,3,4,
               12,3,4,
               13,4,4,
               14,4,4,
               15,4,4,
               16,4,4,
               17,4,5,
               18,4,5,
               19,4,5,
               20,4,5), ncol = 3, byrow = TRUE)
    d = which(n==z[,1])
    return(z[d,2:3])
  }
  ops = list(...)
  n = length( series )
  n.ahead = x@model$n.ahead
  n.roll	= x@model$n.roll
  CI.level= qnorm(1-level/2)                # Added
  test <-  tail(data, horizon)             # Added
  if( n.ahead == 1 && n.roll > 0 ){
    n.start = x@model$modeldata$n.start
    n.assets = dim(x@model$modeldata$data)[2]
    cnames = x@model$modeldata$asset.names
    T = x@model$modeldata$T
    xDat = x@model$modeldata$data[(T-49):(T+n.roll),]
    #nn = length(x@uforecast@forecast[[1]]@forecast$series)
    yDat = x@model$modeldata$index[(T-49):(T+n.roll)]
    indx = c(-49:0, 1:(n.roll))
    yforc = rbind(
      matrix(NA, ncol = n.assets, nrow = 50),
      t(fitted(x)[1,,]))
    CI = rbind(                              # CI variable Added
      matrix(NA, ncol = n.assets, nrow = 50),#
      t(sigma(x)[1,,]))                      #
    CI.up = yforc + CI.level*CI              # Upper band
    CI.low= yforc - CI.level*CI              # Lower band
    test.f = rbind(                          # test forecast included
      matrix(NA, ncol = n.assets, nrow = 50),#
      t(test[1,]))
    # discard last roll in case it is outside of realized observations
    yforc = yforc[1:(50+n.roll),]
    if( n > 16 ){
      scr = floor( n/16 )
      z	= n - 16 * floor( n/16 )
      start	= dev.next( which = dev.cur() )
      xn	= 0
      for( j in 1:scr ){
        dev.new( start + j )
        par( mfrow = c(4, 4) )
        for(i in 1:16){
          tmp = yforc[,series[i + xn]]
          CI.up.tmp  = CI.up[,series[i + xn]]           # Added
          CI.low.tmp = CI.low[,series[i + xn]]          # Added
          test.f.tmp = test.f[,series[i + xn]]          # Added
          plot( zoo(xDat[,series[i + xn]], yDat), col = colors()[16], main = cnames[i + xn],
                ylab = "Series", xlab = "Time")
          lines( zoo(tmp, yDat), col = colors()[134])
          lines( zoo(test.f.tmp, yDat), col=colors()[134], lty="longdash") # Added
          lines( zoo(CI.up.tmp, yDat), col="blue", lty="dashed") # Added
          lines( zoo(CI.low.tmp, yDat), col="blue", lty="dashed")# Added
          
          abline( v = yDat[51], lty = 3, col = "steelblue")
          mtext(paste("rmgarch  : DCC model forecast"), side = 4, adj = 0, padj=0, col = "gray", cex = 0.4)
          legend("topleft", c("Returns", "Forecast"), col=c(colors()[16], colors()[134]), lty=c(1,1),
                 bty="n", cex = 0.7)
          grid()
        }
        title( paste( "DCC Series Rolling Forecast\n(page...", j, ")", sep = ""), outer = TRUE, line = -1.5, cex = 0.75)
        xn = xn + 16
      }
      if( z != 0 ){
        dev.new( dev.next( which = dev.cur() ) + 1 )
        par( mfrow = c(4, 4) )
        for(i in 1:z){
          tmp = yforc[,series[i + xn]]
          CI.up.tmp  = CI.up[,series[i + xn]]           # Added
          CI.low.tmp = CI.low[,series[i + xn]]          # Added
          test.f.tmp = test.f[,series[i + xn]]          # Added
          plot( zoo(xDat[,series[i + xn]], yDat), col = colors()[16], main = cnames[i + xn],
                ylab = "Series", xlab = "Time")
          lines( zoo(tmp, yDat), col = colors()[134])
          lines( zoo(test.f.tmp, yDat), col=colors()[134], lty="longdash") # Added
          lines( zoo(CI.up.tmp, yDat), col="blue", lty="dashed") # Added
          lines( zoo(CI.low.tmp, yDat), col="blue", lty="dashed")# Added
          
          abline( v = yDat[51], lty = 3, col = "steelblue")
          mtext(paste("rmgarch  : DCC model forecast"), side = 4, adj = 0, padj=0, col = "gray", cex = 0.4)
          legend("topleft", c("Returns", "Forecast"), col=c(colors()[16], colors()[134]), lty=c(1,1),
                 bty="n", cex = 0.7)
          grid()
        }
        title( paste( "DCC Series Rolling Forecast\n(page...", j, ")", sep = ""), outer = TRUE, line = -1.5, cex = 0.75)
      }
    } else{
      d = .divisortable( n )
      par( mfrow= c(3, 1) )
      for(i in 1:n){
        tmp   = yforc[,series[i]]
        CI.up.tmp  = CI.up[,series[i]]           # Added
        CI.low.tmp = CI.low[,series[i]]          # Added
        test.f.tmp = test.f[,series[i]]          # Added
        plot( zoo(xDat[,series[i]], yDat),  col = colors()[16], main = cnames[i],
              ylab = "Series", xlab = "Time")
        lines( zoo(tmp, yDat), col = colors()[134])            # Added
        lines( zoo(test.f.tmp, yDat), col=colors()[16], lty="longdash") # Added
        lines( zoo(CI.up.tmp, yDat), col="blue", lty="dashed") # Added
        lines( zoo(CI.low.tmp, yDat), col="blue", lty="dashed")# Added
        
        abline( v = yDat[51], lty = 3, col = "steelblue")
        mtext(paste("rmgarch  : DCC model forecast"), side = 4, adj = 0, padj=0, col = "gray", cex = 0.4)
        legend("topleft", c("Returns", "Forecast"), col=c(colors()[16], colors()[134]), lty=c(1,1),
               bty="n", cex = 0.7)
        grid()
      }
      title( paste( "DCC Series Rolling Forecast", sep = ""), outer = TRUE, line = -1.5, cex = 0.75)
      
    }
  } else if( n.ahead > 1 && n.roll == 0 ){
    period   = x@model$modeldata$period
    n.start  = x@model$modeldata$n.start
    n.assets = NCOL(x@model$modeldata$data)
    cnames   = x@model$modeldata$asset.names
    T = x@model$modeldata$T
    # To provide for a realistic plot:
    # If some of n.ahead lies within the out.sample period (if at all),
    # then find those dates which it lies within.
    # Otherwise, and for all n.ahead > available data/dates, generate
    # the n.ahead dates using the 'seq' and periodicity of the data.
    if(n.start>0 && n.ahead<n.start){
      xDat = x@model$modeldata$data[(T-49):(T+n.ahead),]
      yDat = x@model$modeldata$index[(T-49):(T+n.ahead)]
    } else if(n.start>0 && n.ahead>n.start){
      xDat = rbind(
        x@model$modeldata$data[(T-49):(T+n.start),],
        matrix(NA, ncol = n.assets, nrow = n.ahead - n.start))
      yDat = c(
        x@model$modeldata$index[(T-49):(T+n.start)],
        seq(x@model$modeldata$index[T+n.start], by = period, length.out=n.ahead - n.start +1)[-1])
    } else{
      xDat = rbind(
        x@model$modeldata$data[(T-49):T,],
        matrix(NA, ncol = n.assets, nrow = n.ahead))
      yDat = c(
        x@model$modeldata$index[(T-49):(T)],
        seq(x@model$modeldata$index[T], by = period, length.out=n.ahead+1)[-1])
    }
    
    yforc = rbind(                            
      matrix(NA, ncol = n.assets, nrow = 50),
      x@mforecast$mu[,,1])
    CI = rbind(                              # CI variable Added
      matrix(NA, ncol = n.assets, nrow = 50),#
      sigma(x)[,,1])                         #
    CI.up = yforc + CI.level*CI              # Upper band
    CI.low = yforc - CI.level*CI             # Lower band
    test.f = rbind(                          # test forecast included
      matrix(NA, ncol = n.assets, nrow = 50),#
      test)
    if( n > 16 ){
      scr = floor( n/16 )
      z	= n - 16 * floor( n/16 )
      start	= dev.next( which = dev.cur() )
      xn	= 0
      for( j in 1:scr ){
        dev.new( start + j )
        par( mfrow = c(4, 4) )
        for(i in 1:16){
          tmp = yforc[,series[i + xn]]
          CI.up.tmp  = CI.up[,series[i + xn]]           # Added
          CI.low.tmp = CI.low[,series[i + xn]]          # Added
          test.f.tmp = test.f[,series[i + xn]]          # Added
          plot( zoo(xDat[,series[i + xn]], yDat), col = colors()[16], main = cnames[i + xn],
                ylab = "Series", xlab = "Time")
          lines( zoo(tmp, yDat), col = colors()[134])
          lines( zoo(test.f.tmp, yDat), col=colors()[134], lty="longdash") # Added
          lines( zoo(CI.up.tmp, yDat), col="blue", lty="dashed") # Added
          lines( zoo(CI.low.tmp, yDat), col="blue", lty="dashed")# Added
          
          abline( v = yDat[51], lty = 3, col = "steelblue")
          mtext(paste("rmgarch  : DCC model forecast"), side = 4, adj = 0, padj=0, col = "gray", cex = 0.4)
          legend("topleft", c("Returns", "Forecast"), col=c(colors()[16], colors()[134]), lty=c(1,1),
                 bty="n", cex = 0.7)
          grid()
        }
        title( paste( "DCC Series Unconditional Forecast\n(page...", j, ")", sep = ""), outer = TRUE, line = -2, cex = 0.75)
        xn = xn + 16
      }
      if( z != 0 ){
        dev.new( dev.next( which = dev.cur() ) + 1 )
        par( mfrow = c(4, 4) )
        for(i in 1:z){
          tmp = yforc[,series[i + xn]]
          CI.up.tmp  = CI.up[,series[i + xn]]           # Added
          CI.low.tmp = CI.low[,series[i + xn]]          # Added
          test.f.tmp = test.f[,series[i + xn]]          # Added
          plot( zoo(xDat[,series[i + xn]], yDat), col = colors()[16], main = cnames[i + xn],
                ylab = "Series", xlab = "Time")
          lines( zoo(tmp, yDat), col = colors()[134])
          lines( zoo(test.f.tmp, yDat), col=colors()[134], lty="longdash") # Added
          lines( zoo(CI.up.tmp, yDat), col="blue", lty="dashed") # Added
          lines( zoo(CI.low.tmp, yDat), col="blue", lty="dashed")# Added
          
          abline( v = yDat[51], lty = 3, col = "steelblue")
          mtext(paste("rmgarch  : DCC model forecast"), side = 4, adj = 0, padj=0, col = "gray", cex = 0.4)
          legend("topleft", c("Returns", "Forecast"), col=c(colors()[16], colors()[134]), lty=c(1,1),
                 bty="n", cex = 0.7)
          grid()
        }
        title( paste( "DCC Series Unconditional Forecast\n(page...", j, ")", sep = ""), outer = TRUE, line = -1.5, cex = 0.75)
      }
    } else{
      d = .divisortable( n )
      par( mfrow= c(3, 1) )
      for(i in 1:n){
        tmp   = yforc[,series[i]]
        CI.up.tmp  = CI.up[,series[i]]           # Added
        CI.low.tmp = CI.low[,series[i]]          # Added
        test.f.tmp = test.f[,series[i]]          # Added
        plot( zoo(xDat[,series[i]], yDat),  col = colors()[16], main = cnames[i],
              ylab = "Standardized Variable", xlab = "Time")
        lines( zoo(tmp, yDat), col = colors()[134])            # Added
        lines( zoo(test.f.tmp, yDat), col=colors()[16], lty="longdash") # Added
        lines( zoo(CI.up.tmp, yDat), col="blue", lty="dashed") # Added
        lines( zoo(CI.low.tmp, yDat), col="blue", lty="dashed")# Added
        
        abline( v = yDat[51], lty = 3, col = "steelblue")
        legend("topleft", c("Variable", "Forecast", "CI"), col=c(colors()[16], colors()[134], "blue"), lty=c(1,1,2),
               bty="n", cex = 0.7)
        grid()
      }
    }
  } else if( n.ahead > 1 && n.roll  > 0 ){
    cat("\nNo plot available for mixed unconditional and rolling forecasts.")
  }
  invisible()
}


# Plots our Z. Fits our GARCH on the training set to forecast
Z_forecast <- function (x, horizon, level){  # x is our DCCforecast object, horizon is the forecast horizon, and level is the CI.level
  
  
 
  # Splitting into test and training
  CI.level   <-  qnorm(1-level/2)  # Confidence level
  train      <-  head(data,-(horizon)) # Training sample
  test       <-  tail(data, (horizon)) # Test sample
  def.train  <-  head(def,   -(horizon)) # Training target
  def.test   <-  tail(def,    (horizon)) # Testing target
  
  spec <- extract_spec(fit)
  # Fitting VAR and DCC model to training dataset
  varfit_train <- varxfit(X=train, p=10, exogen = NULL, robust = F, postpad = "constant")
  fit_train <- dccfit(spec, data = train, fit.control = list(eval.se=TRUE), 
              VAR.fit = varfit_train) 
  
  # Forecasting which serve as input for plots
  x <- dccforecast(fit_train, n.ahead = horizon)
  yDat       <-  c(index(def.test))      # Getting time index vector
  yforc      <-  ts(x@mforecast$mu[,,1][1:horizon,], start = yDat[1], freq = 4) # VAR forecasts obtained in DCCforecast model
  colnames(yforc) <- c("GDP_-1", "Exports_-1", "Private Consumption_-1") # Label
  
  
  
  # Regression training sample
  indicator     <- dyn$lm(def.train~., data = train) # Run OLS
  indicator_fit <- fitted(indicator)                 # fitted values
  
  
  # Prediction on test
  pred <-predict(indicator, newdata = yforc)
  
  # Weighting the function with the coefficients of indicator
  coef <- coef(indicator)[2:4]/sum(coef(indicator)[2:4])            # Get coefficents (without constant)
  
  sigma_list <- rep(NA, horizon)                                    # Empty list to fill up with volatilities
  for (i in 1:(horizon)){
    sigma_t       <- t(coef) %*% x@mforecast$H[[1]][,,i] %*% coef   # According to equation ()
    sigma_list[i] <- sigma_t
  }
  
  # Confidence intervals
  CI_up  <- pred + CI.level*sigma_list          # According to equation ()
  CI_low <- pred - CI.level*sigma_list
  
  # Preparing objects for plotting
  na <- rep(NA, length(def.test))               # preparing NA vector for plot
  indicator_fit =ts(append(indicator_fit, na), end  = c(index(def.test))[nrow(def.test)], freq = 4) # add NA vector to indicator fit so that the plot doesn't cut too early
  
  # Plotting confidence intervals
  par(mfrow=c(1,1))
  plot( zoo(indicator_fit),  col = colors()[16],
        ylab = "Z", xlab = "Time")
  grid()
  lines( zoo(pred), col=colors()[134])
  lines( zoo(CI_up), col="blue", lty="dashed") # Added
  lines( zoo(CI_low), col="blue", lty="dashed") # Added
  abline( v = yDat[1], lty = 3, col = "steelblue")
  legend("topleft", c("Z-variable", "Forecast", "CI"), col=c(colors()[16], colors()[134], "blue"),  lty=c(1,1, 2),
         bty="n", cex = 0.7)
  
  title( paste("Z-variable with forecasts and CI" ), outer = TRUE, line = -1.5, cex = 0.75)
  
}



