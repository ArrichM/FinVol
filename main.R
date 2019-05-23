# *************************************************************** #
# Financial Volatility          : Group Project                   #
# Code File                     : Main                            #
# *************************************************************** #

rm(list = ls())
gc()






# ============================== Library Calls  ==============================

toload <- c("portes","rugarch","rmgarch","tseries","zoo","forecast","vars","MTS","dyn","magrittr","stargazer","psych","rstudioapi","stats","tsDyn","ggplot2")
toinstall <- toload[which(toload %in% installed.packages()[,1] == F)]
lapply(toinstall, install.packages, character.only = TRUE)
lapply(toload, require, character.only = TRUE)

## Working Directory Setting
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

source("resources.R")





# ============================== Set Parameters ==============================

# Confidence level for extrapolation
level <- 0.05

# Forecasting horizon for the finally fitted DCC model
horizon <- 5









# ============================== Import and Prepare Data  ==============================

#gdp data was loded from:
# https://data.oecd.org/gdp/quarterly-gdp.htm#indicator-chart

#consumption data was loaded form:
#https://fred.stlouisfed.org/series/NAEXKP02ATQ657S

#export data was laoded from:
#https://fred.stlouisfed.org/series/NAEXKP06ATQ661S

# Load scaled default series, the default series is obtained form an austrian bank
load("Data/adjusted_defaults_scaled.Rdata")
def <- tosave; rm(tosave)

#i Import and format GDP
gdp <- read.csv("Data/GDP_oecd.csv") %>% subset(FREQUENCY == "Q" & MEASURE == "PC_CHGPY")
gdp <- ts(gdp[,"Value"], start = c(1961,1), freq = 4)

# Import and format private consumption
cons <- read.csv("Data/CONS_fred.csv")
cons <- ts(cons[,2], start = c(1989,2), freq = 4)

# Import and format export
exp <- read.csv("Data/EXP_fred.csv")
exp <- ts(exp[,2], start = c(1989,1), freq = 4)

# Merge and cut data
data <- cbind(gdp,exp,cons) %>% window(start = c(1990,3), end = c(2018,3))

# Load and prepare data
data <- cbind(stats::lag(data[,"gdp"],-1), stats::lag(data[,"exp"],-1), stats::lag(data[,"cons"],-1)) # Combine
colnames(data) <- c("GDP_-1", "Exports_-1", "Private Consumption_-1") # Label
data <- scale(data)        #------#
nrow(data) #number of observations










# ============================== Summary Statistics and Testing  ==============================

# Test for Stationarity individually each series
lapply(data, adf.test) # the data is partly non-stationary

# Test for Autocorrelation
LjungBox(data) # there is autocorrelation in the data

# Test for Cointegration
ca.jo(data, type = "trace") %>% summary #wthere is cointegration in t he model - one could prncipally fit a VECM












# ============================== VAR Model for the Mean  ==============================

#select lags for VAR
VAR <- VARselect(data) #we select lag 10 accrding to AIC, HQ and FPE info criteria
min(VAR$criteria)

#first we fit a VAR model to the data and use it later in the dcc mdoelling to demean the series
vfit = varxfit(X=data, p=10, exogen = NULL, robust = F, postpad = "none") ##----

#extract residuals from var
res_var <- vfit$xresiduals

#testing the residuals shows that the model captures the autoregression well
LjungBox(res_var)

autoplot(res_var) + facet_wrap( ~ series, nrow = 3, scale = "free")  +  theme(legend.position = "none")

#we check for ARCH effects in the squared residuals:
apply(res_var^2, 2, Box.test) #some significantautocorrelartion in the squares


# Significant dependencies in the cross correlations / covariances-. This is the indication to fit a DSS 
# model in order to use all information availble in the data when constructiong conditional variance estimates.










# ============================== DCC Model selection ==============================

# For this task, we first choose a set of univariate models that shows the lowest AIC
# under a set of considered models. To this end, we use some functions that we stored in the resources.R file.

# Run selection algorithm
selected_model <- select_dcc(var_order = 10, 
           uni_range = list(1:2,1:2,1:2), #which models should be tested? needs to have as many elements as data?
           order_range = list(c(1,1),c(1,1),c(1,1)), #which orders should be tested? Will be used for estimation, not object of optimasation
           innovations = "norm", cluster = F, full_out = F)

# We use the selected model choice as direct input into the fitting function we use to fit our dcc. 

# Do not worry about the errors, they are normal.
fit <- fit_dcc_quick(uni_models = selected_model$Chosen, full_out = T)












# ============================== DCC Residual Analysis ==============================

# First, we extract the standardized residuals form the chosen model:
res <- fit@mfit$stdresid

# We have a quick look at the standardized residuals
autoplot(ts(res)) + facet_wrap( ~ series, nrow = 3, scale = "free")  +  theme(legend.position = "none")

# A QQ Plot revauls a pretty good fitting. Normal residuals might have been the right choice
qqline(res, distribution =qnorm)

# We jointly test the residuals for autocorrelation
LjungBox(res, squared.residuals = F) # no autocorrealtion in the first moment
LjungBox(res, squared.residuals = T) # no autororrelation in the squares, DCC seems to fit well

# We have a look at the autocorrelations and cross correlations
acf(res)# It seems that the DCC captured the information flow in the covarinaces well. No significant cross correlation


apply(res, 2, Box.test)











# ============================== Plotting in and out sampple ==============================

# Plots the forecasts for the macroeconomic variables with CIs. Horizon and confidence level can be choses accordingly

# png(file = "Z-Variable.png", width = 9000, height = 4000, units = "px", res = 800) # width and height need to be adjusted properly
Z_forecast(fit, horizon = 4, level = 0.05)
# dev.off()

# Plots the forecasts for the indicator Z. Horizon and confidence level can be chosen accordingly

# png(file = "Macro_Forecasts.png", width = 9000, height = 12000, units = "px", res = 1200) # width and height need to be adjusted properly
conf_plot(fit, horizon = 4, level = 0.05)
# dev.off()



