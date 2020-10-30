# Install the necessary packages:

install.packages("urca")
install.packages("vars")
install.packages("car")
install.packages("dynlm")
install.packages("tsDyn")
install.packages("gets")
install.packages("zoo")
install.packages("pdfetch")
install.packages("nlme")
install.packages("tseries")
install.packages("forecast", dependencies = TRUE)
install.packages("dynamac")
install.packages("aod")
install.packages("readxl")


# Load the necessary packages:

library(dynamac)           # For ARDL estimation
library(forecast)		       # For ARIMA model estimation
library(tseries)	    	   # for time series models and diagnostic checks
library(nlme)              # to estimate ARIMA models
library(pdfetch)           # to fetch data directly from online data bases
library(zoo)		           # for the zoo function for daily time series
library(urca)         	   # for unit root tests
library(vars)	    	       # for Granger tests and VAR estimation
library(car)          	   # for regression diagnostics and hypothesis testing
library(dynlm)        	   # for Vector Error Correction Model(VECM)
library(tsDyn)             # for linear and non-linear VAR and VECM models
library(gets)              # for Isat function: step and impsulse indicator saturation
library(readxl)            # to read Excel files and load the data from Excel files
library(aod)               # for Wald tests


# ----------------------------------------------------------------------------------------------------
# Load the data from the CSV files (on the local computer)
# ----------------------------------------------------------------------------------------------------

# Clear the data buffer:
rm(list=ls())

# Load the data from the CSV files in the working directory:
# Transform each series into a time series object:
# For annual, quartely, and monthly data we use the ts() function
# For daily data we use the zoo() function from package 'zoo'
# Monthly data from FRED St Louis



# USA Unemployment Rate - Monthly (January 1948 to February 2020):
US_Unemployment_Rate <- read.csv("UNRATE.csv", header=TRUE)
attach(US_Unemployment_Rate)
US_Unemployment_Rate = ts(US_Unemployment_Rate, start=c(1948, 1), frequency=12)



# USA Non-Farm Job Openings - Monthly (December 2000 to December 2019):
US_Job_Openings  <- read.csv("JTSJOL.csv", header=TRUE)
attach(US_Job_Openings)
US_Job_Openings = ts(US_Job_Openings, start=c(2000, 12), frequency=12)




# ----------------------------------------------------------------------------------------------------
# Visualize the data
# ----------------------------------------------------------------------------------------------------

# Plot the data in levels:
par(mfrow=c(2,1))

plot(US_Unemployment_Rate)
plot(US_Job_Openings)


# Plot the data in levels, together, using a data frame:
# Bind time series which have a common frequency
# ts.intersect() restricts to the time covered by all the series:


data_frame = na.omit(ts.intersect(
  
  US_Unemployment_Rate,
  US_Job_Openings, 
  
  dframe=TRUE))


data_frame_timeseries = ts(data_frame, start=c(2000, 12), frequency=12)

data_frame_timeseries

plot(data_frame_timeseries, plot.type="single", col = 1:ncol(data_frame_timeseries))
legend("topright", colnames(data_frame_timeseries), col=1:ncol(data_frame_timeseries), lty=1, cex=.65)

# Plot the data in levels, together:

plot(US_Unemployment_Rate, 
     main="US Unemployment Rate and US Non Farm Job Openings 2000-2019",
     ylab="%",
     xlab=" ",
     col="blue",
     lwd=2)

lines(US_Job_Openings,
      lwd=2, 
      col="red")


legend("topright", inset=.02,
       legend=c("UK Unemployment Rate","US Non Farm Job Openings"),
       col=c("blue", "red"), 
       lty=1, 
       cex=0.8,
       title="Monthly data from FRED", 
       text.font=2, 
       bg='lightblue')

# Create the series in first differences:
Diff_US_Unemployment_Rate    = diff(US_Unemployment_Rate)
Diff_US_Job_Openings      = diff(US_Job_Openings)

# Plot the data in first differences:


par(mfrow=c(2,1))

plot(Diff_US_Unemployment_Rate)
plot(Diff_US_Job_Openings)

# ----------------------------------------------------------------------------------------------------
# Unit Root Tests - For US_Unemployment_Rate
# ----------------------------------------------------------------------------------------------------

# Checking for non-stationarity 
# Plot of the lag correlations (levels):
lag.plot(US_Unemployment_Rate, 6, do.lines=FALSE)       

# Plot of the lag correlations (first diference):
lag.plot(diff(US_Unemployment_Rate), 6, do.lines=FALSE)   

# Auto-correlation functions(ACF) and Partial auto-correlation functions(PACF).
# The dashed lines indicate the bounds for statistical significance at the 10% level.
par(mfrow=c(2,2))
acf     (US_Unemployment_Rate,          main="ACF for US Unemployment Rate (levels)")
pacf    (US_Unemployment_Rate,          main="PACF for US Unemployment Rate (levels)")
acf     (diff(US_Unemployment_Rate),    main="ACF for US Unemployment Rate (first difference)")
pacf    (diff(US_Unemployment_Rate),    main="PACF for US Unemployment Rate (first difference)")

# Unit Root Tests: ADF, PP, KPSS
# Using package URCA.

# Augmented Dickey-Fuller(ADF)tests on the chosen series:
# Using BIC to determine the number of lags
# ADF:  Ho = Residuals have a unit root = Non-stationary
US_Unemployment_Rate.adf.none <- ur.df(US_Unemployment_Rate, selectlags="BIC", type="none")
print(summary(US_Unemployment_Rate.adf.none))
plot(US_Unemployment_Rate.adf.none)

# Philips-Perron(PP) tests on the chosen series:
# PP:   Ho = Residuals have a unit root = Non-stationary
US_Unemployment_Rate.pp <- ur.pp(US_Unemployment_Rate, type="Z-tau", model="constant", lags="short")
print(summary(US_Unemployment_Rate.pp))
plot(US_Unemployment_Rate.pp)

# KPSS tests on the chosen series:
# Deterministic components: constant "mu"; or a constant with linear trend "tau".
# KPSS:   Ho = Residuals do not have a unit root = Stationary [opposite of ADF and PP]
US_Unemployment_Rate.kpss <- ur.kpss(US_Unemployment_Rate, type="mu", lags="short")
print(summary(US_Unemployment_Rate.kpss))
plot(US_Unemployment_Rate.kpss)

US_Unemployment_Rate.kpss <- ur.kpss(US_Unemployment_Rate, type="mu", lags="long")
print(summary(US_Unemployment_Rate.kpss))
plot(US_Unemployment_Rate.kpss)

# ----------------------------------------------------------------------------------------------------
# Unit Root Tests - For US_Job_Openings
# ----------------------------------------------------------------------------------------------------


# Checking for non-stationarity 
# Plot of the lag correlations (levels):
lag.plot(US_Job_Openings, 6, do.lines=FALSE)       

# Plot of the lag correlations (first diference):
lag.plot(diff(US_Job_Openings), 6, do.lines=FALSE)   

# Auto-correlation functions(ACF) and Partial auto-correlation functions(PACF).
# The dashed lines indicate the bounds for statistical significance at the 10% level.
par(mfrow=c(2,2))
acf     (US_Job_Openings,          main="ACF for US Job Openings (levels)")
pacf    (US_Job_Openings,          main="PACF for US Job Openings (levels)")
acf     (diff(US_Job_Openings),    main="ACF for US Job Openings (first difference)")
pacf    (diff(US_Job_Openings),    main="PACF for US Job Openings (first difference)")

# Unit Root Tests: ADF, PP, KPSS
# Using package URCA.

# Augmented Dickey-Fuller(ADF)tests on the chosen series:
# Using BIC to determine the number of lags
# ADF:  Ho = Residuals have a unit root = Non-stationary
US_Job_Openings.adf.none <- ur.df(US_Job_Openings, selectlags="BIC", type="none")
print(summary(US_Job_Openings.adf.none))
plot(US_Job_Openings.adf.none)

# Philips-Perron(PP) tests on the chosen series:
# PP:   Ho = Residuals have a unit root = Non-stationary
US_Job_Openings.pp <- ur.pp(US_Job_Openings, type="Z-tau", model="constant", lags="short")
print(summary(US_Job_Openings.pp))
plot(US_Job_Openings.pp)

# KPSS tests on the chosen series:
# Deterministic components: constant "mu"; or a constant with linear trend "tau".
# KPSS:   Ho = Residuals do not have a unit root = Stationary [opposite of ADF and PP]
US_Job_Openings.kpss <- ur.kpss(US_Job_Openings, type="mu", lags="short")
print(summary(US_Job_Openings.kpss))
plot(US_Job_Openings.kpss)

US_Job_Openings.kpss <- ur.kpss(US_Job_Openings, type="mu", lags="long")
print(summary(US_Job_Openings.kpss))
plot(US_Job_Openings.kpss)

#----------------------------------------------------------------------------------------------------
# ADF, PP & KPSS in First Differences 
#----------------------------------------------------------------------------------------------------

# Unit root tests for US Unemployment Rate in first differences 
Diff_US_Unemployment_Rate.adf.none <- ur.df(diff(US_Unemployment_Rate), selectlags="BIC", type="none")
print(summary(Diff_US_Unemployment_Rate.adf.none))

Diff_US_Unemployment_Rate.pp <- ur.pp(diff(US_Unemployment_Rate), type="Z-tau", model="constant", lags="short")
print(summary(Diff_US_Unemployment_Rate.pp))

Diff_US_Unemployment_Rate.kpss <- ur.kpss(diff(US_Unemployment_Rate), type="mu", lags="short")
print(summary(Diff_US_Unemployment_Rate.kpss))

Diff_US_Unemployment_Rate.kpss <- ur.kpss(diff(US_Unemployment_Rate), type="mu", lags="long")
print(summary(Diff_US_Unemployment_Rate.kpss))

# Unit root tests for US Job Openings in first differences 
Diff_US_Job_Openings.adf.none <- ur.df(diff(US_Job_Openings), selectlags="BIC", type="none")
print(summary(Diff_US_Job_Openings.adf.none))

Diff_US_Job_Openings.pp <- ur.pp(diff(US_Job_Openings), type="Z-tau", model="constant", lags="short")
print(summary(Diff_US_Job_Openings.pp))

Diff_US_Job_Openings.kpss <- ur.kpss(diff(US_Job_Openings), type="mu", lags="short")
print(summary(Diff_US_Job_Openings.kpss))

Diff_US_Job_Openings.kpss <- ur.kpss(diff(US_Job_Openings), type="mu", lags="long")
print(summary(Diff_US_Job_Openings.kpss))

#----------------------------------------------------------------------------------------------------
# Let's shorten the names
#----------------------------------------------------------------------------------------------------

# USA Unemployment Rate - Monthly (January 1948 to February 2020):
UNRATE <- read.csv("UNRATE.csv", header=TRUE)
attach(UNRATE)
UNRATE = ts(UNRATE, start=c(1948, 1), frequency=12)
UNRATE = window(UNRATE, start = c(2000, 12))


# USA Non-Farm Job Openings - Monthly (December 2000 to December 2019):
JTSJOL <- read.csv("JTSJOL.csv", header=TRUE)
attach(JTSJOL)
JTSJOL = ts(JTSJOL, start=c(2000, 12), frequency=12)

# ----------------------------------------------------------------------------------------------------
# Create the Data Frame 
# ----------------------------------------------------------------------------------------------------

# Create an unordered dataset with 2 variables:
# ts.intersect() to use only the intersection of the series


data.set = ts.intersect(
  
  UNRATE,
  JTSJOL, 
  
  dframe=TRUE)

data.set = ts(data.set, start=c(2000, 12), frequency=12)

# ----------------------------------------------------------------------------------------------------
# Optimal Lag Selection
# ----------------------------------------------------------------------------------------------------

VARselect(data.set, lag.max=5, type="none",  season = NULL, exogen = NULL)$selection
VARselect(data.set, lag.max=5, type="const", season = NULL, exogen = NULL)$selection	  
VARselect(data.set, lag.max=5, type="trend", season = NULL, exogen = NULL)$selection	  
VARselect(data.set, lag.max=5, type="both",  season = NULL, exogen = NULL)$selection	  	             

VARselect(data.set, lag.max=10, type="none",  season = NULL, exogen = NULL)$selection
VARselect(data.set, lag.max=10, type="const", season = NULL, exogen = NULL)$selection	  
VARselect(data.set, lag.max=10, type="trend", season = NULL, exogen = NULL)$selection	  
VARselect(data.set, lag.max=10, type="both",  season = NULL, exogen = NULL)$selection	  	             

VARselect(data.set, lag.max=20, type="none",  season = NULL, exogen = NULL)$selection
VARselect(data.set, lag.max=20, type="const", season = NULL, exogen = NULL)$selection	  
VARselect(data.set, lag.max=20, type="trend", season = NULL, exogen = NULL)$selection	  
VARselect(data.set, lag.max=20, type="both",  season = NULL, exogen = NULL)$selection	  	             

VARselect(data.set, lag.max=50, type="none",  season = NULL, exogen = NULL)$selection
VARselect(data.set, lag.max=50, type="const", season = NULL, exogen = NULL)$selection	  
VARselect(data.set, lag.max=50, type="trend", season = NULL, exogen = NULL)$selection	  
VARselect(data.set, lag.max=50, type="both",  season = NULL, exogen = NULL)$selection	

optimal.lags = 3

# ----------------------------------------------------------------------------------------------------
# Cointegration Analysis
# ----------------------------------------------------------------------------------------------------

# Johansen cointegration test using the data set with 2 variables (x and y) from the previous section 
# Using package "urca" and function 'ca.jo'

# Linear trend outside of the cointegrating vector; 
# No constant and no trend in the cointegrating vector
johansen.none = ca.jo(data.set, type="eigen", ecdet="none",  K = optimal.lags, spec="longrun")
summary(johansen.none)

johansen.none = ca.jo(data.set, type="trace", ecdet="none",  K = optimal.lags, spec="longrun")
summary(johansen.none)

plot(johansen.none)

plotres(johansen.none)


# With constant in the cointegrating vector:
johansen.const = ca.jo(data.set, type="eigen", ecdet="const",  K = optimal.lags, spec="longrun")
summary(johansen.const)

johansen.const = ca.jo(data.set, type="trace", ecdet="const",  K = optimal.lags, spec="longrun")
summary(johansen.const)

plot(johansen.const)

plotres(johansen.const)


# With trend in the cointegrating vector:
johansen.trend = ca.jo(data.set, type="eigen", ecdet="trend",  K = optimal.lags, spec="longrun")
summary(johansen.trend)

johansen.trend = ca.jo(data.set, type="trace", ecdet="trend",  K = optimal.lags, spec="longrun")
summary(johansen.trend)

plot(johansen.trend)

plotres(johansen.trend)


# ----------------------------------------------------------------------------------------------------
# ARDL
# ----------------------------------------------------------------------------------------------------

ARDL = dynardl(
  
  JTSJOL ~ UNRATE, 
  
  data      = data.set,
  
  levels    = c("UNRATE"),
  lags      = list("JTSJOL" = 1, "UNRATE" = 1),
  diffs     = c("UNRATE"),
  lagdiffs  = list("JTSJOL" = 1, "UNRATE" = 1),
  
  ec        = FALSE,
  constant  = FALSE,
  trend     = FALSE,
  simulate  = FALSE
)

summary(ARDL)
dynardl.auto.correlated(ARDL) 

# ----------------------------------------------------------------------------------------------------
# ARDL - Making the Residuals white noise
# ----------------------------------------------------------------------------------------------------

ARDL = dynardl(
  
  JTSJOL ~ UNRATE, 
  
  data      = data.set,
  
  levels    = c("UNRATE"),
  lags      = list("JTSJOL" = c(1:3), "UNRATE" = c(1:3)),
  diffs     = c("UNRATE"),
  lagdiffs  = list("JTSJOL" = 1, "UNRATE" = 1),
  
  ec        = FALSE,
  constant  = FALSE,
  trend     = FALSE,
  simulate  = FALSE
)

summary(ARDL)
dynardl.auto.correlated(ARDL) 

# ----------------------------------------------------------------------------------------------------
# ARDL - Cointergration Testing
# ----------------------------------------------------------------------------------------------------

# first model  = no   lagged first-differences of the endogenous variable
ARDL = dynardl(
  
  JTSJOL ~ UNRATE, 
  
  data     = data.set, 
  
  lags     = list("JTSJOL" = 1, "UNRATE" = 1),
  diffs    = c("UNRATE"), 
  
  ec       = TRUE, 
  simulate = FALSE
)
 summary(ARDL)
 dynardl.auto.correlated(ARDL) 
 pssbounds(ARDL)
 
 
 ARDL = dynardl(
   
   JTSJOL ~ UNRATE, 
   
   data     = data.set, 
   
   lags     = list("JTSJOL" = 1, "UNRATE" = 1),
   diffs    = c("UNRATE"), 
   lagdiffs = list("JTSJOL" = 1),
   
   ec       = TRUE, 
   simulate = FALSE
 )
 summary(ARDL)
 dynardl.auto.correlated(ARDL) 
 pssbounds(ARDL)
 
 # ----------------------------------------------------------------------------------------------------
 # ARDL - Impulse Response Functions
 # ----------------------------------------------------------------------------------------------------
 
   set.seed(123456789)
 
 ARDL = dynardl(
   
   JTSJOL ~ UNRATE, 
   
   data     = data.set, 
   
   lags     = list("JTSJOL" = 1, "UNRATE" = 1),
   diffs     = c("UNRATE"), 
   lagdiffs  = list("JTSJOL" = 1),
   
   ec       = FALSE, 
   
   simulate = TRUE,
   shockvar = "UNRATE",
   range    = 30,
   fullsims = TRUE
 )
 
 summary(ARDL)
 
 # Impulse-Response plots:
 
 dynardl.simulation.plot(ARDL, type = "area", response = "levels")	      	 # in colors
 dynardl.simulation.plot(ARDL, type = "area", response = "levels", bw = TRUE)  # in black and white
 
 par(mfrow = c(2, 3))
 dynardl.simulation.plot(ARDL, type = "area", response = "levels")
 dynardl.simulation.plot(ARDL, type = "area", response = "levels.from.mean")
 dynardl.simulation.plot(ARDL, type = "area", response = "diffs")
 dynardl.simulation.plot(ARDL, type = "area", response = "shock.effect.decay")
 dynardl.simulation.plot(ARDL, type = "area", response = "cumulative.diffs", axes = F)
 dynardl.simulation.plot(ARDL, type = "area", response = "cumulative.abs.diffs")
 
 dynardl.all.plots(ARDL)
 
 # ----------------------------------------------------------------------------------------------------
 # Diagnostics Tests
 # ----------------------------------------------------------------------------------------------------
 
 #ARDL Models using "dynlm"
 
 ARDL = dynlm( 
   
   d(JTSJOL, 1) ~ 
     L(UNRATE, 0:3)              	                	
   
  
   ,data=data.set)
 
 summary(ARDL)
 
 # HAC standard errors
 # The HAC standard errors are larger than those from OLS
 # If we ignore the autocorrelation, we will overstate the reliability of the OLS estimates.
 
 coeftest(ARDL, vcov=vcovHAC(ARDL))
 
 # ARDL Model Residuals:

 residuals = ARDL$residuals
 
 plot(residuals)
 
 acf(residuals)
 
 pacf(residuals)
 
 # ----------------------------------------------------------------------------------------------------
 # Bivariate VAR(p) Model
 # ----------------------------------------------------------------------------------------------------
 
 # Transforming into first differneces:
 UNRATE =  diff(UNRATE)
 JTSJOL =  diff(JTSJOL)
 
 VARselect(data.set, lag.max=50, type="none",  season = NULL, exogen = NULL)$selection
 optimal.lags = 3
 
 data.set = ts.intersect(
   
   UNRATE,
   JTSJOL, 
   
   dframe=TRUE)
 
 data.set = ts(data.set, start=c(2001, 1), frequency=12)
 
 
 # Correlation between the two variables:
 cor(data.set)   
 
 # Reduced-form VAR(p) with p = optimal.lags:
 var.model.none  <- VAR(data.set,  p=optimal.lags, type="none",  exogen=NULL)
 summary(var.model.none)
 plot(var.model.none)
 
 # Check the contemporaneus correlation matrix of VAR residuals
 # Contemporaneous effects across edogenous variables operate through the residual correlations
 # If correlations are significantly different from zero then the forecast error of the endogenous
 # variables are correlated
 
 # When there are several lags in a VAR, using the IRF to analyze dynamic interactions is more informative
 # than reporting the coefficient estimates. 
 
 
 # ----------------------------------------------------------------------------------------------------
 # Granger- and Instantaneous causality test for the VAR model
 # ----------------------------------------------------------------------------------------------------
 
 # Test of forecast improvement:  lags of one variable improve forecats on another variable
 # Require stationary variables
 # F test of the joint Ho = all lags of a partirclar variable have zero coefficients
 # Granger-causality means that Ho is rejected
 
 # Two causality tests are implemented. The first is a F-type Granger-causality test and the second is
 # a Wald-type test that is characterized by testing for nonzero correlation between the error processes
 # of the cause and effect variables.
 
 # The Granger-causality test is problematic if some of the variables are nonstationary. In that case the
 # usual asymptotic distribution of the test statistic may not be valid under the null hypothesis.
 
 # Not using a robust heteroskedasticity variance-covariance matrix for the Granger test:
 
 # UNRATE --> JTSJOL
 causality(var.model.none,  cause="UNRATE", boot=FALSE, boot.runs=1000)
 
 # JTSJOL --> UNRATE
 causality(var.model.none,  cause="JTSJOL", boot=FALSE, boot.runs=1000)
 
 
 # Now run the Granger tests using the HC covar matrix
 # Using a robust heteroskedasticity variance-covariance matrix for the Granger test:
 
 # UNRATE --> JTSJOL
 causality(var.model.none,  cause="UNRATE", boot=FALSE, boot.runs=1000, vcov.=vcovHC(var.model.none))
 
 # JTSJOL --> UNRATE
 causality(var.model.none,  cause="JTSJOL", boot=FALSE, boot.runs=1000, vcov.=vcovHC(var.model.none))
 
 
 
 # ----------------------------------------------------------------------------------------------------
 # Recursive VAR: Order the variables
 # ----------------------------------------------------------------------------------------------------
 
 # Cholesky decompositions for orthogonal errors:
 
 # Ordering: UNRATE --> JTSJOL
 ordered.data.set = data.set[, c("UNRATE","JTSJOL")]
 
 # Estimate the ordered VARs:
 var.ordered.none   <- VAR(ordered.data.set,  p=optimal.lags,   type="none",    exogen=NULL)
 
 # Forecast error variance decomposition (in percentage terms) using ordered VAR
 
 # Based upon the orthogonalised impulse response coefficient matrices
 # Contribution of a variable to the n-step forecast error variance of another variable
 # The orthogonalised impulse reponses are divided by the variance of the forecast error, 
 # the resultant is a percentage figure
 
 # The FFEVD reports how the behavior of a variable is affected by itself and by other variables in the system.
 # It measures the relative weights of each structural shock on the total variances of each endogenous variable.
 # Reports how variations in one variable are explained by shocks from other variables.
 
 plot(fevd(var.ordered.none,  n.ahead=20), main=c("UNRATE","JTSJOL"), xlab="", ylab="", sub="", oma=c(0,0,0,0), legend=FALSE)
 
 
 # ----------------------------------------------------------------------------------------------------
 # Impulse response functions:
 # ----------------------------------------------------------------------------------------------------
 
 # Using ordered VAR
 #Short run (non-cumulative):
 plot(irf(var.ordered.none,  n.ahead=20, ortho=TRUE, cumulative=FALSE, boot=TRUE, ci=0.90, runs=100))
 
 # Ordering: JTSJOL --> UNRATE
 ordered.data.set = data.set[, c("JTSJOL","UNRATE")]
 var.ordered.none   <- VAR(ordered.data.set,  p=optimal.lags,   type="none",    exogen=NULL)
 plot(fevd(var.ordered.none,  n.ahead=20), main=c("JTSJOL","UNRATE"), xlab="", ylab="", sub="", oma=c(0,0,0,0), legend=FALSE)
 plot(irf(var.ordered.none,  n.ahead=20, ortho=TRUE, cumulative=FALSE, boot=TRUE, ci=0.90, runs=100))
 
 
 plot(irf(var.ordered.none,  impulse="UNRATE", response="JTSJOL", n.ahead=20, ortho=TRUE, cumulative=FALSE, boot=TRUE, ci=0.90, runs=100, seed=NULL),
      main="UNRATE to JTSJOL", xlab="Lag", ylab="", sub="", oma=c(3,0,3,0))
 
  plot(irf(var.ordered.none,  impulse="JTSJOL", response="UNRATE", n.ahead=20, ortho=TRUE, cumulative=FALSE, boot=TRUE, ci=0.90, runs=100, seed=NULL),
      main="JTSJOL to UNRATE", xlab="Lag", ylab="", sub="", oma=c(3,0,3,0))
  
 #Long run (cumulative):
 plot(irf(var.ordered.none,  n.ahead=20, ortho=TRUE, cumulative=TRUE, boot=TRUE, ci=0.90, runs=100))
 
 plot(irf(var.ordered.none,  impulse="UNRATE", response="JTSJOL", n.ahead=20, ortho=TRUE, cumulative=TRUE, boot=TRUE, ci=0.90, runs=100, seed=NULL),
      main="UNRATE to JTSJOL", xlab="Lag", ylab="", sub="", oma=c(3,0,3,0))
 
 plot(irf(var.ordered.none,  impulse="JTSJOL", response="UNRATE", n.ahead=20, ortho=TRUE, cumulative=TRUE, boot=TRUE, ci=0.90, runs=100, seed=NULL),
      main="JTSJOL to UNRATE", xlab="Lag", ylab="", sub="", oma=c(3,0,3,0))
 
 
 # ----------------------------------------------------------------------------------------------------
 # Diagnostic Testing for the estimated VAR models:
 # ----------------------------------------------------------------------------------------------------
 
 # Serial correlation test
 # Ho = residuals do not have serial correlation
 
 serialtest <- serial.test(var.model.none, type = "PT.asymptotic")
 serialtest
 plot(serialtest)
 
 serialtest <- serial.test(var.model.none, type = "PT.adjusted")
 serialtest
 plot(serialtest)
 
 serialtest <- serial.test(var.model.none, type = "BG")
 serialtest
 plot(serialtest)
 
 serialtest <- serial.test(var.model.none, type = "ES")
 serialtest
 plot(serialtest)
 
 # Normality test
 # Jarque-Bera normality test of jointly normal residuals
 # Non-normal residuals distort estimates and confidence intervals
 # Test whether the multivariate skewness and kurtosis match a normal distribution
 # Joint Ho = skewness is zero, and excess kurtosis is zero
 # Non-normal residuals are a problem in small samples but less so in large samples (due to asymptotic properties)
 
 normalitytest <- normality.test(var.ordered.none)
 normalitytest
 plot(normalitytest)
 
 # ARCH test
 # Autoregressive conditional heteroscedasticity
 # ARCH Engle's Test For Residual Heteroscedasticity
 # Ho: residuals are homoscedastic
 archtest <- arch.test(var.model.none)
 archtest
 plot(archtest)
 
 # Out-of-sample Prediction
 # Number of period ahead = 10
 var.prd.none  <- predict(var.model.none,  n.ahead = 10, ci = 0.95)
 plot(var.prd.none)
 
 
 # ----------------------------------------------------------------------------------------------------
 # ARIMA Models for Forecasting
 # ----------------------------------------------------------------------------------------------------
 
 # Using package "nlme" for ARIMA model estimation
 
 # For each model we estimate the parameters: s.d. of residuals, LogLikelihood, AIC and BIC
 # We wish to compare models using information criteria, 
 # we need to make sure that we use the same data set to estimate all the specifications.
 # We need to know the model with the largest number of X lags and drop the first X observations
 
 
 
 # Select a time series:
 
 z = UNRATE
 
 # Estimate the ARIMA models:
 
 
 # ---- ARIMA (0,1,2)----- 
 
 (arima.012 <- arima(z[-(1:3)], order = c(0, 1, 2)))
 (BIC.arima.012 <- AIC(arima.012, k=log(length(z[-(1:3)]))))
 sqrt(arima.012$sigma2)
 
 
 # ---- ARIMA (1,1,1)----- 
 
 (arima.111     =   arima(z[-(1:3)], order = c(1, 1, 1)))
 (BIC.arima.111 =   AIC(arima.111, k=log(length(z[-(1:3)]))))
 sqrt(arima.111$sigma2)
 
 
 # ---- ARIMA (2,1,2)----- 
 
 (arima.212     =   arima(z[-(1:3)], order = c(2, 1, 2)))
 (BIC.arima.212 =   AIC(arima.212, k=log(length(z[-(1:3)]))))
 sqrt(arima.212$sigma2)
 
 
 # ---- ARIMA (3,1,0)----- 
 
 (arima.310 <- arima(z[-(1:3)], order = c(3, 1, 0)))
 (BIC.arima.310 <- AIC(arima.310, k=log(length(z[-(1:3)]))))
 sqrt(arima.310$sigma2)
 
 
 # ---- ARIMA (2,1,0)----- 
 
 (arima.210 <- arima(z[-(1:3)], order = c(2, 1, 0)))
 (BIC.arima.210 <- AIC(arima.210, k=log(length(z[-(1:3)]))))
 sqrt(arima.210$sigma2)
 
 
 
 # ---- ARIMA (0,1,0)----- 
 
 (arima.010 <- arima(z[-(1:3)], order = c(0, 1, 0)))
 (BIC.arima.010 <- AIC(arima.010, k=log(length(z[-(1:3)]))))
 sqrt(arima.010$sigma2)
 
 # To conduct diagnostic tests to verify that the ARIMA model is well-specified, 
 # with residuals behaving as white noises and being normally distributed, we run the following
 # tests for the ARIMA model:
 
 # Select the best ARIMA model: 
 arima.model = arima.111
 plot(arima.model)
 plot(resid(arima.model))
 
 #---- Ljung-Box -----
 # Use lag > fitdf, in which fitdf=(p+q) because the series we are testing
 # are the residuals from an estimated ARMA(p,q) model.
 # H0: series is independent; H1: series has dependence across lags
 # Test statistic is distributed as a chi-squared random variable
 # If p-value is small then reject the null.
 Box.test(resid(arima.model), lag = 2, fitdf=0)
 Box.test(resid(arima.model), lag = 2, type= "Ljung", fitdf=0)
 
 #---- Shapiro-Wilk Normality Test -----
 shapiro.test(resid(arima.model)) 
 
 #---- Jarque-Bera Normality Test ------
 
 # Using package "tseries"
 jarque.bera.test(resid(arima.model))
 
 

 # ----------------------------------------------------------------------------------------------------
 # Automatic ARIMA model selection
 # ----------------------------------------------------------------------------------------------------
 
 # Returns best ARIMA model according to either AIC, AICc or BIC value. 
 # The function conducts a search over possible model within the order constraints provided.
 
 # Using package "forecast"
 # https://www.rdocumentation.org/packages/forecast/versions/8.9/topics/auto.arima
 
 
 arima.model = auto.arima(z,
                          
                          D = 1,
                          stationary = FALSE,
                          ic = c("aicc", "aic", "bic"),
                          stepwise = FALSE,
                          approximation = FALSE,
                          seasonal = TRUE,
                          allowdrift = TRUE
 ) 
 
 arima.model
 plot(arima.model)
 plot(resid(arima.model))
 
 # ----------------------------------------------------------------------------------------------------
 # Diagnostics Checks
 # ----------------------------------------------------------------------------------------------------
 
 #---- Ljung-Box -----
 # Use lag > fitdf, in which fitdf=(p+q) because the series we are testing
 # are the residuals from an estimated ARMA(p,q) model.
 # H0: series is independent; H1: series has dependence across lags
 # Test statistic is distributed as a chi-squared random variable
 # If p-value is small then reject the null.
 Box.test(resid(arima.model), lag = 2, fitdf=0)
 Box.test(resid(arima.model), lag = 2, type= "Ljung", fitdf=0)
 
 #---- Shapiro-Wilk Normality Test -----
 shapiro.test(resid(arima.model)) 
 
 #---- Jarque-Bera Normality Test ------
 
 # Using package "tseries"
 jarque.bera.test(resid(arima.model))
 
 
 
 
 # ----------------------------------------------------------------------------------------------------
 # Forecasting
 # ----------------------------------------------------------------------------------------------------
 
 # Out-of-sample forecasting:
 ARIMA.forecast = forecast(arima.model, h = 10)
 plot(ARIMA.forecast)
 
 # In-sample forecasting:
 install.packages("smooth")
 library(smooth)
 # Seasonal ARIMA(1,1,2)(2,1,0)[12] for the UNRATE:
 
 # h = in-sample forecast period = last h months of the original time series
 # Change the value of h below and see what happens to the in-sample forecast
 # Compare the original series with the in-sample forecast
 
 
 sarima.model = msarima(z, orders=list(
   
   ar = c(1,2),
   i = c(1,1),
   ma = c(2,0)),
   lags = c(1,12),
   h = 10,
   holdout = TRUE)
 
 summary(sarima.model)
 values = sarima.model
 graphmaker(z,values$forecast,values$fitted,values$lower,values$upper,level=0.95,legend=TRUE)
 
 
 # ----------------------------------------------------------------------------------------------------
 # ARIMA for Job Openings
 # ----------------------------------------------------------------------------------------------------
 
 z = JTSJOL
 
 arima.model = auto.arima(z,
                          
                          D = 1,
                          stationary = FALSE,
                          ic = c("aicc", "aic", "bic"),
                          stepwise = FALSE,
                          approximation = FALSE,
                          seasonal = TRUE,
                          allowdrift = TRUE
 ) 
 
 arima.model
 plot(arima.model)
 plot(resid(arima.model))
 
 # ----------------------------------------------------------------------------------------------------
 # Diagnostics Checks
 # ----------------------------------------------------------------------------------------------------
 
 #---- Ljung-Box -----
 Box.test(resid(arima.model), lag = 2, fitdf=0)
 
 Box.test(resid(arima.model), lag = 2, type= "Ljung", fitdf=0)
 
 #---- Shapiro-Wilk Normality Test -----
 shapiro.test(resid(arima.model)) 
 
 #---- Jarque-Bera Normality Test ------
 
 # Using package "tseries"
 jarque.bera.test(resid(arima.model))
 
 
 
 
 # ----------------------------------------------------------------------------------------------------
 # Forecasting
 # ----------------------------------------------------------------------------------------------------
 
 # Out-of-sample forecasting:
 ARIMA.forecast = forecast(arima.model, h = 10)
 plot(ARIMA.forecast)
 
 # In-sample forecasting:
 # Seasonal ARIMA(1,1,0)(2,1,2)[12] for the JTSJOL
 sarima.model = msarima(z, orders=list(
   
   ar = c(1,2),
   i = c(1,1),
   ma = c(0,2)),
   lags = c(1,12),
   h = 10,
   holdout = TRUE)
 
 summary(sarima.model)
 values = sarima.model
 graphmaker(z,values$forecast,values$fitted,values$lower,values$upper,level=0.95,legend=TRUE)
