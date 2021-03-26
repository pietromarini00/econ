install.packages('tidyverse')
install.packages('ggplot2')
install.packages('lmtest')
install.packages('normtest') #to test Normality
install.packages('skedastic')
library('ggplot2')
library('readr')
library('lmtest')
library('broom')
library('normtest')
library('skedastic')

rm(list=ls())

#DATASET
infl <- read.csv("~/Desktop/econ/infl.csv")
u <- read.csv("~/Desktop/econ/u.csv")
oil <- read.csv("~/Desktop/econ/spotcrudeoilprice.csv")
oil2 <- read.csv("~/Desktop/econ/crude_oil_import_price.csv") #ONLY ANNUAL
PPP <- read.csv("~/Desktop/econ/PPP.csv")
e_p <- read.csv("~/Desktop/econ/empl_prot.csv")
# share prices index [IRL]
s_p <- read.csv("~/Desktop/econ/Share_prices.csv")
# oil price adjusted for inflation [WTI]
real_oil_prices <- read_excel("~/Desktop/econ/real_oil_prices.xlsx")
# adult education
#a_e <- read.csv("~/Desktop/econ/adult_educ.csv") ---> missing values
# social security contribution
SSC <- read.csv("~/Desktop/econ/Soc_sec_contrib.csv")
# fatalities from terrorism [IRA]
Terr <- read.csv("~/Desktop/econ/fatalities_from_terrorism.csv") #missing values; wrong csv
# Housing Market
H_p <- read.csv("~/Desktop/econ/Housing_IRL.csv")
# Yield 10 year on Irish Government Bonds
y10 <- read.csv("~/Desktop/econ/Yield_10y.csv")


#VARIABLE NAMES
pi <- infl$Value[26:479]
time_pi <- infl$TIME[26:467]
change_pi <- pi[13:454]-pi[1:442]
unemployment <- u$Value[1:442]
time_ut <- u$TIME[1:442]
oil_d <- oil$WTISPLC
oil_time <- oil$DATE[1:442]
change_oil <- oil_d[13:454] - oil_d[1:442]
s_p <- s_p$Value
s_p <- s_p[1:442]
oil_real <-  real_oil_prices$Oil_Real[109:550]
oil_change <- (oil_real[110:552]-oil_real[98:540])/(oil_real[98:540])
empl_prot1 <- e_p$Value[1:30] # strictness of dismissal regulation for workers on regular contracts (1985-2019) 
empl_prot2 <- e_p$Value[31:52] # strictness of regulation of individual dismissals of workers on regular contracts (1998-2019)
empl_prot3 <- e_p$Value[53:64] # strictness of regulation of collective dismissals of workers on regular contracts (2000-2019)
empl_prot4 <- e_p$Value[65:71] # Version 4 (2008-2019)

year <- seq(1, 442, by=12)
change_pi_y <- (pi[13:454]-pi[1:442])[year]
unemployment_y <- u$Value[1:442][year]



### LINEAR REGRESSION ###

# Data
df <- data.frame(xvar = unemployment_y, yvar = change_pi_y)
infl_variation <- change_pi_y

# Linear model fitting
(summary(phillips <- lm(infl_variation ~ unemployment_y, data=df)))

model.diag.metrics <- augment(phillips)
ggplot(model.diag.metrics, aes(x= unemployment_y, y= infl_variation)) + 
  geom_point()+
  geom_smooth(method=lm) +
  geom_segment(aes(xend = unemployment_y, yend = .fitted), color = "red", size = 0.3)


### LINEAR REGRESSION MONTHLY ###

# Data
df <- data.frame(xvar = unemployment, yvar = change_pi)
infl_variation_m <- change_pi

# Linear model fitting
(summary(phillips_m <- lm(infl_variation_m ~ unemployment, data=df)))

model.diag.metrics_m <- augment(phillips_m)
ggplot(model.diag.metrics_m, aes(x= unemployment, y= infl_variation_m)) + 
  geom_point()+
  geom_smooth(method=lm) +
  geom_segment(aes(xend = unemployment, yend = .fitted), color = "red", size = 0.3)



### INIDIVDUAL VARIATIONS OVER TIME ###


# Unemployment over the months
dat <- data.frame(xvar = u$TIME, yvar = u$Value)
months <- 1:442 # We plot it over the months, the equivalent of 37 years
ggplot(data=NULL, aes(x=months, y=unemployment))+
  geom_line()

# Unemployment over the years
dat <- data.frame(xvar = u$TIME[year], yvar = u$Value[year])
# We plot it over the 37 years
Years = 1983:2019
ggplot(data=NULL, aes(x=Years, y=unemployment_y))+
  geom_line()

# Inflation over the years
dat2 <- data.frame(xvar = infl$TIME, yvar = infl$Value)
inflation <- pi[13:454]
month <- 1:442 # We plot it over the months, the equivalent of 40 years
ggplot(data=NULL, aes(x=month, y=inflation))+ 
  geom_line()

# inflation over the years
dat2 <- data.frame(xvar = infl$TIME, yvar = infl$Value[year])
# We plot it over the 37 years
Years = 1983:2019
ggplot(data=NULL, aes(x=Years, y=inflation[year]))+
  geom_line()


#change inflation over the years


### INFLATION AND UNEMPLOYMENT VARIATIONS OVER TIME ###

ggplot(data=NULL, aes(x=month, y=inflation))+ 
  geom_line(color ='blue')+
  geom_line(aes(x=month, y=unemployment), color='red') #+ 
  #geom_point(aes(x=month, y=e_hat), color="gray")


#Unemployment and change in inflation over the years 
ggplot(data=NULL, aes(x=Years, y=change_pi[year]))+ 
  geom_line(color ='blue')+
  geom_line(aes(x=Years, y=unemployment_y), color='red') #+ 
#geom_point(aes(x=month, y=e_hat), color="gray")

#Unemployment and inflation over the years 
ggplot(data=NULL, aes(x=Years, y=unemployment_y))+ 
  geom_line(color ='blue')+
  geom_line(aes(x=Years, y=inflation[year]), color='red') 

### MODEL RESIDUALS ###

# We fit a simple model from our wage data: infl  = a + b*unempl + e
inflation_hat <- fitted(phillips) # fitted values: infl_hat = a + b*unempl
e_hat <- resid(phillips)  # redisuals: e_hat = infl - a - b*unempl = infl - infl_hat

ggplot(model.diag.metrics, aes(x = .fitted, y = .resid)) + geom_point()

# Model Residuals over time
ggplot(model.diag.metrics, aes(x = year, y = e_hat)) + 
  geom_line(color = 'green')+
  geom_line(aes(x=year, y=unemployment_y), color='red')+
  geom_line(aes(x= year, y= infl_variation), color ='blue')+
  geom_line(aes(x= year, y= phillips$fitted.values), color ='orange')

# ONLY RESIDUALS
ggplot(df, aes(x = year, y = e_hat)) + geom_line(color = 'blue')



### ALTERNATIVE LINEAR REGRESSIONS ###


#LINEAR REGRESSION WITH CRUDE OIL SPOT PRICE

N <- length(unemployment)
df2 <- data.frame(unemployment = unemployment[year], 
                  oil_real = oil_real[year], 
                  share_prices = s_p[year],
                  #adult_education = a_e[year], ---> missing values in some years :)
                  social_contribution = SSC$Value,
                  housing_prices = H_p$Value,
                  yield_10year = y10$IRLTLT01IEM156N[year],
                  change_inflation = change_pi[year])
kvar_model <-(lm(change_inflation ~ unemployment + oil_real + share_prices + social_contribution + housing_prices + yield_10year, data = df2))
summary(kvar_model)

years = 1:37
ggplot(df2, aes(x = years, y = kvar_model$residuals)) + geom_line(color = 'green')
 # geom_line(aes(x=years, y=unemployment), color='red')+
#  geom_line(aes(x= years, y= infl_variation), color ='blue')+
#  geom_line(aes(x= years, y= phillips$fitted.values), color ='orange')
ggplot(df2, aes(x  = kvar_model$residuals)) + geom_histogram(color = 'green')
mean(kvar_model$residuals)
var(kvar_model$residuals)
# THEY LOOK QUASI-Normal

# verify with White test
white_lm(kvar_model, interactions = FALSE, statonly = FALSE)



# OLS in Matrix Form
Y <- as.matrix(df2[c("xvar2")] )         # Y vector (N x 1)
X <- as.matrix(df2[c("xvar","xvar2")] )  # X matrix (N x K)

# we must add a first column of ones to the X matrix, so as to account for the constant b0 in beta_hat
const  <- rep(1,N)     # vector of ones (or any number you wish) of length 10
X <- cbind(const,X)
XtX <- t(X)%*%X       # X'X matrix (K x K): t(X) gives the transpose and %*% matrix multiplication
XtY <- t(X)%*%Y       # X'Y matrix (K x 1)
invXtX <- solve(XtX)  # (X'X)^{-1} matrix (K x K)

beta_hat <- invXtX%*%XtY; beta_hat  #OLS estimators di cui change_oil viene 0
K <- length(beta_hat)               # number of estimated parameters
# you can easily check that we get the same coefficients as with R's "lm" command

Yhat <- X%*%beta_hat      # vector of fitted values Y_hat = X*beta_hat
uhat <- Y - Yhat          # vector of residuals

# estimated variance of the errors (under homoskedasticity)
sig2_hat <- as.numeric(t(uhat)%*%uhat / (N-K)); sig2_hat
# estimated variance-covariance matrix of beta_hat: Vhat = sig2_hat * (X'X)^{-1}
Vhat <- as.matrix(sig2_hat * invXtX); Vhat
# this command extracts diagonal elements of Vhat (estimated variances)
var_hat <- as.matrix(diag(Vhat)); var_hat  # get a K-dimensional vector of estimated variances
# standard errors of the estimated parameters
se <- sqrt(var_hat); se
# you can again easily check that we get the same standard errors as with R's "lm" command



#LINEAR REGRESSION WITH LABOUR PROTECTION Z


#TO DO: BISOGNA TAGLIARE UNEMPLOYMENT (QUANDO INIZIA) E CHANGE_PI (QUANDO INIZIA)
#       E PRENDERE SOLO UN VALORE OGNI DODICI DAL MOMENTO CHE EMPL_PROT1 è ANNUALE
df2 <- data.frame(xvar = unemployment_y[0:30], xvar2 = empl_prot1, yvar = change_pi_y[0:30])
summary(lm(change_pi_y[0:30] ~ unemployment_y[0:30] + empl_prot1, data = df2))


### TEST ON THE ASSUMPTIONS ###

# GRAPH TESTS

par(mfrow=c(2,2))
plot(phillips)
plot(phillips, 4)

fitWithoutOutlier <- lm(infl_variation[-29][-22][-3][-1] ~ unemployment_y[-29][-22][-3][-1], data=df)

model.diag.metrics2 <- augment(fitWithoutOutlier)
ggplot(model.diag.metrics2, aes(x= unemployment_y[-29][-22][-3][-1], y= infl_variation[-29][-22][-3][-1])) + 
  geom_point()+
  geom_smooth(method=lm) +
  geom_segment(aes(xend = unemployment_y[-29][-22][-3][-1], yend = .fitted), color = "red", size = 0.3)

par(mfrow=c(2,2))
plot(fitWithoutOutlier)
plot(fitWithoutOutlier, 5)
plot(fitWithoutOutlier, 4)

summary(fitWithoutOutlier)
summary(phillips)


# OLS TRIVIAL TEST

# OLS imposes 0 covariance between the residuals and the unemployment
# and zero mean for the error term
cov(u$Value[1:442][year],e_hat)
mean(e_hat)


# TESTS FOR HOMOSCEDASTICITY

# Godfeld-Quand Test
gqtest(phillips, point = 0.5, fraction = 0, alternative = c("greater", "two.sided", "less"),
       order.by = NULL, data = list())

# Breusch-Pagan Test
bptest(phillips, varformula = NULL, studentize = TRUE, data = list())
#p-value = 0.0006893: 0.069% small SHOULD BE GOOD --> Acceptance region

# White Test
white_lm(phillips, interactions = FALSE, statonly = FALSE)
#p-value 0.000304: 0.03% small HOWEVER HERE WE SHOULD LOOK FOR R^2


# TESTS FOR SERIALLY CORRELATED ERRORS


# Durbin-Watson Test
dwtest(phillips, order.by = NULL, alternative = c("greater", "two.sided", "less"),
       iterations = 15, exact = NULL, tol = 1e-10, data = list())
#DW = 0.095004: too small, DW should be around 2 in H0, VERY BAD --> Rejection region
#CHECK MEANING OF DYNAMIC MODELS (cannot be used under those)

# Breusch-Godfrey Test
bgtest(phillips, order = 1, order.by = NULL, type = c("Chisq", "F"), data = list())
#p-value < 2.2e-16: r should be close to 0, I don't know about the p-vale
# very small though, SHOULD BE GOOD --> Acceptance region 


# TESTS FOR NORMALITY


# Jarque and Bera Test
jb.norm.test(phillips$residuals, nrepl = 200)
#JB = 597.29, p-value < 2.2e-16: SHOULD LOOK FOR JB DISTRIBUTION (chi-square(2))


#TEST FOR LINEARITY

#DO TO


### INTERPRETATION OF COEFFICIENTS ###
#DO TO


# level-level
summary(lm(infl_variation ~ unemployment, data=df))

# log-level y
#if (infl_variation >0){
#  log_infl <- log(infl_variation)
#}
#log_infl <- function(infl_variation) {
#  for(i in 1:length(infl_variation)) {
#    if (infl_variation > 0){ 
#      b <- i^2 print(b)}}}
summary(lm(logwage ~ educ, data=wage2))

# level-log x
wage2$logeduc <- log(wage2$educ)
summary(lm(wage ~ logeduc, data=wage2))

# log-log both
summary(lm(logwage ~ logeduc, data=wage2))