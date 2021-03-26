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
oil_real <-  real_oil_prices$Oil_Real
oil_change <- (oil_real[110:552]-oil_real[98:540])/(oil_real[98:540])
empl_prot1 <- e_p$Value[1:30] #-> Version 1 (1985-2019)
empl_prot2 <- e_p$Value[31:52] #-> Version 2 (1998-2019)
empl_prot3 <- e_p$Value[53:64] #-> Version 3 (1998-2019)
empl_prot4 <- e_p$Value[65:71] #-> Version 4 (2008-2019)



                          ### LINEAR REGRESSION ###


df <- data.frame(xvar = unemployment, yvar = change_pi)
infl_variation <- change_pi
ggplot(df, aes(x= unemployment, y= infl_variation)) + 
  geom_point()+
  geom_smooth(method=lm)

# Linear model fitting
(summary(phillips <- lm(infl_variation ~ unemployment, data=df)))
# We fit a simple model from our wage data: infl  = a + b*unempl + e
inflation_hat <- fitted(phillips) # fitted values: infl_hat = a + b*unempl
e_hat <- resid(phillips)  # redisuals: e_hat = infl - a - b*unempl = infl - infl_hat



                   ### INIDIVDUAL VARIATIONS OVER TIME ###


# Unemployment over the years
dat <- data.frame(xvar = u$TIME, yvar = u$Value)
months <- 1:442 # We plot it over the months, the equivalent of 37 years
ggplot(data=NULL, aes(x=months, y=unemployment))+
  geom_line()

# Inflation over the years
dat2 <- data.frame(xvar = infl$TIME, yvar = infl$Value)
inflation <- pi[13:454]
month <- 1:442 # We plot it over the months, the equivalent of 40 years
ggplot(data=NULL, aes(x=month, y=inflation))+ 
  geom_line()



             ### INFLATION AND UNEMPLOYMENT VARIATIONS OVER TIME ###


#PIETRO
#dat2 <- data.frame(xvar = infl$TIME, yvar = infl$Value)
#inflation <- infl$Value
#month <- 1:479 # We plot it over the months, the equivalent of 40 years
#ggplot(data=NULL, aes(x=month, y=inflation))+ 
#  geom_line(color ='blue')+
#  geom_line(aes(x=months, y=unemployment), color='red')
#month <- 1:442

#PILVI
ggplot(data=NULL, aes(x=month, y=inflation))+ 
  geom_line(color ='blue')+
  geom_line(aes(x=month, y=unemployment), color='red') + 
  geom_point(aes(x=month, y=e_hat), color="gray")



                          ### MODEL RESIDUALS ###


df <- augment(phillips)
ggplot(df, aes(x = .fitted, y = .resid)) + geom_point()

# Model Residuals over time
ggplot(df, aes(x = 1:442, y = e_hat)) + geom_line(color = 'green')+
  geom_line(aes(x=months, y=unemployment), color='red')+
  geom_line(aes(x= months, y= infl_variation), color ='blue')+
  geom_line(aes(x= months, y= phillips$fitted.values), color ='orange')

# ONLY RESIDUALS
ggplot(df, aes(x = 1:442, y = e_hat)) + geom_line(color = 'blue')
ggplot(df, aes(x = e_hat)) + geom_histogram(color = 'blue')
mean(e_hat)
var(e_hat)


                  ### ALTERNATIVE LINEAR REGRESSIONS ###


#LINEAR REGRESSION WITH CRUDE OIL SPOT PRICE


N <- length(unemployment)
df2 <- data.frame(xvar = unemployment, xvar2 = change_oil, yvar = change_pi)
summary(lm(change_pi ~ unemployment + change_oil, data = df2))

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
N <- length(unemployment)
df2 <- data.frame(xvar = unemployment, xvar2 = empl_prot1, yvar = change_pi)
summary(lm(change_pi ~ unemployment + empl_prot1, data = df2))



                        ### TEST ON THE ASSUMPTIONS ###


# OLS TRIVIAL TEST


# OLS imposes 0 covariance between the residuals and the unemployment
# and zero mean for the error term
cov(u$Value[1:442],e_hat)
mean(e_hat)


# TESTS FOR HOMOSCEDASTICITY

#roust inference
# Usual SE (homoskedasticity, no correlation)
coeftest(phillips)
# Heteroskedasticity-robust standard errors
coeftest(phillips, vcov=hccm)

# Godfeld-Quand Test
gqtest(phillips, point = 0.5, fraction = 0, alternative = c("greater", "two.sided", "less"),
       order.by = NULL, data = list())
#p-value = 0.001334: 0.13% small enough SHOULD BE GOOD --> Acceptance region

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
#since the value is big we fail is rejecting autocorrelation

# if serial correlation was an issue, one would want to use HAC-robust standard errors
coeftest(phillips, vcov=vcovHAC)


# TESTS FOR NORMALITY

# Jarque and Bera Test
jb.norm.test(phillips$residuals, nrepl = 200)
#The null hypothesis is a joint hypothesis of the skewness being zero and the excess
#kurtosis being zero. Samples from a normal distribution have an expected skewness of 
#0 and an expected excess kurtosis of 0 (which is the same as a kurtosis of 3). As 
#the definition of JB shows, any deviation from this increases the JB statistic.


#TEST FOR LINEARITY

#DO TO



<<<<<<< Updated upstream
# log-log both
summary(lm(logwage ~ logeduc, data=wage2))
=======
>>>>>>> Stashed changes


