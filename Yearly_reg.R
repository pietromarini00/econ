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
#empl_prot2 <- e_p$Value[31:52] # strictness of regulation of individual dismissals of workers on regular contracts (1998-2019)
#empl_prot3 <- e_p$Value[53:64] # strictness of regulation of collective dismissals of workers on regular contracts (2000-2019)
#empl_prot4 <- e_p$Value[65:71] # Version 4 (2008-2019)
year <- seq(1, 442, by=12)
change_pi_y <- (pi[13:454]-pi[1:442])[year]
unemployment_y <- u$Value[1:442][year]



                        ### LINEAR REGRESSION ###

# Data
df <- data.frame(xvar = unemployment_y, yvar = change_pi_y)
infl_variation <- change_pi_y

# Linear model fitting: first model with only unemployment
(summary(phillips <- lm(infl_variation ~ unemployment_y, data=df)))
# the scores as we can see from the summary aren't great. Let's Understand why 
# the linear pattern is not so evident
print(model.diag.metrics <- augment(phillips)) # some metrics plotted
ggplot(model.diag.metrics, aes(x= unemployment_y, y= infl_variation)) + 
  geom_point()+
  geom_smooth(method=lm) +
  geom_segment(aes(xend = unemployment_y, yend = .fitted), color = "red", size = 0.3)

# maybe adding some data can solve the issue so we regress year-on-year for each 
# month
                    ### LINEAR REGRESSION MONTHLY ###

# Data
df <- data.frame(xvar = unemployment, yvar = change_pi)
infl_variation_m <- change_pi

# Linear model fitting monthly data
(summary(phillips_m <- lm(infl_variation_m ~ unemployment, data=df)))
# these results have a higher p-value but the R^2 is still low, even lower if
# possible

# a less nice plot
model.diag.metrics_m <- augment(phillips_m)
ggplot(model.diag.metrics_m, aes(x= unemployment, y= infl_variation_m)) + 
  geom_point()+
  geom_smooth(method=lm) +
  geom_segment(aes(xend = unemployment, yend = .fitted), color = "red", size = 0.3)


# maybe something we should have done at the beginning. Look at the time series
# for all the data
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

# Inflation over the months
dat2 <- data.frame(xvar = infl$TIME, yvar = infl$Value)
inflation <- pi[13:454]
month <- 1:442 # We plot it over the months, the equivalent of 40 years
ggplot(data=NULL, aes(x=month, y=inflation))+ 
  geom_line()

# inflation over the years
dat2 <- data.frame(xvar = infl$TIME[year], yvar = infl$Value[year])
# We plot it over the 37 years
Years = 1983:2019
ggplot(data=NULL, aes(x=Years, y=inflation[year]))+
  geom_line()



          ### INFLATION AND UNEMPLOYMENT VARIATIONS OVER TIME ###

e_hat_m <- resid(phillips_m)

#Unemployment and inflation over the months
ggplot(data=NULL, aes(x=month, y=inflation))+ 
  geom_line(color ='blue')+
  geom_line(aes(x=month, y=unemployment), color='red') + 
  geom_point(aes(x=month, y=e_hat_m), color="gray")

#Unemployment and change in inflation over the years 
#ggplot(data=NULL, aes(x=Years, y=change_pi[year]))+ 
#  geom_line(color ='blue')+
#  geom_line(aes(x=Years, y=unemployment_y), color='red') + 
#  geom_point(aes(x=month, y=e_hat_m), color="gray")

#Unemployment and inflation over the years 
ggplot(data=NULL, aes(x=Years, y=unemployment_y))+ 
  geom_line(color ='blue')+
  geom_line(aes(x=Years, y=inflation[year]), color='red') 

# it looks like it goes down when the other goes up:
# testing this we realized it wasn't true
(summary(model_no_delta <- lm(inflation[year] ~ unemployment_y)))



                          ### MODEL RESIDUALS ###

# We fit a simple model from our wage data: infl  = a + b*unempl + e
inflation_hat <- fitted(phillips) # fitted values: infl_hat = a + b*unempl
e_hat <- resid(phillips)  # redisuals: e_hat = infl - a - b*unempl = infl - infl_hat

# Model Residuals over the years
ggplot(model.diag.metrics, aes(x = Years, y = e_hat)) + 
  geom_line(color = 'green')+
  geom_line(aes(x=year, y=unemployment_y), color='red')+
  geom_line(aes(x= year, y= infl_variation), color ='blue')+
  geom_line(aes(x= year, y= phillips$fitted.values), color ='orange')

# ONLY RESIDUALS
ggplot(model.diag.metrics, aes(x = Years, y = e_hat)) + geom_line(color = 'blue')



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

ggplot(df2, aes(x = Years, y = kvar_model$residuals)) + geom_line(color = 'green')+
  geom_line(aes(x=Years, y=unemployment), color='red')+ # unemployment
  geom_line(aes(x= Years, y= infl_variation), color ='blue')+ #inflation change
  geom_line(aes(x= Years, y= phillips$fitted.values), color ='orange') #fitted

# We define supplementary elements
e = kvar_model$residuals
mu = mean(e)
V = var(e)
#ggplot(df2, aes(x = e)) + geom_histogram(color = 'green')
hist(e, freq=F, breaks=32)
lines(seq(-5, 5, by=.1), dnorm(seq(-5, 5, by=.1), mu, V^0.5))
# THEY LOOK QUASI-Normal


#  LINEAR REGRESSION WITH 10y YIELD

df2 <- data.frame(unemployment = unemployment, 
                  yield_10 = y10$IRLTLT01IEM156N)
yield10_model <-(lm(change_pi ~ unemployment + yield_10, data = df2))
summary(yield10_model)

error = yield10_model$residuals
mu = mean(error)
V = var(error)
#ggplot(df2, aes(x = e)) + geom_histogram(color = 'green')
hist(error, freq=F, breaks=32)
#lines(seq(-5, 5, by=.1), dnorm(seq(-5, 5, by=.1), mu, V^0.5))
lines(seq(-10, 10, by=.1), dt(seq(-10, 10, by=.1), 2))
# looks more like a t-student


#  LINEAR REGRESSION WITH SHARE PRICES
share_prices = s_p[1:442]
df3 <- data.frame(unemployment = unemployment, 
                  share_prices = share_prices)
shares_model <-(lm(change_pi ~ unemployment + share_prices, data = df3))
summary(shares_model)

sherror = shares_model$residuals
mu = mean(sherror)
V = var(sherror)
#ggplot(df2, aes(x = e)) + geom_histogram(color = 'green')
hist(error, freq=F, breaks=32)
lines(seq(-10, 10, by=.1), dt(seq(-10, 10, by=.1), 2))


#  LINEAR REGRESSION WITH CHANGES IN OIL PRICES

df4 <- data.frame(unemployment = unemployment, 
                  change_oil = change_oil)
oil_model <-(lm(change_pi ~ unemployment + change_oil, data = df4))
summary(oil_model)

oil_error = oil_model$residuals
mu = mean(oil_error)
V = var(oil_error)
#ggplot(df2, aes(x = e)) + geom_histogram(color = 'green')
hist(error, freq=F, breaks=32)
#lines(seq(-5, 5, by=.1), dnorm(seq(-5, 5, by=.1), mu, V^0.5))
lines(seq(-10, 10, by=.1), dt(seq(-10, 10, by=.1), 2))
# looks more like a t-student


#LINEAR REGRESSION WITH LABOUR PROTECTION Z


#TO DO: BISOGNA TAGLIARE UNEMPLOYMENT (QUANDO INIZIA) E CHANGE_PI (QUANDO INIZIA)
#       E PRENDERE SOLO UN VALORE OGNI DODICI DAL MOMENTO CHE EMPL_PROT1 è ANNUALE
df2 <- data.frame(xvar = unemployment_y[0:30], xvar2 = empl_prot1, yvar = change_pi_y[0:30])
summary(lm(change_pi_y[0:30] ~ unemployment_y[0:30] + empl_prot1, data = df2))



                        ### TEST ON THE ASSUMPTIONS ###

# GRAPHs TEST

par(mfrow=c(2,2))
plot(phillips)
plot(phillips, 4) #Cook's Line
    # Remove outliers
fitWithoutOutlier <- lm(infl_variation[-29][-22][-3][-1] ~ unemployment_y[-29][-22][-3][-1], data=df)
    # Draw the graph
model.diag.metrics2 <- augment(fitWithoutOutlier)
ggplot(model.diag.metrics2, aes(x= unemployment_y[-29][-22][-3][-1], y= infl_variation[-29][-22][-3][-1])) + 
  geom_point()+
  geom_smooth(method=lm) +
  geom_segment(aes(xend = unemployment_y[-29][-22][-3][-1], yend = .fitted), color = "red", size = 0.3)
    # Run the graphical test once again
par(mfrow=c(2,2))
plot(fitWithoutOutlier)
plot(fitWithoutOutlier, 4)
    # Compare the two regressions
summary(fitWithoutOutlier)
summary(phillips)

    # -> The model without ouliers has sensibly improved    


# OLS TRIVIAL TEST

# OLS imposes 0 covariance between the residuals and the unemployment
# and zero mean for the error term
    #For original model
cov(u$Value[1:442],e_hat_m) 
mean(e_hat_m)
    #For modified model
cov(u$Value[1:442][year],e_hat) 
mean(e_hat)
    #For version with oil
cov(u$Value[1:442][year],e) #NOT SURE
mean(e)

    # -> They both pass this test


# TESTS FOR HOMOSCEDASTICITY

# Godfeld-Quand Test

gqtest(phillips, point = 0.5, fraction = 0, alternative = c("greater", "two.sided", "less"), order.by = NULL, data = list())
#GQ = 1.2, large enough p-value, we accept H0 
gqtest(fitWithoutOutlier, point = 0.5, fraction = 0, alternative = c("greater", "two.sided", "less"), order.by = NULL, data = list())
#GQ = 1.4, p-value smaller than before but acceptable 
gqtest(kvar_model, point = 0.5, fraction = 0, alternative = c("greater", "two.sided", "less"), order.by = NULL, data = list())
#GQ = 1.4465, p-value = 0.2743 could be acceptable fro model with oil

# Breusch-Pagan Test

bptest(phillips, varformula = NULL, studentize = TRUE, data = list())
#BP = 1.5, p-value large enough to accept H0
bptest(fitWithoutOutlier, varformula = NULL, studentize = TRUE, data = list())
#BP = 0.33, p-value = 0.564, optival values 
bptest(kvar_model, varformula = NULL, studentize = TRUE, data = list())
#BP = 7.5061, p-value = 0.2766, this test, however, could reveal some heteroscedasticity

# White Test

white_lm(phillips, interactions = FALSE, statonly = FALSE)
#W = 1.77, p-value = 0.412, acceptable value 
white_lm(fitWithoutOutlier, interactions = FALSE, statonly = FALSE)
#W = 2.24, p-value = 0.326, acceptable value 
white_lm(kvar_model, interactions = FALSE, statonly = FALSE)
#W = 22.2, p-value = 0.035, heteroscedasticity confirmed from this test too

    # -> the linear regression is not heteroscedastic. 
      #Overall, without outliers it presented a better alternative
      #The alternative with oil, however, has shown heteroscedasticity.


# TESTS FOR SERIALLY CORRELATED ERRORS

# Durbin-Watson Test

dwtest(phillips, order.by = NULL, alternative = c("greater", "two.sided", "less"),
       iterations = 15, exact = NULL, tol = 1e-10, data = list())
#DW = 1.6, p-value = 0.08, not great values
dwtest(fitWithoutOutlier, order.by = NULL, alternative = c("greater", "two.sided", "less"),
       iterations = 15, exact = NULL, tol = 1e-10, data = list())
#DW = 1.7 p-value = 0.1368
dwtest(kvar_model, order.by = NULL, alternative = c("greater", "two.sided", "less"),
       iterations = 15, exact = NULL, tol = 1e-10, data = list())
#DW = 1.9442, p-value = 0.183 #istead, the model with oil produces nicer values (since DW = 2)

# Breusch-Godfrey Test

bgtest(phillips, order = 1, order.by = NULL, type = c("Chisq", "F"), data = list())
#LM = 0.36, p-value = 0.546, great values
bgtest(fitWithoutOutlier, order = 1, order.by = NULL, type = c("Chisq", "F"), data = list())
#LM = 0.73, p-value = 0.39, still good values
bgtest(kvar_model, order = 1, order.by = NULL, type = c("Chisq", "F"), data = list())
#LM = 0.061544, p-value = 0.8041, optimal values, as before

    # -> the linear regression is not likely to have correlation errors,
        #but not all tests produce the same result. 
      #Overall, without outliers it presented a similar alternative.
      #The regression with oil, on the other hand, has shown the
        #best results for all tests.


# TESTS FOR NORMALITY

# Jarque and Bera Test

jb.norm.test(phillips$residuals, nrepl = 200)
#JB = 7.8915, p-value = 0.03, bad values
jb.norm.test(fitWithoutOutlier$residuals, nrepl = 200)
#JB = 5.7754, p-value = 0.04 the model has sensibly improved
jb.norm.test(kvar_model$residuals, nrepl = 200)
#JB = 23.874, p-value < 2.2e-16, the oil regression has problems with normality

    # -> the linear regression is likely to have some normality errors,
          #but not clearly displayed. 
        #Overall, without outliers it presented a better, but not great, alternative.
        #The regression with oil suggests instead that the values 
         #are not normally distributed.
