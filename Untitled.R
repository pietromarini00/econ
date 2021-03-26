


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


ggplot(model.diag.metrics, aes(x = .fitted, y = .resid)) + geom_point()