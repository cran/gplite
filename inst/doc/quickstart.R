## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 6,
  fig.height = 4,
  fig.retina = 2
)

## ---- results='hide', message=F-----------------------------------------------
library(gplite)
library(ggplot2)
theme_set(theme_classic())

## -----------------------------------------------------------------------------
# create some toy 1d regression data
set.seed(32004)
n <- 200
sigma <- 0.1
x <- rnorm(n)
y <- sin(3*x)*exp(-abs(x)) +  rnorm(n)*sigma 
ggplot() + 
  geom_point(aes(x=x,y=y), size=0.5) + 
  xlab('x') + ylab('y') + xlim(-4,4)

## -----------------------------------------------------------------------------
gp <- gp_init(cfs = cf_sexp(), lik = lik_gaussian())

## -----------------------------------------------------------------------------
print(gp)

## ---- results='hide'----------------------------------------------------------
gp <- gp_optim(gp, x, y)

## -----------------------------------------------------------------------------
print(gp)

## -----------------------------------------------------------------------------
# compute the predictive mean and variance in a grid of points
xt <- seq(-4,4,len=150)
pred <- gp_pred(gp, xt, var=T)

# visualize
mu <- pred$mean
lb <- pred$mean - 2*sqrt(pred$var)
ub <- pred$mean + 2*sqrt(pred$var)
ggplot() + 
  geom_ribbon(aes(x=xt, ymin=lb, ymax=ub), fill='lightgray') +
  geom_line(aes(x=xt, y=mu), size=1) +
  geom_point(aes(x=x, y=y), size=0.5) +
  xlab('x') + ylab('y')

## -----------------------------------------------------------------------------
# predict in a grid of points
xt <- seq(-4,4,len=150)
ndraws <- 30
draws <- gp_draw(gp, xt, draws=ndraws)


pp <- ggplot() + xlab('x') + ylab('y')
for (i in 1:ndraws) {
  pp <- pp + geom_line(data=data.frame(x=xt, y=draws[,i]), aes(x=x,y=y), color='darkgray')
}
pp <- pp + geom_point(aes(x=x,y=y), color='black', size=0.5)
pp
  

## -----------------------------------------------------------------------------
# create 1d toy binary classification data
set.seed(32004)
n <- 150
sigma <- 0.1
x <- rnorm(n)
ycont <- sin(3*x)*exp(-abs(x)) +  rnorm(n)*sigma
y <- rep(0,n)
y[ycont > 0] <- 1

ggplot() + 
  geom_point(aes(x=x,y=y), size=0.5) + 
  xlab('x') + ylab('y')

## ---- results = 'hide'--------------------------------------------------------
gp <- gp_init(cfs = cf_sexp(), lik = lik_bernoulli())
gp <- gp_optim(gp, x, y)

## -----------------------------------------------------------------------------
# predict in a grid of points
xt <- seq(-4,4,len=150)
pred <- gp_pred(gp, xt, quantiles=c(0.05, 0.95), transform=T)
pred_mean <- pred$mean
pred_lb <- pred$quantiles[,1]
pred_ub <- pred$quantiles[,2]


ggplot() + 
  geom_ribbon(aes(x=xt, ymin=pred_lb, ymax=pred_ub), fill='lightgray') +
  geom_line(aes(x=xt, y=pred_mean), color='black', size=1) +
  geom_point(aes(x=x,y=y), color='black', size=0.5) +
  xlab('x') + ylab('y')


## ---- results='hide'----------------------------------------------------------
gp <- gp_init(cfs = cf_sexp(), lik = lik_bernoulli(), approx = approx_ep())
gp <- gp_optim(gp, x, y)

## -----------------------------------------------------------------------------
# predict in a grid of points
xt <- seq(-4,4,len=150)
pred <- gp_pred(gp, xt, quantiles=c(0.05, 0.95), transform=T)
pred_mean <- pred$mean
pred_lb <- pred$quantiles[,1]
pred_ub <- pred$quantiles[,2]

ggplot() + 
  geom_ribbon(aes(x=xt,ymin=pred_lb,ymax=pred_ub), fill='lightgray') +
  geom_line(aes(x=xt,y=pred_mean), color='black', size=1) +
  geom_point(aes(x=x,y=y), color='black', size=0.5) +
  xlab('x') + ylab('y')


## -----------------------------------------------------------------------------
x <- as.numeric(time(datasets::discoveries))
y <- as.numeric(datasets::discoveries)

ggplot() + 
  geom_point(aes(x=x,y=y), size=0.5) + 
  xlab('Year') + ylab('Number of discoveries')

## ---- results='hide'----------------------------------------------------------
gp <- gp_init(cfs = cf_matern32(), lik = lik_poisson())
gp <- gp_optim(gp, x, y)

## -----------------------------------------------------------------------------

# predict in a grid of points
xt <- seq(1860, 1968, len=150)
pred <- gp_pred(gp, xt, quantiles=c(0.05, 0.95), transform=T)
pred_mean <- pred$mean
pred_lb <- pred$quantiles[,1]
pred_ub <- pred$quantiles[,2]

ggplot() + 
  geom_ribbon(aes(x=xt,ymin=pred_lb,ymax=pred_ub), fill='lightgray') +
  geom_line(aes(x=xt,y=pred_mean), color='black', size=1) +
  geom_point(aes(x=x,y=y), color='black', size=0.5) +
  xlab('Year') + ylab('Number of discoveries')


## -----------------------------------------------------------------------------
# create synthetic 2d binary classification data
set.seed(2)
n <- 6000
n_per_cluster <- n/3
d <- 2
x <- 0.5*cbind( matrix(rnorm(n_per_cluster*d), nrow=d) + c(3,3), 
                matrix(rnorm(n_per_cluster*d), nrow=d) - c(0,0),
                matrix(rnorm(n_per_cluster*d), nrow=d) + c(-3,3))
x <- t(x)
y <- c(rep(0,n_per_cluster), rep(1,n_per_cluster), rep(0,n_per_cluster))

# plot 
ggplot() +
  geom_point(data=data.frame(x=x[,1],y=x[,2]), aes(x=x,y=y), color=y+2, size=1) +
  xlab('x1') + ylab('x2')

## ---- results='hide'----------------------------------------------------------
# fit the model
gp <- gp_init(
  lik = lik_bernoulli(), 
  cfs = cf_sexp(), 
  method = method_fitc(num_inducing=50)
)
gp <- gp_optim(gp, x, y)

## -----------------------------------------------------------------------------
# predict
ng <- 20
x1g <- seq(-4,4,len=ng)
x2g <- seq(-2,4,len=ng)

xnew <- cbind( rep(x1g,each=ng), rep(x2g,ng) )
pred <- gp_pred(gp, xnew, transform=T)
prob <- pred$mean

# visualize
ggplot() + 
  geom_contour(data=data.frame(x=xnew[,1], y=xnew[,2], prob=prob),
                aes(x=x, y=y, z=prob, colour=..level..) ) +
  scale_colour_gradient(low = "red", high = "green", guide='none') +
  geom_point(data=data.frame(x=x[,1],y=x[,2]), aes(x=x,y=y), color=y+2, size=1) +
  xlab('x1') + ylab('x2')


## ---- results='hide'----------------------------------------------------------
# fit the model
gp <- gp_init(
  cfs = cf_sexp(), 
  lik = lik_bernoulli(), 
  method = method_rf(num_basis=100)
)
gp <- gp_optim(gp, x, y, tol=1e-5)

## -----------------------------------------------------------------------------
# predict
ng <- 20
x1g <- seq(-4,4,len=ng)
x2g <- seq(-2,4,len=ng)

xnew <- cbind( rep(x1g,each=ng), rep(x2g,ng) )
pred <- gp_pred(gp, xnew, transform=T)
prob <- pred$mean

# visualize
ggplot() + 
  geom_contour(data=data.frame(x=xnew[,1], y=xnew[,2], prob=prob),
                aes(x=x, y=y, z=prob, colour=..level..) ) +
  scale_colour_gradient(low = "red", high = "green", guide='none') +
  geom_point(data=data.frame(x=x[,1],y=x[,2]), aes(x=x,y=y), color=y+2, size=1) +
  xlab('x1') + ylab('x2')


## -----------------------------------------------------------------------------
y_all <- datasets::AirPassengers
x_all <- seq_along(y_all)

# hold out 2 years as a test set
nt <- 24
n <- length(x_all) - nt
x <- x_all[1:n]
y <- y_all[1:n]
xt <- tail(x_all, nt)
yt <- tail(y_all, nt)

ggplot() + 
  geom_line(aes(x=x,y=y), color='black') +
  geom_line(aes(x=xt,y=yt), color='red') +
  xlab('Time (months)') + ylab('Num. of passengers (thousands)')

## -----------------------------------------------------------------------------

# take a log transformation to get a more stationary process
yscaled <- log(y)

# set up the model
cf0 <- cf_const(magn=5)
cf1 <- cf_lin(magn=0.01)
cf2 <- cf_periodic(
  period=12, 
  prior_period = prior_fixed(),
) * cf_sexp(
  lscale=100, 
  magn=1,
  prior_magn = prior_fixed()
)
cfs <- list(cf0, cf1, cf2)
gp <- gp_init(cfs, lik=lik_gaussian(sigma=0.05))

## ---- results='hide'----------------------------------------------------------
gp <- gp_optim(gp, x, yscaled)

## -----------------------------------------------------------------------------

pred_scaled <- gp_pred(gp, xt, var=T)

pred <- exp(pred_scaled$mean)
pred_lb <- exp(pred_scaled$mean - 2*sqrt(pred_scaled$var))
pred_ub <- exp(pred_scaled$mean + 2*sqrt(pred_scaled$var))

ggplot() + 
  geom_ribbon(aes(x=xt, ymin=pred_lb, ymax=pred_ub), fill='lightgray') +
  geom_line(aes(x=xt, y=pred), color='black', alpha=0.5) +
  geom_line(aes(x=x, y=y), color='black') +
  geom_line(aes(x=xt, y=yt), color='red', alpha=0.5) +
  xlab('Time (months)') + ylab('Num. of passengers (thousands)')

