## ----include=FALSE------------------------------------------------------------
library(Spower)
set.seed(42)
formals(Spower)$verbose <- FALSE

## ----include=FALSE------------------------------------------------------------
eval <- as.logical(Sys.getenv("SPOWER_EVAL"))
if(is.na(eval)) eval <- FALSE  # set to FALSE for normal run
store <- list()
if(!eval)
	store <- readRDS(system.file("gpower.rds", package = 'Spower'))

## ----include=eval-------------------------------------------------------------
getwd()

## ----eval=eval----------------------------------------------------------------
# p_r(n = interval(500, 3000), r = .65, rho = .60) |> Spower(power = .95)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex3.3 <- getLastSpower()
print(store$ex3.3)

## -----------------------------------------------------------------------------
# this is equivalent (not run):
# p_r(n = NA, r = .65, rho = .60) |> Spower(power = .95, interval=c(500,3000))

## ----eval=eval----------------------------------------------------------------
# p_r(n = 100, r = .3) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$rconst <- getLastSpower()
print(store$rconst)

## ----eval=eval----------------------------------------------------------------
# p_r(n = interval(50, 1000), r = .3) |> Spower(power = .95)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$rconst2 <- getLastSpower()
print(store$rconst2)

## -----------------------------------------------------------------------------
pwr::pwr.r.test(r=.3, power=.95, n=NULL)

## ----eval=eval----------------------------------------------------------------
# p_2r(n=206, r.ab=.75, r.ab2=.88, n2_n1=51/206) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$rineq <- getLastSpower()
print(store$rineq)

## -----------------------------------------------------------------------------
# From Gpower 3.1 manual
Cp <- matrix(c(1, .5, .4, .1, 
			   .5, 1, .2, -.4, 
			   .4, .2, 1, .8, 
			   .1, -.4, .8, 1), 4, 4)

# rearrange rows for convenience
Cp <- Cp[c(1,4,2,3), c(1,4,2,3)]
colnames(Cp) <- rownames(Cp) <- c('x1', 'y1', 'x2', 'y2')
Cp

## ----eval=eval----------------------------------------------------------------
# p_2r(n=interval(500, 2000), r.ab=.1, r.ac=.5, r.ad=.4, r.bc=-.4, r.bd=.8, r.cd=.2, two.tailed=FALSE) |>
# 	Spower(power = .80)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.28.3.1 <- getLastSpower()
print(store$ex.28.3.1)

## -----------------------------------------------------------------------------
Cp[c(4,3,1),c(4,3,1)]

## ----eval=eval----------------------------------------------------------------
# p_2r(n=interval(50, 500), r.ab=.4, r.ac=.2, r.bc=.5, two.tailed=FALSE) |>
# 	Spower(power = .80)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.28.3.2 <- getLastSpower()
print(store$ex.28.3.2)

## ----eval=eval----------------------------------------------------------------
# p_2r(n=144, r.ab=.4, r.ac=.2, r.bc=.5, two.tailed=FALSE) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.28.3.2b <- getLastSpower()
print(store$ex.28.3.2b)

## ----eval=eval----------------------------------------------------------------
# # confirm solution obtained by G*power (post hoc power estimate)
# p_2r(n=144, r.ab=.4, r.ac=0.047702, r.bc=-0.6, two.tailed=FALSE) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.28.3.3 <- getLastSpower()
print(store$ex.28.3.3)

## ----eval=eval----------------------------------------------------------------
# # note that interval is specified as c(upper, lower) as higher values
# # of r.ac result in lower power in this context
# p_2r(n=144, r.ab=.4, r.ac=interval(.4, .001), r.bc=-0.6, two.tailed=FALSE) |>
# 	Spower(power = .80)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.28.3.3b <- getLastSpower()
print(store$ex.28.3.3b)

## ----eval=eval----------------------------------------------------------------
# # solution per group
# out <- p_t.test(r = .25, n = interval(50, 200), two.tailed=FALSE) |>
# 	Spower(power = .95)
# out

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.16.3 <- getLastSpower()
out <- store$ex.16.3
print(store$ex.16.3)

## -----------------------------------------------------------------------------
# total sample size required
ceiling(out$n) * 2

## -----------------------------------------------------------------------------
F <- matrix(c(203, 186, 167, 374), 2, 2)
N <- sum(F)
(marginal.x <- colSums(F)/N)
(marginal.y <- rowSums(F)/N)

# converted to intercepts
tauX <- qnorm(1-marginal.x)[2]
tauY <- qnorm(1-marginal.y)[2]
c(tauX, tauY)

## ----echo=FALSE, eval=FALSE---------------------------------------------------
# curve(dnorm, -6, 6, las=1, ylab='Normal PDF', xlab = 'z')
# abline(v = tauX)
# text(2, .3, sprintf("tauX = %.2f", tauX))
# text(-1, .05, round(marginal.x[1], 2))
# text(1, .05, round(marginal.x[2], 2))

## ----eval=FALSE---------------------------------------------------------------
# p_r.cat(n=interval(100, 500), r=0.2399846, tauX=tauX, tauY=tauY,
# 		score=TRUE, two.tailed=FALSE) |>
# 	Spower(power = .95, parallel=TRUE)

## ----eval=eval----------------------------------------------------------------
# pi <- .65
# g <- .15
# p <- pi + g
# 
# p_prop.test(n=20, prop=p, pi=pi, two.tailed=FALSE) |>
# 	Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.4.3 <- getLastSpower()
print(store$ex.4.3)

## ----eval=eval----------------------------------------------------------------
# # Fisher exact test
# p_prop.test(n=20, prop=p, pi=pi, exact=TRUE,
# 			two.tailed=FALSE) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.4.3b <- getLastSpower()
print(store$ex.4.3b)

## ----eval=eval----------------------------------------------------------------
# p_wilcox.test(n=649, d=.1, type='one.sample', two.tailed=FALSE) |>
# 	Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.22.1 <- getLastSpower()
print(store$ex.22.1)

## ----eval=eval----------------------------------------------------------------
# # For Gaussian(d,1)
# out <- p_wilcox.test(type='one.sample', two.tailed=FALSE) |>
# 	SpowerBatch(n=c(649, 164, 42, 20, 12, 9),
# 				d=c(.1, .2, .4, .6, .8, 1.0), replications = 50000, fully.crossed=FALSE)
# as.data.frame(out)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.22.1b <- getLastSpower()
print(as.data.frame(store$ex.22.1b))

## ----eval=eval----------------------------------------------------------------
# library(extraDistr)
# 
# # generate data with scale 0-1 for d effect size to be same as mean
# # VAR = 2*b^2, so scale should be 1 = 2*b^2 -> sqrt(1/2)
# parent <- function(n, d, sigma=sqrt(1/2))
# 	extraDistr::rlaplace(n, d, sigma=sigma)
# 
# p_wilcox.test(n=11, d=.8, parent1=parent, type='one.sample',
# 			  two.tailed=FALSE, correct = FALSE) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.4.3c <- getLastSpower()
print(store$ex.4.3c)

## ----eval=eval----------------------------------------------------------------
# # For Laplace(0,1)
# out <- p_wilcox.test(parent1=parent, type='one.sample',
# 			  two.tailed=FALSE) |>
# 	SpowerBatch(n=c(419, 109, 31, 16, 11, 8),
# 				d=c(.1, .2, .4, .6, .8, 1.0), replications=50000, fully.crossed=FALSE)
# as.data.frame(out)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.4.3d <- getLastSpower()
print(as.data.frame(store$ex.4.3d))

## -----------------------------------------------------------------------------
obrien2002 <- matrix(c(.54, .32, .08, .06), 2, 2,
					 dimnames = list('Treatment' = c('Yes', 'No'),
					 				'Standard' = c('Yes', 'No')))
obrien2002

## ----eval=eval----------------------------------------------------------------
# p_mcnemar.test(n=50, prop=obrien2002, two.tailed=FALSE) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.5.3 <- getLastSpower()
print(store$ex.5.3)

## ----eval=eval----------------------------------------------------------------
# OR <- obrien2002[1,2] / obrien2002[2,1]
# disc <- obrien2002[1,2] + obrien2002[2,1]
# p_mcnemar.test(n=50, OR=OR, prop.disc=disc, two.tailed=FALSE) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.5.3b <- getLastSpower()
print(store$ex.5.3b)

## ----eval=eval----------------------------------------------------------------
# p_mcnemar.test(n=50, prop=obrien2002, two.tailed=FALSE, correct=FALSE) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.5.3c <- getLastSpower()
print(store$ex.5.3c)

## ----eval=eval----------------------------------------------------------------
# p_lm.R2(n=95, R2=.1, k=5) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.13.1 <- getLastSpower()
print(store$ex.13.1)

## ----eval=eval----------------------------------------------------------------
# p_lm.R2(n=90, R2=.3, k=9, R2_0=.25, k.R2_0=5) |> Spower(sig.level=.01)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.14.3 <- getLastSpower()
print(store$ex.14.3)

## ----eval=eval----------------------------------------------------------------
# p_lm.R2(n=interval(100, 400), R2=.3, R2_0 = .25, k=9, k.R2_0=5) |>
# 		Spower(sig.level=.01, power=.8)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.14.3b <- getLastSpower()
print(store$ex.14.3b)

## ----eval=eval----------------------------------------------------------------
# p_lm.R2(n=200, R2=.16, R2_0 = .1, k=12, k.R2_0=9, R2.resid=.8) |>
# 	Spower(sig.level=.01)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.14.3c <- getLastSpower()
print(store$ex.14.3c)

## ----eval=eval----------------------------------------------------------------
# p_lm.R2(n=95, R2=.1, k=5, fixed=FALSE) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.7.3 <- getLastSpower()
print(store$ex.7.3)

## ----eval=eval----------------------------------------------------------------
# p_slr(n=100, beta=-0.0667, sd_x=7.5, sd_y = 4) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.12.3 <- getLastSpower()
print(store$ex.12.3)

## ----eval=eval----------------------------------------------------------------
# p_anova.test(n=interval(20, 300), k=10, f=.25) |>  Spower(power=.95)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.10.3 <- getLastSpower()
print(store$ex.10.3)

## ----eval=eval----------------------------------------------------------------
# p_anova.test(n=20, k=10, f=.25) |> Spower(beta_alpha=1, replications=30000)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.10.3b <- getLastSpower()
print(store$ex.10.3b)

## -----------------------------------------------------------------------------
gen_twogroup <- function(n, dbeta, sdx1, sdx2, sigma, n2_n1 = 1, ...){
	X1 <- rnorm(n, sd=sdx1)
	X2 <- rnorm(n*n2_n1, sd=sdx2)
	X <- c(X1, X2)
	N <- length(X)
	S <- c(rep(0, n), rep(1, N-n))
	y <- dbeta * X*S + rnorm(N, sd=sigma)
	dat <- data.frame(y, X, S)
	dat
}

## ----eval=eval----------------------------------------------------------------
# p_glm(formula=y~X*S, test="X:S = 0",
# 	  n=28, n2_n1=44/28, sdx1=9.02914, sdx2=11.86779, dbeta=0.01592,
# 	  sigma=0.5578413, gen_fun=gen_twogroup) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.17.3 <- getLastSpower()
print(store$ex.17.3)

## ----eval=eval----------------------------------------------------------------
# p_glm(formula=y~X*S, test="X:S = 0",
# 	  n=interval(100, 1000), n2_n1=44/28, sdx1=9.02914, sdx2=11.86779, dbeta=0.01592,
# 	  sigma=0.5578413, gen_fun=gen_twogroup) |>
# 	Spower(power=.8)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.17.3b <- getLastSpower()
print(store$ex.17.3b)

## ----eval=eval----------------------------------------------------------------
# p_var.test(n=interval(10, 200), vars=1, sigma2=1.5, two.tailed=FALSE) |>
# 	Spower(power=.80)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.26.3 <- getLastSpower()
print(store$ex.26.3)

## ----eval=eval----------------------------------------------------------------
# # solve n for variance ratio of 1/1.5 = 2/3, two.tailed, 80% power
# p_var.test(n=interval(50, 300), vars=c(1, 1.5), two.tailed=TRUE) |>
# 	Spower(power=.80)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.15.3 <- getLastSpower()
print(store$ex.15.3)

## ----eval=eval----------------------------------------------------------------
# (out <- p_t.test(n = interval(10,500), d = .5, two.tailed=FALSE) |>
# 			   Spower(power = .95))

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.15.3b <- getLastSpower()
out <- store$ex.15.3b
print(store$ex.15.3b)

## ----echo=FALSE---------------------------------------------------------------
so <- summary(out)

## ----eval=eval----------------------------------------------------------------
# p_t.test(n=50 * 2, d=0.421637, type = 'paired') |> Spower(replications=50000)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.19.3 <- getLastSpower()
print(store$ex.19.3)

## ----eval=eval----------------------------------------------------------------
# p_t.test(n=50 * 2, d=.2828427, type = 'paired') |> Spower(replications=50000)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.19.3b <- getLastSpower()
print(store$ex.19.3b)

## ----eval=eval----------------------------------------------------------------
# p_t.test(n=interval(100,300), d=0.2828427, type = 'paired') |>
# 	Spower(power=0.832114)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.19.3c <- getLastSpower()
print(store$ex.19.3c)

## ----eval=eval----------------------------------------------------------------
# p_t.test(n=interval(10, 100), d=.625, two.tailed=FALSE, type='one.sample') |>
# 	Spower(power=.95)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.20.3 <- getLastSpower()
print(store$ex.20.3)

## ----eval=eval----------------------------------------------------------------
# p_t.test(n=interval(100,2000), d=.1, type='one.sample') |>
# 	Spower(power=.9,sig.level=.01)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.20.3b <- getLastSpower()
print(store$ex.20.3b)

## ----eval=eval----------------------------------------------------------------
# p_wilcox.test(n=649, d=.1, type='one.sample', two.tailed=FALSE) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.22.1 <- getLastSpower()
print(store$ex.22.1)

## ----eval=eval----------------------------------------------------------------
# library(extraDistr)
# parent1 <- function(n, d) extraDistr::rlaplace(n, mu=d, sigma=sqrt(1/2))
# 
# # properties of sampled distribution
# descript(parent1(n=100000, d=0.8))
# 
# p_wilcox.test(n=11, d=.8, type='one.sample', two.tailed=FALSE, parent1 = parent1) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.22.1b <- getLastSpower()
print(store$ex.22.1b)

## ----eval=eval----------------------------------------------------------------
# library(extraDistr)
# 
# parent1 <- function(n, d) extraDistr::rlaplace(n, mu=d, sigma=sqrt(1/2))
# parent2 <- function(n, d) extraDistr::rlaplace(n, sigma=sqrt(1/2))
# 
# # properties of sampled distributions
# descript(parent1(n=100000, d=0.375))
# descript(parent1(n=100000, d=0))
# 
# nr <- 134/67
# p_wilcox.test(n=67, n2_n1=nr, d=0.375, parent1=parent1, parent2=parent2) |>
# 	Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.22.1c <- getLastSpower()
print(store$ex.22.1c)

## ----eval=eval----------------------------------------------------------------
# parent1 <- function(n, d) extraDistr::rlaplace(n, mu=d, sigma=sqrt(1/2))
# parent2 <- function(n, d) extraDistr::rlaplace(n, sigma=sqrt(1/2))
# 
# descript(parent1(n=100000, d=1.13842))
# descript(parent1(n=100000, d=0))
# 
# p_wilcox.test(n=10*2, d=1.13842, type = 'paired',
# 			  parent1=parent1, parent2=parent2) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ex.23.3 <- getLastSpower()
print(store$ex.23.3)

## ----include=FALSE, eval=eval-------------------------------------------------
# saveRDS(store, '../inst/gpower.rds') # rebuild package when done

