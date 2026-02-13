## ----include=FALSE------------------------------------------------------------
library(Spower)
set.seed(42)
formals(SpowerCurve)$plotly <- FALSE

## ----include=FALSE------------------------------------------------------------
eval <- as.logical(Sys.getenv("SPOWER_EVAL"))
if(is.na(eval)) eval <- FALSE  # set to FALSE for normal run
store <- list()
if(!eval)
	store <- readRDS(system.file("intro2.rds", package = 'Spower'))

## ----include=eval-------------------------------------------------------------
getwd()

## -----------------------------------------------------------------------------
p_single.t <- function(n, mean, mu=0){
	g <- rnorm(n, mean=mean)
	p <- t.test(g, mu=mu)$p.value
	p
}

## -----------------------------------------------------------------------------
l_single.t <- function(n, mean, mu=0, conf.level=.95){
	g <- rnorm(n, mean=mean)
	out <- t.test(g, mu=mu, conf.level=conf.level)
	CI <- out$conf.int
	is.outside_CI(mu, CI)   # equivalent to: !(CI[1] < mu && mu < CI[2])
}

l_single.t(100, mean=.2)

## ----eval=eval----------------------------------------------------------------
# p_single.t(n=100, mean=.3) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$pPower <- getLastSpower()
pPower <- store$pPower
print(pPower)

## ----eval=eval----------------------------------------------------------------
# l_single.t(n=100, mean=.3) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$lPower <- getLastSpower()
lPower <- store$lPower
print(lPower)

## -----------------------------------------------------------------------------
l_single.t <- function(n, mean, mu=0, conf.level=.95){
	# return analysis output from t.test() for further extraction
	out <- p_t.test(n=n, d=mean, mu=mu, type='one.sample', 
					conf.level=conf.level, return_analysis=TRUE)
	CI <- out$conf.int
	is.outside_CI(mu, CI)
}

l_single.t(100, mean=.2)

## -----------------------------------------------------------------------------
l_precision <- function(n, mean, CI.width, mu=0, alpha=.05){
	g <- rnorm(n, mean=mean)
	out <- t.test(g, mu=mu)
	CI <- out$conf.int
	width <- CI[2] - CI[1]
	# return TRUE if significant and CI is sufficiently narrow
	out$p.value < alpha && width < CI.width   
}

## ----eval=eval----------------------------------------------------------------
# l_precision(n=interval(10, 500), mean=.2, CI.width=1/4) |>
# 	Spower(power=.80)
# 
# # equivalently:
# # l_precision(n=NA, mean=.2, CI.width=1/4) |>
# #  	 Spower(power=.80, interval=c(10, 500))

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$lprecision <- getLastSpower()
lprecision <- store$lprecision
print(lprecision)

## ----eval=eval----------------------------------------------------------------
# l_precision(n=interval(10, 500), mean=.2, CI.width=Inf) |>
# 	Spower(power=.80)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$lprecision2 <- getLastSpower()
lprecision2 <- store$lprecision2
print(lprecision2)

## ----eval=eval----------------------------------------------------------------
# l_single.Bayes.t_BF <- function(n, mean, mu=0, bf.cut=3){
# 	g <- rnorm(n, mean=mean)
# 	res <- BayesFactor::ttestBF(g, mu=mu)	
# 	bf <- exp(as.numeric(res@bayesFactor[1])) # Bayes factor
# 	data.frame(largeBF=bf > bf.cut, bf=bf)
# }

## ----eval=eval----------------------------------------------------------------
# l_single.Bayes.t_BF(n=100, mean=.5, mu=.3) |> Spower(select='largeBF') -> BFsim
# BFsim

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$prospectiveBF3 <- getLastSpower()
BFsim <- store$prospectiveBF3
print(BFsim)

## ----include=FALSE------------------------------------------------------------
library(ggplot2)

## -----------------------------------------------------------------------------
BFresults <- SimResults(BFsim)
BFresults

# use log-scale for Bayes factors as this is a more useful metric
library(ggplot2)
ggplot(BFresults, aes(log(bf), fill=largeBF)) + 
	geom_histogram(bins=50) + geom_vline(xintercept=log(3)) + 
	ggtitle('log(BF) distribution')

## ----eval=eval----------------------------------------------------------------
# # assuming P(H1)/P(H0) are equally likely; hence, prior_odds = 1
# pp_single.Bayes.t <- function(n, mean, mu, prior_odds = 1){
# 	g <- rnorm(n, mean=mean)
# 	res <- BayesFactor::ttestBF(g, mu=mu)	
# 	bf <- exp(as.numeric(res@bayesFactor[1])) # Bayes factor
# 	posterior_odds <- bf * prior_odds
# 	posterior <- posterior_odds / (posterior_odds + 1)
# 	posterior   # P(H_1|D)
# }

## ----eval=eval----------------------------------------------------------------
# # power cut-off for a significantly supportive posterior is > 0.90
# pp_single.Bayes.t(n=100, mean=.5, mu=.3) |>
# 	Spower(sig.level = .90, sig.direction = 'above')

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$prospectiveBayes <- getLastSpower()
prospectiveBayes <- store$prospectiveBayes
print(prospectiveBayes)

## -----------------------------------------------------------------------------
l_equiv.t <- function(n, delta, equiv, sds = c(1,1), 
					  sig.level = .025){
	g1 <- rnorm(n, mean=0, sd=sds[1])
	g2 <- rnorm(n, mean=delta, sd=sds[2])
	outL <- t.test(g2, g1, mu=-equiv[1], alternative = "less")$p.value
	outU <- t.test(g2, g1, mu=equiv[2], alternative = "less")$p.value
	outL < sig.level && outU < sig.level
}

## ----eval=eval----------------------------------------------------------------
# l_equiv.t(50, delta=1, equiv=c(-2.5, 2.5),
# 		  sds=c(2.5, 2.5)) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$equivT <- getLastSpower()
equivT <- store$equivT
print(equivT)

## ----eval=FALSE---------------------------------------------------------------
# TOSTER::power_t_TOST(n = 50,
# 			 delta = 1,
# 			 sd = 2.5,
# 			 eqb = 2.5,
# 			 alpha = .025,
# 			 power = NULL,
# 			 type = "two.sample")

## -----------------------------------------------------------------------------
l_equiv.t_CI <- function(n, delta, equiv, 
						 sds = c(1,1), conf.level = .95){
	out <- p_t.test(n, delta, sds=sds, conf.level=conf.level, 
					return_analysis=TRUE)
	is.CI_within(out$conf.int, interval=equiv)  # returns TRUE if CI is within equiv interval
}

## ----eval=eval----------------------------------------------------------------
# # an equivalent power analysis for "equivalence tests" via CI evaluations
# l_equiv.t_CI(50, delta=1, equiv=c(-2.5, 2.5),
# 		  sds=c(2.5, 2.5)) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$equivTL <- getLastSpower()
equivTL <- store$equivTL
print(equivTL)

## ----eval=eval----------------------------------------------------------------
# library(bayestestR)
# library(rstanarm)
# 
# rope.lm <- function(n, beta0, beta1, range, sigma=1, ...){
# 	# generate data
# 	x <- matrix(rep(0:1, each=n))
# 	y <- beta0 + beta1 * x + rnorm(nrow(x), sd=sigma)
# 	dat <- data.frame(y, x)
# 	
# 	# run model, but tell stan_glm() to use its indoor voice
# 	model <- quiet(rstanarm::stan_glm(y ~ x, data = dat))
# 	rope <- bayestestR::rope(model, ci=1, range=range, parameters="x")
# 	as.numeric(rope)
# }

## ----eval=eval----------------------------------------------------------------
# rope.lm(n=50, beta0=2, beta1=1, sigma=1/2, range=c(.8, 1.2)) |>
# 	Spower(sig.level=.95, sig.direction='above', parallel=TRUE)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ROPE.lm <- getLastSpower()
ROPE.lm <- store$ROPE.lm
print(ROPE.lm)

## ----eval=eval----------------------------------------------------------------
# rope.lm(n=interval(50, 200), beta0=2, beta1=1, sigma=1/2, range=c(.8, 1.2)) |>
# 	Spower(power=.80, sig.level=.95, sig.direction='above', parallel=TRUE)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$ROPE.lm_n <- getLastSpower()
ROPE.lm_n <- store$ROPE.lm_n
print(ROPE.lm_n)

## ----include=FALSE, eval=eval-------------------------------------------------
# saveRDS(store, '../inst/intro2.rds') # rebuild package when done

