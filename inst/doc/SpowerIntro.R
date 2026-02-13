## ----include=FALSE------------------------------------------------------------
library(Spower)
set.seed(42)
formals(SpowerCurve)$plotly <- FALSE

## ----include=FALSE------------------------------------------------------------
eval <- as.logical(Sys.getenv("SPOWER_EVAL"))
if(is.na(eval)) eval <- FALSE  # set to FALSE for normal run
store <- list()
if(!eval)
	store <- readRDS(system.file("intro.rds", package = 'Spower'))

## ----include=eval-------------------------------------------------------------
getwd()

## -----------------------------------------------------------------------------
p_lm.R2(50, k=3, R2=.3)

## ----eval=eval----------------------------------------------------------------
# p_lm.R2(50, k=3, R2=.3) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$R2ex <- getLastSpower()
print(store$R2ex)

## -----------------------------------------------------------------------------
p_single.t <- function(n, mean, mu=0){
	g <- rnorm(n, mean=mean)
	p <- t.test(g, mu=mu)$p.value
	p
}

## -----------------------------------------------------------------------------
# a single experiment
p_single.t(n=100, mean=.2)

## ----eval=eval----------------------------------------------------------------
# p_single.t(n=100, mean=.5, mu=.3) |> Spower() -> prospective
# prospective

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$prospective <- prospective
prospective <- store$prospective
print(prospective)

## ----eval=eval----------------------------------------------------------------
# p_single.t(n=100, mean=.5, mu=.3) |>
# 	Spower(beta_alpha=4) -> compromise
# compromise

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$compromise <- getLastSpower()
compromise <- store$compromise
print(compromise)

## -----------------------------------------------------------------------------
# satisfies q = 4 ratio
with(compromise, (1 - power) / sig.level)

## -----------------------------------------------------------------------------
# using previous post-hoc/prospective power analysis
update(prospective, beta_alpha=4)

## ----eval=eval----------------------------------------------------------------
# p_single.t(n=NA, mean=.5) |>
# 	Spower(power=.8, interval=c(20,200))

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$apriori <- getLastSpower()
print(store$apriori)

## ----eval=FALSE---------------------------------------------------------------
# p_single.t(n=interval(20, 200), mean=.5) |> Spower(power=.8)

## ----echo=FALSE---------------------------------------------------------------
print(store$apriori)

## ----eval=eval----------------------------------------------------------------
# p_single.t(n=100, mean=NA) |>
# 	Spower(power=.8, interval=c(.1, 3))

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$sensitive <- getLastSpower()
print(store$sensitive)

## ----eval=eval----------------------------------------------------------------
# # p_single.t(n=100, mean=interval(.1, 3)) |> Spower(power=.8)

## ----eval=eval----------------------------------------------------------------
# p_single.t(n=50, mean=.5) |>
# 	Spower(power=.8, sig.level=NA)

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$criterion <- getLastSpower()
print(store$criterion)

## ----eval=eval----------------------------------------------------------------
# p_single.t(mean=.5) |>
# 	SpowerBatch(n=c(30, 60, 90)) -> prospective.batch
# prospective.batch

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$prospective.batch <- prospective.batch
prospective.batch <- store$prospective.batch
print(prospective.batch)

## -----------------------------------------------------------------------------
as.data.frame(prospective.batch)

## ----eval=eval----------------------------------------------------------------
# apriori.batch <- p_single.t(mean=.5, n=NA) |>
# 	SpowerBatch(power=c(.7, .8, .9), interval=c(20, 200))
# apriori.batch

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$apriori.batch <- apriori.batch
apriori.batch <- store$apriori.batch
print(apriori.batch)

## -----------------------------------------------------------------------------
as.data.frame(apriori.batch)

## ----eval=FALSE---------------------------------------------------------------
# p_single.t(mean=.5) |>
# 	SpowerCurve(n=c(30, 60, 90, 120))

## ----echo=FALSE---------------------------------------------------------------
if(eval)
	store$gg1 <- p_single.t(mean=.5) |> SpowerCurve(n=c(30, 60, 90, 120))
print(store$gg1)

## ----eval=FALSE---------------------------------------------------------------
# # pass previous SpowerBatch() object
# SpowerCurve(batch=batch)

## ----echo=FALSE---------------------------------------------------------------
if(eval) 
	SpowerCurve(batch=store$prospective.batch)	

## ----eval=FALSE---------------------------------------------------------------
# p_single.t() |>
# 	SpowerCurve(n=c(30, 60, 90, 120), mean=c(.2, .5, .8))

## ----echo=FALSE---------------------------------------------------------------
if(eval)
	store$gg2 <- p_single.t() |> 
		SpowerCurve(n=c(30, 60, 90, 120), mean=c(.2, .5, .8))
print(store$gg2)

## ----include=FALSE, eval=eval-------------------------------------------------
# saveRDS(store, '../inst/intro.rds') # rebuild package when done

