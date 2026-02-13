## ----include=FALSE------------------------------------------------------------
library(Spower)
set.seed(42)
formals(SpowerCurve)$plotly <- FALSE

## ----include=FALSE------------------------------------------------------------
eval <- as.logical(Sys.getenv("SPOWER_EVAL"))
if(is.na(eval)) eval <- FALSE  # set to FALSE for normal run
store <- list()
if(!eval)
	store <- readRDS(system.file("intro3.rds", package = 'Spower'))

## ----include=eval-------------------------------------------------------------
getwd()

## ----eval=eval----------------------------------------------------------------
# l_two.t_correct.sign <- function(n, mean, mu = 0, alpha = .05, ...){
# 	while(TRUE){
# 		g1 <- rnorm(n)
# 		g2 <- rnorm(n, mean=mean)
# 		out <- t.test(g2, g1, mu=mu, ...)
# 		if(out$p.value < alpha) break   # if "significant" then break while() loop
# 	}
# 	mean_diff <- unname(out$estimate[1] - out$estimate[2])
# 	mean_diff > mu                      # return TRUE if the correct sign is observed
# }

## ----eval=eval----------------------------------------------------------------
# l_two.t_correct.sign(n=15, mean=.2) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$typeS <- getLastSpower()
typeS <- store$typeS
print(typeS)

## ----eval=eval----------------------------------------------------------------
# typeS <- .01
# l_two.t_correct.sign(n=NA, mean=.2) |>
# 	Spower(power=1-typeS, interval=c(10, 200))

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$typeS.N <- getLastSpower()
typeS.N <- store$typeS.N
print(typeS.N)

## -----------------------------------------------------------------------------
l_two.t_correct.sign <- function(n, mean, mu = 0, alpha = .05, ...){
	while(TRUE){
		# return_analysis argument used to return model object
		out <- p_t.test(n=n, d=mean, mu=mu, return_analysis=TRUE, ...)
		if(out$p.value < alpha) break
	}
	mean_diff <- unname(out$estimate[1] - out$estimate[2])
	mean_diff > mu
}

## -----------------------------------------------------------------------------
l_two.t_correct.sign(100, mean=.5)

## ----eval=eval----------------------------------------------------------------
# l_two.t_correct.sign(n=15, mean=.2) |> Spower()

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$typeS2 <- getLastSpower()
typeS2 <- store$typeS2
print(typeS2)

## ----eval=eval----------------------------------------------------------------
# l_two.t_typeM <- function(n, mean, mu = 0,
# 						  alpha = .05, M.ratio = 3, ...){
# 	while(TRUE){
# 		# return_analysis argument used to return model object
# 		out <- p_t.test(n=n, d=mean, mu=mu, return_analysis=TRUE, ...)
# 		if(out$p.value < alpha) break
# 	}
# 	diff <- unname(out$estimate[1] - out$estimate[2])
# 	M <- abs(diff)/mean
# 	# return data.frame, where "retain" indicates the (logical) power information
# 	data.frame(retain=M < M.ratio, M=M)
# }

## ----eval=eval----------------------------------------------------------------
# # only use the "retain" information to compute power, though store the rest
# l_two.t_typeM(n=50, mean=.2) |> Spower(select='retain') -> typeM
# typeM

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$typeM <- getLastSpower()
typeM <- store$typeM
print(typeM)

## -----------------------------------------------------------------------------
results <- SimResults(typeM)
results
subset(results, select=M) |> descript()
with(results, hist(M, 30))

## ----eval=eval----------------------------------------------------------------
# # double the total sample size
# l_two.t_typeM(n=100, mean=.2) |> Spower(select='retain') -> typeM2
# typeM2

## ----echo=FALSE---------------------------------------------------------------
if(eval) store$typeM2 <- getLastSpower()
typeM2 <- store$typeM2
print(typeM2)

## ----eval=FALSE---------------------------------------------------------------
# last <- getLastSpower()
# 1 - last$power

## ----echo=FALSE---------------------------------------------------------------
1 - typeM2$power

## -----------------------------------------------------------------------------
results <- SimResults(typeM2)
results
subset(results, select=M) |> descript()
with(results, hist(M, 30))

## ----include=FALSE, eval=eval-------------------------------------------------
# saveRDS(store, '../inst/intro3.rds') # rebuild package when done

