% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/p_mediation.R
\name{p_mediation}
\alias{p_mediation}
\alias{gen_mediation}
\title{p-value from three-variable mediation analysis simulation}
\usage{
p_mediation(
  n,
  a,
  b,
  cprime,
  dichotomous.X = FALSE,
  two.tailed = TRUE,
  method = "wald",
  sd.X = 1,
  sd.Y = 1,
  sd.M = 1,
  gen_fun = gen_mediation,
  ...
)

gen_mediation(
  n,
  a,
  b,
  cprime,
  dichotomous.X = FALSE,
  sd.X = 1,
  sd.Y = 1,
  sd.M = 1,
  ...
)
}
\arguments{
\item{n}{total sample size unless \code{dichotomous.X = TRUE}, in which the
value represents the size per group}

\item{a}{regression coefficient for the path X -> M}

\item{b}{regression coefficient for the path M -> Y}

\item{cprime}{partial regression coefficient for the path X -> Y}

\item{dichotomous.X}{logical; should the X variable be generated as though it
were dichotomous? If TRUE then \code{n} represents the sample size per group}

\item{two.tailed}{logical; should a two-tailed or one-tailed test be used?}

\item{method}{type of inferential method to use. Default uses the Wald
(a.k.a., Sobel) test}

\item{sd.X}{standard deviation for X}

\item{sd.Y}{standard deviation for Y}

\item{sd.M}{standard deviation for M}

\item{gen_fun}{function used to generate the required two-sample data.
Object returned must be a \code{data.frame} with the columns
\code{"DV"} and \code{"group"}. Default uses \code{\link{gen_mediation}}
to generate conditionally Gaussian distributed samples.
User defined version of this function must include the argument \code{...}}

\item{...}{additional arguments to be passed to \code{gen_fun}. Not used
unless a customized \code{gen_fun} is defined}
}
\value{
a single p-value
}
\description{
Simple 3-variable mediation analysis simulation to test the hypothesis that
X -> Y is mediated by the relationship X -> M -> Y. Currently,
M and Y are assumed to be continuous variables with Gaussian errors, while
X may be continuous or dichotomous.
}
\examples{

# joint test H0: a*b = 0
p_mediation(50, a=sqrt(.35), b=sqrt(.35), cprime=.39)
p_mediation(50, a=sqrt(.35), b=sqrt(.35), cprime=.39, dichotomous.X=TRUE)

\donttest{

  # power to detect mediation
  p_mediation(n=50, a=sqrt(.35), b=sqrt(.35), cprime=.39) |>
    Spower(parallel=TRUE, replications=1000)

  # sample size estimate for .95 power
  p_mediation(n=NA, a=sqrt(.35), b=sqrt(.35), cprime=.39) |>
    Spower(power=.95, interval=c(50, 200), parallel=TRUE)

}

}
\seealso{
\code{\link{gen_mediation}}
}
\author{
Phil Chalmers \email{rphilip.chalmers@gmail.com}
}
