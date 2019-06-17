\name{MCS}
\alias{MCS}
\title{Monte Carlo Simulation}
\usage{
MCS(initial, mean, sd, steps = 2, paths = 5, log = FALSE)
}
\description{
Applying Monte Carlo method to simulate the price paths.
Generate a steps*paths matrix with each element following iid N(log(mean), log(sd)^2).
}
\examples{
MCS(initial = 30, mean = 0.004, sd = 0.005, steps = 20, paths = 20000)
}
\arguments{
  \item{initial}{current price}
  \item{mean}{expected return}
  \item{sd}{volatility of return}
  \item{steps}{steps of each simulation}
  \item{paths}{number of simulations}
  \item{log}{log the simulation data}
}
