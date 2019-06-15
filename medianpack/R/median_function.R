#' Monte Carlo Simulation
#' This function recursively generates the values of factorials.
#' @param k a scalar determining the factorials.
#' @return a scalar equivalent to \eqn{n!}.
#' @export
#' @seealso \code{prod}
#' @references
#'
#' @examples
#' ## a trivial example
#' frac(10)
#' prod(1:10)


MCS <- function(initial,mean.logret,sd.logret,steps,paths) {
  rand.ret <- rnorm(steps*paths, mean.logret, sd.logret) # 生成20*200000個常態分佈的亂數
  ret.matrix <- matrix(rand.ret, steps, paths)
  tree <- data.frame(initial * exp(apply(ret.matrix, 2, cumsum)))
}
