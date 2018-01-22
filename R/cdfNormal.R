#' @title test function for cdf
#' @description cdf implemented in R and fortran
#' @param q vector of quantiles
#' @param mean vector of means, default is 0
#' @param sd vector of standard deviations, default is 1
#' @param type character vector: 'R' or 'Fortran'
#' @examples
#' cdfNormal(1, type = "R")
#' cdfNormal(1, type = "Fortran")
#' @export

cdfNormal <- function(q, mean = 0, sd = 1, type = c("Fortran", "R")) {
  type <- match.arg(type)
  # stopifnot(length(n) == 1)

  if(type == "Fortran") {
    out <- .Fortran(
      "normal_cdf",
      x = as.double(q),
      a = as.double(mean),
      b = as.double(sd),
      cdf = as.double(0)
    )$cdf
    out <- as.numeric(out)
  }

  if(type == "R") {
    out <- pnorm(q, mean, sd)
  }

  return(out)
}
