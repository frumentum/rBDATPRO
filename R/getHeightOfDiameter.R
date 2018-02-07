#' @title Height of given diameter inside tree taper
#'
#' @description This function calculates the height of a given diameter inside
#' a tree taper of given dimensions (dbh, h)
#'
#' @param sp species code from BDAT
#' @param d diameter in breast height (dbh) from tree
#' @param h height of tree
#' @param dx diameter for which height is required
#' @param H1 height where \code{d} was measured; default is 1.3[m] for dbh
#' @param D2 diameter at second height \code{H2}; if \code{D2 = 0} (default)
#' data from the first BWI (Bundeswaldinventur) are used
#' @param H2 according to D2, default is 0
#' @return height of diameter dx inside stem taper
#' @examples
#' getHeightOfDiameter(1, 30, 40, 20)
#' @export

getHeightOfDiameter <- function(
  sp,
  d,
  h,
  dx,
  H1 = 1.3,
  D2 = 0,
  H2 = 0
){
  # at first, use loadBDAT for loading fortran function in global.env
  loadBDAT(fun = "BDATHXDX")

  dat <- data.frame(BDATArt = sp,
                    dbh = d,
                    h = h,
                    dx = dx,
                    H1 = H1,
                    D2 = D2,
                    H2 = H2)
  ## get height of diameter inside stem
  H <- sapply(1:nrow(dat), function(a) {
    BDATHXDX(BDATBArtNr = dat$BDATArt[a],
             D1 = dat$dbh[a],
             H1 = dat$H1[a],
             D2 = dat$D2[a],
             H2 = dat$H2[a],
             H = dat$h[a],
             Hx = 0, # fortran output variable
             Dx = dat$dx[a],
             IFeh = 0)[1] # IFeh is output variable
  })
  # remove BDATHXDX before returning the result
  rm(BDATHXDX, envir = .GlobalEnv)
  return(H)
}
