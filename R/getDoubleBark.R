#' @title get double bark thickness inside tree taper
#'
#' @description This function calculates bark's double thickness of a given
#' diameter inside for a given tree of dimensions (dbh, h).
#'
#' @param sp species code from BDAT
#' @param d diameter in breast height (dbh) of tree
#' @param h height of tree
#' @param hx height for which diameter is required
#' @param H1 height where \code{d} was measured; default is 1.3[m] for dbh
#' @param D2 diameter at second height \code{H2}; if \code{D2 = 0} (default)
#' taper form from the first NFI ('Bundeswaldinventur I') is used
#' @param H2 according to D2, default is 0
#' @details
#' Strictly speaking \code{getH} is a wrapper function. It loads the fortran
#' subroutine \code{BDATHXDX} within a call of \code{loadBDAT}.
#' @return height of diameter \code{dx}
#' @examples
#' getDoubleBark(sp=1, d=30, h=40, hx=20)
#' @export

getDoubleBark <- function(
  sp,
  d,
  h,
  hx,
  H1 = 1.3,
  D2 = 0,
  H2 = 0
) {

  dat <- data.frame(BDATArt = sp,
                    dbh = d,
                    h = h,
                    hx = hx,
                    H1 = H1,
                    D2 = D2,
                    H2 = H2)

  eval(loadBDAT("BDATRINDE2HX"))

  ## get diameter at height x
  D <- sapply(1:nrow(dat), function(a) {
    BDATRINDE2HX(BDATBArtNr = dat$BDATArt[a],
                 D1 = dat$dbh[a],
                 H1 = dat$H1[a],
                 D2 = dat$D2[a],
                 H2 = dat$H2[a],
                 Hges = dat$h[a],
                 Hx = dat$hx[a])[1]
  })

  return(D)

}
