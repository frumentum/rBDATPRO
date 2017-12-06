#' @title Height of given diameter inside tree taper
#'
#' @description this function calculates the height of a given diameter inside
#' a tree taper of given dimensions (dbh, h)
#'
#' @param sp species code from BDAT
#' @param d diameter in breast height (dbh) from tree
#' @param h height of tree
#' @param dx diameter for which height is required
#' @export
#' @return height of diameter dx inside stem taper

get_height_of_d <- function(
  sp,
  d,
  h,
  dx,
  H1 = 1.3,
  D2 = 0,
  H2 = 0,
  Hx = 0,
  IFeh = 0
){
  # at first, use loadBDAT for loading fortran function in global.env
  loadBDAT(fun = "BDATHXDX")

  dat <- data.frame(BDATArt = sp,
                    dbh = d,
                    h = h,
                    dx = dx,
                    H1 = H1,
                    D2 = D2,
                    H2 = H2,
                    Hx = Hx,
                    IFeh = IFeh)
  ## get height of diameter inside stem
  H <- sapply(1:nrow(dat), function(a) {
    BDATHXDX(BDATBArtNr = dat$BDATArt[a],
             D1 = dat$dbh[a],
             H1 = dat$H1[a],
             D2 = dat$D2[a],
             H2 = dat$H2[a],
             H = dat$h[a],
             Hx = dat$Hx[a],
             Dx = dat$dx[a],
             IFeh = dat$IFeh[a])[1]
  })
  # remove BDATHXDX before returning the result
  rm(BDATHXDX, envir = .GlobalEnv)
  return(H)
}
