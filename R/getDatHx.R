#' @title get diameter of given height inside tree taper w/ or w/o bark
#'
#' @description This function calculates the diameter of a given height inside
#' for a given tree of dimensions (dbh, h).
#'
#' @param sp species code from BDAT
#' @param d diameter in breast height (dbh) of tree
#' @param h height of tree
#' @param hx height for which diameter is required
#' @param H1 height where \code{d} was measured; default is 1.3[m] for dbh
#' @param D2 diameter at second height \code{H2}; if \code{D2 = 0} (default)
#' taper form from the first NFI ('Bundeswaldinventur I') is used
#' @param H2 according to D2, default is 0
#' @param bark boolean, if TRUE volume including bark is calculated (default)
#' @details
#' Strictly speaking \code{getDatHx} is a wrapper function. It loads the fortran
#' subroutine \code{BDATDMRHX} or \code{BDATDORHX} (depending on whether or not
#' bark is set to TRUE or FALSE) within a call of \code{loadBDAT}.
#' @return diameter at height \code{hx}
#' @examples
#' getDatHx(sp=1, d=30, h=40, hx=20)
#' @export

getDatHX <- function(
  sp,
  d,
  h,
  hx,
  H1 = 1.3,
  D2 = 0,
  H2 = 0,
  bark = TRUE
) {

  dat <- data.frame(BDATArt = sp,
                    dbh = d,
                    h = h,
                    hx = hx,
                    H1 = H1,
                    D2 = D2,
                    H2 = H2)


  if (bark == TRUE) {
    # load necessary function from BDAT dll
    eval(loadBDAT("BDATDMRHX"))

    ## get diameter at height x
    D <- sapply(1:nrow(dat), function(a) {
      BDATDMRHX(BDATBArtNr = dat$BDATArt[a],
                D1 = dat$dbh[a],
                H1 = dat$H1[a],
                D2 = dat$D2[a],
                H2 = dat$H2[a],
                Hges = dat$h[a],
                Hx = dat$hx[a],
                IErr = 0, # IErr is output variable
                DmRHx = 0)[1] # fortran output variable
    })
  }

  if (bark == FALSE) {
    # load necessary function from BDAT dll
    eval(loadBDAT("BDATDORHX"))

    ## get diameter at height x
    D <- sapply(1:nrow(dat), function(a) {
      BDATDORHX(BDATBArtNr = dat$BDATArt[a],
                D1 = dat$dbh[a],
                H1 = dat$H1[a],
                D2 = dat$D2[a],
                H2 = dat$H2[a],
                Hges = dat$h[a],
                Hx = dat$hx[a],
                IErr = 0, # IErr is output variable
                DoRHx = 0)[1] # fortran output variable
    })
  }

  return(D)

}
