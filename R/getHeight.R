#' @title Height of given diameter inside tree taper
#' @description this function calculates the height of a given diameter inside
#' a tree taper of given dimensions (dbh, h)
#' @param speciesID species code from BDAT
#' @param dbh diameter in breast height (dbh) from tree
#' @param h height of tree
#' @param Dx diameter for which height is required
#' @param H1 ...
#' @param D2 ...
#' @param H2 ...
#' @return height of diameter dx inside stem taper
#' @export

getHeight <- function(
  speciesID,
  dbh,
  h,
  Dx,
  H1 = 1.3,
  D2 = 0,
  H2 = 0
){
  dat <- data.frame(
    sp = speciesID,
    dbh = dbh,
    h = h,
    Dx = Dx,
    H1 = H1,
    D2 = D2,
    H2 = H2
  )
  ## get height of diameter inside stem
  heightS <- sapply(
    1:nrow(dat),
    function(a){
      .Fortran(
        "FNBDATHxDx",
        as.integer(dat$sp[a]),
        as.single(dat$dbh[a]),
        as.single(dat$H1[a]),
        as.single(dat$D2[a]),
        as.single(dat$H2[a]),
        as.single(dat$h[a]),
        as.single(dat$Hx[a]),
        Dx = as.single(0),
        IErr = as.integer(0)
      )$Dx
    }
  )

  return(heightS)
}
