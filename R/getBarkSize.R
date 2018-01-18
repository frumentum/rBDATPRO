#' @title double size of bark at height X
#' @description The bark's size at height X is doubled for a tree with dimensions <<dbh, H1, D2, H2, h>>.
#' @param speciesID species code from BDAT
#' @param D1 diameter at H1 (usually at breast height (dbh))
#' @param H1 height 1
#' @param D2 ...
#' @param H2 ...
#' @param H total height
#' @param Hx height X
#' @details ...
#' @return double size of bark at height x
#' @export

getBarkSize <- function(
  speciesID,
  D1,
  H1 = 1.3,
  D2 = 0,
  H2 = 0,
  H,
  Hx
){
  dat <- data.frame(
    sp = speciesID,
    D1 = D1,
    H1 = H1,
    D2 = D2,
    H2 = H2,
    H = H,
    Hx = Hx
  )
  ## get height of diameter inside stem
  barkSizeS <- sapply(
    1:nrow(dat),
    function(a){
      .Fortran(
        "BDATRinde2Hx",
        as.integer(dat$sp[a]),
        as.single(dat$D1[a]),
        as.single(dat$H1[a]),
        as.single(dat$D2[a]),
        as.single(dat$H2[a]),
        as.single(dat$H[a]),
        as.single(dat$Hx[a]),
        wIErr = as.integer(0),
        wRinde2Hx = as.single(0)
      )$wRinde2Hx
    }
  )

  return(barkSizeS)
}
