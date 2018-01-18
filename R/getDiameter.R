#' @title diameter at height X
#' @description
#' The diameter at height X is calculated for a tree with dimensions <<dbh, H1, D2, H2, h>>.
#' @param speciesID ... BDATBArtNr in BDAT
#' @param dbh ... D1 in BDAT
#' @param h ... H in BDAT
#' @param Hx ...
#' @param H1 ...
#' @param D2 ...
#' @param H2 ...
#' @details ...
#' @return ...
#' @examples ...
#' @export

getDiameter <- function(
  speciesID,
  dbh,
  h,
  Hx,
  H1 = 1.3,
  D2 = 0,
  H2 = 0,
  bark = TRUE
) {

  dat <- data.frame(
    sp = speciesID,
    dbh = dbh,
    h = h,
    Hx = Hx,
    H1 = H1,
    D2 = D2,
    H2 = H2
  )

  if (isTRUE(bark)) {
    diameterS <- sapply(
      1:nrow(dat),
      function(a){
        .Fortran(
          "BDATDmRHx",
          as.integer(dat$sp[a]),
          as.single(dat$dbh[a]),
          as.single(dat$H1[a]),
          as.single(dat$D2[a]),
          as.single(dat$H2[a]),
          as.single(dat$h[a]),
          as.single(dat$Hx[a]),
          IFeh = as.integer(0),
          DmRHx = as.single(0)
        )$DmRHx
      }
    )
  } else {
    diameterS <- sapply(
      1:nrow(dat),
      function(a){
        .Fortran(
          "BDATDoRHx",
          as.integer(dat$sp[a]),
          as.single(dat$dbh[a]),
          as.single(dat$H1[a]),
          as.single(dat$D2[a]),
          as.single(dat$H2[a]),
          as.single(dat$h[a]),
          as.single(dat$Hx[a]),
          IFeh = as.integer(0),
          DoRHx = as.single(0)
        )$DoRHx
      }
    )
  }
}
