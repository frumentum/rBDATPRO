#' @title diameter at height X
#' @description
#' The diameter at height X is calculated for a tree with dimensions <<dbh, H1, D2, H2, h>>.
#' @param speciesID ... BDATBArtNr in BDAT
#' @param D1 ... D1 in BDAT
#' @param H1 ...
#' @param D2 ...
#' @param H2 ...
#' @param H ... H in BDAT
#' @param Hx ...
#' @details ...
#' @return ...
#' @examples ...
#' @export

getDiameter <- function(
  speciesID,
  D1,
  H1 = 1.3,
  D2 = 0,
  H2 = 0,
  H,
  Hx,
  bark = TRUE
) {

  dat <- data.frame(
    sp = speciesID,
    D1 = D1,
    H1 = H1,
    D2 = D2,
    H2 = H2,
    H = H,
    Hx = Hx
  )

  if (isTRUE(bark)) {
    diameterS <- sapply(
      1:nrow(dat),
      function(a){
        .Fortran(
          "BDATDmRHx",
          as.integer(dat$sp[a]),
          as.single(dat$D1[a]),
          as.single(dat$H1[a]),
          as.single(dat$D2[a]),
          as.single(dat$H2[a]),
          as.single(dat$H[a]),
          as.single(dat$Hx[a]),
          wIErr = as.integer(0),
          wDmRHx = as.single(0)
        )$wDmRHx
      }
    )
  } else {
    diameterS <- sapply(
      1:nrow(dat),
      function(a){
        .Fortran(
          "BDATDoRHx",
          as.integer(dat$sp[a]),
          as.single(dat$D1[a]),
          as.single(dat$H1[a]),
          as.single(dat$D2[a]),
          as.single(dat$H2[a]),
          as.single(dat$H[a]),
          as.single(dat$Hx[a]),
          wIErr = as.integer(0),
          wDoRHx = as.single(0)
        )$wDoRHx
      }
    )
  }

  return(diameterS)
}
