#' @title Volume section A to B
#' @description
#' Calculates tree's volume beginning from section A to B.
#' Balk is included.
#' @param speciesID here comes the description of speciesID...
#' @param D1 ... and here the one of bhd
#' @param H1 maybe just link to BDAT20 for details of H1, D2, H2, IErr
#' @param D2 ...
#' @param H2 ...
#' @param H ...
#' @param lh ...
#' @param uh ...
#' @param SecLng section length, default is 0.1m...
#' @param bark logical... is output attr(, "bark") really necessary???
#' @details
#' maybe some details? e.g. that function is vectorized and therefore one
#' can simply use a numeric vector as input (not only one number).
#' But using a numeric vector assumes all vectors need to be from  the same length.
#' @return
#' which values will be returned?
#' @examples
#' one example would be nice
#' @useDynLib rBDATPRO
#' @export

getVolume <- function(
  speciesID,
  D1,
  H1 = 1.3,
  D2 = 0,
  H2 = 0,
  H,
  lh,
  uh,
  SecLen = 0.1,
  bark = T
){

  dat <- data.frame(
    sp = speciesID,
    D1 = D1,
    H1 = H1,
    D2 = D2,
    H2 = H2,
    H = H,
    lh = lh,
    uh = uh,
    SecLen = SecLen
  )

  if (isTRUE(bark)) {
    volumeS <- sapply(
      1:nrow(dat),
      function(a){
        .Fortran(
          "BDATVolABmR",
          as.integer(dat$sp[a]),
          as.single(dat$D1[a]),
          as.single(dat$H1[a]),
          as.single(dat$D2[a]),
          as.single(dat$H2[a]),
          as.single(dat$H[a]),
          as.single(dat$lh[a]),
          as.single(dat$uh[a]),
          as.single(dat$SecLen[a]),
          IFeh = as.integer(0),
          VolABmR = as.single(0)
        )$VolABmR
      }
    )
  } else {
    volumeS <- sapply(
      1:nrow(dat),
      function(a){
        .Fortran(
          "BDATVolABoR",
          as.integer(dat$sp[a]),
          as.single(dat$D1[a]),
          as.single(dat$H1[a]),
          as.single(dat$D2[a]),
          as.single(dat$H2[a]),
          as.single(dat$H[a]),
          as.single(dat$lh[a]),
          as.single(dat$uh[a]),
          as.single(dat$SecLen[a]),
          IFeh = as.integer(0),
          VolABoR = as.single(0)
        )$VolABoR
      }
    )
  }

  ## there might be equal values in a and b, and the calculated value should be
  ## zero: BDAT obviously is not able to produce zero --> override such case!
  volumeS <- ifelse(dat$lh == dat$uh, 0, volumeS)
  ## add indicator, whether calculated volume includes bark (=> TRUE)
  attr(volumeS, "bark") <- bark # really necessary??

  return(volumeS)
}
