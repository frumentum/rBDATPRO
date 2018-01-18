#' @title shortcut wrapper for \code{getVolume}
#' @description Default values for H1, D2, H2
#' @param speciesID ...
#' @param dbh ... and here the one of dbh
#' @param h ...
#' @param H1 maybe just link to BDAT20 for details of H1, D2, H2, IErr
#' @param D2 ...
#' @param H2 ...
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

getVolumenShort <- function(
  speciesID,
  dbh,
  h,
  lh,
  uh,
  H1 = 1.3,
  D2 = 0,
  H2 = 0,
  SecLen = 0.1,
  bark = T
) {

  volumeS <- getVolume(
    speciesID = speciesID,
    D1 = dbh,
    H1 = H1,
    D2 = D2,
    H2 = H2,
    H = h,
    lh = lh,
    uh = uh,
    SecLen = SecLen,
    bark = bark
  )

  return(volumeS)
}
