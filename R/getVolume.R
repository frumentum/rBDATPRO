#' @title Volume of tree segment
#'
#' @description This function calculates the volume of a tree segment given
#'  species, diameter, height (NOT YET: d03), beginning and end height
#' of the segment. One can choose if bark should be included or excluded via
#' the boolean parameter bark, which will include bark if it is set to TRUE.
#'
#' @param sp species code from BDAT
#' @param d diameter in breast height (dbh) from tree
#' @param h height of tree
#' @param lh lower height in tree of segment for which volume is required
#' @param uh upper height in tree of segment for which volume is required
#' @param H1 height where \code{d} was measured; default is 1.3[m] for dbh
#' @param D2 diameter at second height \code{H2}; if \code{D2 = 0} (default)
#' data from the first BWI (Bundeswaldinventur) are used
#' @param H2 according to D2, default is 0
#' @param SeqLen the tree is divided in \code{tree height / SeqLen}
#' compartiments so \code{SeqLen} influences the calculated precision
#' @param bark boolean, if TRUE volume including bark is calculated (default)
#' @return volume with bark of given segment inside stem in cubic meter
#' @examples
#' getVolume(1, 30, 40, 10, 20, bark = T)
#' @export

getVolume <- function(
  sp,
  d,
  h,
  lh,
  uh,
  H1 = 1.3,
  D2 = 0,
  H2 = 0,
  SekLng = 0.1,
  VolABmR = 0,
  VolABoR = 0,
  bark = TRUE
){
  # at first load BDAT
  if (isTRUE(bark)) {
    loadBDAT(fun = "BDATVOLABMR")
  } else loadBDAT(fun = "BDATVOLABOR")
  get_vol_dat <- data.frame(BDATArt = sp,
                            dbh = d,
                            h = h,
                            a = lh,
                            b = uh,
                            H1 = H1,
                            D2 = D2,
                            H2 = H2,
                            wSekLng = SekLng)

  if (isTRUE(bark)){
    vol <- sapply(1:nrow(get_vol_dat), function(a){
      BDATVOLABMR(wBDATBArtNr = get_vol_dat$BDATArt[a],
                  wD1 = get_vol_dat$dbh[a],
                  wH1 = get_vol_dat$H1[a],
                  wD2 = get_vol_dat$D2[a],
                  wH2 = get_vol_dat$H2[a],
                  wHges = get_vol_dat$h[a],
                  wA = get_vol_dat$a[a],
                  wB = get_vol_dat$b[a],
                  wSekLng = get_vol_dat$wSekLng[a],
                  wIErr = 0, # because it's a fortran output variable
                  wVolABmR = 0)}) # fortran output variable
  } else {
    vol <- sapply(1:nrow(get_vol_dat), function(a){
      BDATVOLABOR(wBDATBArtNr = get_vol_dat$BDATArt[a],
                  wD1 = get_vol_dat$dbh[a],
                  wH1 = get_vol_dat$H1[a],
                  wD2 = get_vol_dat$D2[a],
                  wH2 = get_vol_dat$H2[a],
                  wHges = get_vol_dat$h[a],
                  wA = get_vol_dat$a[a],
                  wB = get_vol_dat$b[a],
                  wSekLng = get_vol_dat$wSekLng[a],
                  wIErr = 0, # fortran output variable
                  wVolABoR = 0)}) # fortran output variable
  }
  ## there might be equal values in a and b, and the calculated value should be
  ## zero: BDAT obviously is not able to produce zero --> override such case!
  vol <- ifelse(get_vol_dat$a == get_vol_dat$b, 0, vol)
  ## add indicator, whether calculated volume includes bark (=> TRUE)
  attr(vol, "bark") <- bark

  # remove fortran function from global.env
  if(isTRUE(bark)) rm(BDATVOLABMR, envir = .GlobalEnv) else
    rm(BDATVOLABOR, envir = .GlobalEnv)

  return(vol)
}
