#' @title get coarse wood volume of tree
#'
#' @description This function calculates the coarse wood volume of a tree given
#' species, diameter and height. One can choose if bark should be included or
#' excluded via the boolean parameter bark, which will include bark if it is set
#' to TRUE.
#'
#' @param sp species code from BDAT
#' @param d diameter in breast height (dbh) from tree
#' @param h height of tree
#' @param H1 height where \code{d} was measured; default is 1.3[m] for dbh
#' @param D2 diameter at second height \code{H2}; if \code{D2 = 0} (default)
#' taper form from the first german NFI ('Bundeswaldinventur I') are used
#' @param H2 measuring height of D2, default is 0
#' @param SegLen segment length for calculating the volume
#' @param bark boolean, if TRUE volume including bark is calculated (default)
#' @details
#' Strictly speaking \code{getDhVol} is a wrapper function. It loads the fortran
#' subroutine \code{BDATVOLDHMR} or \code{BDATVOLDHOR} (depending on whether or
#' not bark is set to TRUE or FALSE) within a call of \code{loadBDAT}.
#' @return volume with or without bark of given segment AB in cubic meter
#' @examples
#' getDhVol(sp=1, d=30, h=40, bark = T)
#' @export

getDhVol <- function(
  sp,
  d,
  h,
  H1 = 1.3,
  D2 = 0,
  H2 = 0,
  DHGrz = 7,
  HDHGrz = NULL,
  SekLng = 0.1,
  bark = TRUE
){
  # at first load BDAT
  if (isTRUE(bark))
    eval(loadBDAT(fun = "BDATVOLDHMR"))
  else
    eval(loadBDAT(fun = "BDATVOLDHOR"))

  if(is.null(HDHGrz)){
    ## FIX: get HDHGrz from respective function
    HDHGrz=0.9*h #this is just to remove NULL
  }
  # build data.frame with tree data
  get_vol_dat <- data.frame(BDATArt = sp,
                            dbh = d,
                            h = h,
                            H1 = H1,
                            D2 = D2,
                            H2 = H2,
                            SekLng = SekLng,
                            DHGrz = DHGrz,
                            HDHGrz = HDHGrz)

  if (isTRUE(bark)){
    vol <- sapply(1:nrow(get_vol_dat), function(a){
      BDATVOLDHMR(wBDATBArtNr = get_vol_dat$BDATArt[a],
                  wD1 = get_vol_dat$dbh[a],
                  wH1 = get_vol_dat$H1[a],
                  wD2 = get_vol_dat$D2[a],
                  wH2 = get_vol_dat$H2[a],
                  wHges = get_vol_dat$h[a],
                  wDHGrz = get_vol_dat$DHGrz[a],
                  wHDHGrz = get_vol_dat$HDHGrz[a],
                  wSekLng = get_vol_dat$SekLng[a],
                  wIErr = 0, # because it's a fortran output variable
                  wVolDHmR = 0)}) # fortran output variable
  } else {
    vol <- sapply(1:nrow(get_vol_dat), function(a){
      BDATVOLDHOR(wBDATBArtNr = get_vol_dat$BDATArt[a],
                  wD1 = get_vol_dat$dbh[a],
                  wH1 = get_vol_dat$H1[a],
                  wD2 = get_vol_dat$D2[a],
                  wH2 = get_vol_dat$H2[a],
                  wHges = get_vol_dat$h[a],
                  wDHGrz = get_vol_dat$DHGrz[a],
                  wHDHGrz = get_vol_dat$HDHGrz[a],
                  wSekLng = get_vol_dat$SekLng[a],
                  wIErr = 0, # fortran output variable
                  wVolDHoR = 0)}) # fortran output variable
  }

  ## add indicator, whether calculated volume includes bark (=> TRUE)
  attr(vol, "bark") <- bark

  return(vol)
}
