#' @title BDAT sort function
#' @description Vectorized BDAT sort function
#' @param sp ...
#' @param d ...
#' @param h ...
#'
#' @export

getSort <- function(
  sp,
  d,
  h,
  hx,
  H1 = 1.3,
  D2 = 0,
  H2 = 0,
  Hkz = 0,
  Skz = 0,
  Az = 0,
  Hsh = 0.1,
  Zsh = 0,
  Zab = 7,
  Sokz = 1,
  FixLngDef = 0,
  NMaxFixLng = 0
) {

  dat <- data.frame(BDATArt = sp,
                    dbh = d,
                    h = h,
                    hx = hx,
                    H1 = H1,
                    D2 = D2,
                    H2 = H2,
                    Hkz = Hkz,
                    Skz = Skz,
                    Az = Az,
                    Hsh = Hsh,
                    Zsh = Zsh,
                    Zab = Zab,
                    Sokz = Sokz,
                    FixLngDef = FixLngDef,
                    NMaxFixLng = NMaxFixLng)

  eval(loadBDAT("V_BDAT20"))

  S <- sapply(1:nrow(dat), function(a) {
    V_BDAT20(BDATBArtNr = dat$BDATArt[a],
             D1 = dat$dbh[a],
             H1 = dat$H1[a],
             D2 = dat$D2[a],
             H2 = dat$H2[a],
             H = dat$h[a],
             Hkz = dat$Hkz[a],
             Skz = dat$Skz[a],
             Az = dat$Az[a],
             Hsh = dat$Hsh[a],
             Zsh = dat$Zsh[a],
             Zab = dat$Zab[a],
             Sokz = dat$Sokz[a],
             FixLngDef = dat$FixLngDef[a],
             NMaxFixLng = dat$NMaxFixLng[a])[1]
  })

  return(S)
}
