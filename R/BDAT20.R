#' @title BDAT 2.0
#' @description
#' Calculates volumes and sorts, useful for forest inventories.
#' @param speciesID ... BDATBArtNr in BDAT
#' @param dbh D1 in BDAT
#' @param h H in BDAT
#' @param H1 ...
#' @param D2 ...
#' @param H2 ...
#' @param hXwood length of X-Wood; Hxh in BDAT
#' @param hInd height indicator; Hkz in BDAT
#' @param trunkInd trunk indicator; Skz in BDAT
#' @param Az What's the appropriate english name for Az?
#' @param hTrunk height of trunk; Hsh in BDAT
#' @param Zsh What's the appropriate english name for Zsh?
#' @param Zab english name for Zab?
#' @param sortInd sorting indicator; Sokz in BDAT
#' @param FixLenDef list with length 4; FixLngDef in BDAT
#' @param NMaxFixLen NMaxFixLng in BDAT
#' @param result maybe a character vector containing "Volume",
#' "DBH", "IFeh", "FixLength" or "NFixLength"; look at BDAT20 documentation
#' @export

BDAT20 <- function(
  speciesID,
  dbh,
  h,
  H1 = 1.3,
  D2 = 0,
  H2 = 0,
  hXwood,
  hInd,
  trunkInd,
  Az = 0,
  hTrunk,
  Zsh = 0,
  Zab = 0,
  sortInd = 0,
  FixLenDef = list(),
  NMaxFixLen,
  result = "Volume"
) {

  dat <- data.frame(
    sp = speciesID,
    dbh = dbh,
    h = h,
    H1 = H1,
    D2 = D2,
    H2 = H2,
    hXwood = hXwood,
    hInd = hInd,
    trunkInd = trunkInd,
    Az = Az,
    hTrunk = hTrunk,
    Zsh = Zsh,
    Zab = Zab,
    sortInd = sortInd,
    FixLenDef = FixLenDef,
    NMaxFixLen = NMaxFixLen
  )

  n = length(speciesID)
  Skl = matrix(rep(1,n*6), ncol=6)
  Vol = matrix(rep(0,n*7), ncol=7)
  FixLng = matrix(rep(0,n*180), ncol=180)
  NFixLng = rep(0,n)
  iErr = rep(0,n)

  # if (result == "Volume") {
    tmp <- sapply(
      1:nrow(dat),
      function(a){
        .Fortran(
          "BDAT20",
          as.integer(dat$sp[a]),
          as.single(dat$dbh[a]),
          as.single(dat$H1[a]),
          as.single(dat$D2[a]),
          as.single(dat$H2[a]),
          as.single(dat$h[a]),
          as.single(dat$hXwood[a]),
          as.integer(dat$hInd[a]),
          as.integer(dat$trunkInd[a]),
          as.single(dat$Az[a]),
          as.single(dat$hTrunk[a]),
          as.single(dat$Zsh[a]),
          as.single(dat$Zab[a]),
          as.integer(dat$sortInd[a]),
          Skl = as.single(Skl),
          Vol = as.single(Vol),
          BHD = as.single(0),
          Ifeh = as.integer(iErr), # in subroutine parameter is called 'Ifeh' with little f
          FixLngDef = as.single(dat$FixLenDef), # how shall this be done vectorized?
          NMaxFixLng = as.integer(dat$NMaxFixLen[a]),
          FixLng = as.single(FixLng),
          NFixLng = as.integer(NFixLng)
        )$Vol
      }
    )
  # }

    return(list(Skl = matrix(tmp$Skl,ncol=6,byrow=T),
                Vol = matrix(tmp$Volumen,ncol=7,byrow=T),
                iErr = tmp$iErr,
                FixLng = matrix(tmp$FixLng,ncol=180,byrow=T),
                NFixLng = tmp$NFixLng))

}
