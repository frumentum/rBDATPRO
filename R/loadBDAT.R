#' @title load BDATpro
#'
#' @description
#' \code{loadBDAT} imports a dll file which contains a bunch of functions for
#' modelling different biomass parts.
#' @param path path to BDATPRO_R.dll which need to be installed on your computer (use 32bit R version). see details
#' @param fun name of the function as character string(s) which shall be loaded into global environment. see details
#' @details
#' \code{path} default is \code{"C:/Program Files (x86)/BDATPRO/R/BDATPRO_R.dll"} which is the link to installed BDAT on your FVA computer. \cr
#' \code{fun} needs at least one of the following character strings to load a fortran function of BDAT in your global environment: \cr
#' \itemize{
#'   \item 'BDATORHX': diameter without bark in height x
#'   \item 'BDATMRHX': diameter with bark in height x
#'   \item 'BDATVOLABOR': volume without bark between height A und B
#'   \item 'BDATVOLABMR': volume with bark between height A und B
#'   \item 'BDATRINDE2HX': double bark thickness at height X
#'   \item 'BDATHXDX': get height x for given diameter x
#'   \item 'V_BDAT20': sort function inclusive fix length
#' }
#' @return Returns a fortran function translated in R and loads it into your global environment.
#' @examples loadBDAT(fun = 'BDATDORHX')
#' @export

# don't know how to call @useDynLib
# @useDynLib '/dll/BDATPRO_R.dll'

loadBDAT <- function(fun) {

  load_dll()

  # Funktion laden: Durchmesser ohne Rinde in Höhe x
  if ("BDATDORHX" %in% fun) {
    .GlobalEnv$BDATDORHX <- function(
      wBDATBArtNr,
      wD1,
      wH1,
      wD2,
      wH2,
      wHges,
      wHx,
      wIErr,
      wDoRHx
    ) {
      .C("R_BDATDORHX",
         as.integer(wBDATBArtNr),
         as.single(wD1),
         as.single(wH1),
         as.single(wD2),
         as.single(wH2),
         as.single(wHges),
         as.single(wHx),
         as.integer(wIErr),
         DoRHx = as.single(wDoRHx))$DoRHx

    }
  }

  # Funktion laden: Durchmesser mit Rinde in Höhe x
  if ("BDATDMRHX" %in% fun) {
    .GlobalEnv$BDATDMRHX <- function(
      wBDATBArtNr,
      wD1,
      wH1,
      wD2,
      wH2,
      wHges,
      wHx,
      wIErr,
      wDmRHx
    ) {
      .C("R_BDATDMRHX",
         as.integer(wBDATBArtNr),
         as.single(wD1),
         as.single(wH1),
         as.single(wD2),
         as.single(wH2),
         as.single(wHges),
         as.single(wHx),
         as.integer(wIErr),
         DmRHx = as.single(wDmRHx))$DmRHx
    }
  }

  # Funktion laden: Abschnittsvolumen mit Rinde zwischen Höhe A und B
  if ("BDATVOLABMR" %in% fun) {
    .GlobalEnv$BDATVOLABMR <- function(
      wBDATBArtNr,
      wD1,
      wH1,
      wD2,
      wH2,
      wHges,
      wA,
      wB,
      wSekLng,
      wIErr,
      wVolABmR
    ){
      .C("R_BDATVOLABMR",
         as.integer(wBDATBArtNr),
         as.single(wD1),
         as.single(wH1),
         as.single(wD2),
         as.single(wH2),
         as.single(wHges),
         as.single(wA),
         as.single(wB),
         as.single(wSekLng),
         as.integer(wIErr),
         VolABmR = as.single(wVolABmR))$VolABmR
    }
  }

  # Funktion laden: Abschnittsvolumen ohne Rinde zwischen Höhe A und B
  if ("BDATVOLABOR" %in% fun) {
    .GlobalEnv$BDATVOLABOR <- function(
      wBDATBArtNr,
      wD1,
      wH1,
      wD2,
      wH2,
      wHges,
      wA,
      wB,
      wSekLng,
      wIErr,
      wVolABoR
    ){
      .C("R_BDATVOLABOR",
         as.integer(wBDATBArtNr),
         as.single(wD1),
         as.single(wH1),
         as.single(wD2),
         as.single(wH2),
         as.single(wHges),
         as.single(wA),
         as.single(wB),
         as.single(wSekLng),
         as.integer(wIErr),
         VolABoR = as.single(wVolABoR))$VolABoR
    }
  }

  # Funktion laden: Für einen Baum mit den Dimensionsdaten <<D1,H1,D2,H2,H>> wird die
  #doppelte Rindenstärke [cm] an der Stelle Hx [m] berechnet.
  if ("BDATRINDE2HX" %in% fun) {
    .GlobalEnv$BDATRINDE2HX <- function(
      wBDATBArtNr,
      wD1,
      wH1,
      wD2,
      wH2,
      wHges,
      wHx,
      wIErr,
      wRinde2Hx
    ){
      .C("R_BDATRINDE2HX",
         as.integer(wBDATBArtNr),
         as.single(wD1),
         as.single(wH1),
         as.single(wD2),
         as.single(wH2),
         as.single(wHges),
         as.single(wHx),
         as.integer(wIErr),
         Rinde2Hx = as.single(wRinde2Hx))$Rinde2Hx
    }
  }


  # Funktion: Höhe x bei gegebenem Durchmesser Dx iterativ bestimmen
  if ("BDATHXDX" %in% fun) {
    .GlobalEnv$BDATHXDX <- function(
      BDATBArtNr,
      D1,
      H1,
      D2,
      H2,
      H,
      Hx,
      Dx,
      IFeh
    ) {
      .C("R_BDATHXDX",
         as.integer(BDATBArtNr),
         as.single(D1),
         as.single(H1),
         as.single(D2),
         as.single(H2),
         as.single(H),
         Hx = as.single(Hx),
         as.single(Dx),
         as.integer(IFeh))$Hx
    }
  }

  # BDAT-Sortierfunktion inkl. Fixlängen
  if ("V_BDAT20" %in% fun) {
    .GlobalEnv$V_BDAT20 <- function(
      wBDATBArtNr,
      D1,
      H1,
      D2,
      H2,
      H,
      Hx,
      Hkz,
      Skz,
      Az,
      Hsh,
      Zsh,
      Zab,
      Sokz,
      FixLngDef,
      NMaxFixLng
    )
    {
      n = length(wBDATBArtNr)
      Skl = matrix(rep(1,n*6), ncol=6)
      Vol = matrix(rep(0,n*7), ncol=7)
      FixLng = matrix(rep(0,n*180), ncol=180)
      NFixLng = rep(0,n)
      iErr = rep(0,n)
      tmp = .C("R_V_BDAT20",
               as.integer(n),
               as.integer(wBDATBArtNr),
               as.single(D1),
               as.single(H1),
               as.single(D2),
               as.single(H2),
               as.single(H),
               as.single(Hx),
               as.integer(Hkz),
               as.integer(Skz),
               as.single(Az),
               as.single(Hsh),
               as.single(Zsh),
               as.single(Zab),
               as.integer(Sokz),
               Skl = as.integer(Skl),
               Volumen = as.single(Vol),
               iErr = as.integer(iErr),
               FixLngDef = as.single(FixLngDef),
               NMaxFixLng = as.integer(NMaxFixLng),
               FixLng = as.single(FixLng),
               NFixLng = as.integer(NFixLng))
      return(list(Skl = matrix(tmp$Skl,ncol=6,byrow=T),
                  Vol = matrix(tmp$Volumen,ncol=7,byrow=T),
                  iErr = tmp$iErr,
                  FixLng = matrix(tmp$FixLng,ncol=180,byrow=T),
                  NFixLng = tmp$NFixLng))
    }
  }
}


