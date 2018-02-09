#' @title load BDATPro
#'
#' @description
#' \code{loadBDAT} imports a dll file which contains a bunch of functions for
#' modelling tree taper given diameters and height of a tree.
#' @param fun name of the function as character string(s) which shall be loaded
#'  into global environment. See details.
#' @param type character vector; if type is \code{expr} (default), an expression
#' will be returned; if type is \code{global}, \code{fun} is loaded into global
#' environment
#' @details
#' Since this package contains the dll files compiled on Windows 32bit,
#' \code{loadBDAT()} access them directly. \cr
#' \code{fun} needs at least one of the following character strings to load a
#' fortran function of BDAT in your global environment: \cr
#' \itemize{
#'   \item 'BDATDORHX': diameter without bark in height x
#'   \item 'BDATDMRHX': diameter with bark in height x
#'   \item 'BDATHXDX': get height x for given diameter x
#'   \item 'BDATVOLDHOR': total volume with bark
#'   \item 'BDATVOLDHMR': total volume without bark
#'   \item 'BDATVOLABOR': volume without bark between height A und B
#'   \item 'BDATVOLABMR': volume with bark between height A und B
#'   \item 'BDATRINDE2HX': double bark thickness at height X
#'   \item 'V_BDAT20': vectorised sorting function inclusive fix length
#' }
#' Not yet implemented are the following BDAT-functions:
#' \itemize{
#'   \item 'BDATMwQ03BWI': Mittelwert, Streuung und Formquotienten-Verteilung
#'   \item 'BDATPctQ03BWI': Perzentilwert des Formquotienten
#'   \item 'FNBDATQ03VHDx': Volumenäquivalenter Formquotienten
#'   \item 'FNBDATEstQ032': Fortschreibung der Schaftform bei Folgeinventur
#' }
#' @return Returns a fortran function and loads it into your global environment.
#' @examples
#' loadBDAT(fun = 'BDATDORHX')
#' @export


loadBDAT <- function(fun, type = "expr") {

  load_dll()

  # Funktion laden: Durchmesser ohne Rinde in Höhe x
  if ("BDATDORHX" %in% fun) {
    expr <- expression(
      BDATDORHX <- function(
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
    )
  }

  # Funktion laden: Durchmesser mit Rinde in Höhe x
  if ("BDATDMRHX" %in% fun) {
    expr <- expression(
      BDATDMRHX <- function(
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
    )
  }

  # Funktion laden: Derbholzvolumen ohne Rinde
  if ("BDATVOLDHOR" %in% fun) {
    expr <- expression(
      BDATVOLDHOR <- function(
        wBDATBArtNr,
        wD1,
        wH1,
        wD2,
        wH2,
        wHges,
        wDHGrz,
        wHDHGrz,
        wSekLng,
        wIErr,
        wVolDHmR
      ) {
        .C("R_BDATVOLDHOR",
           as.integer(wBDATBArtNr),
           as.single(wD1),
           as.single(wH1),
           as.single(wD2),
           as.single(wH2),
           as.single(wHges),
           as.single(wDHGrz),
           as.single(wHDHGrz),
           as.single(wSekLng),
           as.integer(wIErr),
           VolDHoR = as.single(wVolDHoR))$VolDHoR
      }
    )
  }

  # Funktion laden: Derbholzvolumen mit Rinde
  if ("BDATVOLDHMR" %in% fun) {
    expr <- expression(
      BDATVOLDHMR <- function(
        wBDATBArtNr,
        wD1,
        wH1,
        wD2,
        wH2,
        wHges,
        wDHGrz,
        wHDHGrz,
        wSekLng,
        wIErr,
        wVolDHmR
      ) {
        .C("R_BDATVOLDHMR",
           as.integer(wBDATBArtNr),
           as.single(wD1),
           as.single(wH1),
           as.single(wD2),
           as.single(wH2),
           as.single(wHges),
           as.single(wDHGrz),
           as.single(wHDHGrz),
           as.single(wSekLng),
           as.integer(wIErr),
           VolDHmR = as.single(wVolDHmR))$VolDHmR
      }
    )
  }

  # Funktion laden: Abschnittsvolumen mit Rinde zwischen Höhe A und B
  if ("BDATVOLABMR" %in% fun) {
    expr <- expression(
      BDATVOLABMR <- function(
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
    )
  }

  # Funktion laden: Abschnittsvolumen ohne Rinde zwischen Höhe A und B
  if ("BDATVOLABOR" %in% fun) {
    expr <- expression(
      BDATVOLABOR <- function(
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
    )
  }

  # Funktion laden: Für einen Baum mit den Dimensionsdaten <<D1,H1,D2,H2,H>> wird die
  #doppelte Rindenstärke [cm] an der Stelle Hx [m] berechnet.
  if ("BDATRINDE2HX" %in% fun) {
    expr <- expression(
      BDATRINDE2HX <- function(
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
    )
  }


  # Funktion: Höhe x bei gegebenem Durchmesser Dx iterativ bestimmen
  if ("BDATHXDX" %in% fun) {
    expr <- expression(
      BDATHXDX <- function(
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
    )
  }

  # BDAT-Sortierfunktion inkl. Fixlängen
  if ("V_BDAT20" %in% fun) {
    expr <- expression(
      V_BDAT20 <- function(
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
    )
  }

  if (type == "expr") return(expr)
  if (type == "global") return(eval(expr, envir = globalenv()))

}


