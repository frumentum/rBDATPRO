#' Load BDAT Dynamically Linked Libraries
#'
#' @return Invisibly NULL.
#' @useDynLib rBDATPRO
#'@export
load_dll <- function() {
    # dyn.load(system.file("dll", "DFORRT.dll", package = "rBDATPRO"))
    # dyn.load(system.file("dll", "BDATPRO.dll", package = "rBDATPRO"))
    # dyn.load(system.file("dll", "BDATPRO_R.dll", package = "rBDATPRO"))
    # return(invisible(NULL))
}
