#' @export

BDATD2H2Trans <- function(
  speciesID,
  D1,
  H1,
  D2,
  H2,
  H
) {
  dat <- data.frame(
    sp = speciesID,
    D1 = D1,
    H1 = H1,
    D2 = D2,
    H2 = H2,
    H = H
  )

  D2 <- sapply(
    1:nrow(dat),
    function(a){
      .Fortran(
        "BDATD2H2Trans",
        as.integer(dat$sp[a]),
        as.single(dat$D1[a]),
        as.single(dat$H1[a]),
        D2 = as.single(dat$D2[a]),
        H2 = as.single(dat$H2[a]),
        as.single(dat$H[a])
      )$D2
    }
  )
  H2 <- sapply(
    1:nrow(dat),
    function(a){
      .Fortran(
        "BDATD2H2Trans",
        as.integer(dat$sp[a]),
        as.single(dat$D1[a]),
        as.single(dat$H1[a]),
        D2 = as.single(dat$D2[a]),
        H2 = as.single(dat$H2[a]),
        as.single(dat$H[a])
      )$H2
    }
  )

  return(data.frame(
    D2 = D2,
    H2 = H2
  ))
}
