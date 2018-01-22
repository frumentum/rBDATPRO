# dat <- data.frame(
#   sp = 1,
#   D1 = 30,
#   H1 = 1.3,
#   D2 = 0,
#   H2 = 0,
#   H = 40,
#   Hx = 20,
#   lh = 1,
#   uh = 30,
#   SecLen = 0.1
# )
# bark <- FALSE
# # View(dat)
#
# a <- 1
#
# is.loaded("BDATDmRHx")
# ## aufruf funktioniert nicht
# # .Fortran("BDATDoRHx",
# #   as.integer(dat$sp[a]),
# #   as.single(dat$D1[a]),
# #   as.single(dat$H1[a]),
# #   as.single(dat$D2[a]),
# #   as.single(dat$H2[a]),
# #   as.single(dat$H[a]),
# #   as.single(dat$Hx[a]),
# #   IFeh = as.integer(0),
# #   wDmRHx = as.single(0)
# # )
#
# ## Aufruf funktioniert!
# .Fortran("BDATVolABmR",
#   as.integer(dat$sp[a]),
#   as.single(dat$D1[a]),
#   as.single(dat$H1[a]),
#   as.single(dat$D2[a]),
#   as.single(dat$H2[a]),
#   as.single(dat$H[a]),
#   as.single(dat$lh[a]),
#   as.single(dat$uh[a]),
#   as.single(dat$SecLen[a]),
#   IFeh = as.integer(0),
#   VolABmR = as.single(0)
# )$VolABmR
#
# getVolume(1, 30, 1.3, 0, 0, 40, 1, 30, 0.1, bark = F)
#
# getwd()
# # dyn.load(file.path(getwd(), "src/rBDATPRO.dll"))
# is.loaded("bdatvolabmr") #>TRUE
# is.loaded("bdatdmrhx") #>TRUE
# is.loaded("bdatdorhx_") #>TRUE
#
#
# is.loaded("cdfnorm") #>TRUE
# .Fortran("cdfnorm",
#          as.single(0),
#          as.single(1),
#          as.single(1.5),
#          CDFx = as.single(0)
# )$CDFx #> OK
#
# .Fortran("cdfnorminv",
#          as.single(0),
#          as.single(1),
#          CDFx = as.single(0.5),
#          x = as.single(0)
# )$x #> OK
#
# is.loaded("bdatrinde2hx") #>TRUE
# ## aufruf funktioniert nicht!
# # .Fortran("bdatrinde2hx",
# #          as.integer(dat$sp[a]),
# #          as.single(dat$D1[a]),
# #          as.single(dat$H1[a]),
# #          as.single(dat$D2[a]),
# #          as.single(dat$H2[a]),
# #          as.single(dat$H[a]),
# #          as.single(dat$Hx[a]),
# #          IFeh = as.integer(0),
# #          wRinde2Hx = as.single(0)
# # )$wRinde2Hx #> OK
#
# is.loaded("bdatd2h2trans") #>TRUE
# .Fortran("bdatd2h2trans",
#          BdatArt = as.integer(dat$sp[a]),
#          D1 = as.single(dat$D1[a]),
#          H1 = as.single(dat$H1[a]),
#          D2 = as.single(dat$D2[a]),
#          H2 = as.single(dat$H2[a]),
#          H = as.single(dat$H[a])
# )
