library(medfateutils)
data(examplemeteo)
data(exampleforest)
data(SpParamsMED)
examplesoil <- defaultSoilParams(4)


test_that("rockOptimization works",{
  expect_type(spwb_rockOptimization(exampleforest, soil = examplesoil,
                        SpParams = SpParamsMED, meteo = examplemeteo,
                        control = defaultControl("Granier"),
                        elevation = 100, latitude = 41.82592), "list")
})
