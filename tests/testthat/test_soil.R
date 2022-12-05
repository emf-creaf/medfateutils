library(sf)
coords_sf <- st_sfc(st_point(c(-5.6333, 42.6667)), crs = 4326)
test_that("Can retrieve soilgrids from point 'sf' object and modify properties",{
  foo_1 <- soilgridsParams(coords_sf, widths = c(300, 700, 1000, 2000))
  expect_s3_class(foo_1, "data.frame")
  foo_2 <- modifySoilDepth(foo_1, 900) # 90 cm depth
  expect_s3_class(foo_2, "data.frame")
  foo_3 <- modifySoilRockContent(foo_2, 20) # 20 % rocks in the surface
  expect_s3_class(foo_3, "data.frame")
})
