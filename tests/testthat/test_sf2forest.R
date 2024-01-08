# test data -----------------------------------------------------------------------------------




test_that("forestplotlist_es  works as intended", {


#buscar uno que tenga shrub data
  test_file_ifn<-standard_ifn[5,]

expected_names <- c(
  "ID",
  "version",
  "treeData",
  "shrubData",
  "herbCover",
  "herbHeight"
)

expect_true(
  is.list(
  test_res<-suppressWarnings(forestplotlist_es(
    test_file_ifn[["ID_UNIQUE_PLOT"]],
    test_file_ifn[["version"]] ,
    test_file_ifn[["COUNTRY"]],
    test_file_ifn[["tree"]],
    test_file_ifn[["regen"]],
    test_file_ifn[["understory"]][[1]][["shrub"]],
    filterNA =TRUE,
    filterDead=TRUE,
    minDBH = 30,
    filterminDBH =TRUE,
    setDefaults=TRUE,
    .verbose = TRUE
    ))
  )
)

# data integrity
expect_named(test_res, expected_names, ignore.order = TRUE)
expect_true(length(test_res) > 0)


expect_length(unique(test_res$ID), 1)
expect_length(unique(test_res$version), 1)


expect_identical(unique(test_res$ID),  test_file_ifn[["ID_UNIQUE_PLOT"]])

expect_identical(unique(test_res$version) , test_file_ifn[["version"]])




})


test_that("forestplotlist_fr  works as intended", {

  #buscar uno que tenga shrub data
  test_file_ffi <- standard_ffi[41,]

  #hacer pruebas con ausencias de parametros y modificar la funcion para warnings


  expected_names <- c(
    "ID",
    "YEAR",
    "treeData",
    "shrubData",
    "herbCover",
    "herbHeight"
  )

  expect_true(
    is.list(
      test_res<-suppressWarnings(forestplotlist_fr(
        test_file_ffi[["ID_UNIQUE_PLOT"]],
        test_file_ffi[["YEAR"]] ,
        test_file_ffi[["COUNTRY"]],
        test_file_ffi[["tree"]],
        test_file_ffi[["regen"]],
        test_file_ffi[["understory"]][[1]][["shrub"]],
        test_file_ffi[["understory"]][[1]][["herbs"]],
        filterNA = TRUE,
        filterDead = TRUE,
        minDBH = 30,
        filterminDBH = TRUE,
        setDefaults = TRUE,
        .verbose = FALSE)
      ))
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(length(test_res) > 0)


  expect_length(unique(test_res$ID), 1)
  expect_length(unique(test_res$YEAR), 1)


  expect_identical(unique(test_res$ID),  test_file_ffi[["ID_UNIQUE_PLOT"]])

  expect_identical(unique(test_res$YEAR) , test_file_ffi[["YEAR"]])




})

test_that("forestplotlist_us  works as intended", {

  #buscar uno que tenga shrub data
  test_file_fia<-standard_fia[46,]

  #hacer pruebas con ausencias de parametros y modificar la funcion para warnings


  expected_names <- c(
    "ID",
    "YEAR",
    "treeData",
    "shrubData",
    "herbCover",
    "herbHeight"
  )

  expect_true(
    is.list(
      test_res<-suppressWarnings(forestplotlist_us(
        test_file_fia[["ID_UNIQUE_PLOT"]],
        test_file_fia[["YEAR"]] ,
        test_file_fia[["COUNTRY"]],
        test_file_fia[["tree"]],
        test_file_fia[["regen"]],
        test_file_fia[["understory"]][[1]][["shrub"]],
        test_file_fia[["understory"]][[1]][["herbs"]],
        filterNA = TRUE,
        filterDead = TRUE,
        minDBH = 30,
        filterminDBH = TRUE,
        setDefaults = TRUE,
        .verbose = FALSE)
      ))
  )

  # data integrity
  expect_named(test_res, expected_names, ignore.order = TRUE)
  expect_true(length(test_res) > 0)


  expect_length(unique(test_res$ID), 1)
  expect_length(unique(test_res$YEAR), 1)


  expect_identical(unique(test_res$ID),  test_file_fia[["ID_UNIQUE_PLOT"]])

  expect_identical(unique(test_res$YEAR) , test_file_fia[["YEAR"]])




})


test_that("sf2forest  works as intended  for ES", {



  #hacer pruebas con ausencias de parametros y modificar la funcion para warnings



  test_input_ifn<-standard_ifn
  # test_file_ffi <- standard_ffi
  # test_file_fia<-standard_fia

  expect_true(
    is.list(
      test_res<-suppressWarnings(sf2forest(
        input_df = test_input_ifn,
        country = "ES",
        filterNA = TRUE,
        filterDead = TRUE,
        minDBH = 30,
        filterminDBH = TRUE,
        setDefaults = TRUE,
        .verbose = TRUE)
      ))
  )




})
# #hacer lo mismo con la funcion general
# sf2forest (prova2_fia, "US", filterNA=TRUE, filterDead=TRUE, minDBH=30,filterminDBH=TRUE, setDefaults=TRUE, .verbose = TRUE)
#
# # errors
# expect_warning(
#   test_error <- forestplotlist_es(
#     test_file_ifn[["ID_UNIQUE_PLOT"]],
#     NA_character_,
#     test_file_ifn[["version"]] ,
#     test_file_ifn[["tree"]],
#     test_file_ifn[["regen"]],
#     test_file_ifn[["understory"]][[1]][["shrub"]],
#     filterNA =TRUE,
#     filterDead=TRUE,
#     minDBH = 30,
#     filterminDBH =TRUE,
#     setDefaults=TRUE,
#     .verbose = TRUE),
#   "Some files"
# )
# expect_s3_class(test_error, "tbl")
# expect_true(nrow(test_error) < 1)
#
# # error in department name, gives an empty tibble
# expect_s3_class(
#   test_error <- suppressWarnings(ifn_tree_table_process(
#     test_input$tree_table[63],
#     test_version,
#     test_input$plots[63],
#     test_input$province[63],
#     test_especies
#   )),
#   "tbl"
# )
# expect_true(nrow(test_error) < 1)
# # error in plot name, should return an empty tibble
# expect_s3_class(
#   test_error <- suppressWarnings(ifn_tree_table_process(
#     test_input$tree_table[61],
#     test_version,
#     test_input$plots[61],
#     test_input$province[61],
#     test_especies
#   )),
#   "tbl"
# )
# expect_true(nrow(test_error) < 1)
#
#
#
# #un test para cada inventario con unas pocas parcelas
# # los datos- preguntar a victor donde los puedo poner en sys?
