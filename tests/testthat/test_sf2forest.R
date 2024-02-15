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


expect_error(
  test_res<-suppressWarnings(forestplotlist_es(
    test_file_ifn[["ID_UNIQUE_PLOT"]],
    test_file_ifn[["version"]] ,
    "TURURU",
    test_file_ifn[["tree"]],
    test_file_ifn[["regen"]],
    test_file_ifn[["understory"]][[1]][["shrub"]],
    filterNA =TRUE,
    filterDead=TRUE,
    minDBH = 30,
    filterminDBH =TRUE,
    setDefaults=TRUE,
    .verbose = TRUE
  )),
  "Country must be especified as a character vector either ES, US or FR"
)

})


test_that("forestplotlist_fr  works as intended", {

  #buscar uno que tenga shrub data
  test_file_ffi <- standard_ffi[1,]

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

  #COUNTRY
  expect_error(
    test_res<-suppressWarnings(forestplotlist_fr(
      test_file_ffi[["ID_UNIQUE_PLOT"]],
      test_file_ffi[["version"]] ,
      "TURURU",
      test_file_ffi[["tree"]],
      test_file_ffi[["regen"]],
      test_file_ffi[["understory"]][[1]][["shrub"]],
      filterNA =TRUE,
      filterDead=TRUE,
      minDBH = 30,
      filterminDBH =TRUE,
      setDefaults=TRUE,
      .verbose = TRUE
    )),
    "Country must be especified as a character vector either ES, US or FR"
  )


})

test_that("forestplotlist_us  works as intended", {

  #buscar uno que tenga shrub data
  test_file_fia<-standard_fia[11,]

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


  expect_error(
    test_res<-suppressWarnings(forestplotlist_us(
      test_file_fia[["ID_UNIQUE_PLOT"]],
      test_file_fia[["version"]] ,
      "TURURU",
      test_file_fia[["tree"]],
      test_file_fia[["regen"]],
      test_file_fia[["understory"]][[1]][["shrub"]],
      filterNA =TRUE,
      filterDead=TRUE,
      minDBH = 30,
      filterminDBH =TRUE,
      setDefaults=TRUE,
      .verbose = TRUE
    )),
    "Country must be especified as a character vector either ES, US or FR"
  )

})


test_that("sf2forest  works as intended  for ES", {



  #hacer pruebas con ausencias de parametros y modificar la funcion para warnings



 test_input_ifn <-standard_ifn

  expect_true(
    is.list(
      test_res<-suppressWarnings(sf2forest(
        input_df = test_input_ifn,
        filterNA = TRUE,
        filterDead = TRUE,
        minDBH = 30,
        filterminDBH = TRUE,
        setDefaults = TRUE,
        .verbose = TRUE)
      ))
  )

  expect_identical(length(test_res), 61L)




  expect_error(
    test_res<-sf2forest(
      input_df = test_input_ifn,
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = "-30",
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "minDBH must be a numeric and positive value"
  )

  expect_error(
    test_res<-sf2forest(
      input_df = NULL,
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = 30,
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "The input is empty. Please specified an input that follows the standard data frame structure."
  )

  expect_error(
    test_res<-sf2forest(
      input_df = tibble::tibble(),
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = 30,
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "The input is empty. Please specified an input that follows the standard data frame structure."
  )


  expect_error(
    test_res<-sf2forest(
      input_df = test_input_ifn[-1],
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = 30,
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "Some columns are missing. Check that all of these are present: ID_UNIQUE_PLOT,COUNTRY, tree, regen, understory)"
  )

  expect_error(
    test_res<-sf2forest(
      input_df = test_input_ifn[-8],
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = 30,
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "Column version is missing."
  )

})


test_that("sf2forest  works as intended  for US", {



  #hacer pruebas con ausencias de parametros y modificar la funcion para warnings

  test_input_fia <- standard_fia


  expect_true(
    is.list(
      test_res<-suppressWarnings(sf2forest(
        input_df = test_input_fia,
        filterNA = TRUE,
        filterDead = TRUE,
        minDBH = 30,
        filterminDBH = TRUE,
        setDefaults = TRUE,
        .verbose = TRUE)
      ))
  )


  expect_identical(length(test_res), 12L)




  expect_error(
    test_res<-sf2forest(
      input_df = test_input_fia,
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = "-30",
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "minDBH must be a numeric and positive value"
  )

  expect_error(
    test_res<-sf2forest(
      input_df = NULL,
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = 30,
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "The input is empty. Please specified an input that follows the standard data frame structure."
  )

  expect_error(
    test_res<-sf2forest(
      input_df = tibble::tibble(),
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = 30,
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "The input is empty. Please specified an input that follows the standard data frame structure."
  )


  expect_error(
    test_res<-sf2forest(
      input_df = test_input_fia[-2],
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = 30,
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "Some columns are missing. Check that all of these are present: ID_UNIQUE_PLOT,COUNTRY, tree, regen, understory)"
  )
  expect_error(
    test_res<-sf2forest(
      input_df = test_input_fia[-1],
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = 30,
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "Column YEAR is missing."
  )


})

test_that("sf2forest  works as intended  for FR", {



  #hacer pruebas con ausencias de parametros y modificar la funcion para warnings

  test_input_ffi <- standard_ffi


  expect_true(
    is.list(
      test_res<-suppressWarnings(sf2forest(
        input_df = test_input_ffi,
        filterNA = TRUE,
        filterDead = TRUE,
        minDBH = 30,
        filterminDBH = TRUE,
        setDefaults = TRUE,
        .verbose = TRUE)
      ))
  )

  expect_identical(length
                   (test_res), 14L)



  expect_error(
    test_res<-sf2forest(
      input_df = test_input_ffi,
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = "-30",
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "minDBH must be a numeric and positive value"
  )

  expect_error(
    test_res<-sf2forest(
      input_df = NULL,
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = 30,
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "The input is empty. Please specified an input that follows the standard data frame structure."
  )

  expect_error(
    test_res<-sf2forest(
      input_df = tibble::tibble(),
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = 30,
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "The input is empty. Please specified an input that follows the standard data frame structure."
  )


  expect_error(
    test_res<-sf2forest(
      input_df = test_input_ffi[-1],
      filterNA = TRUE,
      filterDead = TRUE,
      minDBH = 30,
      filterminDBH = TRUE,
      setDefaults = TRUE,
      .verbose = TRUE)
    ,
    "Some columns are missing. Check that all of these are present: ID_UNIQUE_PLOT,COUNTRY, tree, regen, understory)"
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
