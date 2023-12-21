
load(file="data-raw/prova2_ifn.RData")

test_file_ifn<-prov2_ifn[5,]
#buscar uno que tenga shrub data
test_res_ifn<-forestplotlist_es(
  test_file_ifn[["ID_UNIQUE_PLOT"]],  test_file_ifn[["COUNTRY"]], test_file_ifn[["version"]] , test_file_ifn[["tree"]], test_file_ifn[["regen"]], test_file_ifn[["understory"]]["shrub"], filterNA =TRUE, filterDead=TRUE, minDBH = 30,filterminDBH =TRUE, setDefaults=TRUE, .verbose = TRUE)
#hacer pruebas con ausencias de parametros y modificar la funcion para warnings

#hacer lo mismo para el resto



#hacer lo mismo con la funcion general
