
#' Forest inventory processing
#'
#' This function takes a standard data frame as input and creates a list of \code{\link{forest}} objects
#'
#' @param input_df Standard data frame generated using package \code{esus}.
#' @param filterNA Boolean flag. If TRUE records with missing values for DBH or Height are eliminated.
#' @param filterDead Boolean flag. If TRUE only records of live tree are kept.
#' @param minDBH  Min DBH value to keep.
#' @param filterminDBH Boolean flag. If TRUE records are eliminated below minDBH
#' @param setDefaults set defaults param for roots
#' @param .verbose A boolean flag to indicate console output
#'
#' @returns A list of \code{\link{forest}} objects.
#'
#' @export
sf2forest <- function(input_df, filterNA, filterDead, minDBH,filterminDBH, setDefaults, .verbose = TRUE) {

  assertthat::assert_that(
    !is.null(input_df),
    msg = cli::cli_abort("The input is empty. Please specified an input that follows the standard data frame structure.")

  )

  assertthat::assert_that(
    nrow(input_df)>0,
    msg = cli::cli_abort("The input is empty. Please specified an input that follows the standard data frame structure.")

  )

  assertthat::assert_that(
    # !any(is.null(input_df[c("ID_UNIQUE_PLOT", "tree" ,"regen","understory")])),

    all(c("ID_UNIQUE_PLOT", "COUNTRY","tree" ,"regen","understory") %in% names(input_df)),
    msg = cli::cli_abort("Some columns are missing. Check that all of these are present: ID_UNIQUE_PLOT,COUNTRY, tree, regen, understory)"
    )
  )
  assertthat::assert_that(
    is.character(input_df$COUNTRY), length(unique(input_df$COUNTRY)) > 0, unique(input_df$COUNTRY) %in% c("ES", "US", "FR"),
    msg = cli::cli_abort("Country must be especified as a character vector either ES, US or FR")
  )


  assertthat::assert_that(
    is.numeric(minDBH), minDBH > 0,
    msg = cli::cli_abort("minDBH must be a numeric and positive value ")
  )

  country<-unique(input_df$COUNTRY)

  if (country != "ES"){
    input_df$version = NA
    assertthat::assert_that(
      all(c("YEAR") %in% names(input_df)),
      msg = cli::cli_abort("Column YEAR is missing."
      )
    )
  } else{
    assertthat::assert_that(
      all(c("version") %in% names(input_df)),
      msg = cli::cli_abort("Column version is missing."
      )
    )

  }

  purrr::pmap(
    .l = list(
      id = input_df[["ID_UNIQUE_PLOT"]],
      year = input_df[["YEAR"]],
      version = input_df[["version"]],
      country = input_df[["COUNTRY"]],
      tree = input_df[["tree"]],
      regen= input_df[["regen"]],
      understory = input_df[["understory"]]
    ),
    .f = \(id,
           year,
           version,
           country,
           tree,
           regen,
           understory) {

      cat("Processing ID:", id)

       # browser()
      understory <- data.frame(understory)

    if (is.null(understory[["shrub"]])) {
        shrub <- tibble::tibble()
      } else {
        shrub <- understory[["shrub"]]
      }
      if (is.null(understory[["herbs"]])) {
        herbs <- tibble::tibble()
      } else {
        herbs <- understory[["herbs"]]
      }
      if (is.null(regen)) {
        regen <- tibble::tibble()
      } else {
        regen <- regen
      }
      forest <- switch(country,
                       "FR" = forestplotlist_fr(id, year, country, tree, regen, shrub, herbs, filterNA, filterDead,  minDBH, filterminDBH, setDefaults, .verbose),
                       "ES" = forestplotlist_es(id, version, country, tree, regen, shrub, filterNA, filterDead,  minDBH, filterminDBH, setDefaults, .verbose),
                       "US" = forestplotlist_us(id, year, country, tree, regen, shrub, herbs, filterNA, filterDead,  minDBH, filterminDBH, setDefaults, .verbose),
                       stop("Country not recognized"))

      return(forest)
    } |> purrr::list_rbind()

   )
}






# This function takes row of a standard data frame of FIA, FR or ES and creates a list following the format of a forest object
#
# @param id id code for plot
# @param year year of sampling , NA as default for ES
# @param country country code "FR" "ES" or "US"
# @param version NA as default, enter "ifn2" "ifn3" OR "ifn4" only for SPAIN
# @param tree standard data frame with tree info
# @param regen  standard data frame with regen info
# @param understory standard data frame with understory info
# @param herbs standard data frame with herbs info
# @param filterNA if TRUE records with NA for DBH or HT  are eliminated
# @param filterDead if TRUE only records of alive trees are conserved
# @param minDBH  min DBH value
# @param filterminDBH if TRUE records are eliminated below minDBH
# @param setDefaults set defaults params for root depth
# @param .verbose A boolean flag to indicate console output
forestplotlist_es <- function(id,version, country, tree, regen, shrub, filterNA, filterDead, minDBH,filterminDBH, setDefaults, .verbose = TRUE){


  assertthat::assert_that(
    is.character(country), length(country) > 0, country %in% c("ES", "US", "FR"),
    msg = cli::cli_abort("Country must be especified as a character vector either ES, US or FR")
  )


   # browser()
  treeData <- data.frame(tree)
  regenData <- data.frame(regen)
  shrubData <- data.frame(shrub)

  forest_list <- list()

  forest_list$ID <- id
  forest_list$version <- version

  if (nrow(treeData)>0){

  cli::cli_inform("Processing tree data...")



    tree <- treeData |>
      dplyr::select(any_of(c(
        "ID_UNIQUE_PLOT",
        "SP_CODE",
        "SP_NAME",
        "TREE",
        "nArbol",
        "OrdenIf3" ,
        "OrdenIf4" ,
        "OrdenIf2",
        "DIA",
        "HT",
        "DENSITY"
      ))) |>
      dplyr::rename(
        ID = ID_UNIQUE_PLOT,
        Species = SP_NAME,
        DBH = DIA,
        Height = HT,
        N = DENSITY
      ) |>
      dplyr::mutate(
        COUNTRY = country,
        #conversion to cm
        Height = Height * 100,
        Z50 = NA,
        Z95 = NA
      ) |>
      dplyr::select(
        any_of(c(
          "ID",
          "SP_CODE",
          "Species",
          "DBH",
          "Height",
          "TREE",
          "nArbol",
          "OrdenIf3",
          "OrdenIf4",
          "OrdenIf2",
          "N",
          "Z95",
          "Z50"
        )))

    if (filterDead) {

      cli::cli_inform("Filtering missing and zero values..." )


        if ("OrdenIf3" %in% names(tree)) {


          tree <- tree |>
            dplyr::filter(
              !(OrdenIf3 %in% c("888", "999")))}

        if ("OrdenIf4" %in% names(tree)) {
          tree <- tree |>
            dplyr::filter(
              !(OrdenIf4 %in% c("888", "999")))}




    }

    if (filterNA ){

      cli::cli_inform("Filtering NAs..." )



        tree<-tree|>
          dplyr::filter(
            !is.na(DBH),
            !is.na(Height),
            DBH > 0,
            Height > 0
          )
      }

    if (filterminDBH) {

      cli::cli_inform("Filtering minDBH..." )

      tree<-tree|>
        dplyr::filter(DBH > minDBH)
    }

    if(setDefaults) {

      cli::cli_inform("Establishing setdefaults..." )


      tree$Z95[is.na(tree$Z95)] <- 1000

      tree$Z50[is.na(tree$Z50)] <- exp(log(tree$Z95[is.na(tree$Z50)])/1.3)}

  }
    else{
    tree = tibble::tibble()}


  if (nrow(regenData)>0) {

    cli::cli_inform("Processing regen data..." )

      if  ( version %in% c("ifn3", "ifn4")){

        regen <- regenData |>
          dplyr::select(
            ID_UNIQUE_PLOT,
            SP_CODE,
            SP_NAME,
            DBH,
            N,
            Height
          ) |>
          dplyr::mutate(

            nArbol = NA,
            OrdenIf2 = NA,
            OrdenIf3 = NA,
            OrdenIf4 = NA,
            Z50 = NA ,
            Z95 = NA
          ) |>
          dplyr::rename(
            ID =  ID_UNIQUE_PLOT,
            Species = SP_NAME) |>
          dplyr::select(ID,
                        SP_CODE,
                        Species,
                        DBH,
                        Height,
                        nArbol,
                        OrdenIf2,
                        OrdenIf3,
                        OrdenIf4,
                        N,
                        Z95,
                        Z50
          )
      } else {

        if  ( version == "ifn2"){

          regen <- regenData |>
            dplyr::select(
              ID_UNIQUE_PLOT,
              SP_CODE,
              SP_NAME,
              N,
              Height,
              DBH
            ) |>
            dplyr::mutate(
              Z50 = NA ,
              Z95 = NA,
              OrdenIf2 = NA) |>

            dplyr::rename(
              ID =  ID_UNIQUE_PLOT,
              Species = SP_NAME) |>
            dplyr::select(
              ID,
              SP_CODE,
              Species,
              DBH,
              Height,
              OrdenIf2,
              N,
              Z95,
              Z50
            ) |>
            dplyr::filter(complete.cases(DBH, Height, N))
        }
      }

      if(setDefaults) {

        regen$Z95[is.na(regen$Z95)] <- 1000

        regen$Z50[is.na(regen$Z50)] <- exp(log(regen$Z95[is.na(regen$Z50)])/1.3)
      }



  }else {regen = tibble::tibble()}



  if(nrow(tree)>0 && nrow(regen)>0){

    intersect <- intersect(names(tree), names(regen))
        tree <- tree[, intersect]
    regen <- regen[, intersect]
        tree <- rbind(tree,regen)

  } else if (nrow(regen) > 0) {
    tree <- regen
  }

  forest_list$treeData <- tree


  if (nrow(shrubData) > 0) {

    cli::cli_inform("Processing shrub data.." )

      shrub <- shrubData |>
        dplyr::select(
          ID = ID_UNIQUE_PLOT,
          SP_CODE = SP_CODE,
          Species = SP_NAME,
          Height = HT,
          Cover = COVER
        ) |>
        dplyr::mutate(
          Cover = as.numeric(Cover),
          Z50 = NA ,
          Z95 = NA,
          #conversion from m to cm
          # Height = 100 * Height
        )

    if (filterNA) {



        shrub <- shrub |>
          dplyr::filter(
            !is.na(Cover),
            !is.na(Height),
            Cover > 0,
            Height > 0

          )

      }
    if (setDefaults) {
      shrub$Z95[is.na(shrub$Z95)] <- 800
      shrub$Z50[is.na(shrub$Z50)] <- exp(log(shrub$Z95[is.na(shrub$Z50)])/1.3)
    }

  }  else  {
    shrub = tibble::tibble()
    }



  forest_list$shrubData <- shrub
  forest_list$herbCover <- NA
  forest_list$herbHeight <- NA


  return(forest_list)

}


forestplotlist_fr <- function(id, year , country, tree, regen, shrub, herbs, filterNA, filterDead, minDBH,filterminDBH, setDefaults, .verbose = TRUE){

  assertthat::assert_that(
    is.character(country), length(country) > 0, country %in% c("ES", "US", "FR"),
    msg = cli::cli_abort("Country must be especified as a character vector either ES, US or FR")
  )

  # browser()
  treeData <- data.frame(tree)
  regenData <- data.frame(regen)
  shrubData <- data.frame(shrub)
  herbsData <- data.frame(herbs)

  forest_list <- list()

  forest_list$ID <- id
  forest_list$YEAR <- year

  if (nrow(treeData)>0){

    cli::cli_inform("Processing tree data..." )

    tree <- treeData |>
      dplyr::select(any_of(c(
        "ID_UNIQUE_PLOT",
        "SP_CODE",
        "SP_NAME",
        "TREE",
        "STATUS",
        "VEGET5",
        "DIA",
        "HT",
        "DENSITY"
      ))) |>
      dplyr::rename(
        ID = ID_UNIQUE_PLOT,
        Species = SP_NAME,
        DBH = DIA,
        Height = HT,
        N = DENSITY
      ) |>
      dplyr::mutate(
        COUNTRY = country,
        #conversion to cm
        Height = Height * 100,
        Z50 = NA,
        Z95 = NA
      ) |>
      dplyr::select(
        any_of(c(
          "ID",
          "SP_CODE",
          "Species",
          "DBH",
          "Height",
          "TREE",
          "STATUS",
          "VEGET5",
          "N",
          "Z95",
          "Z50"
        )))

    if (filterDead) {

      cli::cli_inform("Filtering missing and zero values..." )



    if (!is.na(tree$STATUS[1])){
      tree <- tree|>
        dplyr::filter(
          STATUS == 0
        )

    } else{

      if (!is.na(tree$VEGET5[1])){
        tree <- tree |>
          dplyr::filter(
            VEGET5 == 0
          )
      }
    }

    }

    if (filterNA == TRUE){

      if (.verbose) cat("Filtering NAs..." )

        if (!is.na(tree$STATUS[1])){

          tree <- tree |>
            dplyr::filter(
              !is.na(DBH),
              !is.na(Height),
              DBH > 0,
              Height > 0
            )
        }

        if (!is.na(tree$VEGET5[1])){

          tree <- tree |>
            dplyr::filter(
              !is.na(DBH),
              DBH > 0
            )
        }

    }

    if (filterminDBH) {

      if (.verbose) cat("Filtering minDBH..." )

      tree <- tree |>
        dplyr::filter(DBH > minDBH)
    }

    if (setDefaults) {

      if (.verbose) cat("Establishing setdefaults..." )


      tree$Z95[is.na(tree$Z95)] <- 1000

      tree$Z50[is.na(tree$Z50)] <- exp(log(tree$Z95[is.na(tree$Z50)])/1.3)
    }

  }else{tree = tibble::tibble()}


  if (nrow(regenData)>0) {

    cli::cli_inform("Processing regendata..." )

      regen <- regenData |>
        dplyr::select(
          ID_UNIQUE_PLOT,
          PLOT,
          DEP,
          YEAR,
          SP_CODE,
          SP_NAME,
          # DBH,
          COVER

        ) |> dplyr::mutate(

          Z50 = NA ,
          Z95 = NA,

          Height = NA,
          # # menos de 7.5 cm   (MUTEAR DESPUÉS ESTA LINEA)
          DBH = 7.5

        )|>
        dplyr::rename(
          ID =  ID_UNIQUE_PLOT,
          Species = SP_NAME) |>
        dplyr::select(ID,
                      SP_CODE,
                      Species,
                      PLOT,
                      DBH,
                      COVER,
                      Height,
                      # N,
                      Z95,
                      Z50
        )

  }else {regen = tibble::tibble()}


  if(nrow(tree)>0 && nrow(regen)>0){

    intersect <- intersect(names(tree), names(regen))
    tree <- tree[, intersect]
    regen <- regen[, intersect]
    tree <- rbind(tree,regen)

  } else if (nrow(regen) > 0) {
    tree <- regen
  }


  forest_list$treeData<-tree


  if (nrow(shrubData)>0) {

    cli::cli_inform("Processing shrub data.." )

      shrub<- shrubData|>
        dplyr::select(
          ID = ID_UNIQUE_PLOT,
          SP_CODE = SP_CODE,
          Species = SP_NAME,
          Cover = COVER
        ) |>
        dplyr::mutate(
          Height = NA,
          Z50 = NA ,
          Z95 = NA,

        )

    if (filterNA == TRUE){

        shrub <- shrub |>
          dplyr::filter(
            !is.na(Cover),
            # !is.na(Height),
            Cover > 0,
            # Height>0
          )

    }

    if (setDefaults) {
      shrub$Z95[is.na(shrub$Z95)] <- 800
      shrub$Z50[is.na(shrub$Z50)] <- exp(log(shrub$Z95[is.na(shrub$Z50)])/1.3)
    }

  }  else  { shrub = tibble::tibble()}

  forest_list$shrubData <- shrub

  if (nrow(herbsData)>0){
    if (.verbose) cat("Processing herbs data.." )

    forest_list$herbCover <- herbsData$HERB*10
    forest_list$herbHeight <- NA
    }else{
    forest_list$herbCover <- NA
    forest_list$herbHeight <- NA
  }


  return(forest_list)


}



forestplotlist_us <- function(id, year, country,  tree, regen, shrub, herbs, filterNA, filterDead, minDBH,filterminDBH, setDefaults, .verbose = TRUE){

  assertthat::assert_that(
    is.character(country), length(country) > 0, country %in% c("ES", "US", "FR"),
    msg = cli::cli_abort("Country must be especified as a character vector either ES, US or FR")
  )

  # browser()
  treeData <- data.frame(tree)
  regenData <- data.frame(regen)
  shrubData <- data.frame(shrub)
  herbsData <- data.frame(herbs)

  forest_list <- list()

  forest_list$ID <- id
  forest_list$YEAR <- year

  if (nrow(treeData)>0){

    cli::cli_inform("Processing tree data..." )

    tree <- treeData |>
      dplyr::select(any_of(c(
        "ID_UNIQUE_PLOT",
        "SP_CODE",
        "SP_NAME",
        "TREE",
        "STATUS",
        "DIA",
        "HT",
        "DENSITY"
      ))) |>
      dplyr::rename(
        ID = ID_UNIQUE_PLOT,
        Species = SP_NAME,
        DBH = DIA,
        Height = HT,
        N = DENSITY
      ) |>
      dplyr::mutate(
        COUNTRY = country,
        #conversion to cm
        Height = Height * 100,
        Z50 = NA,
        Z95 = NA
      ) |>
      dplyr::select(
        any_of(c(
          "ID",
          "SP_CODE",
          "Species",
          "DBH",
          "Height",
          "TREE",
          "STATUS",
          "N",
          "Z95",
          "Z50"
        )))

    if (filterDead) {

      cli::cli_inform("Filtering missing and zero values..." )

        tree <- tree |>
          dplyr::filter(
            #ONLY ALIVE TREES
            STATUS == 1
          )

    }

    if (filterNA == TRUE){

      if (.verbose) cat("Filtering NAs...\n")

        tree <- tree |>
          dplyr::filter(
            !is.na(DBH),
            !is.na(Height),
            DBH > 0,
            Height > 0
          )
    }

    if (filterminDBH) {

      if (.verbose) cat("Filtering minDBH...\n")

      tree <- tree |>
        dplyr::filter(DBH > minDBH)
    }

    if (setDefaults) {

      cli::cli_inform("Establishing setdefaults...\n")


      tree$Z95[is.na(tree$Z95)] <- 1000

      tree$Z50[is.na(tree$Z50)] <- exp(log(tree$Z95[is.na(tree$Z50)])/1.3)

    }

  }else{tree = tibble::tibble()}


  if (nrow(regenData)>0) {

    cli::cli_inform("Processing regendata...\n")

          regen <- regenData |>
        dplyr::select(
          ID_UNIQUE_PLOT,
          SP_CODE,
          SP_NAME,
          SUBP,
          DENSITY,
          Height,
          DBH

        ) |> dplyr::mutate(

          Z50 = NA ,
          Z95 = NA
          # #LESS THAN 6 INCH FOR CONIFER AND 12 FOR HARDWOOD MINIMUM default = 6 inch
          # Height = 15,
          # #LESS THAN 1 INCH = 2.54 CM default ? revisar ifn
          # DBH = 0.5

        )|>
        dplyr::rename(
          ID =  ID_UNIQUE_PLOT,
          Species = SP_NAME,
          N = DENSITY
        ) |>
        dplyr::select(
          ID,
          SP_CODE,
          Species,
          SUBP,
          DBH,
          Height,
          N,
          Z95,
          Z50
        )


      nsubp <- length(unique(regen$SUBP))

      regen <- regen |>
        dplyr::group_by(Species) |>
        dplyr::mutate(
          meanN = sum(N)/nsubp
        )|>
        dplyr::rename(
          N_subplot = N,
          N = meanN
        )|>
        dplyr::select(
          ID,
          SP_CODE,
          Species,
          DBH,
          Height,
          N,
          Z95,
          Z50
        ) |>

        #comprobar si hace falta

        unique()

  }else {regen = tibble::tibble()}


  if(nrow(tree)>0 && nrow(regen)>0){

    intersect <- intersect(names(tree), names(regen))
    tree <- tree[, intersect]
    regen <- regen[, intersect]
    tree <- rbind(tree,regen)

  } else if (nrow(regen) > 0) {
    tree <- regen
  }


  forest_list$treeData <-
    tree


  if (nrow(shrubData)>0) {

    cli::cli_inform("Processing shrub data..." )

      shrub <- shrubData |>
        dplyr::select(
          ID = ID_UNIQUE_PLOT,
          SP_CODE = SP_CODE,
          Species = SP_NAME,
          Height = HT,
          Cover = COVER,
          SUBP
        ) |>
        dplyr::mutate(

          Z50 = NA ,
          Z95 = NA,
          #conversion from m to cm
          # Height = 100 * Height
        )


      nsubp <- length(unique(shrub$SUBP))

      shrub <- shrub |>



        dplyr::group_by(Species)|>


        dplyr::mutate(

          #calculate means for plot
          sumCover =  sum(Cover),
          meanCover = sumCover/nsubp,

          #ponderado
          Height_p = (Height*Cover),
          #pasar division abajo
          meanHeight = sum(Height_p)/sumCover
        ) |>
        dplyr::rename(
          Cover_subplot = Cover,
          Cover = meanCover,
          Height_s = Height,
          Height = meanHeight
          ) |>

        dplyr::select(
          ID,
          SP_CODE,
          Species,
          Height,
          Cover,
          Z95,
          Z50
        ) |>

        #revisar
        unique()







    if (filterNA == TRUE){
        shrub <- shrub |>
          dplyr::filter(
            !is.na(Cover),
            !is.na(Height),
            Cover > 0,
            Height > 0

          )


    }

    if (setDefaults) {
      shrub$Z95[is.na(shrub$Z95)] <- 800
      shrub$Z50[is.na(shrub$Z50)] <- exp(log(shrub$Z95[is.na(shrub$Z50)])/1.3)
    }

  }  else  { shrub = tibble::tibble()}

  forest_list$shrubData<-shrub

  if (nrow(herbsData)>0){
    cli::cli_inform("Processing herbs data.." )

      herbs <- herbsData |>
        dplyr::select(
          ID = ID_UNIQUE_PLOT,
          SP_CODE = SP_CODE,
          Species = SP_NAME,
          Height = HT,
          Cover = COVER,
          SUBP
        ) |>
        dplyr::mutate(
          #conversion from m to cm
          Height = 100 * Height
        )

      nsubp <- length(unique(herbs$SUBP))

      herbs<-herbs|>

        dplyr::mutate(
          sumCover =  sum(Cover),
          meanCover = sumCover/nsubp,
          Height_p = (Height*Cover),
          meanHeight = sum(Height_p)/sumCover
        ) |>
        dplyr::rename(
          Cover_subplot = Cover,
          Cover = meanCover,
          Height_s = Height,
          Height =  meanHeight)|>
        dplyr::select(
          ID,
          SP_CODE,
          Species,
          Height,
          Cover
        )|>
        unique()


      #what to do with SUBPLOT

      herbCover<-herbs$Cover

      herbHeight<-herbs$Height

      forest_list$herbCover <- herbCover
      forest_list$herbHeight <- herbHeight

  }else{
    forest_list$herbCover <- NA
    forest_list$herbHeight <- NA
  }


  return(forest_list)


}

