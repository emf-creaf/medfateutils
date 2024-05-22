IFN_path <- "~/OneDrive/EMFDatasets/ForestInventories/IFN/"
MFWdir <- "~/OneDrive/mcaceres_work/model_development/medfate_development/"
NFIparamDir <- "~/OneDrive/mcaceres_work/model_development/medfate_development/MedfateSpeciesParametrization/NFIs_parametrization/"

# SpParamsES, SpParamsUS, SpParamsFR, SpParamsAU --------------------------------------
SpParamsES <- readRDS(paste0(NFIparamDir, "Rdata/sp/SpParams_filled_strict_allom_sp.rds"))
# Results of meta-modelling exercise
metamodellingParamsSpecies = readRDS(paste0(MFWdir, "Metamodelling_TR_WUE/Rdata/metamodelling_params.rds"))
SpParamsES = medfate::modifySpParams(SpParamsES, metamodellingParamsSpecies, subsetSpecies = FALSE)
# Load growth calibration results
RGRcambiummaxTrees = readRDS(paste0(MFWdir,"GrowthCalibration/Rdata/RGRcambiummax_trees.rds"))
SpParamsES = medfate::modifySpParams(SpParamsES, RGRcambiummaxTrees, subsetSpecies = FALSE)
# Load ingrowth calibration results
## SHOULD BE RECALIBRATED: THEY REFER TO INGROWTH (~7.5 cm)
recruitmentParamsSpecies = readRDS(paste0(MFWdir,"MortalityRegenerationCalibration/Rdata/final_recruitment_params.rds"))
recruitmentParamsSpecies$RecrTreeHeight <- recruitmentParamsSpecies$RecrTreeHeight/10
recruitmentParamsSpecies$IngrowthTreeDensity <- recruitmentParamsSpecies$RecrTreeDensity
recruitmentParamsSpecies$RecrTreeDensity <- NULL
SpParamsES = medfate::modifySpParams(SpParamsES, recruitmentParamsSpecies, subsetSpecies = FALSE)
# Load Baseline mortality calibration results
mortalityParamsSpecies = readRDS(paste0(MFWdir,"MortalityRegenerationCalibration/Rdata/mort_rates.rds"))
SpParamsES = medfate::modifySpParams(SpParamsES, mortalityParamsSpecies, subsetSpecies = FALSE)
# Load SurvivalModel calibration results
survivalParamsSpecies = readRDS(paste0(MFWdir,"MortalityRegenerationCalibration/Rdata/survival_models.rds"))
SpParamsES = medfate::modifySpParams(SpParamsES, survivalParamsSpecies, subsetSpecies = FALSE)
# Load SurvivalModel calibration results
resproutingParamsSpecies = readxl::read_xlsx(paste0(MFWdir,"MortalityRegenerationCalibration/Data/ResproutingMED.xlsx"))
names(resproutingParamsSpecies)[1] = "Species"
SpParamsES = medfate::modifySpParams(SpParamsES, resproutingParamsSpecies, subsetSpecies = FALSE)
usethis::use_data(SpParamsES, overwrite = T)

SpParamsFR <- readRDS(paste0(NFIparamDir, "Rdata/fr/SpParams_filled_strict_allom_fr.rds"))
usethis::use_data(SpParamsFR, overwrite = T)
SpParamsUS <- readRDS(paste0(NFIparamDir, "Rdata/us/SpParams_filled_strict_allom_us.rds"))
usethis::use_data(SpParamsUS, overwrite = T)
SpParamsAU <- readRDS(paste0(NFIparamDir, "Rdata/au/SpParams_filled_strict_allom_au.rds"))
usethis::use_data(SpParamsAU, overwrite = T)

# Species mapping table
IFN_species_mapping <- read.table("data-raw/IFN_species_mapping.csv", sep="\t", header=TRUE)
# IFN_species_mapping$Name[IFN_species_mapping$Name == "Arbutus unedo "] <- "Arbutus unedo"
usethis::use_data(IFN_species_mapping, overwrite = T)

NFI_SP_mapping <- read.table("data-raw/NFI_SP_mapping.csv", sep=";", header=TRUE, na.strings = "")
NFI_SP_mapping$NFICode <- as.character(NFI_SP_mapping$NFICode)
usethis::use_data(NFI_SP_mapping, overwrite = T)

NFI_FR_mapping <- read.table("data-raw/NFI_FR_mapping.csv", sep=";", header=TRUE, na.strings = "")
NFI_FR_mapping$NFICode <- as.character(NFI_FR_mapping$NFICode)
usethis::use_data(NFI_FR_mapping, overwrite = T)

NFI_US_mapping <- read.table("data-raw/NFI_US_mapping.csv", sep=";", header=TRUE, na.strings = "")
NFI_US_mapping$NFICode <- as.character(NFI_US_mapping$NFICode)
usethis::use_data(NFI_US_mapping, overwrite = T)

# Read IFN2 data
piesMayoresIFN2 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN2/piesMayoresDataIFN2_Catalunya.csv"),
                                                sep="\t", header=TRUE, colClasses = c("character", "character",
                                                                                      "character", "character", "character",
                                                                                      "numeric", "numeric", "numeric", "numeric")))
piesMenoresIFN2 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN2/piesMenoresDataIFN2_Catalunya.csv"),
                                                sep="\t", header=TRUE, colClasses = c("character", "character", "character",
                                                                                      "character", "integer", "integer", "integer")))
matorralIFN2 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN2/shrubDataIFN2_Catalunya.csv"),
                                             sep="\t", header=TRUE, colClasses = c("character", "character", "character",
                                                                                   "character", "numeric", "numeric")))

IDs = c("081065", "081066", "081067", "081068", "081069")
piesMayoresIFN2 <- piesMayoresIFN2[piesMayoresIFN2$ID %in% IDs,c("Provincia", "Estadillo", "ID", "OrdenIf2",
                                                                 "Especie", "Dn1", "Dn2", "Ht")]
piesMenoresIFN2 <- piesMenoresIFN2[piesMenoresIFN2$ID %in% IDs,]
matorralIFN2 <- matorralIFN2[matorralIFN2$ID %in% IDs,]
usethis::use_data(piesMayoresIFN2, overwrite = T)
usethis::use_data(piesMenoresIFN2, overwrite = T)
usethis::use_data(matorralIFN2, overwrite = T)

# Read IFN3 data
piesMayoresIFN3 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN3/treeDataIFN3_Catalunya.csv"),
                                                sep="\t", header=TRUE, colClasses = "character"))
piesMayoresIFN3 <- piesMayoresIFN3[piesMayoresIFN3$IDPARCELA %in% IDs,]
piesMayoresIFN3<-piesMayoresIFN3[,c("Provincia", "Estadillo", "Clase", "Subclase","IDPARCELA","IDCLASE", "ID", "OrdenIf2", "OrdenIf3",
                                    "Especie", "Dn1", "Dn2", "Ht")]
piesMayoresIFN3$Dn1 <- as.numeric(piesMayoresIFN3$Dn1)
piesMayoresIFN3$Dn2 <- as.numeric(piesMayoresIFN3$Dn2)
piesMayoresIFN3$Ht <- as.numeric(piesMayoresIFN3$Ht)
usethis::use_data(piesMayoresIFN3, overwrite = T)

regeneraIFN3 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN3/regDataIFN3_Catalunya.csv"),
                                                sep="\t", header=TRUE, colClasses = "character"))
regeneraIFN3 <- regeneraIFN3[regeneraIFN3$IDPARCELA %in% IDs,]
regeneraIFN3 <- regeneraIFN3[,c("Provincia", "Estadillo", "Clase", "Subclase","IDPARCELA","IDCLASE", "ID",
                                "Especie", "CatDes", "Tipo", "Densidad", "NumPies", "Hm")]
regeneraIFN3$NumPies <- as.numeric(regeneraIFN3$NumPies)
regeneraIFN3$Hm <- as.numeric(regeneraIFN3$Hm)
usethis::use_data(regeneraIFN3, overwrite = T)

matorralIFN3 <- tibble::as_tibble(read.table(file=paste0(IFN_path, "Products/IFN3/shrubDataIFN3_Catalunya.csv"),
                                             sep="\t", header=TRUE, colClasses = "character"))
matorralIFN3 <- matorralIFN3[matorralIFN3$IDPARCELA %in% IDs,]
matorralIFN3<-matorralIFN3[,c("Provincia", "Estadillo", "Clase", "Subclase", "IDPARCELA","IDCLASE", "ID",
                              "Especie", "Fcc", "Hm")]
matorralIFN3$Fcc <- as.numeric(matorralIFN3$Fcc)
matorralIFN3$Hm <- as.numeric(matorralIFN3$Hm)
usethis::use_data(matorralIFN3, overwrite = T)

shrubDataIFN3 = readRDS(file="~/OneDrive/Datasets/IFN/Products/IFN3/Rdata/shrubDataIFN3_Catalunya.rds")
treeDataIFN3 = readRDS(file="~/OneDrive/Datasets/IFN/Products/IFN3/Rdata/treeDataIFN3_Catalunya.rds")

IDs = c("81065", "81066", "81067", "81068", "81069")
example_shrubdata_ifn = shrubDataIFN3[shrubDataIFN3$ID %in% IDs,]
example_shrubdata_ifn$REG = NULL
usethis::use_data(example_shrubdata_ifn, overwrite = T)

example_treedata_ifn = treeDataIFN3[treeDataIFN3$ID %in% IDs,]
example_treedata_ifn$Provincia = NULL
example_treedata_ifn$Estadillo = NULL
usethis::use_data(example_treedata_ifn, overwrite = T)

load("D:/Datasets/IFN/R scripts/LM optimized RS.RData")
usethis::use_data(reg.V.2, reg.Z.2, internal=TRUE, overwrite=T)
rm(list=ls())

#test data for forest
load("data-raw/example_ffi.RData")
load("data-raw/example_fia.RData")
load("data-raw/prova2_ifn.RData")
standard_ifn<-prov2_ifn
standard_ffi<-example_ffi
standard_fia<-example_fia
usethis::use_data(standard_ifn, standard_ffi, standard_fia, internal =TRUE,overwrite = T)

