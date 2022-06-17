# Read shrub data
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

# Poblet tree data
poblet_trees = openxlsx::read.xlsx("data-raw/PobletData.xlsx", sheet="TreeData")
usethis::use_data(poblet_trees, overwrite = T)
