args <- commandArgs(trailingOnly = TRUE)

library(arcgisbinding)
arc.check_product()

source('./sldtool.R')
  library(purrr)
  library(dplyr)
  library(sf)
  library(tibble)
  library(ggplot2)
  library(tidygraph)
  library(ggraph)
#  library(netcapacity)
  library(stringr)
  library(gt)

  precision=1000

  #  in_params <- c()
  #  in_params$greys <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/ACN_POM_CLD_V1_VO_WFL1/FeatureServer/0'
  #  in_params$whites <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/ACN_POM_CLD_V1_VO_WFL1/FeatureServer/1'
  #  in_params$cables <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT66_Fibre_Cable_CLD/FeatureServer/2'
  #  in_params$boundaries <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT66_Boundaries_CLD/FeatureServer/9'
  #  in_params$closures <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/ACN_POM_CLD_V1_VO_WFL1/FeatureServer/4'
  #  in_params$dir <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\boundaries\\pom'
  # # #  out_layer <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\MyProject16.gdb\\drops_named3'
  # # #####
  # # #### to be launched from Rstudio
# in_params <- c()
#  in_params$greys <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT57_CLD_Grey_Premises/FeatureServer/4'
#  in_params$whites <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT57_CLD_White_Premises/FeatureServer/3'
# in_params$cables <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT57_CLD_Fibre_Cable/FeatureServer/6'
# in_params$boundaries <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT57_CLD_Boundaries/FeatureServer/7'
# in_params$closures <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT57_CLD_Splice_Closures/FeatureServer/0'
#  in_params$dir <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\QC reports\\OLT59_20230404'
# in_params$hparam <- 1.5
# in_params$wparam <- 1
# #   # #  out_layer <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\MyProject16.gdb\\drops_named3'
# #####

  cables <-  net_arcgisread(args[1], precision=precision)[[1]]
  closures <- net_arcgisread(args[2], precision=precision)[[1]]
  boundaries <- net_arcgisread(args[3], precision=precision)[[1]]

  cables <- cables[cables$cab_size!=7,]
  cables <- cables[!st_is_empty(cables),]
  boundaries <- valid_boundaries(boundaries[!is_empty(boundaries),])
  closures <- net_devices(closures[!is_empty(closures),])

  output_dir <- args[4]

  cables <- mutate(cables,ref1= str_remove(str_extract(ref, "(^.+?)/"), '/'), ref2= str_remove(str_extract(ref, "/(.+?$)"), '/'))

  #list of not standard cable ref
  wrong_ref <- cables |> filter(is.na(ref1) | is.na(ref2))
  wrong_ref |> as_tibble() |> select(ref)  |> mutate(id=row_number()) |> relocate(id) |> gt() |>  tab_header(title='Not standard cable ref', subtitle = 'The reference must contain one only "/"') |> gtsave(filename = file.path(output_dir,(paste0("cables_check_",1,".html"))))


  #list of duplicated cab ref
#  dist_ref <- cables |> distinct(ref,.keep_all=TRUE)
#  dup_ref <- cables |> setdiff(wrong_ref) |> setdiff(dist_ref)
  dup_ref <- cables[which(duplicated(cables$ref)),] |> setdiff(wrong_ref)
  dup_ref |> as_tibble() |> select(ref) |> mutate(id=row_number()) |> relocate(id) |> gt() |> tab_header(title='Duplicated cable ref', subtitle = 'This reference must be unique') |> gtsave(filename = file.path(output_dir,(paste0("cables_check_",2,".html"))))


  #list of cables pointing to the same device
  same_dev <- cables |> setdiff(wrong_ref) |> setdiff(dup_ref) |> group_by(ref2) |> mutate(n = n()) |> ungroup() |>  filter(n>1) |> select(-n) |> arrange(ref2)
  same_dev |> as_tibble() |> select(ref) |> mutate(id=row_number()) |> relocate(id) |> mutate(id=row_number()) |> relocate(id) |> gt() |> tab_header(title='Cables pointing to the same device', subtitle = 'Each device must be fed by one only cable') |> gtsave(filename = file.path(output_dir,(paste0("cables_check_",3,".html"))))  # same_dev <- unique(cables$ref[!no_std_ref & cables$cab_size!=7]) |> sapply(\(x) strsplit(x,'/')[[1]][[2]]) |> (\(x) x[duplicated(x)])() |>

  #list of cables pointing to no device
  cables_no_dev <- cables |> setdiff(wrong_ref) |> setdiff(dup_ref) |> setdiff(same_dev) |> filter(!(ref1 %in% closures$name & ref2 %in% closures$name))
  cables_no_dev |> as_tibble() |> select(ref) |> mutate(id=row_number()) |> relocate(id) |> gt() |> tab_header(title='Cables pointing to no device', subtitle = 'Each cable must point to two existing devices') |> gtsave(filename = file.path(output_dir,(paste0("cables_check_",4,".html"))))

  #list of cables that don't touch their own devices
  f <- function(x, Y=closures){
#    Z <- Y[unlist(st_touches(x$geom,Y)),]
    Z <- Y[unlist(st_is_within_distance(x$geom,Y,dist=0.1)),]
    return(!(x$ref1 %in% Z$name & x$ref2 %in% Z$name))
  }
  A <- cables |> setdiff(wrong_ref) |> setdiff(dup_ref) |> setdiff(same_dev) |> setdiff(cables_no_dev)
  cables_no_own_dev <- A[sapply(1:nrow(A), (\(i) f(A[i,]))),]
  cables_no_own_dev |> as_tibble() |> select(ref) |> mutate(id=row_number()) |> relocate(id) |> gt() |> tab_header(title='Cables not touching their own devices', subtitle = 'Each cable must touch its own devices') |> gtsave(filename = file.path(output_dir,(paste0("cables_check_",5,".html"))))

  #closures without boundaries
  C <- closures[closures$enc_type %in% c(0,1,2,3,5,7),]
  labels <- C$name[!is.na(C$name) & C$name!=" "]
  c_no_b <- labels[!(labels %in% boundaries$name)]
  c_no_b |> as_tibble() |> mutate(id=row_number()) |> relocate(id) |> gt() |> tab_header(title='Missing boundaries', subtitle = 'Each of these devices must have its own boundary') |> gtsave(filename = file.path(output_dir,(paste0("cables_check_",6,".html"))))

  walk(1:6, (\(i) try(browseURL(file.path(output_dir,(paste0("cables_check_",i,".html")))))))

