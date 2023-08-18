args <- commandArgs(trailingOnly = TRUE)

library(arcgisbinding)
arc.check_product()

source('./sldtool.R')
  library(purrr)
  library(dplyr)
  library(sf)
  library(lwgeom)
  library(tibble)
  library(ggplot2)
  library(tidygraph)
  library(ggraph)
#  library(netcapacity)
  library(gt)
  # library(reticulate)
  # use_condaenv("C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3")
  # arcpy <- import("arcpy")

  env = arc.env()
  wkspath <- env$workspace

  precision=100

# #### to be launched from Rstudio
  # in_params <- c()
  # in_params$greys <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\GTN.gdb\\OLT50_Grey_Premises_Merge'
  # #  in_params$whites <- 'https://services9.arcgis.com/arW8CTVUZbXZBFD9/arcgis/rest/services/STR_STB1_Full_OLT_CLD/FeatureServer/7'
  # in_params$cables <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\GTN.gdb\\DAF_OLT50_Drop_Wires_Merge'
  # in_params$boundaries <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT50_Boundaries/FeatureServer/23'
  # in_params$closures <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT50_Splice_Closures/FeatureServer/8'
  # in_params$dir <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\QC reports\\OLT50-20230726'
  #  out_layer <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\MyProject16.gdb\\drops_named3'
  # greys <-  net_arcgisread(in_params$greys, precision=precision)[[1]]
  # cables <-  net_arcgisread(in_params$cables, precision=precision)[[1]]
  # boundaries <-  net_arcgisread(in_params$boundaries, precision=precision)[[1]]
  # closures <-  net_arcgisread(in_params$closures, precision=precision)[[1]]
  # output_dir <- in_params$dir
#   #####




  greys <-  net_arcgisread(args[2], precision=precision)[[1]]
  cables <-  net_arcgisread(args[5], precision=precision)[[1]]
  boundaries <-  net_arcgisread(args[3], precision=precision)[[1]]
  closures <-  net_arcgisread(args[4], precision=precision)[[1]]

  output_dir <- args[6]

  greys <- greys[!st_is_empty(greys),]
  cables <- cables[!st_is_empty(cables),]
  boundaries <- valid_boundaries(boundaries[!is_empty(boundaries),])
  closures <- net_devices(closures[!is_empty(closures),])

  if (nchar(args[1])<2){
    dp <- designed_prems(greys, cables)
  } else {
    whites <- net_arcgisread(args[1], precision=precision)[[1]]
    whites <- whites[!is_empty(whites),]
    dp <- designed_prems(greys, cables, whites)
  }

  gy <- net_geomgrouped(dp)#premises raggruppati
  fx <- cables[cables$cab_size == '7',]
  fxgy <- net_groupedbytouch(gy, fx)#drops raggruppati
  gzfx <- net_groupedbytouch2(fxgy, closures)#closures raggruppati
  b_min <- net_groupedbyintersect(gy, boundaries) |> lapply (\(x) x[which.min(st_area(x)),])#boundary minimo

  alert1 <- mapply((\(x,y) nrow(x) != nrow(y)), fxgy, gy)
  alert2 <- mapply((\(x,y) (nrow(x) != 1 || !(x$name %in% y$name))), b_min, gzfx)

  alert <- alert1 | alert2


  stringfy <- function(x){
    ifelse(length(x) == 0 , " " , paste0(sapply(x, as.character), collapse='/'))
  }

  temp <- pmap(list(gy[alert],fxgy[alert], b_min[alert], gzfx[alert]),(\(x,t,y,z) data.frame(Premise=stringfy(x$uprn),N_prem=nrow(x),N_drops=nrow(t), Boundary=stringfy(y$name),Device=stringfy(z$name)))) |> list_rbind()

  fname = file.path(output_dir,(paste0("dropsQC", ".html")))
  temp |> mutate(id=row_number()) |> relocate(id) |> gt() |> tab_header(title='Premise connection issues', subtitle = 'The #drops should match the #premises - The boundary name should be in the list of the devices connected to the premise') |> gtsave(filename=fname)
  browseURL(fname)


  fname2 = file.path(output_dir,(paste0("dpremsQC", ".html")))
  boundaries$d_prems <- sapply(st_intersects(boundaries,dp), (\(x) length(x)))
  boundaries |> as_tibble() |> filter(t_homes != d_prems) |> select(name, t_homes, d_prems) |> mutate(id=row_number()) |> relocate(id) |> gt() |> tab_header(title='Designed premises', subtitle = "Value in boundary layer's t_homes field  vs Designed premises in boundary") |> gtsave(filename=fname2)
  browseURL(fname2)

  # d_named <- pmap(list(fxgy[!alert], gy[!alert], b_min[!alert], gzfx[!alert]), \(g,h,s,r) mutate(g, from = s$name, prem_id = h$uprn , ref=paste0(from, '/', prem_id))) |> list_rbind() #|> as_tibble() |> select(GlobalID, ref, prem_id)
  # arc.write(file.path(out_layer), data=st_as_sf(d_named), validate=TRUE)
  #d_named <- pmap(list(fxgy[!alert], gy[!alert], b_min[!alert], gzfx[!alert]), \(g,h,s,r) mutate(g, from = s$name, prem_id = h$uprn , ref=paste0(from, '/', prem_id))) |> list_rbind() |> as_tibble() |> select(GlobalID, ref, prem_id)
  d_named <- pmap(list(fxgy[!alert], gy[!alert], b_min[!alert], gzfx[!alert]), \(g,h,s,r) mutate(g, from = s$name, tprem_id = h$uprn)) |> list_rbind() |> as_tibble() |> group_by(from) |> mutate(tref=paste0(from, '/', 1:n())) |> ungroup() |> select(GlobalID, tref, tprem_id)


  arc.write(args[7], data=d_named, overwrite = TRUE)
#  arc.write(file.path(wkspath,'drops_label'), data=d_named, overwrite = TRUE)

#  gdb = "'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\MyProject16.gdb"
#  tocopy = file.path(gdb, "OLT52CablesCopy")
#  layer = arcpy$management$MakeFeatureLayer(tocopy, 'copied')
#  arcpy$analysis$Buffer(tocopy, file.path(gdb,"Cases_state_Buffer_1"), "1 Kilometers", "FULL", "ROUND", "NONE", NULL, "PLANAR")
#  arcpy$management$SelectLayerByAttribute(file.path(in_params$cables), "NEW_SELECTION", "cab_size LIKE '%7%'")
#  arcpy$management$JoinField(in_params$cables, "GlobalID", out_layer, "GlobalID", "ref;prem_id", "NOT_USE_FM", None)
#  py_run_file('provapy')
#  source_python('C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\Capacity.atbx\\provapy')
#  pytool(in_params$cables)

