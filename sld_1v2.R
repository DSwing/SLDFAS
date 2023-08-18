args <- commandArgs(trailingOnly = TRUE)

library(arcgisbinding)
arc.check_product()

source('./sldtool.R')

library(tidyr)
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
library(ggforce)
library(ggiraph)
library(stringr)
library(igraph)

precision=100
cable_sizes<-c('12F','24F','48F','96F','144F','288F','1F','4F','8F','60F')
placements<-c('UG','OH','UG/OH')

enc_types<-c('OLT','ODP1','ODP2','ODP3','Joint','ODP1/ODP2','JN','MPT','MDU')
sp_types<-c('1:8','1:4','','1:32')
sp_types_n<-c(8,4,0,32)
enc_sizes <- c('BPEO 0 1x 1:4','BPEO 0 2x 1:4','BPEO 1.5 1x 1:8 + 1x 1:4','BPEO 1.5 1x 1:8 + 2x 1:4','BPEO 1.5 1x 1:8','BPEO 1.5 3x 1:4','BPEO 0','BPEO 1.5 12 Tray Joint','BPE02 24 Tray Joint','MPT 4 Ports','MPT 8 Ports','MPT 12 Ports','MPT 12 Ports + 4 Ports','MPT 12 Ports + 8 Ports','MPT 12 Ports + 12 Ports','BPEO Size 2 with 1 Splitters','BPEO Size 2 with 2 Splitters','BPEO Size 2 with 3 Splitters','BPEO Size 3 (288F)','BPE0 1.5 with 1x 1:32 Splitter','BPE0 1.5 with 2x 1:32 Splitter','BPEO 1.5 1x 1:8 + 3x 1:4','BPE0 1.5 1x 1:4','BPE0 1.5 2x 1:4','BPE0 1.5 4x 1:8','BPE0 1.5 4x 1:4')
closures_placements <- c('Aerial', 'Underground', 'Building', 'Internal')
REEL_LENGTH <- 3500


# # # # # # # # #### to be launched from Rstudio
args <- c()
args[1] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/Hazel_Flimby_CLD_V1_VO_WFL1/FeatureServer\\1'
args[2] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/Hazel_Flimby_CLD_V1_VO_WFL1/FeatureServer\\0'
args[3] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/Hazel_Flimby_CLD_V1_VO_WFL1/FeatureServer\\6'#cables
args[4] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/Hazel_Flimby_CLD_V1_VO_WFL1/FeatureServer\\2'#splice_closures
args[5] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\GTN.gdb\\Hazel_Flimby_C_ExportFeature'#boundaries
args[6] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\QC reports\\Flimby\\Flimby20230818'
args[7] <- 'True'

# # # ####
# args <- c()
# args[1] <- ''
# args[2] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\GTN.gdb\\Premises_Merge'
# args[3] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\GTN.gdb\\CableMerge'#cables
# args[4] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\GTN.gdb\\MPT_merge'#splice_closures
# args[5] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\GTN.gdb\\Aboundaries'#boundaries
# args[6] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\QC reports\\GCN\\GCN-20230814'
# # # #   ### to be launched from Rstudio
# in_params <- c()
# args[2] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT27_CLD_VO/FeatureServer\\4'#greys
# args[1] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT27_CLD_VO/FeatureServer\\3'#whites
# args[3] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT27_CLD_VO/FeatureServer\\5'#cables
# args[5] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT27_CLD_VO/FeatureServer\\7'#boundaries
# args[4] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/DAF_OLT27_CLD_VO/FeatureServer\\0'#closures
# args[6] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\QC reports\\OLT27\\OLT27-20230808'
# # # # #  out_layer <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\MyProject16.gdb\\drops_named3'
# # #####


cables <-  net_arcgisread(args[3], precision=precision)[[1]]
closures <- net_arcgisread(args[4], precision=precision)[[1]]
boundaries <- net_arcgisread(args[5], precision=precision)[[1]]
greys <-  net_arcgisread(args[2], precision=precision)[[1]]

hazel <- as.logical(args[7])
# hparam <- as.integer(args[7])
# wparam <- as.integer(args[8])

output_dir <- args[6]

if (nchar(args[1])<2){
  dp <- designed_prems(greys, cables)
} else {
  whites <- net_arcgisread(args[1], precision=precision)[[1]]
  dp <- designed_prems(greys, cables, whites)
}

closures <- closures[!st_is_empty(closures),]
cables <- cables[!st_is_empty(cables),]
cables <- cables[!duplicated(cables$ref),]
boundaries <- valid_boundaries(boundaries[!is_empty(boundaries),])

closures_fas <- closures
cables_fas <- cables
boundaries_fas <- boundaries

closures <- mutate(closures, label_2yn = case_when(!is.na(label_2) & label_2 != "" & label_2 != " " & sp2_type %in% c(1,2,4) ~ TRUE,
                                                   TRUE ~ FALSE))
#  devices <- net_devices(closures[!is_empty(closures),])
devices <- net_devices(closures)

devices$t_homes <- sapply(devices$name, (\(x) boundaries[boundaries$name==x,]$t_homes))

###cab_size, placement, ref

cables <- cables[cables$cab_size != 7,]
cables <- mutate(cables,ref1= str_remove(str_extract(ref, "(^.+?)/"), '/'), ref2= str_remove(str_extract(ref, "/(.+?$)"), '/')) |> filter(!is.na(ref1) & ref1 !=" " & !is.na(ref2) & ref2 !=" " )
net_c_v <- net_virtual_cables(closures)

if (length(E(net_c_v))>0){
  vcables <- net_c_v |> activate(edges) |> mutate(ref1=.N()$name[from], ref2=.N()$name[to], ref=paste0(ref1, '/',ref2), cab_size='1') |> filter(!is.na(ref1) & ref1 !=" " & ref1 != 'NA' & !is.na(ref2) & ref2 !=" " & ref2 != 'NA') |> as_tibble()
} else {
  vcables <- net_c_v |> activate(edges) |> as_tibble()
}
# E(vcables)$ref1 <- str_remove(str_extract(E(vcables)$ref, "(^.+?)/"), '/')
# E(vcables)$ref1 <- str_remove(str_extract(E(vcables)$ref, "/(.+?$)"), '/')
# E(vcables)$cab_size <- 1
##################qua
if (nrow(vcables)>0){cables <- bind_rows(cables, vcables)}

cables$node_text <- cables |> (\(x) paste0(x$ref,"\n",
                                           cable_sizes[as.integer(x$cab_size)]," / ",
                                           as.integer(sf::st_length(sf::st_geometry(x))),"m",
                                           "(",placements[as.integer(x$placement)],")"))()

devices <-  mutate(devices, splitt=NULL, label_text=NULL)

sp_t <- function(x){
  if(x %in% c(1,2,3,4))
  {sp_types[as.numeric(x)]}
  else {""}
}
splitt <- function(x,y,s1,s2,s3){
  if (!y & s3 %in% c(1,2,4))
  {paste0(sp_t(s1),'+',sp_t(s2),'+',sp_t(s3),'\n')}
  else if (!y & !(s3 %in% c(1,2,4)) & s2 %in% c(1,2,4))
  {paste0(sp_t(s1),'+',sp_t(s2),'\n')}
  else if ((!y & !(s3 %in% c(1,2,4)) & !(s2 %in% c(1,2,4)) & s1 %in% c(1,2,4)) | (y & x=='label'))
  {paste0(sp_t(s1),'\n')}
  else if (x=='label_2' & s3 %in% c(1,2,4))
  {paste0(sp_t(s2),'+',sp_t(s3),'\n')}
  else if (x=='label_2' & !(s3 %in% c(1,2,4)) & s2 %in% c(1,2,4))
  {paste0(sp_t(s2),'\n')}
}


devices$split <- mapply((\(x,y,s1,s2,s3)splitt(x,y,s1,s2,s3)), devices$lab, devices$label_2yn, devices$sp1_type,devices$sp2_type, devices$sp3_type)

splitt2 <- function(x,y,sn1,s1,sn2,s2,sn3,s3){
  if (!y & s3 %in% c(1,2,4))
  {paste0(sn1, ':', sp_t(s1),'\n', sn2, ':' ,sp_t(s2),'\n', sn3, ':' ,sp_t(s3),'\n')}
  else if (!y & !(s3 %in% c(1,2,4)) & s2 %in% c(1,2,4))
  {paste0(sn1, ':', sp_t(s1),'\n', sn2, ':' ,sp_t(s2),'\n')}
  else if ((!y & !(s3 %in% c(1,2,4)) & !(s2 %in% c(1,2,4)) & s1 %in% c(1,2,4)) | (y & x=='label'))
  {paste0(sn1, ':', sp_t(s1),'\n')}
  else if (x=='label_2' & s3 %in% c(1,2,4))
  {paste0(sn2, ':', sp_t(s2),'\n', sn3, ':', sp_t(s3),'\n')}
  else if (x=='label_2' & !(s3 %in% c(1,2,4)) & s2 %in% c(1,2,4))
  {paste0(sn2, ':', sp_t(s2),'\n')}
}

devices$split2 <- mapply((\(x,y,sn1,s1,sn2,s2,sn3,s3)splitt2(x,y,sn1,s1,sn2,s2,sn3,s3)), devices$lab, devices$label_2yn,
                         devices$sp1_label, devices$sp1_type,
                         devices$sp2_label, devices$sp2_type,
                         devices$sp3_label, devices$sp3_type)


devices$d_type <- mapply((\(x,y) (if(is.na(y)) {""}
                                  else if (y %in% c(0:8)[-6]) {enc_types[1+as.numeric(y)]}
                                  else if (y == 5 & x =='label') {'ODP1'}
                                  else if (y == 5 & x =='label_2') {'ODP2'} )), devices$lab, devices$enc_type)


# devices <- mutate(rowwise(devices),label_text=ifelse((!is.na(name) & name !=" "),
#                                                                paste0( d_type,
#                                                                        if(!is.na(enc_type) & enc_type!=4) {paste0('\t-\t', 'HC: ' , t_homes)},
#                                                                        '\n', name,
#                                                                        '\n', split,
#                                                                      'X:',round(st_coordinates(geom)[1],digits=1),
#                                                                      ' Y:',round(st_coordinates(geom)[2],digits=1)),
#                                                                      NA))

devices <- mutate(rowwise(devices),label_text=ifelse((!is.na(name) & name !=" "),
                                                     paste0( d_type,
                                                             '(', switch(placement, '1'='Aerial', '2'='Underground', '3'='Building', '4'='Internal', 'NA'), ')',
                                                             if(!is.na(enc_type) & enc_type!=4) {paste0('\t-\t', 'HC: ' , t_homes)},
                                                             '\n', name,
                                                             '\n', 'Enc Size: ', enc_sizes[as.integer(enc_size)],
                                                             '\n', split2,
                                                             'Reel end: ', switch(reel_end, "1"="Yes", "2"="No", "NA"),
                                                             '\n',
                                                             'X:',round(st_coordinates(geom)[1],digits=1),
                                                             ' Y:',round(st_coordinates(geom)[2],digits=1)),
                                                     NA))


cables$name <- cables$ref
cables <- cables[!duplicated(cables$name),]

devices <- dplyr::mutate(devices, status=as.integer(status),owner=as.integer(owner), placement=as.integer(placement), node_type='device')
cables  <-  dplyr::mutate(cables, status=as.integer(status),owner=as.integer(owner), placement=as.integer(placement), node_type='cable')

c_edges <- data.frame(from=c(cables$ref1,cables$ref), to=c(cables$ref,cables$ref2))

g1 <- tbl_graph(edges = c_edges)
g2 <- tbl_graph(nodes = cables)
g3 <- tbl_graph(nodes = devices)

g <- Reduce((\(x,y) graph_join(x,y,by='name')),list(g2,g3,g1))

g_grouped <-  mutate(g, group = group_components(type = "weak"))
net_g_list <- map(unique(tibble::as_tibble(activate(g_grouped,nodes))$group), (\(k) filter(g_grouped, group==k)))


# net_plot_sld_loc <- function(x, dir, fname,layout='tree', h=1, w =1){
#
#   tt <- function(name){
#     paste0('fas/',name,'.svg')
#   }
#
#   layout <- ggraph::create_layout(x, layout)
#   layout$y <- -layout$y
#   yr <- max(sapply(unique(layout$x), (\(k) length(layout$y[layout$x==k]))))
#   xr <- max(sapply(unique(layout$y), (\(k) length(layout$x[layout$y==k]))))
#   p <- ggraph::ggraph(layout) +
#     ggraph::geom_edge_diagonal(width = 0.2,alpha=0.2) +
#     ggraph::geom_node_text(ggplot2::aes(label = node_text), size=3, hjust=0.5, vjust=-0.2, repel=FALSE) +
#
#     ggiraph::geom_label_interactive(ggplot2::aes(x=x,y=y,label = label_text, colour = as.factor(d_type),
#                                                  onclick=paste0('window.open("',tt(name),'")')),
#                                     size=3, label.padding=unit(0.25,"lines"), repel=FALSE) +
#     ####
#     ggplot2::coord_flip() +
#     scale_colour_manual(values=c('OLT'='violet', "ODP1"="blue", "ODP2"="red", "ODP3"="orange","Joint"="black","JN"="purple","MPT"="orange", "MDU"="grey"))+
#     #    scale_fill_manual(values=c("TRUE"="#fce15d", "FALSE"="white"))+
#     ggplot2::scale_y_continuous(limits = c(min(layout$y)-1, max(layout$y)+1))+
#     ggplot2::scale_x_continuous(limits = c(min(layout$x)-3, max(layout$x)+3))+
#     theme(legend.position="none")
#   ggplot2::ggsave(plot=p, filename=file.path(dir, paste0(fname,'.svg')), device = 'svg', width = w*15*yr,height=h*1.5*xr+2,units="in", limitsize=FALSE)
#
#   x <- ggiraph::girafe(ggobj = p,width_svg=100*w, height_svg=100*h/2)
#   x <- ggiraph::girafe_options(x, ggiraph::opts_zoom(min = .7, max = 1000) )
#   htmltools::save_html(x, file.path(dir, paste0('sldfas_',fname,'.html')))
# }
#  iwalk(net_g_list, (\(k_i , i) try(net_plot_sld_loc(k_i , dir=output_dir, fname=paste0("SLD", i),'tree', hparam ,wparam))))
#  iwalk(net_g_list, (\(k_i , i) try(net_plot_sld(k_i , dir=output_dir, fname=paste0("SLD", i),'tree', hparam ,wparam))))



##########################


#######################FAS#######################
#  cables <-  net_arcgisread(args[3], precision=precision)[[1]]
#  closures <- net_arcgisread(args[4], precision=precision)[[1]]
#  boundaries <- net_arcgisread(args[5], precision=precision)[[1]]
#  greys <-  net_arcgisread(args[2], precision=precision)[[1]]

#  boundaries <- valid_boundaries(boundaries[!is_empty(boundaries),])
# #################adding olt where it is not

pickup <- unique(as_tibble(filter(g, node_is_root(), !node_is_leaf() ,(enc_type!=0|is.na(enc_type)) & lab!='label_2'))$name)
if(length(pickup)>0){
  bg <- lapply(pickup, (\(x) filter(g, node_distance_from(which(as_tibble(g)$name==x))>=0 & is.finite(node_distance_from(which(as_tibble(g)$name==x)))) %>% as_tibble() %>% filter(name %in% boundaries_fas$name)))
  ####start20230816
  pickup <- pickup[sapply(bg,nrow)>0]
  bg <- bg[sapply(bg,nrow)>0]
  ####end20230816
  bpickup <- imap(bg,\(x, idx) filter( boundaries_fas, name %in% x$name)  %>% st_combine() %>% (\(x) st_buffer(x,1.5))() %>% st_sfc() %>% (\(x) st_as_sf(x, crs=st_crs( boundaries_fas)))() %>% mutate(name = paste0('OLT(other map)',idx), level_='1')) %>% bind_rows()
  dpickup <- st_as_sf(data.frame(label=paste0('OLT(other map)',1:length(pickup)), enc_type='0', geom=rep(st_sfc(st_point(c(0,0))),length(pickup))), crs=st_crs(closures_fas))
  #  cpickup <- st_as_sf(data.frame(ref1=paste0('OLT',1:length(pickup)), ref2=pickup, ref='')), geom=(st_sfc(st_linestring(c(rep(st_point(c(0,0)),length(pickup)), closures_fas[closures_fas$label==pickup]$geom))))),crs=st_crs(cables_fas))
  cpickup <- st_as_sf(data.frame(ref1=paste0('OLT(other map)',1:length(pickup)),
                                 ref2=pickup,
                                 cab_size=rep('6',length(pickup)),
                                 geom=st_sfc(mapply((\(x,y) st_sfc(st_linestring(c(st_point(st_coordinates(x)),st_point(st_coordinates(y)))))),dpickup$geom, closures_fas[closures_fas$label %in% pickup,]$geom))[1:length(pickup)]), crs=st_crs(cables_fas))
  cpickup$ref <- paste0(cpickup$ref1,'/',cpickup$ref2)
  dpickup <- rename(dpickup, geom=geometry)
  bpickup <- rename(bpickup, geom=x)
  
  boundaries_fas <- bind_rows( boundaries_fas, bpickup)
  closures_fas <- bind_rows(closures_fas, dpickup)
  cables_fas <-  bind_rows(cables_fas, cpickup)}########bind_rows(cables_fas, cpickup)}###########


devices <- closures_fas[!is_empty(closures_fas),] |>
  mutate(closures_fas, label_2yn = case_when(!is.na(label_2) & label_2 != "" & label_2 != " " & sp2_type %in% c(1,2,4) ~ TRUE, TRUE ~ FALSE)) |>
  net_devices() |> as_tibble() |> select(name, enc_type,enc_size,Nout, lab, label_2yn,sp1_label, sp1_type, sp2_label,sp2_type, sp3_label,sp3_type)

b <- valid_boundaries( boundaries_fas)
b <- mutate(b, label_text= paste('Name: ',name,' - ', 'Level: ', dev_type))

b <- left_join(b,devices,by="name")


vc <- vcables[!(vcables$ref %in% cables_fas$ref),]
if(nrow(vc)>0) {
  net_c <- bind_rows(cables_fas, vc) |> net_cables()
}else{net_c <- cables_fas |> net_cables()}#  net_c <- net_cables(cables_fas) |> graph_join(filter(vcables, !(ref %in% cables_fas)))

net_b <-  mutate(net_boundaries(b), group = group_components(type = "weak"))

warns <- function(cables_net, nfrom, nto, clos = closures_fas){
  #    cw <- net_cables(cables) |> graph_join(net_virtual_cables(clos)) |> igraph::distances(mode="out") |> is.finite()
  cw <- cables_net |> igraph::distances(mode="out") |> is.finite()
  cwres <- function(x,y){
    if ((x %in% unlist(dimnames(cw))) & (y %in% unlist(dimnames(cw)))){
      cw[x,y]
    } else {
      FALSE
    }
  }
  mapply(cwres, nfrom, nto)
}

#####temporary test
#dp <-  boundaries_fas |> filter(name=='WTH-2-JN05') |> st_sample(10000)|> st_as_sf() |> rename(geom=x) |> mutate(virtual_prem=TRUE)  |> bind_rows(dp)
#
######


net_bload <- function(x, dp=dp, cables_net=net_c){
  net_load(x,dp) |>
    activate(edges) |>
    mutate(nfrom = .N()$name[from], nto = .N()$name[to], n_min=.N()$in_load[to]) |>
    mutate(warn = !warns(cables_net, nfrom, nto)) |>
    activate(nodes)
}


net_b_list <- map(unique(tibble::as_tibble(activate(net_b,nodes))$group), (\(k) net_bload(filter(net_b, group==k),dp)))

h <- function(nc,nb){
  cable_sizes<-c(12,24,48,96,144,288,1,4,8,60)
  nb <- nb |> activate(edges) |> as_tibble() |> filter(!warn)
  nc <- nc |> activate(edges) |> mutate(w=0, w1=0, w2=0, wj1=0, wj2=0) |> activate(nodes)
  
  if (nrow(nb)>0){
    temp <- pmap(list(nb$nfrom,nb$nto,nb$n_min), (\(x,y,t) morph(nc, to_shortest_path, from=which(as_tibble(nc)$name==x), to=which(as_tibble(nc)$name==y))|>
                                                    activate(edges) |> mutate(w=t,
                                                                              w1 = ifelse(.N()$name[from]==x, t, 0),
                                                                              w2 = ifelse(.N()$name[to]==y,t,0),
                                                                              wj1 = ifelse(.N()$name[from]==x, 0, t),
                                                                              wj2 = ifelse(.N()$name[to]==y, 0, t)) |> activate(nodes) |> unmorph() |> activate(edges) |> as_tibble()))
    tab <- do.call("rbind", temp)# |> group_by(name) |> summarise(min_f=sum(w), wt1=sum(w1), wt2=sum(w2), wjt1=sum(wj1), wjt2=sum(wj2)) |> filter(min_f>0)|> ungroup()
    
    return (tab)}
}


#  lapply(net_b_list,(\(x) try(h(net_c,x)))) |> list_rbind() |> group_by(name) |> summarise(tot_f=sum(min_f), wtt1=sum(wt1), wtt2=sum(wt2), wjtt1=sum(wjt1), wjtt2=sum(wjt2))
fibres <- lapply(net_b_list,(\(x) try(h(net_c,x)))) |> list_rbind() |>
  group_by(ref,cab_size) |> summarise(tot_f=sum(w), wtt1=sum(w1), wtt2=sum(w2), wjtt1=sum(wj1), wjtt2=sum(wj2)) |>
  (\(x) lapply(x,rep, x$tot_f))() |> as_tibble() |> group_by(ref) |>
  mutate(fibre=1:n(),
         a=ifelse(fibre<=wtt1, TRUE, FALSE),
         b=ifelse(fibre<=wtt2, TRUE, FALSE),
         name=paste0(ref, ' fibre:', fibre),
         ref1= str_remove(str_extract(ref, "(^.+?)/"), '/'),
         ref2= str_remove(str_extract(ref, "/(.+?$)"), '/')
  )

# net_fibres <- tbl_graph(nodes=fibres)
##########devices

lab2 <- devices |> filter(!(enc_type %in% c(3,7,8))) |> filter(lab=='label_2') |> tidyr::pivot_longer(cols=c(sp2_type, sp3_type), names_to='sp1_sp2_sp3', values_to='n_ports') |> mutate(n_ports=sp_types_n[as.numeric(n_ports)]) |> filter(n_ports > 0)
lab1_nolab2 <- devices |> filter(!(enc_type %in% c(3,7,8))) |> filter(lab=='label' & !label_2yn) |> tidyr::pivot_longer(cols=c(sp1_type, sp2_type, sp3_type), names_to='sp1_sp2_sp3', values_to='n_ports') |> mutate(n_ports=sp_types_n[as.numeric(n_ports)]) |> filter(n_ports > 0)
lab1_lab2 <- devices |> filter(!(enc_type %in% c(3,7,8))) |> filter(lab=='label', label_2yn) |> tidyr::pivot_longer(cols=c(sp1_type), names_to='sp1_sp2_sp3', values_to='n_ports') |> mutate(n_ports=sp_types_n[as.numeric(n_ports)]) |> filter(n_ports > 0)


splitters <- bind_rows(lab1_nolab2, lab1_lab2,lab2) |> mutate(sp_name=case_when(sp1_sp2_sp3 == 'sp1_type' ~ sp1_label,
                                                                                sp1_sp2_sp3 == 'sp2_type' ~ sp2_label,
                                                                                sp1_sp2_sp3 == 'sp3_type' ~ sp3_label)) |>
  select(name,enc_type,sp_name,n_ports)
splitters_out <- splitters |> (\(x) lapply(x,rep, x$n_ports))() |> as_tibble() |> group_by(name) |> mutate(out_port=1:n(), dev=name, name=paste0(dev,' out:', out_port), from_to='to')
splitters_in <- splitters |> group_by(name) |> mutate(in_port=1:n(), dev=name, name=paste0(dev,' in:', in_port), from_to='from')


splitters_edges <- splitters_in |> (\(x) lapply(x,rep, x$n_ports))() |> as_tibble() |> arrange(dev, sp_name) |> select(name) |> rename(from='name') |> mutate(to=arrange(splitters_out,dev,sp_name, out_port)$name)

dev_sp <- tbl_graph(nodes=bind_rows(splitters_in, splitters_out), edges = splitters_edges)

mpts <- function(y=devices){
  y <-  filter(y, enc_type==0 | enc_type==3 | enc_type==7| enc_type==8)#agg
  if (nrow(y)==0)
  {tbl_graph(data.frame(from=c(),to=c()))
  }else{ y |>
      (\(x) lapply(x,rep, x$Nout))() |>
      as_tibble() |>
      rename(dev=name) |>
      group_by(dev) |>
      mutate(row=1:n(),
             from = paste0(dev,' in:', row),
             to = paste0(dev,' out:', row),
             in_port = row, out_port=row) |>
      ungroup() |>
      pivot_longer(cols=c(from, to), names_to='from_to', values_to = 'name') |>
      (\(x) tbl_graph(nodes=x, edges=pivot_wider(x,names_from='from_to', values_from=name) |> select(from,to)))() |>
      mutate(in_port=ifelse(from_to=='from',in_port,NA), out_port=ifelse(from_to=='to',out_port,NA))
  }}

mpt_odp3 <- mpts(devices)

########################




########################crea un dataframe degli spare fibres
#all_fibres(fibres, net_c)
all_fibres <- function(fibres_df, cables_graph){
  
  #sorted_spares_df(fibres_df=fibres, cables_graph=net_c)
  #  sorted_spares_df <- function(fibres_df, cables_graph){
  #input: fibres dataframe - output: spare_df dataframe (contains only the spare fibres)
  spare_df <- function(f_df){
    spare_f <- function(x){
      r=c((x$fibre+1):x$cable_nfib)
      nr=length(r)
      tempdf <- do.call(rbind, replicate(nr, x, simplify=FALSE))
      tempdf$a <- FALSE
      tempdf$b <- FALSE
      tempdf$spare <- TRUE
      tempdf$fibre <- r
      tempdf$name <- paste0(tempdf$ref, ' fibre:', tempdf$fibre)
      return (tempdf)
    }
    
    spare_dftemp <- f_df |>
      group_by(ref) |> filter(fibre==max(fibre)) |> filter(spare_f>0) |> (\(x) map(c(1:nrow(x)), (\(y) spare_f(x[y,]))))() |> bind_rows() |> mutate(ref1=strsplit(ref,'/')[[1]][[1]], ref2=strsplit(ref,'/')[[1]][[2]])
    
    return (spare_dftemp)
  }
  
  all_fibres_temp <- function(fibres_df){
    fibres_df <- mutate(fibres_df,cable_nfib = as.numeric(gsub('F','',cable_sizes[as.numeric(cab_size)])),spare_f=cable_nfib-tot_f, spare=FALSE)
    bind_rows(fibres_df, spare_df(fibres_df))}
  # #######################priorita' cavi
  #   # input: grafo net_c - output grafo con colonna priority agli edge
  # sorting_rule <- function(graph){
  #   ordering1 <- function(graph){
  #     graph %>%
  #       mutate(value = 1) %>%
  #       mutate(priority1 = map_bfs_back(node_is_source(), .f = function(node, path, ...) {
  #         if (nrow(path) == 0) .N()$value[node]
  #         else {
  #           1+max(unlist(path$result[path$parent == node]))
  #         }
  #       }))|> activate(edges) |> mutate(priority1 = .N()$priority1[from]) |> activate(nodes) -> result
  #     return(result)
  #   }
  #
  #   ######now
  
  all_the_paths <- function(g1){
    
    result <- tbl_graph()
    E(g1)$length <- g1 |> activate(edges) |> as_tibble() %>% mutate(length=st_length(st_as_sf((.))))  %>% .$length
    
    root <- function(graph){V(graph)[degree(graph, mode='in')==0 & degree(graph, mode='out')>0]}
    
    longest_paths <- function(graph=g1){
      furthest_node <- function(graph, root_vertex){
        node_name <- mutate(graph |> activate(nodes),
                            wdist_from_root = node_distance_from(root_vertex, weights=length),
                            dist_from_root = node_distance_from(root_vertex)) |>
          filter(wdist_from_root < REEL_LENGTH) |>
          filter(dist_from_root == max(dist_from_root)) %>%
          as_tibble %>% .$name
        return (V(graph)[V(graph)$name == node_name[[1]]])
      }
      
      root_vertex <- root(graph)[[1]]
      furthest <- furthest_node(graph, root_vertex)
      
      complete <- graph |> activate(edges) |> mutate(on_the_path=FALSE) |>
        activate(nodes) |> morph(to_shortest_path,from=root_vertex, to=furthest) |>
        activate(edges) |> mutate(on_the_path=TRUE, reel_start=root_vertex$name, reel_end=furthest$name, reel = paste0('reel:',reel_start,'/',reel_end)) |>
        activate(nodes) |> unmorph()
      result <<- bind_graphs(result, filter(complete |> activate(edges), on_the_path) |> activate(nodes) |> filter(!node_is_isolated()) |> activate(edges))
      remain <- filter(complete |> activate(edges),!on_the_path)  |> activate(nodes) |> filter(!node_is_isolated()) |> activate(edges)
      
      # print(ggraph(result)+geom_node_label(aes(label=name))+geom_edge_link(aes(label=reel))+coord_flip())
      if(gsize(remain)>0){
        walk(remain %>% morph(to_components,type='weak'), longest_paths)}
      
      return(result)
    }
    return (longest_paths)
  }
  
  flist <- sapply(unique(E(cables_graph)$cab_size), (\(x) (filter(activate(cables_graph, edges), cab_size==x)))) |> (\(x) x[sapply(x,gsize)>0])() |> sapply(all_the_paths) |> (\(x) sapply(x, (\(y) do.call(y,list()))))()
  ref_reel <- Reduce(bind_graphs, flist) |> activate(edges) |> as_tibble() |> select(ref,reel_start, reel_end, reel) |> distinct(ref, .keep_all = TRUE)
  all_fibres_df <- merge(all_fibres_temp(fibres_df), ref_reel, by='ref', all.x=TRUE)
  return (all_fibres_df)
}
#

###########

all_fibres_df <- all_fibres(fibres, net_c)

##########


net_plot_sld2 <- function(x, dir, fname,layout='tree', h = 1, w = 1, hazel=FALSE){
  
  tt <- function(name){paste0('fas/',name,'.svg')}
  
  layout <- ggraph::create_layout(x, layout)
  layout$y <- -layout$y
  
  #reels <- unique(E(x)$reel)
  # cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
  #                    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  #
  # tibble(cbp1, letters[1:13])
  # names(cbp1 ,names=letters[1:13])
  #
  if (!hazel) {layout$node_text[!is.na(layout$node_text)] <-  paste(layout$reel[!is.na(layout$node_text)],'\n',layout$node_text[!is.na(layout$node_text)])}
  
  
  p <- ggraph::ggraph(layout) +
    geom_edge_diagonal(width = 2,alpha=1)+#, aes(color=as.factor(reel)))+
    #    ggraph::geom_node_label(ggplot2::aes(label = label_text,colour = as.factor(d_type)), size=3, label.padding=unit(0.25,"lines"), repel=FALSE)+
    
    geom_node_text(ggplot2::aes(label = ifelse(!is.na(node_text),node_text,''), size=3, hjust=0.5, vjust=-0.2)) +
    ####
    ggiraph::geom_label_interactive(ggplot2::aes(x=x,y=y,label = label_text, colour = as.factor(d_type),#tooltip=tt(name),
                                                 onclick=paste0('window.open("',tt(name),'")')),
                                    size=5, label.padding=unit(0.25,"lines")) +
    ####
    ggplot2::coord_flip() +
    scale_colour_manual(values=c('OLT'='violet', "ODP1"="blue", "ODP2"="red", "ODP3"="orange","Joint"="black","JN"="purple","MPT"="orange", "MDU"="grey"))+
    #    scale_fill_manual(values=c("TRUE"="#fce15d", "FALSE"="white"))+
    ggplot2::scale_y_continuous(limits = c(min(layout$y)-1, max(layout$y)+1))+
    ggplot2::scale_x_continuous(limits = c(min(layout$x)-3, max(layout$x)+3))+
    theme(legend.position="none")
  
  h <- max((max(layout$x) - min(layout$x)),1)
  w <- max((max(layout$y) - min(layout$y)),1)
  
  ggplot2::ggsave(plot=p, filename=file.path(dir, paste0(fname,'.svg')), device = 'svg',
                  width = 5*w+6,
                  height=3*h+5,
                  units="in", limitsize=FALSE)
  #
  x <- ggiraph::girafe(ggobj = p,width_svg=5*w+6, height_svg=3*h+5)#, zoom_max=5)# width_svg=15*w*yr, height_svg=15*h*xr, zoom_max=5)
  x <- ggiraph::girafe_options(x, ggiraph::opts_zoom(min = .7, max = 1000))
  #
  htmltools::save_html(x, file.path(dir, paste0('sldfas_',fname,'.html')))
  return (p)
}


reel_list <- distinct(all_fibres_df[,c('ref','reel')])
for (i in seq_along(net_g_list)){
  for (r in V(net_g_list[[i]])$ref[!is.na(V(net_g_list[[i]])$ref)]){
    reelx <- reel_list$reel[reel_list$ref==r]
    V(net_g_list[[i]])[!is.na(V(net_g_list[[i]])$ref) & V(net_g_list[[i]])$ref==r]$reel <- ifelse(length(reelx)>0, reelx[[1]], NA)
    E(net_g_list[[i]])[incident(net_g_list[[i]],V(net_g_list[[i]])[!is.na(V(net_g_list[[i]])$ref) & V(net_g_list[[i]])$ref==r])]$reel <- ifelse(length(reelx)>0, reelx[[1]], NA)#reel_list$reel[reel_list$ref==r][[1]]
  }
  #  V(net_g_list[[i]])[is.na(V(net_g_list[[i]])$reel)]$reel <- ''
}


iwalk(Filter((\(x) length(E(x))>0), net_g_list), (\(k_i , i) try(net_plot_sld2(k_i , dir=output_dir, fname=paste0("SLD", i),'tree', hazel=hazel))))


b_net <- bind_graphs(net_b_list)
dev_net <- bind_graphs(dev_sp, mpt_odp3)
net_c <- simplify(net_c, edge.attr.comb = 'first')
E(net_c)$reel <- as_data_frame(net_c, 'edges') |> left_join(select(all_fibres_df, ref, reel), by=join_by(ref), multiple="any") %>% .$reel
E(net_c)$ref1 <- str_remove(str_extract(E(net_c)$ref, "(^.+?)/"), '/')
E(net_c)$ref2 <- str_remove(str_extract(E(net_c)$ref, "/(.+?$)"), '/')

btob <- function(nc=net_c, nb=b_net, nd=dev_net){
  nb <- nb |> activate(edges) |> filter(!warn)
  E(nb)$sub_cnet <- mapply((\(x,y) list(induced_subgraph(nc, all_shortest_paths(nc, x, y)$res[[1]]))),E(nb)$nfrom, E(nb)$nto)
  V(nb)$dev_net<- sapply(V(nb)$name, (\(x) induced_subgraph(nd, V(nd)[V(nd)$dev==x])))
  E(nb)$fibre_net <- sapply(E(nb)$sub_cnet, (\(x) make_tree(2*(length(x)-1), 1, 'out')))
  
  for (i in seq_along(E(nb))){
    x <- E(nb)$fibre_net[[i]]
    y <- E(nb)$sub_cnet[[i]]
    param <- dfs(y, V(y)[degree(y, mode='in')==0], mode='out')$order
    ordered_path <- E(y, path=param)
    n <- length(E(x))
    if (n>0){
      E(E(nb)$fibre_net[[i]])[seq(1, n, by=2)]$ref <- ordered_path$ref#E(y, path=param)$ref
      E(E(nb)$fibre_net[[i]])[seq(1, n, by=2)]$ref1 <- ordered_path$ref1#E(y, path=param)$ref1
      E(E(nb)$fibre_net[[i]])[seq(1, n, by=2)]$ref2 <- ordered_path$ref2#E(y, path=param)$ref2
      E(E(nb)$fibre_net[[i]])[seq(1, n, by=2)]$reel <- ordered_path$reel#E(y, path=param)$reel
      E(E(nb)$fibre_net[[i]])[seq(1, n, by=2)]$line <- i
      E(E(nb)$fibre_net[[i]])[seq(1, n, by=2)]$bstart <- V(nb)[.from(E(nb)[[i]])]$name
      E(E(nb)$fibre_net[[i]])[seq(1, n, by=2)]$bend <- V(nb)[.to(E(nb)[[i]])]$name #E(nb)[[i]]$nto
      
      V(E(nb)$fibre_net[[i]])$from_to <- c('to','from')
      if (n>1){
        E(E(nb)$fibre_net[[i]])[seq(2, n, by=2)]$fused <- mapply((\(x,y) ifelse(x==y, 'UNCUT', 'FUSED')) ,
                                                                 E(E(nb)$fibre_net[[i]])[seq(1, n, by=2)]$reel,
                                                                 E(E(nb)$fibre_net[[i]])[seq(3, n, by=2)]$reel)
      }
    }
  }
  
  E(nb)$fibre_net2 <- mapply((\(x,y) rep(x,y)) , E(nb)$fibre_net, E(nb)$n_min)
  
  edge_root <- function(g, ed){
    roots <- V(g)[degree(g, V(g), mode='in')==0]
    hed <- head_of(g, ed)
    return(roots[is.finite(distances(g,v=hed,to=roots, mode='in'))][[1]] |> as_ids())
  }
  
  for (i in seq_along(E(nb))){
    x <- E(nb)$fibre_net2[[i]]
    E(E(nb)$fibre_net2[[i]])$sub_line <- sapply(E(x), (\(y) edge_root(x, y)))
  }
  
  
  nb
}


####?????????????????????????????????????????????????
unnamed_dev_net <- function(x){
  V(x)$old_port_name <- V(x)$name
  return (delete_vertex_attr(x, 'name'))
}

all_fnet <- bind_graphs(E(btob())$fibre_net2) |> disjoint_union(unnamed_dev_net(dev_net))

missing_dev1 <- function(x){ #x=one vertex of all_fnet
  if (is.na(x$dev)){
    return(incident(all_fnet, x, mode='in')$ref2[1])
  } else {
    return(x$dev)
  }
}

missing_dev3 <- function(x){ #x=one vertex of all_fnet
  if (is.na(x$dev)){
    neighbors(all_fnet, x, mode="in")[1]$dev[1]
  } else {
    x$dev
  }
}
missing_dev2 <- function(x){ #x=one vertex of all_fnet
  if (is.na(x$dev)){
    incident(all_fnet, x, mode='out')$ref1[1]
  } else {
    x$dev
  }
}

V(all_fnet)$dev <- sapply(seq_along(V(all_fnet)), (\(x) missing_dev1(V(all_fnet)[x])))
V(all_fnet)$dev <- sapply(seq_along(V(all_fnet)), (\(x) missing_dev2(V(all_fnet)[x])))
V(all_fnet)$dev <- sapply(seq_along(V(all_fnet)), (\(x) missing_dev3(V(all_fnet)[x])))




#bbnet <- btob()

compose_net <- function(net=all_fnet){
  devs <- unique(V(net)$dev[!is.na(V(net)$dev) & !is.na(V(net)$old_port_name)])
  mapping=seq_along(V(net))
  
  for (dev_name in devs){
    fy <- V(net)[.from(E(net)[!is.na(E(net)$bstart) & E(net)$bstart==dev_name]) & degree(net, mode='in')==0] # nodes to connect at output ports of dev_name
    fx <- V(net)[.to(E(net)[!is.na(E(net)$bend) & E(net)$bend==dev_name]) & degree(net, mode='out')==0] # nodes to connect at input ports of dev_name
    lfy <- length(fy)
    lfx <- length(fx)
    #    fy <- V(net)[.from(E(net)[!is.na(E(net)$ref1) & E(net)$ref1==dev_name])] # nodes to connect at output ports of dev_name
    #    fx <- V(net)[.to(E(net)[!is.na(E(net)$ref2) & E(net)$ref2==dev_name])] # nodes to connect at input ports of dev_name
    
    dy <- V(net)[!is.na(V(net)$dev) & V(net)$dev==dev_name & degree(net, mode='out')==0 & !is.na(V(net)$old_port_name)]
    dx <- V(net)[!is.na(V(net)$dev) & V(net)$dev==dev_name & degree(net, mode='in')==0 & !is.na(V(net)$old_port_name)]
    ldy <- length(dy)
    ldx <- length(dx)
    
    if(lfy>0 & ldy>0){
      n <- min(lfy, ldy)
      mapping[fy[1:n]] <- mapping[dy[1:n]]}
    if(lfx>0 & ldx>0){
      n <- min(lfx, ldx)
      mapping[fx[1:n]] <- mapping[dx[1:n]]}
  }
  contract(net, mapping, (\(x) ifelse(!is.na(x[2]), x[2], x[1])))
}
compose_net() |> (\(x) induced_subgraph(x,V(x)[degree(x, mode='all')>0]))() -> comp_net


naming <- function(net){
  E(net)$group_id <-
    as_data_frame(net, what='edges')  |> group_by(line, sub_line, reel) |> mutate(group_id = cur_group_id()) %>% .$group_id
  E(net)$group_id[E(net)[is.na(line)| is.na(sub_line) |is.na(reel)]] <- NA
  E(net)$group_card <- as_data_frame(net, what='edges')  |> group_by(group_id) |> mutate(card=n()) %>% .$card
  # E(net)$group_id[is.na(E(net)$line)] <- NA
  # E(net)$sub_line[is.na(E(net)$line)] <- NA
  df <- as_data_frame(net, what='edges') |> arrange(desc(group_card))
  ids <- unique(df$group_id)
  ids <- ids[!is.na(ids)]
  E(net)$f_id <- 0
  for (id in ids){
    refs <- E(net)$ref[E(net)$group_id==id] #filter(df, group_id==id) %>% .$ref
    max_refs <- max(E(net)$f_id[E(net)$ref %in% refs])
    max_group <- max(E(net)$f_id[E(net)$group_id==id])
    E(net)$f_id[E(net)$group_id==id] <- max_refs+1
  }
  net
}

tot_f <- naming(comp_net)


spare_fibres_df <- filter(all_fibres_df, spare)





###################################

temp_str <- function(name){
  # temp <- sub("(^.*) (kin:|kout:)(.*$)", "\\1%%\\2%%\\3",name) %>% strsplit("%%") %>% unlist()
  temp <- strsplit(name,'%') |> unlist()
  from_to=c("in"="from", "out"="to")
  kin=c("in"=1, "out"=0)
  kout=c("in"=0, "out"=1)
  return(c('dev'=temp[[1]],'from_to'=unname(from_to[temp[[3]]]),'kin_port'=unname(kin[temp[[3]]]*as.integer(temp[[2]])),'kout_port'=unname(kout[temp[[3]]]*as.integer(temp[[2]]))))
}




#ok per gli spare fibres
f_uncut_net <- function(sp_f){
  sp_f <- sp_f  |> group_by(ref1) |> mutate(jkout=1:n(), from=paste0(ref1,'%',fibre, '%out%',reel)) |> ungroup() |> #maybe removable
    group_by(ref2) |> mutate(jkin=1:n(), to=paste0(ref2, '%',fibre, '%in%',reel))                                   #maybe removable
  temp <- tbl_graph(edges=sp_f, directed=TRUE) %>% mutate(bind_rows(lapply(V(.)$name, temp_str)))          #maybe removable(second part)
  V(temp)$inc_reel <- sapply(V(temp),(\(x) E(temp)[.to(x)]$reel[1]))
  V(temp)$out_reel <- sapply(V(temp),(\(x) E(temp)[.from(x)]$reel[1]))
  V(temp)$inc_fibre <- sapply(V(temp),(\(x) E(temp)[.to(x)]$fibre[1]))
  V(temp)$out_fibre <- sapply(V(temp),(\(x) E(temp)[.from(x)]$fibre[1]))
  temp1 <-  filter(temp, from_to=='from') |> as_tibble()
  temp2 <-  filter(temp, from_to=='to') |> as_tibble()
  A <- inner_join(temp1,temp2, join_by(dev==dev, inc_reel==out_reel, inc_fibre==out_fibre),na_matches="never") |> select(name.x, name.y) |> rename(from=name.x, to=name.y)
  tbl_graph(edges=A, directed=TRUE) |>  mutate(pname='') |> graph_join(temp) |> activate(edges) |> mutate(fused='UNCUT') |> activate(nodes)
}


# f_other_net <- function(graph){
#   graph |> activate(edges) |> group_by(ref1,a) |> mutate (ref1_a_row=1:n(), from=case_when(a=='TRUE' ~ which(V(graph)$name==paste0(ref1, ' out:', ref1_a_row)),TRUE~from)) |> ungroup() |>
#     group_by(ref2,b) |> mutate (ref2_b_row=1:n(), to=case_when(b=='TRUE'~which(V(graph)$name==paste0(ref2, ' in:', ref2_b_row)),TRUE~to)) |> ungroup() |> activate(nodes)
# }

f_fibres_net <- function(f_df){
  f_df$from <- seq(nrow(f_df))
  f_df$to <- f_df$from+nrow(f_df)
  as_tbl_graph(f_df, directed=TRUE)
}

dev_conn <- function(graph){
  function(dev_name, mode){
    if(mode=='in'){
      ed <- E(graph)[ref2==dev_name]
      V(g)[.to(ed)]} else if(mode=='out'){
        ed <- E(graph)[ref1==dev_name]
        V(g)[.from(ed)]}
  }
}
conn_dev <- dev_conn(g)

# f_tobe_uncut <- function(graph){
#   con <- dev_conn(graph)
#   function(dev_name){
#     input <- con(dev_name, mode='in')
#     output <- con(dev_name, mode='out')
#     input <- input[degree(graph,mode='out')[input]==0]
#     output <- output[degree(graph,mode='in')[output]==0]
#
#     f_in <- E(graph)[.to(input)]
#     f_out <- E(graph)[.from(output)]
#
#     input_df <- data.frame(from=input$name,reel=f_in$reel,fibre=f_in$fibre, spare=f_in$spare)
#     output_df <- data.frame(to=output$name,reel=f_out$reel,fibre=f_out$fibre, spare=f_out$spare)
#     input_df <- input_df[!input_df$spare,]
#     output_df <- output_df[!output_df$spare,]
#     M <- f_in$wjtt2[[1]]
#     N <- f_out[f_out$reel==f_in$reel[1]]$wjtt1[[1]]
#     A <- inner_join(input_df,output_df, join_by(reel==reel, fibre==fibre),na_matches="never") #|> select(name.x, name.y) |> rename(from=name.x, to=name.y)
# #    sapply(seq(N), add.edges(graph, c()))
# #    graph |> filter()
# #   E(g)[ref1=='dsasd' & reel=='reel1']$wjtt1
#     tbl_graph(edges=A[nrow(A)-N+1:nrow(A),], directed=TRUE)
#  }
# }
# tobe_uncut <- f_tobe_uncut(g)

uncut_net <- all_fibres_df |> filter(spare) |> f_uncut_net()


spares <- function(g1=uncut_net, g2=tot_f){
  if('name' %in% names(vertex.attributes(g1))) {g1 <- delete_vertex_attr(g1, 'name')}
  lg1 <- decompose(g1,'weak')
  Elg1 <- lapply(lg1,E)
  diams <- lapply(lg1,diameter)
  spans <- (unlist(diams)+1)/2
  nfib <- sapply(Elg1, (\(x) min(x$cable_nfib[!is.na(x$cable_nfib)])))
  reel <- sapply(Elg1, (\(x) x$reel[!is.na(x$reel)][[1]]))
  g1df <- tibble(lg1,Elg1,diams, spans, nfib, reel)
  g1df <- mutate(g1df,diams=unlist(diams))
  g1df <- arrange(g1df, desc(diams))
  g1df$ref <- lapply(g1df$lg1, (\(x) E(x)$ref))
  g1df$f_ids <- lapply(seq_along(g1df$ref), (\(row) lapply(g1df$ref[[row]] , (\(r) ifelse(is.na(r),NA, list(E(g2)[!is.na(E(g2)$ref) & E(g2)$ref == r]$f_id))))))
  g1df$f_ids_tot <- lapply(g1df$f_ids, (\(x) sort(unique(unlist(x[!is.na(x)])))))
  
  g1df$f_ids_free <- mapply((\(x,y) lapply(x, \(z)setdiff(seq(y), unlist(z)))),g1df$f_ids, g1df$nfib)
  
  g1df$f_ids_free_tot <- mapply((\(x,y) (setdiff(seq(y), x))), g1df$f_ids_tot, g1df$nfib)
  g1df$f_id <- rep(0, length(g1df$reel))
  g1df <- g1df |> arrange(desc(spans))
  for (x in seq_along(g1df$f_ids_free)){
    try(g1df$f_id[[x]] <- g1df$f_ids_free_tot[[x]][[1]])
    sel <- sapply(g1df$ref, (\(t) length(intersect(t, g1df$ref[[x]][!is.na(g1df$ref[[x]])]))>0)) # & g1df$reel==g1df$reel[[x]]
    g1df$f_ids_free_tot[sel] <- lapply(g1df$f_ids_free_tot[sel], (\(y) setdiff(unlist(y), unlist(g1df$f_id[[x]]))))
    #    print(paste(x, ':',  g1df$f_id[[x]] ,'-', g1df$f_ids_free_tot[x]))
    
    #  g1df$f_ids_free[g1df$reel==g1df$reel[[x]] & g1df$ref %in% g1df$ref[[x]]] <- lapply(g1df$f_ids_free[g1df$reel==g1df$reel[[x]] & g1df$ref %in% g1df$ref[[x]]], (\(y) setdiff(unlist(y), unlist(g1df$f_id[[x]]))))
    #    print(x, g1df$f_id[[x]], g1df$f_ids_free[[x]])
  }
  for (x in seq_along(g1df$f_id)) {E(g1df$lg1[[x]])$f_id <- g1df$f_id[[x]]}
  bind_graphs(g1df$lg1)
}


######adding premises and drops
min_boundary <- function(premise, boundary_layer){
  boundary_layer[premise,] |> (\(q) q[!q$level_ %in% c(4,6,7,10,11,12,13,14,15,16),])() |>
    (\(t) t[which.min(sf::st_area(t)),])() |> (\(t) ifelse(nrow(t)>0,t$name[[1]],''))()}

dp$bound <- sapply(dp$geom, (\(x) min_boundary(x,boundary_layer=boundaries_fas)))# |> (\(x) paste0('Premises - ',  x))()

drops <- cables_fas |> filter(cab_size=='7')
connected_ix <- c(unique(unlist(sf::st_is_within_distance(lwgeom::st_startpoint(drops),dp,dist=0.1))),unique(unlist(sf::st_is_within_distance(lwgeom::st_endpoint(drops),dp,dist=0.1))))
dp$connected[connected_ix] <- TRUE
dp$connected[!connected_ix] <- FALSE
#  drops <- cables_fas |> filter(cab_size=='7') |> mutate(ref1= sub("/", ' out:',ref)) |> (\(x) data.frame(from=x$ref1, to=as.character(round(as.numeric(x$prem_id)))))()


uprn <- data.frame(name=as.character(round(as.numeric(dp$uprn))), bound=dp$bound, connected=dp$connected)# |> mutate(connected=ifelse(name %in% as.character(round(as.numeric(cables_fas$prem_id))),TRUE,FALSE))

#  guprn <- tbl_graph(nodes=uprn)
#  gdrops <- tbl_graph(edges=drops)

##############

cut_fibre <- function(graph){
  function(f1){
    next_f <- next_fibre(graph)
    return(length(next_f(f1))==0)
  }
}

next_fibre <- function(graph){
  function(f1){
    next_s <- next_step(graph)
    next_s(next_s(f1))}
}

next_step <- function(graph){
  function(f1){E(graph)[.from(V(graph)[.to(f1)])]}
}

fused_fibre <- function(graph){
  function(f1){
    next_f <- next_fibre(graph)
    cutted_f <- cut_fibre(graph)
    temp <- f1
    if (cutted_f(E(graph)[temp])){
      return (FALSE)} else {
        return (next_f(E(graph)[temp])$reel != E(graph)[temp]$reel)}
  }
}

uncut_fibre <- function(graph){
  function(f1){
    cutted_f <- cut_fibre(graph)
    fused_f <- fused_fibre(graph)
    temp <- f1
    return(!cutted_f(E(graph)[temp]) & !fused_f(E(graph)[temp]))}
}

prev_step <- function(graph){
  function(f1){E(graph)[.to(V(graph)[.from(f1)])]}
}

prev_fibre <- function(graph){
  prev_s <- prev_step(graph)
  return (function(f1){prev_s(prev_s(f1))})
}


# tot_f$cut <- cut_fibre(tot_f)
# tot_f$nf <- next_fibre(tot_f)
# tot_f$ns <- next_step(tot_f)
# tot_f$fused <- fused_fibre(tot_f)
# tot_f$uncut <- uncut_fibre(tot_f)
# tot_f$ps <- prev_step(tot_f)
# tot_f$pf <- prev_fibre(tot_f)
#################

#  tot_f <- graph_join(dev_net,f_net)
#  tot_f <- Reduce((\(x,y) graph_join(x,y,by=c('name'))),list(f_net,dev_net))#,guprn,gdrops))
#  tot_f <- Reduce((\(x,y) graph_join(x,y,by=c('name'))),list(spares_net,dev_net |> select(-c(from_to, dev))))

#  tot_f <- naming(comp_net)
tot_f <- as_tbl_graph(tot_f) |> activate(nodes) |>
  mutate(pname = case_when(in_port>0 ~ paste0('In: ', in_port),
                           out_port>0 ~ paste0('Out: ', out_port),
                           TRUE ~ ''), mrect=TRUE)

####20230806start

tot_f <- add_vertices(tot_f, nrow(uprn), pname=as.character(uprn$name), dev=uprn$bound, connected=uprn$connected, nodety='regular', virtual=FALSE, mrect=FALSE)
for (xdev in unique(uprn$bound)){
  from <- V(tot_f)[!is.na(V(tot_f)$dev) & V(tot_f)$dev==xdev & degree(tot_f, mode='out')==0 & degree(tot_f, mode='in')>0]
  to <- V(tot_f)[!is.na(V(tot_f)$dev) & V(tot_f)$dev==xdev & !is.na(V(tot_f)$connected) & V(tot_f)$connected]
  m <- min(length(from),length(to))
  tot_f <- add_edges(tot_f, c(mapply((\(x,y) c(x,y)), from[1:m],to[1:m])))
}
tot_f <- as_tbl_graph(tot_f)

uncut_net <- spares()
####20230806endhttp://127.0.0.1:16927/graphics/plot_zoom_png?width=1968&height=666

plot_dev <- function(dev_name, w=1, h=2, hazel=FALSE){
  sub_graph <-function(root_id,graph=tot_f, deep_in=2, deep_out=4){
    temp <- tidygraph::activate(graph,nodes)
    temp <- dplyr::mutate(temp, bfs_results = tidygraph::bfs_dist(root = root_id, mode='out'))
    temp <- dplyr::mutate(temp, bfs_results = ifelse(is.finite(bfs_results),bfs_results,-tidygraph::bfs_dist(root = root_id, mode='in')))
    temp <- dplyr::filter(temp,!is.na(bfs_results))
    dplyr::filter(temp, bfs_results<deep_out & bfs_results>-deep_in)}
  
  #########################
  
  y1 <- sub_graph(c(which(as_tibble(tot_f)$dev==dev_name & as_tibble(tot_f)$from_to=='from')), graph=tot_f, deep_in=2, deep_out = 3)
  
  y2 <- sub_graph(c(which(as_tibble(uncut_net)$dev==dev_name & as_tibble(uncut_net)$from_to=='from')), graph=uncut_net, deep_in=2, deep_out = 3)
  
  y3 <- sub_graph(c(which(as_tibble(uncut_net)$dev==dev_name & as_tibble(uncut_net)$from_to=='to' & degree(uncut_net, mode='in')==0)), graph=uncut_net, deep_in=1, deep_out=2)
  V(y3)$bfs_results <-  V(y3)$bfs_results+1
  
  if(hazel){E(y1)$fused[!is.na(E(y1)$fused)] <- 'FUSED'
  E(y2)$fused[!is.na(E(y2)$fused)] <- 'FUSED'
  E(y3)$fused[!is.na(E(y3)$fused)] <- 'FUSED'}
  
  V(y1)$nodety <- 'regular'
  V(y2)$nodety <- 'spare1'
  V(y3)$nodety <- 'spare2'
  
  V(y2)$mrect <- TRUE
  V(y3)$mrect <- TRUE
  
  V(y1)$slipped <- FALSE
  V(y2)$slipped <- FALSE
  V(y3)$slipped <- TRUE
  
  
  
  if(length(E(y2))>0){
    V(y2)[ends(y2, E(y2)[!is.na(E(y2)$reel)])[,1]]$greel <- E(y2)[!is.na(E(y2)$reel)]$reel
    V(y2)[ends(y2, E(y2)[!is.na(E(y2)$reel)])[,2]]$greel <- E(y2)[!is.na(E(y2)$reel)]$reel
    V(y2)[bfs_results==0 & degree(y2, mode='out')==0]$cutted <- 'cutted'
    V(y2)[unlist(adjacent_vertices(y2,V(y2)[bfs_results==0 & degree(y2, mode='out')==0], mode='in'))]$cutted <- 'cutted'
    V(y2)$contracted <- group_by(as_data_frame(y2, 'vertices'), greel, bfs_results, cutted) |> mutate(contracted=cur_group_id()) %>% .$contracted
    y2 <- contract(y2, mapping=V(y2)$contracted, vertex.attr.comb = "first")
    y2 <- simplify(y2, edge.attr.comb = list(f_id='concat', 'first'))
  }
  
  if(length(E(y3))>0){
    V(y3)[ends(y3, E(y3)[!is.na(E(y3)$reel)])[,1]]$greel <- E(y3)[!is.na(E(y3)$reel)]$reel
    V(y3)[ends(y3, E(y3)[!is.na(E(y3)$reel)])[,2]]$greel <- E(y3)[!is.na(E(y3)$reel)]$reel
    
    V(y3)$contracted <- group_by(as_data_frame(y3, 'vertices'), greel, bfs_results) |> mutate(contracted=cur_group_id()) %>% .$contracted
    y3 <- contract(y3, mapping=V(y3)$contracted, vertex.attr.comb = "first")
    y3 <- simplify(y3, edge.attr.comb = list(f_id='concat', 'first'))
  }
  if(!is.list(E(y1)$f_id)){E(y1)$f_id <- as.list(E(y1)$f_id)}
  if(!is.list(E(y2)$f_id)){E(y2)$f_id <- as.list(E(y2)$f_id)}
  if(!is.list(E(y3)$f_id)){E(y3)$f_id <- as.list(E(y3)$f_id)}
  
  
  y <- if(length(E(y2))>0 & length(E(y3))>0){
    Reduce(bind_graphs,list(y1,y2,y3))}else if(length(E(y3))==0 & length(E(y2))>0){
      bind_graphs(y1,y2)} else if(length(E(y3))>0 & length(E(y2))==0){
        bind_graphs(y1,y3)} else if(length(E(y3))==0 & length(E(y2))==0){
          y1}
  
  ##########################20230816
  y <- filter(y, !grepl('other map',dev))
  ##########################20230816end
  y <- arrange(y,by=pick(dev))
  
  y <- mutate(as_tbl_graph(y), group_sp_root = unlist(map_bfs(node_is_root(), .f = function(node, path, ...) {
    split <- !is.na(.N()$sp_name[node])
    if (nrow(path) == 0) paste(.N()$dev[node],.N()$sp_name[node]) else paste(.N()$dev[node],.N()$sp_name[path$node[1]], split)})))
  #     #   .N()$sp_name[before]}))
  V(y)$component <- components(y)$membership
  
  
  
  format_fid <- function(f_id){
    x <- sort(unique(unlist(f_id)))
    n <- length(x)
    group <- rep(1, n)
    if(n>1){
      for (i in seq(2,n)){
        group[i] <- ifelse (x[i]==x[i-1]+1, group[i-1], group[i-1]+1)
      }}
    summ <- tibble(x,group) |> group_by(group) |>
      summarise(summ_min=min(x), summ_max=max(x), summ=ifelse(summ_min==summ_max,paste('[',summ_min,']'),paste('[',summ_min, '-',summ_max,']'))) %>% .$summ
    paste(summ, collapse='+')
  }
  E(y)$formatted_fid <- sapply(E(y)$f_id, format_fid)
  
  if (hazel){E(y)$reel <- ''}
  
  
  
  plot_layout <- function(y){
    layout <- ggraph::create_layout(y, layout='tree')
    
    
    layout$y <- layout$bfs_results
    
    components_regular <- unique(layout$component[layout$nodety=='regular'])
    components_spare1 <- unique(layout$component[layout$nodety=='spare1'])
    components_spare2 <- unique(layout$component[layout$nodety=='spare2'])
    
    max_prev <- 0
    for (i in c(components_regular, components_spare1, components_spare2)){
      lay_cur <- layout$x[layout$component==i]
      layout$x[layout$component==i] <-  lay_cur - min(lay_cur) + max_prev +1
      max_prev <- max(layout$x[layout$component==i])
    }
    
    
    devs <- unique(layout$dev[layout$y==2])
    for (devx in devs){
      sel <- layout$y==2 & layout$dev==devx
      px <- layout$x[sel]
      n <- length(px)
      gr <- rep(1, n)
      if(n>1){
        for (i in seq(2,n)){
          gr[i] <- ifelse(px[i]==px[i-1]+1, gr[i-1], gr[i-1]+1)
        }
      }
      layout$groupfid[sel] <- paste(gr, devx)
    }
    
    
    layout$x <- -layout$x
    
    #ifelse(!is.na(ref), paste0(reel,'\n', ref, ' - ',formatted_fid), ifelse(!is.na(fused),fused,''))
    
    # E(y)$rural_label <- ifelse(!is.na(E(y)$ref), paste(E(y)$reel, '\n', E(y)$ref, ' - ', E(y)$formatted_fid), ifelse(!is.na(E(y)$fused),E(y)$fused,''))
    # E(y)$urban_label <- ifelse(!is.na(E(y)$ref), paste(E(y)$ref, ' - ', E(y)$formatted_fid), ifelse(!is.na(E(y)$fused),E(y)$fused,''))
    # fas_alternative <- ggraph(layout) + geom_node_point() + geom_edge_link(aes(label = ifelse(hazel, urban_label, rural_label),vjust=0)) +
    
    fas_alternative <- ggraph(layout) + geom_node_point() + geom_edge_link(aes(label = ifelse(!is.na(ref),
                                                                                              paste0(reel,'\n', ref, ' - ',formatted_fid),
                                                                                              ifelse(!is.na(fused),fused,'')),
                                                                               vjust=0)) +
      geom_node_label(aes(label= pname))+
      geom_mark_rect(aes(x = x, y = y,fill = dev, alpha=0.3, label = dev, y0=2.5, hjust=0,
                         filter = bfs_results %in% c(2,3) & mrect,
                         group=groupfid #paste(group_sp_root,nodety)
      ), con.size = 0.5, expand = unit(8, "mm")
      , label.fontsize = 16
      ) +
      geom_mark_rect(aes(x = x, y = y,fill = dev, alpha=0.3, label = dev, y0=0.5, x0=max(layout$x[layout$bfs_results %in% c(0,1)]+1),
                         filter = bfs_results %in% c(0,1),
                         #                           group=paste(group_sp_root,nodety)
      ), con.size = 0, expand = unit(8, "mm")#, label.buffer = unit(-0.5, 'cm')
      , label.fontsize = 16
      ) +
      geom_mark_rect(aes(x = x, y = y,fill = dev,  alpha=0.3, label = dev, y0=-1, x0=max(layout$x[layout$bfs_results %in% c(-1,-2)]+1),
                         filter = bfs_results %in% c(-1,-2)),
                     con.size = 0, expand = unit(8, "mm")
                     #, label.buffer = unit(-7, 'cm')
                     , label.fontsize = 16#, label.buffer=unit(-0.5, 'cm')
      ) +
      
      theme_void()+
      theme(legend.position="none") +
      
      #
      scale_y_continuous(expand=expansion(add = c(1,2)))+
      scale_x_continuous(expand=expansion(add = c(1,2)))+
      
      coord_flip()
    
    
    dir.create(paste0(output_dir,'\\fas'))
    
    ggsave(plot=fas_alternative,filename=file.path(paste0(output_dir,'\\fas\\',dev_name,".svg")),device = 'svg',width = 75,height=2.5*(max(layout$x)-min(layout$x)),units="cm",limitsize=FALSE)
    
  }
  plot_layout(y)
}

walk(unique(devices$name) %>% (\(x) x[!grepl('other map', x)]), (\(k) try(plot_dev(k, hazel=hazel))))
#plot_dev("FLM-1-JN05", hazel=TRUE)
