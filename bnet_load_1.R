args <- commandArgs(trailingOnly = TRUE)

library(arcgisbinding)
arc.check_product()

source('./sldtool.R')

  library(igraph)
  library(purrr)
  library(dplyr)
  library(sf)
  library(tibble)
  library(ggplot2)
  library(tidygraph)
  library(ggraph)
#  library(netcapacity)
  library(gt)

  precision=100

  ### to be launched from Rstudio
  # in_params <- c()
  # args[2] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/Hazel_Seaton_CLD_V1_VO_WFL1/FeatureServer\\0'#greys
  # args[1] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/Hazel_Seaton_CLD_V1_VO_WFL1/FeatureServer\\1'#whites
  # args[3] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/Hazel_Seaton_CLD_V1_VO_WFL1/FeatureServer\\5'#cables
  # args[4] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/Hazel_Seaton_CLD_V1_VO_WFL1/FeatureServer\\8'#boundaries
  # args[5] <- 'https://services8.arcgis.com/8nY6VQZZM2Z9cUt0/arcgis/rest/services/Hazel_Seaton_CLD_V1_VO_WFL1/FeatureServer\\2'#closures
  # args[6] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\QC reports\\HAZEL SEATON'
  # # # #  out_layer <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\MyProject16.gdb\\drops_named3'
  # #####
  # args <- c()
  # args[1] <- ''
  # args[2] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\GTN.gdb\\Premises_Merge'
  # args[3] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\GTN.gdb\\CableMerge'#cables
  # args[5] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\GTN.gdb\\MPT_merge'#splice_closures
  # args[4] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\MyProject16\\GTN.gdb\\Aboundaries'#boundaries
  # args[6] <- 'C:\\Users\\DomenicoGuadalupi\\OneDrive - Viberoptix\\Documents\\ArcGIS\\Projects\\QC reports\\Hazel-GCN\\20230814'
#greys <-  net_arcgisread(in_params$greys, precision=precision)[[1]]
#cables <-  net_arcgisread(in_params$cables, precision=precision)[[1]]
#boundaries <-  net_arcgisread(in_params$boundaries, precision=precision)[[1]]
#closures <-  net_arcgisread(in_params$closures, precision=precision)[[1]]
# output_dir <- in_params$dir


  greys <-  net_arcgisread(args[2], precision=precision)[[1]]
  cables <-  net_arcgisread(args[3], precision=precision)[[1]]
  boundaries <-  net_arcgisread(args[4], precision=precision)[[1]]
  closures <-  net_arcgisread(args[5], precision=precision)[[1]]
  output_dir <- args[6]


  if (nchar(args[1])<2){
    dp <- designed_prems(greys, cables)
  } else {
    whites <- net_arcgisread(args[1], precision=precision)[[1]]
    dp <- designed_prems(greys, cables, whites)
  }


  devices <- net_devices(closures) |> as_tibble() |> (\(x) x[,c('name', 'Nout')])()

  b <- valid_boundaries(boundaries)
  b <- mutate(b, label_text= paste('Name: ',name,' - ', 'Level: ', dev_type))

  b <- left_join(b,devices,by="name")

  net_c_with_V <- function(cables, closures=closures){
    net_c <- net_cables(cables)
    virtual_net <- net_virtual_cables(closures)
    if (length(E(virtual_net))>0){net_c <- graph_join(net_c, virtual_net)}
    net_c
  }
  net_c <- net_c_with_V(cables, closures)

    warns <- function(cables, nfrom, nto, clos = closures){
    cw <- net_c_with_V(cables, clos) |> igraph::distances(mode="out") |> is.finite()
    cwres <- function(x,y){
      if ((x %in% unlist(dimnames(cw))) & (y %in% unlist(dimnames(cw)))){
        cw[x,y]
      } else {
        FALSE
      }
    }
    mapply(cwres, nfrom, nto)
  }

  net_b <-  mutate(net_boundaries(b), group = group_components(type = "weak"))

  in_out_load <- function(x, dp=dp){
    net_load(x,dp) |>
      activate(edges) |>
      mutate(nfrom = .N()$name[from], nto = .N()$name[to], n_min=.N()$in_load[to]) |>
      mutate(warn = !warns(cables, nfrom, nto)) |>
      activate(nodes) |>
      mutate(label_text = paste(label_text, "\nCapacity: ",
                                Nout,
                                '- Load: ',
                                load_acc),
             alert=(Nout<load_acc | is.na(Nout)))
  }

  net_b_list <- map(unique(tibble::as_tibble(activate(net_b,nodes))$group), (\(k) in_out_load(filter(net_b, group==k),dp)))

  #  net_b_list[[2]] |> activate(edges) |> as_tibble() |> print()
  #  net_b_list[[3]] |> activate(edges) |> as_tibble() |> print()


  plot_b <- function(x, name='temp.svg',directory=paste0(getwd(),'/boundaries'),layout='tree'){
    layout <- create_layout(x, layout)
    layout$y <- -layout$y
    yr <- max(sapply(unique(layout$x), (\(k) length(layout$y[layout$x==k]))))
    xr <- max(sapply(unique(layout$y), (\(k) length(layout$x[layout$y==k]))))
    p <- ggraph(layout) +
      geom_edge_diagonal(aes(edge_linetype=warn),width = 0.5,alpha=0.5) +
      scale_linetype_manual(values=c("TRUE"='solid', "FALSE"='dotted')) +
      geom_node_label(aes(label = label_text,colour = as.factor(level_), fill=alert), repel=FALSE)+
      scale_colour_manual(values=c("2"="#126b10", "3"="#b81707", "5"="orange","8"="pink","9"="green","1"="black"))+
      scale_fill_manual(values=c("TRUE"="#fce15d", "FALSE"="white"))+
      coord_flip()+
      scale_y_continuous(limits = c(min(layout$y)-1, max(layout$y)+1))+
      scale_x_continuous(limits = c(min(layout$x)-3, max(layout$x)+3))+
      theme(legend.position="none")
    dir.create(directory, showWarnings = FALSE)
    ggsave(plot=p,filename=file.path(directory, name),device = 'svg',width = 14*yr,height=0.85*xr+2,units="in", limitsize=FALSE)
    browseURL(file.path(directory, name))
    return (p)
  }


  iwalk(net_b_list, (\(k_i, i) plot_b(k_i, paste0(i,'.svg'), output_dir)))

  g <- function(nc,nb){
    cable_sizes<-c(12,24,48,96,144,288,1,4,8,60)
    nb <- nb |> activate(edges) |> as_tibble() |> filter(!warn)
    nc <- nc |> activate(edges) |> mutate(w = 0) |> activate(nodes)

    temp <- pmap(list(nb$nfrom,nb$nto,nb$n_min), (\(x,y,t) morph(nc, to_shortest_path, from=which(as_tibble(nc)$name==x), to=which(as_tibble(nc)$name==y))|>
                                                    activate(edges) |> mutate(w = t) |> activate(nodes) |> unmorph() |> activate(edges) |> as_tibble()))
    tab <- do.call("rbind", temp) |> group_by(name, cab_size) |> summarise(min_f=sum(w)) |> mutate(cab_size=cable_sizes[as.numeric(cab_size)]) |> filter(cab_size<min_f) |> ungroup()
    return (tab)
  }

  iwalk(net_b_list, (\(x,i) try(g(net_c,x) |> mutate(id=row_number()) |> relocate(id) |> gt() |> tab_header(title='Cables with unsufficient capacity', subtitle = 'Cables needing a bigger size') |>  cols_label(
    min_f = "fibres required ") |> gtsave(filename = file.path(output_dir,(paste0("tab_",i,".html")))))))
  walk(seq_along(net_b_list), (\(i) try(browseURL(file.path(output_dir,(paste0("tab_",i,".html")))))))


