install.Rtools(check = TRUE, check_r_update = TRUE, GUI = TRUE, ...)
install.packages("rstudioapi")
install.packages("renv")

# set working directory so I know where the .zip file will be located
dir.create("sld_fas_repo_dir")
setwd(dir = "sld_fas_repo_dir")

# download a .zip file of the repository
# from the "Clone or download - Download ZIP" button
# on the GitHub repository of interest
download.file(url = "https://github.com/DSwing/SLDFAS/archive/refs/heads/master.zip"
              , destfile = "sld_fas.zip")

# unzip the .zip file
unzip(zipfile = "sld_fas.zip")

# set the working directory
# to be inside the newly unzipped 
# GitHub repository of interest
setwd(dir = "SLDFAS-master")

openProject(path = "sld-fas.Rproj")

renv::restore()

install.packages("arcgisbinding_1.0.1.300.zip", repos=NULL)
install.packages("svglite")

renv::snapshot()
