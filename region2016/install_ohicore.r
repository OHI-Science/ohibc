# install_ohicore.r

# Instructions to sucessfully work with the OHI Toolbox
  # 1. Download and update R. Find the latest version at http://cran.r-project.org.
  # 2. Download and update RStudio. RStudio is optional, but highly recommended. Find the latest version at http://www.rstudio.com/products/rstudio/download
  # 3. Run the following as a one-time install:

# remove old packages
for (p in c('ohicore','ohigui','rCharts')){
  if (p %in% rownames(installed.packages())){
    lib = subset(as.data.frame(installed.packages()), Package==p, LibPath, drop=T)
    remove.packages(p, lib)
  }
}

# install dependencies
for (p in c('devtools')){
  if (!require(p, character.only=T)){
    install.packages(p)
    require(p, character.only=T)
  }
}

# install packages
install.packages(c('zoo', 'psych'))
install_github('ohi-science/rCharts')
install_github('ohi-science/ohicore')

# Note: you will get warning messages like the following; this is nothing to worry about.
# Warning: replacing previous import by ‘plyr::mutate’ when loading ‘ohicore’
