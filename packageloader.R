# automatically install and load required packages
packages <- c("shiny", "WeibullR", "ggplot2", "fitdistrplus", "shinyjs", "stringi",
              "stringr", "shinyWidgets", "microbenchmark", "dplyr", "reshape2",
              "shinythemes", "RColorBrewer", "scales", "xlsx", "plyr",
              "shinycssloaders", "weibulltools", "plotly", "FAdist", "colourpicker",
              "rmarkdown", "qpcR", "readr", "purrr", "rlang", "rainbow", "tinytest")

# check which packages are currently installed
packs <- lapply(packages, FUN = function(packages) {
  do.call("require", list(packages))
})

packs <- unlist(packs, use.names = F)
instpacks <- packages[!packs]

# installing packages
lapply(instpacks, FUN = function(instpacks){
  do.call("install.packages", list(instpacks))
})

# should return a vector of TRUE entries - one entry for every successfully loaded package
packagecheck <- unlist(lapply(packages, FUN = function(packages) {
  do.call("require", list(packages))
}))

failed <- which(packagecheck == FALSE)
failed <- packages[failed]
if (identical(character(0), failed)) {
  print("All packages loaded successfully.")
} else {
  cat("Packages \n", failed, "\n could not be loaded.")
}

# shiny: interactive apps with R
# WeibullR: the quickplots and quickfit methods used for parameter estimation
# ggplot2: beautiful graphics and charts
# fitdistrplus: simulation of censored data and checks for fit of data to distribution
# shinyjs: more shiny widgets
# stringi: string processing (installed from local .zip)
# stringr: string processing
# shinyWidgets: More shiny widgets
# microbenchmark: benchmarking functions and visualizing results
# dplyr: general data wrangling
# reshape2: for the function "melt" that is used to reshape data into format needed by ggplot2
# shinythemes: for the cosmo theme
# RColorBrewer: for construction of beautiful colorscales on the fly
# scales: for the reshape function, may no longer be needed because of reshape2
# xlsx: for output of tables to xlsx files => used in diagnostics file download
# plyr: data manipulation functions
# shinycssloaders: for splitlayout in UI of app
# weibulltools: zoomable weibullplots through plotly
# plotly: interface for interactive html functions
# FAdist: weibull3 function for 3 parameter weibull distribution
# colourpicker: color inputs for shiny => used in plots tab
# rmarkdown: render documentation files as HTML
# qpcR: cbind.na
# readr: more flexibility in reading xlsx files
# purrr: functional programming syntactic sugar
# rlang: tidy evaluation and nonstandard evaluation to dynamically construct expressions in shiny_filter_df.R
# rainbow: construct functional data objects for easy matrix visualization
# tinytest: automated unit testing (finally!!!)

# installation of package stringi fails. trying to update R

# if (!require(installr)) {
#   install.packages("installr"); require(installr)
#   }
# updateR()

# this also fails because 'updateR()' depends on the package 'stringi'

# manually downloading and installing the package should do the trick
# https://github.com/gagolews/stringi/blob/master/devel/winbuilder/3.5/stringi_1.2.4.zip
# place file in working directory and install with the next line
# install.packages("stringi_1.2.4.zip", repos = NULL)
