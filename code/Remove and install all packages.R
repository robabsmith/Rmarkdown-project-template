#################################################################
#################################################################
# Re-install packages to R
# ========================
#################################################################
#################################################################

# Install LexisNexus tools
install.packages("quanteda")
library(devtools)
remotes::install_github("JBGruber/LexisNexisTools", force = TRUE)

# R packages for a fresh install

install.packages("tidyverse")
install.packages("RJSONIO")
# Dependencies
# ‘sys’, ‘askpass’, ‘tinytex’, ‘generics’, ‘tidyselect’, ‘plogr’, 
# ‘DBI’, ‘openssl’, ‘fs’, ‘rmarkdown’, ‘whisker’, ‘selectr’, ‘lifecycle’, 
# ‘broom’, ‘dplyr’, ‘dbplyr’, ‘httr’, ‘lubridate’, ‘modelr’, ‘purrr’, 
# ‘reprex’, ‘rstudioapi’, ‘rvest’, ‘tidyr’, ‘xml2’
install.packages("randomcoloR")
install.packages("corpcor")
install.packages("perturb")
install.packages("reshape")
install.packages("rowr")
install.packages("date")
install.packages("roll")

# Packages included in tidyverse
# install.packages("reshape2")
# install.packages("ggplot2")
# install.packages("knitr")
# install.packages("curl")
# install.packages("dplyr")
# install.packages("rstudioapi")
# install.packages("tidyr")
# install.packages("markdown")
# install.packages("rmarkdown")
# install.packages("magrittr")
# install.packages("jsonlite")
# install.packages("lubridate")
# install.packages("httr")
# install.packages("stringr")

# Data visualisation
install.packages("DiagrammeR")
# Dependencies
# ‘brew’, ‘XML’, ‘Rook’, ‘gridExtra’, ‘downloader’, ‘igraph’, 
# ‘influenceR’, ‘rgexf’, ‘viridis’, ‘visNetwork’
install.packages("plotrix")
install.packages("xtable")
install.packages("rayshader")
#devtools::install_github("tylermorganwall/rayshader")

# Analsysis and document generation
install.packages("pipeR")
install.packages("extrafont")
install.packages("foreign") # less consistent but highly customisable # read.
install.packages("haven") # more consistent and automatic, but fewer options (SAS (.sas7bdat), STATA (.dta), .SPSS, .SAV, .POR) # read_
install.packages("Formula")
install.packages("Hmisc")
# Dependencies
# ‘checkmate’, ‘latticeExtra’, ‘acepack’, ‘htmlTable’
install.packages("mFilter")
install.packages("networkD3")
install.packages("caTools")
install.packages("data.table")
install.packages("quantmod")
# Dependencies
# ‘checkmate’, ‘latticeExtra’, ‘acepack’, ‘htmlTable’
install.packages("kableExtra")
install.packages("cowplot")

# For working with Excel documents
# ==================================
install.packages("rJava") # Required for XL connect and xlsx packages
# install.packages("xlsxjars")
# install.packages("xlsx")
install.packages("XLConnect") # Bridge between R and Excel. 
# loadWorkbook("path") 
# getSheets(book)
# readWorksheet(book, sheet = "sheetname")
# createSheet(book, "sheetname")
# writeWorksheet(book, dataframeName, "Sheetname")
# saveWorkbook(book, file = "path/filename.xlsx") # new sheets will not be included in files until the workbook has been saved. Best to save to new file name.
# renameSheet(book, "old_sheet_name", "new_sheet_name")
# removeSheet(book, "sheetname")
install.packages("gdata") # read.xls("path") # sheetNames(book) # Allows for reading xls file online
install.packages("openxlsx") # read.xlsx("path") # getSheetNames(book) # readWorkbook(book, sheet = 1)
install.packages("readxl") # read_excel("path") # excel_sheets(book) # read_excel(book, sheet = "sheetname")

# Needs compilation
install.packages("plm")
# Dependencies
# ‘miscTools’, ‘bibtex’, ‘gbRd’, ‘bdsmatrix’, ‘sandwich’, ‘lmtest’, ‘maxLik’, ‘Rdpack’

# Packages for API communication
install.packages("RCurl")
install.packages("zoo") # Dependant of many other packages
install.packages("rsdmx") # SDMX, Statistical Data and Metadata eXchange, sourcing
install.packages("XML")


# Packages for data sourcing (Not available on CRAN)
# install.packages("statsDK")

devtools::install_github("mikkelkrogsholm/statsDK")

# Packages for data sourcing
install.packages("pdfetch")
install.packages("countrycode")
install.packages("ecb")
install.packages("eurostat")
install.packages("OECD")
# Dependencies
# ‘e1071’, ‘units’, ‘classInt’, ‘sf’, ‘RefManageR’
install.packages("Quandl") # Read CRAN for API details

# Packages for relational database editing
install.packages("RMySQL")

# Packages for presetnation tools
install.packages("revealjs")

# Packages for font matter and fonts
library(extrafont)
font_import()
#Type "y" into the console window before continuing.
font_install("fontcm")

# Packages for markdown, bookdown and blogdown
install.packages("bookdown")
install.packages("blogdown")
install.packages("checkpoint")
install.packages("devtools")
# Dependencies
# ‘ini’, ‘clisymbols’, ‘gh’, ‘rprojroot’, ‘rex’, 
# ‘xopen’, ‘commonmark’, ‘praise’, ‘usethis’, ‘covr’, 
# ‘desc’, ‘DT’, ‘git2r’, ‘memoise’, ‘pkgbuild’, ‘pkgload’, 
# ‘rcmdcheck’, ‘remotes’, ‘roxygen2’, ‘rversions’, ‘sessioninfo’, ‘testthat’
install.packages("codetools")
install.packages("tikzDevice")
install.packages("shiny")

# Needs compilation
install.packages("fpp")
# Dependencies
# ‘fracdiff’, ‘timeDate’, ‘urca’, ‘RcppArmadillo’, ‘quadprog’, 
# ‘forecast’, ‘fma’, ‘expsmooth’, ‘tseries’

# Map images and using geographic shape polygons
install.packages("htmlwidgets")
install.packages("leaflet")
install.packages("widgetframe")
install.packages("sp")
install.packages("rgdal")
install.packages("rgeos")
install.packages("raster")
install.packages("rmapshaper")
# Dependencies
# ‘triebeard’, ‘protolite’, ‘urltools’, ‘httpcode’, 
# ‘geojson’, ‘jqr’, ‘crul’, ‘jsonvalidate’, ‘geojsonio’, ‘geojsonlint’
install.packages("tigris")
install.packages("acs")
install.packages("sf")
install.packages("mapview")
install.packages("geojson")
install.packages("geojsonio")

# library(sf)
# geo_sf <- read_sf("https://dawa.aws.dk/kommune/?format=geojson")
# library(ggplot2)
# ggplot(geo_sf) + geom_sf()

# Natural language processing packages

install.packages("alineR")
install.packages("boilerpipeR")
install.packages("corpora")
install.packages("gsubfn")
install.packages("gutenbergr")
install.packages("hunspell")
install.packages("kernlab")
install.packages("KoNLP")
install.packages("koRpus")
install.packages("languageR")
install.packages("lda")
install.packages("lsa")
install.packages("monkeylearn")
install.packages("movMF")
install.packages("mscstexta4r")
install.packages("mscsweblm4r")
install.packages("openNLP")
install.packages("ore")
install.packages("phonics")
install.packages("qdap")
install.packages("quanteda")
install.packages("RcmdrPlugin.temis")
install.packages("readtext")
install.packages("rel")
install.packages("RKEA")
install.packages("RWeka")
install.packages("skmeans")
install.packages("SnowballC")
install.packages("stringdist")
install.packages("stringi")
install.packages("tau")
install.packages("tesseract")
install.packages("text2vec")
install.packages("textcat")
install.packages("textclean")
install.packages("textir")
install.packages("textrank")
install.packages("textreuse")
install.packages("tidytext")
install.packages("tm")
install.packages("tm.plugin.alceste")
install.packages("tm.plugin.dc")
install.packages("tm.plugin.europresse")
install.packages("tm.plugin.factiva")
install.packages("tm.plugin.lexisnexis")
install.packages("tm.plugin.mail")
install.packages("tm.plugin.webmining")
install.packages("tokenizers")
install.packages("topicmodels")
install.packages("udpipe")
install.packages("wordcloud")
install.packages("wordnet")
install.packages("zipfR")

# Needs compilation
install.packages("stm")

# Packages needed for intro to Mathematics for economics
# 
# Matrices
install.packages("Matrix")
install.packages("expm")
install.packages("matlib") 
# Heaps of dependencies
# =====================
#‘colorspace’, ‘ps’, ‘backports’, ‘zeallot’, ‘plyr’, ‘labeling’, 
#‘munsell’, ‘RColorBrewer’, ‘processx’, ‘ellipsis’, ‘vctrs’, 
#‘clipr’, ‘rematch’, ‘prettyunits’, ‘assertthat’, ‘utf8’, ‘glue’, 
#‘stringi’, ‘BH’, ‘gtable’, ‘reshape2’, ‘scales’, ‘viridisLite’, 
#‘withr’, ‘callr’, ‘forcats’, ‘hms’, ‘readr’, ‘cellranger’, ‘progress’, 
#‘zip’, ‘cli’, ‘fansi’, ‘pillar’, ‘pkgconfig’, ‘yaml’, ‘digest’, ‘Rcpp’, 
#‘evaluate’, ‘highr’, ‘markdown’, ‘stringr’, ‘xfun’, ‘httpuv’, ‘mime’, 
#‘R6’, ‘sourcetools’, ‘later’, ‘promises’, ‘crayon’, ‘rlang’, ‘lazyeval’, 
#‘ggplot2’, ‘miniUI’, ‘base64enc’, ‘webshot’, ‘SparseM’, ‘MatrixModels’, 
#‘sp’, ‘haven’, ‘curl’, ‘data.table’, ‘readxl’, ‘openxlsx’, ‘tibble’, 
#‘minqa’, ‘nloptr’, ‘RcppEigen’, ‘htmlwidgets’, ‘htmltools’, ‘knitr’, 
#‘jsonlite’, ‘shiny’, ‘magrittr’, ‘crosstalk’, ‘manipulateWidget’, 
#‘carData’, ‘abind’, ‘pbkrtest’, ‘quantreg’, ‘maptools’, ‘rio’, 
#‘lme4’, ‘xtable’, ‘rgl’, ‘car’
install.packages("MASS")

# Packages for Non-parametrics
install.packages("np")
install.packages("manipulate")
install.packages("MASS")
install.packages("quadprog")
install.packages("crs")
install.packages("sem")

# Needs compilation
install.packages("lmtest")

# Packages to present SAS code in PDF
install.packages("DBI") # for SQL code
install.packages("SASmarkdown") # to allow additional output options

# # Install the R Kernel for processing R code in Jupyter:
# install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 
#                    'devtools', 'uuid', 'digest'))
# devtools::install_github('IRkernel/IRkernel')
# # Make the kernel available to either the specific user or all users:
# IRkernel::installspec()
# IRkernel::installspec(user = FALSE)

# Packages for Python integration
# install.packages("reticulate")
library(reticulate)
use_python('/Users/robertayretonbaileysmith/opt/anaconda3/bin/python')
# use_virtualenv("~/myenv")
# use_condaenv("myenv")

reticulate::repl_python()
reticulate::py_config()



# install.packages("tensorflow")
library(tensorflow)
install_tensorflow()




#################################################################
#################################################################
# Remove all packages from R
# ==========================
#################################################################
#################################################################

# create a list of all installed packages
ip <- as.data.frame(installed.packages())
head(ip)
# if you use MRO, make sure that no packages in this library will be removed
ip <- subset(ip, !grepl("MRO", ip$LibPath))
# we don't want to remove base or recommended packages either\
ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
# determine the library where the packages are installed
path.lib <- unique(ip$LibPath)
# create a vector with all the names of the packages you want to remove
pkgs.to.remove <- ip[,1]
head(pkgs.to.remove)
# remove the packages
sapply(pkgs.to.remove, remove.packages, lib = path.lib)

