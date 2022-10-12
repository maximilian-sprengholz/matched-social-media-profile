################################################################################
#
#   MASTER
#
################################################################################


### PACKAGES ###
# install.packages("R6")
# install.packages("tidyverse")
# install.packages("stringdist")
# install.packages("collections")
# install.packages("stringi")
# install.packages("feather")
# install.packages("tictoc")
# install.packages("furrr")
# install.packages("haven")
library(R6)
library(tidyverse)
library(stringdist)
library(collections)
library(stringi)
library(feather)
library(tictoc)
library(furrr)
library(progressr)
library(parallel)
library(haven)


### PARAMETERS ###
`%!in%` <- Negate(`%in%`)
wd <- '/home/max/Seafile/Projects/matched-social-media-profile'
setwd(wd)


### RUN ###

# original data (you can ignore parsing messages)
df <- read_csv(paste0(wd, "/data/Data_with_printing_rules_20221012.csv"))

# clean
source("src/01_clean.R")

# match
source("src/03_match.R")

# select match
source("src/04_select_match.R")

# export profiles
source("src/05_export_profiles.R")