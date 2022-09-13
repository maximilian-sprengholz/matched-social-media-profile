################################################################################
#
#   MASTER
#
################################################################################

### packages
# install.packages("R6")
# install.packages("tidyverse")
# install.packages("stringdist")
# install.packages("collections")
# install.packages("stringi")
# install.packages("feather")
library(R6)
library(tidyverse)
library(stringdist)
library(collections)
library(stringi)
library(feather)

### definitions
set.seed(42)
`%!in%` <- Negate(`%in%`)
wd <- '/home/max/Seafile/Projects/matched-social-media-profile'
setwd(wd)

### run

# sample data (use smaller subset to speed up matching)
df <- read_csv(paste0(wd, "/data/testdata_2022-09-07.csv"))
df <- df[1:20,]

# clean
source("src/01_clean.R")

# match
source("src/03_match.R")

# export profiles
source("src/04_export_profiles.R")