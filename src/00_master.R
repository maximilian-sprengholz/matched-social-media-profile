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
library(R6)
library(tidyverse)
library(stringdist)
library(collections)
library(stringi)

### definitions
set.seed(42)
`%!in%` <- Negate(`%in%`)
wd <- 'C:/Users/sprenmax/Seafile/projects/matched-social-media-profile'
setwd(wd)

### run

# sample data (use smaller subset to speed up matching)
df <- read.csv(paste(wd, "testdata.csv", sep="/"), fileEncoding = "UTF-8")
df <- df[1:30,]

# clean
source("src/01_clean.R")

# parameters fpr match and export
source("src/02_matchparams.R")

# match
source("src/03_match.R")

# export profiles
source("src/04_export_profiles.R")
