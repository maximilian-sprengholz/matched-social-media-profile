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

# sample data
df <- read.csv(paste(wd, "testdata.csv", sep="/"), fileEncoding = "UTF-8")
df <- df[1:5,]

# clean
source("src/01_clean.R")

# parameters fpr match and export
source("src/02_matchvarparams.R")

# match
source("src/03_match.R")

# export profiles
