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

# # sample data (use smaller subset to speed up matching)
# df <- read.csv(paste(wd, "testdata.csv", sep="/"), fileEncoding = "UTF-8")
# df <- df[1:30,]
#
# # clean
# source("src/01_clean.R")

# parameters fpr match and export
source("src/02_matchparams.R")

# # match
# source("src/03_match.R")

# 15/08/2022: Import data from Ferdi
'
Caution: Nothing has been matched, these are just the profiles for the persons
themselves! Matching not sensible atm based on provided data - check again with
Ferdi.
'
df_merged <- read.csv(paste(wd, "testdata_2022-08-15.csv", sep="/"), fileEncoding = "UTF-8")
df_merged$match_lfdn <- df_merged$lfdn
df_merged$match_simscore <- 600

# export profiles
source("src/04_export_profiles.R")
