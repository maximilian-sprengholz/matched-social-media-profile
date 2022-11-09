################################################################################
#
#   MASTER
#
################################################################################


### PACKAGES ###
install.packages("remotes")
library("remotes")
pkgs <- list(
    names = c("haven", "furrr", "tictoc", "feather", "stringi", "collections", "stringdist", "tidyverse"),
    versions = c("2.5.1", "0.3.1", "1.1", "0.3.5", "1.7.8", "0.3.6", "0.9.10", "1.3.2")
    )
for (i in 1:length(pkgs$names)) {
    install_version(pkgs$names[i], version = pkgs$versions[i], repos = "http://cran.us.r-project.org", dependencies = TRUE)
}
lapply(unlist(pkgs$names), library, character.only=TRUE)


### PARAMETERS ###
`%!in%` <- Negate(`%in%`)
wd <- '/PATH/TO/matched-social-media-profile'
setwd(wd)


### RUN ###

# original data (you can ignore parsing messages)
df <- read_csv(paste0(wd, "/data/Data_with_printing_rules_20221014.csv"))

# clean
source("src/01_clean.R")

# match
source("src/03_match.R")

# select match
source("src/04_select_match.R")

# export profiles
source("src/05_export_profiles.R")