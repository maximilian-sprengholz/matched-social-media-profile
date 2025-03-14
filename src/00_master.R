################################################################################
#
#   MASTER
#
################################################################################


### PACKAGES ###
install.packages("remotes")
library("remotes")
pkgs <- list(
    names = c("haven", "furrr", "tictoc", "feather", "stringi", "collections", "stringdist", "tidyverse", "progressr", "arrow"),
    versions = c("2.5.1", "0.3.1", "1.1", "0.3.5", "1.7.8", "0.3.6", "0.9.10", "1.3.2", "0.14.0", "16.1.0")
    )
for (i in 1:length(pkgs$names)) {
    install_version(pkgs$names[i], version = pkgs$versions[i], repos = "http://cran.us.r-project.org", dependencies = TRUE)
}
lapply(unlist(pkgs$names), library, character.only = TRUE)
library("parallel")

### PARAMETERS ###
`%!in%` <- Negate(`%in%`)
wd <- 'C:/Users/max/Seafile/matched-social-media-profile'
setwd(wd)

### RUN ###

# original data (you can ignore parsing messages)
df <- read_csv(
  paste0(wd, "/data/Data_with_printing_rules_and_wave6_20250203.csv"),
  guess_max = 6079
  )

# clean
source("src/01_clean.R", encoding = "UTF-8")

# match
source("src/03_match.R", encoding = "UTF-8")

# select match
source("src/04_select_match.R", encoding = "UTF-8")

# export profiles
source("src/05_export_profiles.R", encoding = "UTF-8")