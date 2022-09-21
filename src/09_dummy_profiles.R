# dummy files
df <- read_feather(paste0(wd, "/data/pre_match.feather"))
profile <- paste(readLines(paste0(wd, "/src/profile_template_dummy.html")))
df_dummies <- df %>% select(lfdn)
matchids <- unlist(df %>% filter(matchable == 1) %>% select(lfdn))
df_dummies$match_lfdn <- sapply(df_dummies$lfdn, function(lfdn) {
    sample(matchids[-lfdn], 1)
    })
df_dummies$match_profile_url <- unlist(lapply(df_dummies$lfdn, function(lfdn) {
    relpath <- paste0("/profiles/", lfdn, ".html")
    stri_write_lines(profile, paste0(wd, relpath))
    return(relpath)
    }))
df_dummies <- df_dummies %>%
    arrange(lfdn)
write.csv(df_dummies, paste0(wd, "/data/match_id_correspondence.csv"), row.names = FALSE)