# dummy files
df <- read_feather(paste0(wd, "/data/pre_match.feather"))
profile <- paste(readLines(paste0(wd, "/src/profile_template_dummy.html")))
df_dummies <- df %>% select(c_0116)
matchids <- unlist(df %>% filter(matchable == 1) %>% select(c_0116))
df_dummies$match_c_0116 <- sapply(df_dummies$c_0116, function(c_0116) {
    sample(matchids[-c_0116], 1)
    })
df_dummies$match_profile_url <- unlist(lapply(df_dummies$c_0116, function(c_0116) {
    relpath <- paste0("/profiles/", c_0116, ".html")
    stri_write_lines(profile, paste0(wd, relpath))
    return(relpath)
    }))
df_dummies <- df_dummies %>%
    arrange(c_0116)
write.csv(df_dummies, paste0(wd, "/data/match_id_correspondence.csv"), row.names = FALSE)