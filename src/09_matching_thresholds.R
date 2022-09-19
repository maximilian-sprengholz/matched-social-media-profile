################################################################################
#
#   FIND MATCHING THRESHOLDS
#
################################################################################

'
This file checks with a subset of the results what sensible thresholds might be
for the non-focal similarity to be high or low.
'

### PARAMETERS ###
source("src/02_matchparams.R")


### RUN ###

# import matched data
df <- read_feather(paste0(wd, "/data/post_match.feather"))

### distribution of simscores
# plot
ggplot(data = df, aes(x = match_simscore)) +
    geom_histogram(color = 1, alpha = 0.75, position = "identity", binwidth = 10, fill = "#8795E8")
ggsave(paste0(wd, "/results/hist_simscores_all.pdf"))
# max/min
summary(df$match_simscore)
print(df %>% filter(match_simscore > 1000) %>% select(c(lfdn, match_lfdn, match_simscore)), n = 100)
print(df %>% filter(match_simscore < 200) %>% select(c(lfdn, match_lfdn, match_simscore)), n = 100)

### check single matches for plausibility

# subset df to only contain variables of interest
matchvars = c()
for (group in unlist(matchparams$keys())) {
    items <- matchparams$get(group)$get("items")
    matchvars <- c(matchvars, unlist(items$keys()))
    }
cols <- c("lfdn", "match_lfdn", matchvars)
cols <- cols[cols %in% colnames(df)]
df_matchvars <- df[, cols]

# highest score -> seems to be the same person; trim given theshold?
df_match_pair <- df_matchvars %>% 
    filter((lfdn == "4841" & match_lfdn == "19500") | (lfdn == "19500" & match_lfdn == "868")) %>%
    pivot_longer(, cols = -lfdn, values_transform = as.character) %>%
    pivot_wider(, names_from = lfdn) %>%
    as.data.frame() %>% 
    mutate_all(~strtrim(., 50))
print(df_match_pair, n = 150, na.print="NA")

# lowest score
row_p1 <- df_matchvars %>%
    filter(lfdn == 11041 & match_lfdn == 3855)
row_p2 <- df_matchvars %>%
    filter(lfdn == 3855) %>%
    slice_sample()
df_match_pair <- rbind(row_p1, row_p2) %>% 
    pivot_longer(, cols = -lfdn, values_transform = as.character) %>%
    pivot_wider(, names_from = lfdn) %>%
    as.data.frame() %>%
    mutate_all(~strtrim(., 50))
print(df_match_pair, n = 150, na.print = "NA")

### best/worst matches
df_match_best <- df %>% group_by_at("lfdn") %>%
    filter(match_simscore > match_simhigh) %>%
    slice(which.max(match_simscore)) %>%
    mutate(match_type = "best") %>%
    select(c(lfdn, match_lfdn, match_type, match_simscore)) %>%
    filter(!is.na(match_type))
df_match_worst <- df %>% group_by_at("lfdn") %>%
    filter(match_simscore < match_simlow) %>%
    slice(which.min(match_simscore)) %>%
    mutate(match_type = "worst") %>%
    select(c(lfdn, match_lfdn, match_type, match_simscore)) %>%
    filter(!is.na(match_type))
df_match <- rbind(df_match_best, df_match_worst)

# plot
ggplot(data = df_match, aes(x = match_simscore, fill = match_type)) +
    geom_histogram(color = 1, alpha = 0.75, position = "identity", binwidth = 10) +
    scale_fill_manual(values = c("#8795E8", "#FF6AD5"))
ggsave(paste0(wd, "/results/hist_simscores_best_worst.pdf"))