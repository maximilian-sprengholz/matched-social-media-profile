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
summary(df$match_simscore)
# plot
ggplot(data = df, aes(x = match_simscore)) +
    geom_histogram(color = 1, alpha = 0.75, position = "identity", binwidth = 10, fill = "#8795E8")
ggsave(paste0(wd, "/results/hist_simscores_all.pdf"))

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