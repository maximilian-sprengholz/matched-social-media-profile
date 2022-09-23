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
df <- read_feather(paste0(wd, "/data/post_match_preselection.feather"))

# correct mistake that people have been matched to themselves (wrong filter spec)
df <- df %>% filter(c_0116 != match_c_0116)

### distribution of simscores
summary(df$match_simscore)
quantiles <- quantile(df$match_simscore, probs = seq(0, 1, 1/20))
# plot
ggplot(data = df, aes(x = match_simscore)) +
    geom_histogram(color = 1, alpha = 0.75, position = "identity", binwidth = 10, fill = "#8795E8") +
    geom_vline(xintercept = quantiles[c(2, 11, 19)]) +
    annotate("text", x = quantiles[c(2, 11, 19)] - 55, y = 63000,
           label = paste(c("p10", "p50", "p90"), "=", quantiles[c(2, 11, 19)]), size = 3)
ggsave(paste0(wd, "/results/hist_simscores_all.pdf"))

# match group: opinion (same/diff) x simscore (high/low)
df_opsame_simhigh <- df %>% group_by_at("c_0116") %>%
    filter(essay_opinion_prior == match_essay_opinion_prior) %>%
    slice(which.max(match_simscore)) %>%
    mutate(match_group = "Same opinion, same char.") %>%
    select(c(c_0116, match_c_0116, match_group, match_simscore))
df_opsame_simlow <- df %>% group_by_at("c_0116") %>%
    filter(essay_opinion_prior == match_essay_opinion_prior) %>%
    slice(which.min(match_simscore)) %>%
    mutate(match_group = "Same opinion, diff. char.") %>%
    select(c(c_0116, match_c_0116, match_group, match_simscore))
df_opdiff_simhigh <- df %>% group_by_at("c_0116") %>%
    filter(essay_opinion_prior != match_essay_opinion_prior) %>%
    slice(which.max(match_simscore)) %>%
    mutate(match_group = "Diff. opinion, same char.") %>%
    select(c(c_0116, match_c_0116, match_group, match_simscore))
df_opdiff_simlow <- df %>% group_by_at("c_0116") %>%
    filter(essay_opinion_prior != match_essay_opinion_prior) %>%
    slice(which.min(match_simscore)) %>%
    mutate(match_group = "Diff. opinion, diff. char.") %>%
    select(c(c_0116, match_c_0116, match_group, match_simscore))

# merge match group
df_match <- rbind(df_opsame_simhigh, df_opsame_simlow, df_opdiff_simhigh, df_opdiff_simlow)

# plot
ggplot(data = df_match, aes(x = match_simscore, fill = match_group)) +
    geom_boxplot() +
    scale_fill_manual(
        "Respective 'best' match:",
        values = c("#6699CC", "#88CCEE", "#AA4466", "#CC6677")
        ) +
    geom_vline(xintercept = quantiles[c(1, 2, 11, 19, 20)], color = "#999999") +
    annotate(
            "text", x = quantiles[c(2, 11, 19)] - 50, y = 0.45,
           label = paste(c("p10", "p50", "p90"), "=", quantiles[c(2, 11, 19)]), size = 3,
           color = "#666666"
           ) +
    annotate(
        "text", x = quantiles[c(1, 20)] + 50, y = 0.45,
        label = paste(c("p5", "p95"), "=", quantiles[c(1, 20)]), size = 3,
        color = "#666666"
        ) +
    annotate(
        "text", x = quantiles[c(11)], y = 0.5,
        label = "Quantiles of the distribution of all matches", size = 4,
        color = "#333333"
        ) +
    theme(
        legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        )
ggsave(paste0(wd, "/results/hist_simscores_matchgroups.pdf"))