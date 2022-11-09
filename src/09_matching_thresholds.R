################################################################################
#
#   FIND MATCHING THRESHOLDS
#
################################################################################

'
This file checks with a subset of the results what sensible thresholds might be
for the non-focal similarity to be high or low. matches.feather has to be generated
without simhigh/simlow thresholds and without limit to the number of matches to
produce sensible results (using the final matches.feather makes no sense!).
'

### PARAMETERS ###
source("src/02_matchparams.R")


### RUN ###

# import matched data
df <- read_feather(paste0(wd, "/data/matches.feather"))

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