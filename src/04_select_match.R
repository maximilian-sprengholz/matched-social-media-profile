################################################################################
#
#   SELECT MATCH
#
################################################################################

'
This file:

(1) For each person, the matcher returns matches belonging to one of the following
    groups, from which we select a single match whose profile should be exported.

    4 x match_group (polsim: essay_opinion_prior x nonpolsim: simscore):
     - same opinion & same characteristics (=highest simscore)
     - same opinion & diff. characteristics (=lowest simscore)
     - diff. opinion & same characteristics (=highest simscore)
     - diff. opinion & diff. characteristics (=lowest simscore)

    Selected match: match_profile_export = 1

(2) We merge the selected match data to the original, cleaned pre-match data.

'

### PARAMETERS ###
set.seed(42)
source("src/02_matchparams.R")


### SELECT #####################################################################

df <- read_feather(paste0(wd, "/data/pre_match.feather")) # cleaned pre-match data
df_matches <- read_feather(paste0(wd, "/data/matches.feather")) # match data

# (1) random selection of match in match_group (so that each group contains one match)
# (2) random selection of match group: set export indicator (match_profile_export)
set.seed(42)
df_random_match <- df_matches %>%
    filter(!is.na(match_group)) %>%
    group_by(c_0116, match_group) %>%
    slice_sample(n = 1) %>%
    ungroup(match_group) %>%
    group_by(c_0116) %>%
    slice_sample(n = 1) %>%
    mutate(match_profile_export = 1) %>%
    select(., -matches("^essay_opinion_prior$")) %>%
    data.frame()

# distribution of groups (should be uniformly distributed)
table(df_random_match %>% filter(match_profile_export == 1) %>% select(c(match_group)))

# merge info
df <- merge(df, df_random_match, by = "c_0116", all = TRUE)

# save
write_feather(df, paste0(wd, "/data/post_match.feather"))

