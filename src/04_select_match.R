################################################################################
#
#   SELECT MATCH
#
################################################################################

'
This file:

For each person, the matcher returns matches belonging to one of the following
groups, from which we select a single match whose profile should be exported.

    4 x match_group (polsim: essay_opinion_prior x nonpolsim: simscore):
     - same opinion & same characteristics (=highest simscore)
     - same opinion & diff. characteristics (=lowest simscore)
     - diff. opinion & same characteristics (=highest simscore)
     - diff. opinion & diff. characteristics (=lowest simscore)

    Selected match: match_profile_export = 1

Derfore we select, we can manually check the results (especially matches with
very high simscores for potential duplicates).
'

### PARAMETERS ###
set.seed(42)
source("src/02_matchparams.R")


### FUNCTIONS ###

get_matchvars <- function(
    matchparams, # dict containing all var-specific parameters
    fuzzyonly = FALSE # set to TRUE if only fuzzily matched vars should be checked
    ) {
    '
    This function gets all matchvars from the keys in matchparams.
    - fuzzyonly (bool): selects only fuzzily matched vars
    '

    matchvars <- c()
    for (group in unlist(matchparams$keys())) {
        items <- matchparams$get(group)$get("items")
        if (fuzzyonly == TRUE) {
            for (i in unlist(items$keys())) {
                if (items$get(i)$get("fuzzy", FALSE) == TRUE) matchvars <- c(matchvars, i)
                }
        } else {
            matchvars <- c(matchvars, unlist(items$keys()))
            }
        }
    return(matchvars)
}

compare_by_id <- function(df, matchvars = c(), pid, matchid) {
    '
    This function takes a person id (c_0116) and a match id (match_c_0116) and extracts a single
    row per person from the dataframe, and presents them side-by-side as transposed columns.
    - matchvars = vector of variables (column names) to be printed
    '

    # subset dataframe if matchvar vector passed
    if (length(matchvars) > 0) df <- df %>% select(c(matchvars))
    # select rows of persons (random row for matchas matching not necessarily bi-directional)
        print(df %>% filter(c_0116 == pid & match_c_0116 == matchid))
    print(df %>% filter(c_0116 == matchid) %>% slice_sample())
    df_match_pair <- rbind(
        df %>% filter(c_0116 == pid & match_c_0116 == matchid),
        df %>% filter(c_0116 == matchid) %>% slice_sample()
        )
    # pivot and trim string length to allow side-by-side output
    df_match_pair <- df_match_pair %>%
        pivot_longer(, cols = -c_0116, values_transform = as.character) %>%
        pivot_wider(, names_from = c_0116) %>%
        as.data.frame() %>%
        mutate_all(~strtrim(., 50)) # trim s
    # print
    print(df_match_pair, n = 150, na.print = "NA")

}

### MANUAL CHECK ###############################################################

# import matched data
df <- read_feather(paste0(wd, "/data/post_match_preselection.feather"))

# check number of matches per person against maxmatches (=35)
print(df %>% filter(!is.na(match_group)) %>%
    group_by_at(c("c_0116", "match_group")) %>% count(), n = 200)

# high/low values
summary(df$match_simscore)
df_simhigh <- df %>%
    select(c(c_0116, match_c_0116, match_simscore)) %>%
    arrange(desc(match_simscore)) %>%
    slice_head(n = 25)
print(df_simhigh)
df_simlow <- df %>% 
    select(c(c_0116, match_c_0116, match_simscore)) %>%
    arrange(match_simscore) %>%
    slice_head(n = 25)
print(df_simlow)

# subset (save memory)
df_matchvars <- df %>% select(c("c_0116", "match_c_0116", get_matchvars(matchparams = matchparams)))

# check high simscore matches one after another (= 1 row in the corresponding df)
compare_by_id(
    df = df_matchvars,
    pid = as.numeric(df_simhigh[1, "c_0116"]), # change index value to change match/row
    matchid = as.numeric(df_simhigh[1, "match_c_0116"]) # change index value to change match/row
    )

'
Exclude persons/matches from df before selection.
'

### SELECT MATCH ###############################################################

# random selection of match in match_group (so that each group contains one match)
df_random_match <- df %>%
    filter(!is.na(match_group)) %>%
    group_by_at(c("c_0116", "match_group")) %>%
    slice_sample(n = 1) %>%
    select(c(c_0116, match_c_0116, match_group))

# random selection of match group: set export indicator (match_profile_export)
df_random_match <- df_random_match %>%
    group_by_at("c_0116") %>%
    slice_sample(n = 1) %>%
    mutate(match_profile_export = 1) %>%
    select(c(c_0116, match_c_0116, match_profile_export))

# merge export indicator
df <- merge(df, df_random_match, by = c("c_0116", "match_c_0116"), all = TRUE)

# distribution of groups (should be uniformly distributed)
table(df %>% filter(match_profile_export == 1) %>% select(c(match_group)))

# save (might be HUGE if match no. is not restricted!)
write_feather(df, paste0(wd, "/data/post_match_post_selection.feather"))