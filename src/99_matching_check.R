################################################################################
#
#   CHECK MATCHING RESULTS
#
################################################################################

'
This file implements a checking routine for the matching results by randomly 
drawing from the pool of persons, matches and matchvars, juxtaposing matchvar 
values under the matching result.
'

### PARAMETERS ###
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

check_matches <- function(
        df,
        matchvars, # dict containing all var-specific parameters
        matchdummyvalues = c(0, 1, NA) # vector of match results to check (unmatched, matched, NA)
        ) {
    '
    This function provides a step-by-step checking of matches via an interactive sesstion.
    Variabel by variable, a random selection of match pairs is presented with the respective
    values, match result, and match score.
    '
    # subset dataframe to contain just matching specific columns; and what's in the df
    cols <- c(
        "c_0116", matchvars, "match_c_0116", paste0(matchvars, "_matched"), paste0(matchvars, "_score")
        )
    cols <- cols[cols %in% colnames(df)]
    df <- df[cols]
    matchvars <- gsub("_matched", "", names(df %>% select(contains("_matched"))))

    # randomly select a match pair (person, match); display; ask to continue
    if (interactive()) {
        for (matchvar in matchvars) {
            continue <- TRUE
            # runs forever if no manual user abort via terminal input
            while (continue == TRUE) {
                # subset, check if non-empty
                df_matchvar <- df %>% select(ends_with("c_0116") | starts_with(matchvar))
                # randomly select p1 row, check if empty
                row_p1 <- df_matchvar %>%
                    filter(!is.na(match_c_0116) & .data[[paste0(matchvar, "_matched")]] %in% matchdummyvalues)
                if (nrow(row_p1) > 0) {
                    row_p1 <- row_p1 %>% slice_sample(n = 1)
                    # select match row
                    row_p2 <- df_matchvar %>%
                        filter(c_0116 %in% row_p1["match_c_0116"]) %>%
                        slice_sample(n = 1)
                    # display
                    message(paste0("Matchvar:                    ", matchvar))
                    message(paste0("Value P1 (", row_p1["c_0116"], "):  ", row_p1[matchvar]))
                    message(paste0("Value P2 (", row_p2["c_0116"], "):  ", row_p2[matchvar]))
                    message(paste0("Matched:                     ", row_p1[paste0(matchvar, "_matched")]))
                    message(paste0("Score:                       ", row_p1[paste0(matchvar, "_score")]))
                    # user confirmation
                    userinput <- readline(
                        prompt = "[Enter] next match / ['n' + Enter] next variable / ['e' + Enter] exit : "
                        )
                    if (grepl("^e", userinput)) {
                        # exit
                        stop("User exited.")
                    } else if (grepl("^n", userinput)) {
                        # jump to next
                        continue <- FALSE
                        }
                } else {
                    continue <- FALSE
                    }
                }
        }
    } else {
        stop("Please start this program in an interactive session.")
        }
    }


### DATA ########################################################################

# import matched data
df <- read_feather(paste0(wd, "/data/post_match.feather"))


### TEST 1 ########################################################################
'
The first set of tests should check if the worst/best matches in the data make sense. For example:
- Very high similarity scores might indicate that a person participated multiple times
- Very low similarity scores might be due to NAs on the variables instead of non-matches

Please check the top-25 and bottom-25 of matches as shown below and note any peculiarities.
'

# high/low values
summary(df$match_simscore)
df_simhigh <- df %>%
    select(c(c_0116, match_c_0116, match_simscore, match_profile_simscore)) %>%
    arrange(desc(match_simscore)) %>%
    slice_head(n = 25)
print(df_simhigh)
df_simlow <- df %>% 
    select(c(c_0116, match_c_0116, match_simscore, match_profile_simscore)) %>%
    arrange(match_simscore) %>%
    slice_head(n = 25)
print(df_simlow)

# subset (save memory)
df_matchvars <- df %>% select(c("c_0116", "match_c_0116", get_matchvars(matchparams = matchparams)))

# check manually for high/low simscores and each match (= 1 row in the corresponding df)
compare_by_id(
    df = df_matchvars,
    pid = as.numeric(df_simhigh[1, "c_0116"]),
    matchid = as.numeric(df_simhigh[1, "match_c_0116"])
    )
compare_by_id(
    df = df_matchvars,
    pid = as.numeric(df_simlow[1, "c_0116"]),
    matchid = as.numeric(df_simlow[1, "match_c_0116"])
    )


### TEST 2 ########################################################################
'
The second set of tests checks if the matchin algorithm matched correctly. Please
run the following functions one after another and check if the results make sense:

- match status should be 1 if values the same (or very close), 0 if values are different,
  and NA if any of the values is NA
- Scores should be >0 if matched == 1, score == 0 if matched == 0, score == NA if matched == NA

Note any peculiarities.
'

# (1) look at everything: exact and fuzzy, matched or not
check_matches(
    df = df,
    matchvars = get_matchvars(matchparams = matchparams)
    )

# (2) look only at successful fuzzy matches
check_matches(
    df = df,
    matchvars = get_matchvars(matchparams = matchparams, fuzzyonly = TRUE),
    matchdummyvalues = c(0)
    )