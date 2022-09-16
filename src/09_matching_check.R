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

check_matches <- function(
        df,
        matchparams, # dict containing all var-specific parameters
        matchvars = c(), # vector of matching variables to be checked; defaults to all
        fuzzyonly = FALSE, # set to TRUE if only fuzzily matched vars should be checked
        matchdummyvalues = c(0, 1, NA) # vector of match results to check (unmatched, matched, NA)
        ) {
    
    # variables to be checked
    # if nothing passed fetch all varnames from matchparams
    # check if only fuzzily matched variables should be included
    if (length(matchvars) == 0) {
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
    } else if (fuzzyonly == TRUE) {
        for (group in unlist(matchparams$keys())) {
            items <- matchparams$get(group)$get("items")
            for (i in unlist(items$keys())) {
                if (items$get(i)$get("fuzzy", FALSE) == FALSE) {
                    matchvars <- matchvars[!(matchvars %in% i)]
                    }
                }
            }
        }
    cols <- c(
        "lfdn", matchvars, "match_lfdn", paste0(matchvars, "_matched"), paste0(matchvars, "_score")
        )
    cols <- cols[cols %in% colnames(df)]
    df <- df[, cols]

    # randomly select a match pair (person, match); display; ask to continue
    if (interactive()) {
        for (matchvar in matchvars) {
            continue <- TRUE
            # runs forever if no manual user abort via terminal input
            while (continue == TRUE) {
                # subset
                df_matchvar <- df %>% select(ends_with("lfdn") | starts_with(matchvar)) %>%
                    filter(.data[[paste0(matchvar, "_matched")]] %in% matchdummyvalues)
                row_p1 <- df_matchvar %>% slice_sample(n = 1)
                row_p2 <- df_matchvar %>% filter(lfdn %in% row_p1["match_lfdn"]) %>% 
                    slice_sample(n = 1)
                # display
                message(paste0("Matchvar:          ", matchvar))
                message(paste0("Value P1 (", row_p1["lfdn"], "):  ", row_p1[matchvar]))
                message(paste0("Value P2 (", row_p2["lfdn"], "):  ", row_p2[matchvar]))
                message(paste0("Matched:           ", row_p1[paste0(matchvar, "_matched")]))
                message(paste0("Score:             ", row_p1[paste0(matchvar, "_score")]))
                # user confirmation
                userinput <- readline(
                    prompt = "Hit enter to continue. Write 'exit' and hit enter to exit. "
                    )
                if (grepl("^exit", userinput)) {
                    # exit
                    stop("User exited.")
                } else if (grepl("^next", userinput)) {
                    # jump to next
                    continue <- FALSE
                    }
                }
        }
    } else {
        stop("Please start this program in an interactive session.")
        }
    }



### RUN ########################################################################

# import matched data
df <- read_feather(paste0(wd, "/data/post_match.feather"))

### test

# (1) look at everything: exact and fuzzy, matched or not
check_matches(df=df, matchparams=matchparams)

# (2) look only at successful fuzzy matches 
# check_matches(df=df, matchparams=matchparams, fuzzyonly = TRUE, matchdummyvalues = c(1))
