################################################################################
#
#   MATCH
#
################################################################################
'
The matching produces a sum score based on specific weights for each match.
What needs to be accomodated:
- Exact matches on value when only one value of a pre-defined set is possible
- Exact matches on values when multiple values of a pre-defined set are possible
- Fuzzy matches (=exact approx.) for open answers (single, list), either as
  standalone item or "other" extension (such as "othermusic")
- Exclusion rules:
  - Do not match on everything that resembles NA (-99, "", ...), which should
    also cover the case when variables are not set because of a filter

Changes 2022-07-08:
- match, but return just a match indicator, the id of the matched person and the
  score (just point to the matched person data row)

Changes 2022-08-18:
- Return dummy for each variable with a match (match_[varname]=0/1/NA)

Changes 2022-09-07:
- Return score for each match, so that we can select variables with the 
  highest/lowest weights for the profile

Changes 2022-09-20:
- new selection rules for 
    (1) the sample for which we search matches and 
    (2) the sample from which we draw the matches
- speeded up matching by checking group wise if maxmatches reached
- counter of considered matched until maxmatches reached
'

### PARAMETERS ###
set.seed(42)
options(scipen = 99)
source("src/02_matchparams.R")


### FUNCTIONS ###

matcher <- function(
        df,
        df_matchable, # subset who can be matches (convenience option to allow split of df)
        idcol, # person ids, numeric
        matchparams, # dict containing all matching parameters
        valuesNA, # vector of values that should not be matched
        simlow = 395, # similarity is low <= value
        simhigh = 653, # similarity is high >= value
        maxmatches = 35, # n matches allowed per group (opinion (same/diff) x similarity (low/high))
        opinionvar = "essay_opinion_prior"
        ) {

    # get matchvars and corresponding parameters (create new dict)
    matchvarparams <- dict()
    for (k in unlist(matchparams$keys())) {
        items <- matchparams$get(k)$get("items")
        for (i in unlist(items$keys())) {
            matchvarparams$set(i, items$get(i))
        }
    }
    matchvars <- unlist(matchvarparams$keys())

    # subset df to contain just matching vars, and persons for which matches should be found
    df <- df[, c(idcol, opinionvar, matchvars)]
    nsample <- nrow(df)

    # subset df_matchable
    df_matchable <- df_matchable[, c(idcol, opinionvar, matchvars)]

    # keep track in terminal
    message(paste0("\nMatcher started for n=", nsample, " Persons."))
    with_progress({

        # track progress
        p <- progressor(steps = nsample)

        ### Step 1: Do for each row of a single person (p1)
        df_match_all <- df %>%
            slice_sample(n = nsample) %>%
            future_pmap_dfr(function(...) {

                # data of person 1 for whom we search matches
                row_p1 <- tibble(...)

                # dataframe containing all others with can be used in matching, shuffled
                df_p2 <- df_matchable %>% filter(.data[[idcol]] != as.numeric(row_p1[idcol]))
                df_p2 <- df_p2[sample(nrow(df_p2)), ]

                # count toward maximum no of matches allowed per group
                nmatches <- list(
                    opsame = list(simlow = 0, simhigh = 0),
                    opdiff = list(simlow = 0, simhigh = 0)
                )

                # count persons considered until maxmatches found for simlow/simhigh
                p2cnt <- 0
                nconsidered <- list(
                    opsame = list(simlow = 0, simhigh = 0),
                    opdiff = list(simlow = 0, simhigh = 0)
                )

                ### Step 2: Match rows between p1 and all p2
                df_match_person <- df_p2 %>%
                    pmap_dfr(function(...) {

                        # person 2 row from matching set
                        row_p2 <- tibble(...)

                        # indicator if opinion matches (value = list element name in nmatches)
                        opinion_matched <- ifelse(
                            row_p1[opinionvar] == row_p2[opinionvar], "opsame", "opdiff"
                            )

                        # do not bother after having enough matches in group
                        if (any(nmatches[[opinion_matched]] < maxmatches)) {

                            # count
                            p2cnt <<- p2cnt + 1

                            ### Step 3: match variables, gather match scores
                            match_results <- pmap_dfr(
                                list(row_p1, row_p2, names(row_p2)),
                                function(p1, p2, matchvar) {
                                    # do not match on id, opinion, selector
                                    # check if cells contain something
                                    if (! matchvar %in% c(idcol, opinionvar, "matchable")
                                        && length(p1) == 1 && length(p2) == 1
                                        && !is.na(p1) && !is.na(p2)) {

                                        # match and score
                                        '
                                        Note: Change default of fuzzy_distperchar here!

                                        We return a dataframe with 3 columns:
                                        Matchvar, score and a dummy indicating
                                        if matchvar was matched .
                                        '
                                        match_result <- match_values(
                                            p1, p2,
                                            matchvar, matchvarparams,
                                            valuesNA,
                                            matchvarparams$get(matchvar)$get("split", FALSE),
                                            matchvarparams$get(matchvar)$get("fuzzy", FALSE),
                                            matchvarparams$get(matchvar)$get("fuzzy_distperchar", 0.25)
                                            )
                                        match_result <- data.frame(
                                            matchvar = matchvar,
                                            score = match_result$score,
                                            matched = match_result$matched
                                            )
                                        }
                                    }
                                )

                            # sum scores, determine if return matches if still needed in group
                            simscore <- colSums(match_results['score'], na.rm = TRUE)
                            if (simscore <= simlow & nmatches[[opinion_matched]]$simlow < maxmatches) {
                                # trigger save and count match
                                save <- TRUE
                                nmatches[[opinion_matched]]$simlow <<- nmatches[[opinion_matched]]$simlow + 1
                                # write considerations count when maxmtahces reached in group full
                                if (nmatches[[opinion_matched]]$simlow == maxmatches) {
                                    nconsidered[[opinion_matched]]$simlow <<- p2cnt
                                    }
                            } else if (simscore >= simhigh & nmatches[[opinion_matched]]$simhigh < maxmatches) {
                                # trigger save and count match
                                save <- TRUE
                                nmatches[[opinion_matched]]$simhigh <<- nmatches[[opinion_matched]]$simhigh + 1
                                # write considerations count when maxmtahces reached in group full
                                if (nmatches[[opinion_matched]]$simhigh == maxmatches) {
                                    nconsidered[[opinion_matched]]$simhigh <<- p2cnt
                                    }
                            } else if (simscore > simlow & simscore < simhigh & any(nmatches[[opinion_matched]] < 1)) {
                                save <- TRUE
                            } else {
                                save <- FALSE
                                }

                            # prep and return match data
                            if (save == TRUE) {
                                # match dummies and scores (extract from match_results col)
                                row_matched <- match_results[, c('matchvar', 'score', 'matched')] %>%
                                    pivot_wider(
                                        names_from = matchvar,
                                        values_from = c(matched, score),
                                        names_glue = "{matchvar}_{.value}"
                                        )
                                # add person id (for merge), match id, match simscore
                                # values on opinion essay of both persons (select before merge)
                                row_matched[idcol] <- row_p1[idcol]
                                row_matched["essay_opinion_prior"] <- row_p1[opinionvar]
                                row_matched[paste0("match_", idcol)] <- row_p2[idcol]
                                row_matched["match_simscore"] <- simscore
                                row_matched["match_essay_opinion_prior"] <- row_p2[opinionvar]
                                row_matched["essay_opinion_prior_matched"] <- ifelse(
                                    row_p1[opinionvar] == row_p2[opinionvar], 1, 0
                                    )
                                # return
                                return(row_matched)
                            } else {
                                return(NULL)
                                }
                        } else {
                            return(NULL)
                            }
                    })

                    # housekeeping: drop all (or keep one if no match) with simlow < simscore < simhigh
                    # create match group right here

                    # opsame simlow
                    if (nmatches$opsame$simlow > 0) {
                        df_match_opsame_simlow <- df_match_person %>%
                            filter(essay_opinion_prior_matched == 1 & match_simscore <= simlow) %>%
                            mutate(match_group = "Same opinion, different characteristics")
                    } else {
                        df_match_opsame_simlow <- df_match_person %>%
                            filter(essay_opinion_prior_matched == 1) %>%
                            slice(which.min(match_simscore)) %>%
                            mutate(match_group = "Same opinion, different characteristics")
                    }

                    # opsame simhigh
                    if (nmatches$opsame$simhigh > 0) {
                        df_match_opsame_simhigh <- df_match_person %>%
                            filter(essay_opinion_prior_matched == 1 & match_simscore >= simhigh) %>%
                            mutate(match_group = "Same opinion, same characteristics")
                    } else {
                        df_match_opsame_simhigh <- df_match_person %>%
                            filter(essay_opinion_prior_matched == 1) %>%
                            slice(which.max(match_simscore)) %>%
                            mutate(match_group = "Same opinion, same characteristics")
                    }

                    # opdiff simlow
                    if (nmatches$opdiff$simlow > 0) {
                        df_match_opdiff_simlow <- df_match_person %>%
                            filter(essay_opinion_prior_matched == 0 & match_simscore <= simlow) %>%
                            mutate(match_group = "Different opinion, different characteristics")
                    } else {
                        df_match_opdiff_simlow <- df_match_person %>%
                            filter(essay_opinion_prior_matched == 0) %>%
                            slice(which.min(match_simscore)) %>%
                            mutate(match_group = "Different opinion, different characteristics")
                    }

                    # opdiff simhigh
                    if (nmatches$opdiff$simhigh > 0) {
                        df_match_opdiff_simhigh <- df_match_person %>%
                            filter(essay_opinion_prior_matched == 0 & match_simscore >= simhigh) %>%
                            mutate(match_group = "Different opinion, same characteristics")
                    } else {
                        df_match_opdiff_simhigh <- df_match_person %>%
                            filter(essay_opinion_prior_matched == 0) %>%
                            slice(which.max(match_simscore)) %>%
                            mutate(match_group = "Different opinion, same characteristics")
                    }

                    # bind together
                    df_match_person <- rbind(
                        df_match_opsame_simlow, df_match_opsame_simhigh,
                        df_match_opdiff_simlow, df_match_opdiff_simhigh
                        )

                    # add counts to df
                    df_match_person <- df_match_person %>%
                        mutate(nconsidered_opsame_simlow = nconsidered$opsame$simlow) %>%
                        mutate(nconsidered_opsame_simhigh = nconsidered$opsame$simhigh) %>%
                        mutate(nconsidered_opdiff_simlow = nconsidered$opdiff$simlow) %>%
                        mutate(nconsidered_opdiff_simhigh = nconsidered$opdiff$simhigh)

                    # update progress and return
                    p()
                    return(df_match_person)

                },
                # ensure reproducible number generation (RNG)
                # make scheduling dynamic
                .options = furrr_options(seed = TRUE, scheduling = 10L)
            )
        })

        # add parameters to df
        df_match_all <- df_match_all %>%
            mutate(simlow = simlow) %>%
            mutate(simhigh = simhigh) %>%
            mutate(maxmatches = maxmatches)
    }

match_values <- function(
        p1,
        p2,
        matchvar,
        matchvarparams,
        valuesNA = valuesNA,
        split,
        fuzzy,
        fuzzy_distperchar
        ) {
    '
    Matches two persons on each variable passed.

    - p1, p2: values of persons 1 and 2 for the compared variable
    - matchvar: name of variable compared
    - split: indicates if var value is actually a vector of values by specifiying delimiter
    - fuzzy: indicates if fuzzy matching is required (boolean)
    - fuzzy_distperchar: fuzzy distance allowed for each char in the longest of strings compared 
      (default passed is 0.33)
    '

    # result placeholder, defaults to NA
    match_result <- list(matched = NA, score = NA)

    # split string
    if (is.character(split)) {
        p1 <- str_split(p1, split)
        p2 <- str_split(p2, split)
        p1 <- str_trim(unlist(p1), "both")
        p2 <- str_trim(unlist(p2), "both")
    } else {
        p1 <- c(p1)
        p2 <- c(p2)
        }

    # pop NA elements, continue only if vector length>0
    p1 <- setdiff(p1, valuesNA)
    p2 <- setdiff(p2, valuesNA)
    if (length(p1) > 0 && length(p2) > 0) {

        # match
        if (fuzzy == TRUE) {
            '
            Element by element comparison for maximum flexibility in terms of
            cleaning, checking, and fuzzy matching parameters.
            '
            # make lower case (which would inflate needed distance)
            p1 <- str_to_lower(p1)
            p2 <- str_to_lower(p2)
            # fuzzy matching
            common <- unlist(lapply(p1, function(str1) {
                # workaround for lapply not accepting continue:
                # return NA if str1 is already matched
                str1matched <- 0
                unlist(lapply(p2, function(str2) {
                    if (str1matched == 0) {
                        # compute maximum distance based on passed string length
                        # and factor fuzzy_distperchar; minimum is 1
                        dist <- ifelse(
                            nchar(str1) > nchar(str2),
                            fuzzy_distperchar * nchar(str1),
                            fuzzy_distperchar * nchar(str2)
                            )
                        # implement exception handling to know whats going on
                        tryCatch(
                            matchpos <- amatch(str1, str2, maxDist = dist),
                            error = function(e) {
                                flush.console()
                                message(paste0("Caught error: ", e))
                                message(paste0(matchvar, " with: '", str1, "' and '", str2, "'; dist=", dist))
                                matchpos <<- NA
                                }
                            )
                        if (!is.na(matchpos)) {
                            str1matched <<- 1
                            match <- str2
                            p2 <<- p2[! p2 %in% str2]
                            return(str2)
                        } else {
                            return(NA)
                        }
                    } else {
                        return(NA)
                    }
                    }))
                }))
            # drop NAs (logic different here: if common == NA -> no match found!)
            common <- common[!is.na(common)]
        } else {
            # exact matches
            common <- intersect(p1, p2)
            }

        # return match result and score
        # match status is not determined via score alone, as there are matches with score 0
        if (length(common) > 0) {
            match_result$matched <- 1
            match_result$score <- match_score(matchvar, matchvarparams, common)
        } else {
            match_result$matched <- 0
            match_result$score <- 0
            }
        }
    return(match_result)
    }

match_score <- function(matchvar, matchvarparams, common) {
    '
    Returns the score (weight) associated with a specific match:
    - Weights can be constants or functions
    - If function, pass on:
      - value of match (used if atomic)
      - match vector (usually used to score each of common items
        (=matches length), in some cases further checks (e.g. language))
    - Values for which no return is specified throw a message

    The call mirrors the weights.js call of the Balietti paper (as far as I can
    judge without the original matching function).
    '
    weight <- matchvarparams$get(matchvar)$get("weight") # throws error if not set
    if (is.function(weight)) {
        score <- weight(value = common[1], common = common)
        if (length(score) == 0) {
            # debug missing scores
            message(paste0(
                "No score found for ", matchvar, " [", common[1],
                "]. Score set to 0."
                ))
            score <- 0
            }
    } else if (is.numeric(weight)) {
        score <- weight
        }
    return(score)
    }


### RUN ########################################################################

# matcher df input
'
The parameters df and df_matchable are separate inputs to allow for splitting df
in chunks while keeping all the potential matching partners in df_matchable.
'
df <- read_feather(paste0(wd, "/data/pre_match.feather")) # cleaned pre-match data
df_matchable <- df %>% filter(matchable == 1)

# set-up multisession
ncores <- detectCores()
plan(multisession, workers = ncores)

# match
tic()
df_matches <- matcher(
    df = df,
    df_matchable = df_matchable,
    idcol = "c_0116",
    matchparams = matchparams,
    valuesNA = c("-99", "-66", ".", "", "NA", NA),
    simlow = 395,
    simhigh = 653,
    maxmatches = 35
    )
toc()

# save before match-selection (allow manual intervention if necessary)
write_feather(df_matches, paste0(wd, "/data/matches.feather"))
