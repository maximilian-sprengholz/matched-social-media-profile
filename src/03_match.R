################################################################################
#
#   MATCHING
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
source("src/02_matchparams.R")


### FUNCTIONS ###

matcher <- function(
        df,
        idcol, # person ids, numeric
        matchparams, # dict containing all matching parameters
        valuesNA, # vector of values that should not be matched
        simlow = 599, # similarity is low <= value
        simhigh = 600, # similarity is high >= value
        maxmatches = 5, # n matches allowed per group (opinion (same/diff) x similarity (low/high))
        opinionvar = "essay_opinion_prior", # opinion dummy
        nsample = NA # integer, set if you want to select a subset for which a match is searched 
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
    df_matchsubset <- df[, c(idcol, opinionvar, "matchable", matchvars)]
    if (!is.numeric(nsample)) nsample <- nrow(df_matchsubset)

    # keep track in terminal
    message("\nMatcher started.")
    ptot <- nsample
    pcnt <- 0


    ### Step 1: Extract row of a single person (p1)
    df_match_all <- df_matchsubset %>%
        slice_sample(n = nsample) %>%
        pmap_dfr(function(...) {

            # keep track in terminal
            pcnt <<- pcnt + 1
            message(paste0("Matching ", pcnt, "/", ptot, "."), "\r", appendLF = FALSE)
            flush.console()

            # data of person 1 for whom we search matches
            row_p1 <- tibble(...)

            # dataframe containing all others with can be used in matching, shuffled
            df_p2 <- df_matchsubset %>%
                filter(idcol != as.numeric(row_p1[idcol]) & matchable == 1)
            df_p2 <- df_p2[sample(nrow(df_p2)), ]

            # count toward maximum no of matches allowed per group
            nmatches <- list(
                opsame = list(simlow = 0, simhigh = 0),
                opdiff = list(simlow = 0, simhigh = 0)
            )

            # count persons considered until maxmatches found for simlow/simhigh
            nconsidered <- 0
            nconsidered_simlow <- 0
            nconsidered_simhigh <- 0

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
                    if (any(nmatches[[opinion_matched]] <= maxmatches)) {

                        # count
                        nconsidered <<- nconsidered + 1

                        ### Step 3: match variables, gather match scores
                        match_results <- pmap_dfr(
                            list(row_p1, row_p2, names(row_p2)),
                            function(p1, p2, matchvar) {
                                # do not match on id
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
                                    score <- match_values(
                                        p1, p2,
                                        matchvar, matchvarparams,
                                        valuesNA,
                                        matchvarparams$get(matchvar)$get("split", FALSE),
                                        matchvarparams$get(matchvar)$get("fuzzy", FALSE),
                                        matchvarparams$get(matchvar)$get("fuzzy_distperchar", 0.33)
                                        )
                                    matched <- ifelse(score > 0 && !is.na(score), 1, 0)
                                    result <- data.frame(
                                        matchvar = matchvar,
                                        score = score,
                                        matched = matched
                                        )
                                    }
                                }
                            )

                        # sum scores, determine if return matches if still needed in group
                        simscore <- colSums(match_results['score'], na.rm = TRUE)
                        if (simscore <= simlow & nmatches[[opinion_matched]]$simlow <= maxmatches) {
                            nmatches[[opinion_matched]]$simlow <<- nmatches[[opinion_matched]]$simlow + 1
                            nconsidered_simlow <<- nconsidered
                            save <- 1
                        } else if (simscore >= simhigh & nmatches[[opinion_matched]]$simhigh <= maxmatches) {
                            nmatches[[opinion_matched]]$simhigh <<- nmatches[[opinion_matched]]$simhigh + 1
                            nconsidered_simhigh <<- nconsidered
                            save <- 1
                        } else if (simscore > simlow & simscore < simhigh) {
                            save <- 1
                        } else {
                            save <- 0
                            }

                        # prep and return match data
                        if (save == 1) {
                            # match dummies and scores (extract from match_results col)
                            row_matched <- match_results[, c('matchvar', 'score', 'matched')] %>%
                                pivot_wider(
                                    names_from = matchvar,
                                    values_from = c(matched, score),
                                    names_glue = "{matchvar}_{.value}"
                                    )
                            # add person id (for merge), match id, match simscore
                            # simscore low/high cutoffs
                            # value on opinion essay of matched person
                            row_matched[idcol] <- row_p1[idcol]
                            row_matched[paste0("match_", idcol)] <- row_p2[idcol]
                            row_matched["match_simscore"] <- simscore
                            row_matched["match_simlow"] <- simlow
                            row_matched["match_simhigh"] <- simhigh
                            row_matched["match_essay_opinion_prior"] <- row_p2[opinionvar]
                            # return
                            return(row_matched)
                        } else {
                            return(NULL)
                        }
                    } else {
                        return(NULL)
                        }
                })

                # add counts of considered persons to df
                df_match_person <- df_match_person %>% 
                    mutate(nconsidered_simlow = nconsidered_simlow) %>%
                    mutate(nconsidered_simhigh = nconsidered_simhigh)

            }
        )
        # message success, return all matches
        message("\nDone.")
        return(df_match_all)
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

    # vectorize
    if (is.character(split)) {
        p1 <- str_split(p1, split)
        p2 <- str_split(p2, split)
    } else {
        p1 <- c(p1)
        p2 <- c(p2)
        }

    # pop NA elements, continue only if vector length>0
    p1 <- setdiff(unlist(p1), valuesNA)
    p2 <- setdiff(unlist(p2), valuesNA)
    if (length(p1) > 0 && length(p2) > 0) {

        # match
        if (fuzzy == TRUE) {
            '
            Element by element comparison for maximum flexibility in terms of
            cleaning, checking, and fuzzy matching parameters.
            '
            # trim spaces and make lower case (which would inflate needed distance)
            p1 <- str_trim(p1, "both")
            p2 <- str_trim(p2, "both")
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
                                message(paste0("Compared ", matchvar, " with: '", str1, "' and '", str2, "'' with dist=", dist))
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
            # drop NA from common vector
            common <- common[!is.na(common)]
        } else {
            # exact matches
            common <- intersect(p1, p2)
            }

        # score each match
        if (length(common) > 0 && !any(is.na(common))) {
            score <- match_score(matchvar, matchvarparams, common)
        } else {
            score <- 0
            }
    } else {
        score <- NA
        }
    return(score)
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

# import cleaned data
df <- read_feather(paste0(wd, "/data/pre_match.feather"))

# match
df_matches <- matcher(
    df = df,
    idcol = "c_0116",
    matchparams = matchparams,
    valuesNA = c("-99", "-66", ".", "", "NA", NA),
    simlow = 600,
    simhigh = 601,
    maxmatches = 2,
    nsample = 5
    )

# # merge in long format (1 row per match per person).
df <- merge(df, df_matches, by = "c_0116", all = TRUE)

# save
write_feather(df, paste0(wd, "/data/post_match_preselection.feather"))


## SELECT MATCH ###############################################################

'
 Select a match for each person based on 4 match_group options

 4 options (polsim: essay_opinion_prior x nonpolsim: simscore):
     - same opinion & same characteristics (=highest simscore)
     - same opinion & diff. characteristics (=lowest simscore)
     - diff. opinion & same characteristics (=highest simscore)
     - diff. opinion & diff. characteristics (=lowest simscore)

 Selected match: match_profile_export = 1
'

# import matched data
df <- read_feather(paste0(wd, "/data/post_match_preselection.feather"))

# number of matches per person (= number of rows per c_0116)
df %>% select(c(c_0116)) %>% group_by(c_0116) %>% count() %>% summarize(n)

# match group: opinion (same/diff) x simscore (high/low)
df_opsame_simhigh <- df %>% group_by_at("c_0116") %>%
    filter(essay_opinion_prior == match_essay_opinion_prior) %>%
    slice(which.max(match_simscore)) %>%
    mutate(match_group = "Same opinion, same characteristics") %>%
    select(c(c_0116, match_c_0116, match_group))
df_opsame_simlow <- df %>% group_by_at("c_0116") %>%
    filter(essay_opinion_prior == match_essay_opinion_prior) %>%
    slice(which.min(match_simscore)) %>%
    mutate(match_group = "Same opinion, different characteristics") %>%
    select(c(c_0116, match_c_0116, match_group))
df_opdiff_simhigh <- df %>% group_by_at("c_0116") %>%
    filter(essay_opinion_prior != match_essay_opinion_prior) %>%
    slice(which.max(match_simscore)) %>%
    mutate(match_group = "Different opinion, same characteristics") %>%
    select(c(c_0116, match_c_0116, match_group))
df_opdiff_simlow <- df %>% group_by_at("c_0116") %>%
    filter(essay_opinion_prior != match_essay_opinion_prior) %>%
    slice(which.min(match_simscore)) %>%
    mutate(match_group = "Different opinion, different characteristics") %>%
    select(c(c_0116, match_c_0116, match_group))

# merge match group
df <- merge(
    df, rbind(df_opsame_simhigh, df_opsame_simlow, df_opdiff_simhigh, df_opdiff_simlow), 
    by = c("c_0116", "match_c_0116"), all.x = TRUE, all.y = FALSE
    )

# match_profile_export = random selection of match_group
df_random_match <- df %>% 
    group_by_at("c_0116") %>% 
    filter(!is.na(match_group)) %>%
    slice_sample(n = 1) %>%
    select(c(c_0116, match_c_0116)) %>%
    mutate(match_profile_export = 1)
df <- merge(df, df_random_match, by = c("c_0116", "match_c_0116"), all.x = TRUE, all.y = FALSE)

# distribution of groups
table(df %>% filter(match_profile_export == 1) %>% select(c(match_group)))

# save (might be HUGE if match no. is not restricted!)
write_feather(df, paste0(wd, "/data/post_match.feather"))