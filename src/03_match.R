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
- Return dummy for each variable with a match (match_[varname]=0/1)

Changes 2022-09-07:
- Return score for each match, so that we can select variables with the 
  highest/lowest weights for the profile

'

### PARAMETERS ###
source("src/02_matchparams.R")


### FUNCTIONS ###

matcher <- function(
        df,
        idcol="lfdn", # person id, numeric
        matchparams, # dict containing all var-specific parameters
        valuesNA, # vector of values that should not be matched
        simlow=300, # simscore value below which similarity is low
        simhigh=600, # simscore value above which similarity is high
        maxmatches_simlow=5, # max no of low-similarity matches allowed per person
        maxmatches_simhigh=5, # max no of high-similarity matches allowed per person
        opinionvar = "essay_opinion_prior" # opinion dummy
        ) {

    ### Step 1: Extract row of one person with all the variables we match on
    matchvars <- c()
    matchvars <- unlist(sapply(
      unlist(matchparams$keys()),
      function(x) { matchvars <- c(matchvars, matchparams$get(x)$get("items")$keys()) }
      ))
    df_matchsubset <- df[, c(idcol, opinionvar, matchvars)]

    # keep track in terminal
    message("\nMatcher started.")
    ptot <- nrow(df_matchsubset)
    pcnt <- 0

    df_match_all <- df_matchsubset %>%
        pmap_dfr(function(...) {

            # keep track in terminal
            pcnt <<- pcnt + 1
            message(paste0("Matching ", pcnt, "/", ptot, "."), "\r", appendLF=FALSE)
            flush.console()

            # data of person 1 for whom we search matches
            row_p1 <- tibble(...)

            # dataframe containing all others, shuffled
            df_p2 <- df_matchsubset[
              !df_matchsubset[idcol]==as.numeric(row_p1[idcol])
              ,]
            df_p2 <- df_p2[sample(nrow(df_p2)),]

            # count toward maximum no of matches allowed
            nmatches_simlow <- 0
            nmatches_simhigh <- 0

            ### Step 2: Match rows between p1 and all p2
            df_match_person <- df_p2 %>%
                pmap_dfr(function(...) {

                    # do not bother after having enough matches
                    if ((nmatches_simlow < maxmatches_simlow)
                         || (nmatches_simhigh < maxmatches_simhigh)) {

                        # person 2 row from matching set
                        row_p2 <- tibble(...)

                        ### Step 3: match variables, gather match scores
                        match_results <- pmap_dfr(
                            list(row_p1, row_p2, names(row_p2)),
                            function(p1, p2, matchvar) {

                                # do not match on id
                                # check if cells contain something
                                if (matchvar != idcol
                                    && matchvar != opinionvar
                                    && length(p1)==1 && length(p2)==1
                                    && !is.na(p1) && !is.na(p2)) {

                                    # get item parameter sub-dict
                                    for (k in unlist(matchparams$keys())) {
                                        items <- matchparams$get(k)$get("items")
                                        if (items$has(matchvar)) {
                                            matchvarparams <- items$get(matchvar)
                                            break
                                        }
                                    }

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
                                        matchvarparams$get("split", FALSE),
                                        matchvarparams$get("fuzzy", FALSE),
                                        matchvarparams$get("fuzzy_distperchar", 0.33)
                                        )
                                    matched <- ifelse(score>0 && !is.na(score), 1, 0)
                                    result <- data.frame (
                                        matchvar = matchvar,
                                        score = score,
                                        matched = matched
                                        )
                                    }
                                }
                            )

                        # sum scores, return match only if similarity high or low
                        simscore <- colSums(match_results['score'], na.rm=TRUE)
                        if (simscore<simlow || simscore>simhigh) {
                            # count
                            if (simscore<simlow) {
                                nmatches_simlow <<- nmatches_simlow + 1
                            } else {
                                nmatches_simhigh <<- nmatches_simhigh + 1
                                }
                            # return match dummies and scores (extract from match_results col)
                            row_matched <- match_results[,c('matchvar', 'score', 'matched')] %>%
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
        valuesNA=valuesNA,
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
    if (length(p1)>0 && length(p2)>0) {

        # match
        if (fuzzy==TRUE) {
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
                    if (str1matched==0) {
                        # compute maximum distance based on passed string length
                        # and factor fuzzy_distperchar; minimum is 1
                        dist <- ifelse(
                            nchar(str1) > nchar(str2),
                            fuzzy_distperchar * nchar(str1),
                            fuzzy_distperchar * nchar(str2)
                            )
                        # implement exception handling to know whats going on
                        tryCatch(
                            matchpos <- amatch(str1, str2, maxDist=dist),
                            error = function(e) {
                                message(paste0("Caught error: ", e))
                                message(paste0("Compared ", str1, " and ", str2, " with dist=", dist))
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
        if (length(common)>0 && !any(is.na(common))) {
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
    weight <- matchvarparams$get("weight") # throws error if not set
    if (is.function(weight)) {
        score <- weight(value=common[1], common=common)
        if (length(score)==0) {
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
    df=df,
    matchparams=matchparams,
    valuesNA=c("-99", "-66", ".", "", "NA", NA),
    simlow=399,
    simhigh=699,
    maxmatches_simlow=5,
    maxmatches_simhigh=5
    )

# merge in long format (1 row per match per person).
df <- merge(df, df_matches, by="lfdn", all=TRUE)

# save (temp)
write_feather(df, paste0(wd, "/data/post_match_preselection.feather"))


### SELECT MATCH ###############################################################

'
 Select a match for each person based on 4 match_group options

 4 options (polsim: essay_opinion_prior x nonpolsim: simscore):
     - same opinion & same characteristics (=highest simscore)
     - same opinion & diff. characteristics (=lowest simscore)
     - diff. opinion & same characteristics (=highest simscore)
     - diff. opinion & diff. characteristics (=lowest simscore)

 Selected match: match_profile_export = 1
'
# polsim (opinion essay)
df <- df %>% mutate(match_polsim = ifelse(
    essay_opinion_prior == match_essay_opinion_prior, 1, 0
    ))

# nonpolsim (simscore, check against user provided simscore bounds)
df_match_best <- df %>% group_by_at("lfdn") %>%
    slice(which.max(match_simscore)) %>%
    mutate(match_nonpolsim = ifelse(match_simscore > match_simhigh, 1, NA)) %>%
    select(c(lfdn, match_lfdn, match_nonpolsim)) %>% filter(!is.na(match_nonpolsim))
df_match_worst <- df %>% group_by_at("lfdn") %>%
    slice(which.min(match_simscore)) %>%
    mutate(match_nonpolsim = ifelse(match_simscore < match_simlow, 0, NA)) %>%
    select(c(lfdn, match_lfdn, match_nonpolsim)) %>% filter(!is.na(match_nonpolsim))
df <- merge(
    df, rbind(df_match_best, df_match_worst), by=c("lfdn", "match_lfdn"),
    all.x=TRUE, all.y=FALSE
    )

# match_group
df$match_group <- NA
df$match_group[df$match_polsim==1 & df$match_nonpolsim==1] <- "Same opinion, same characteristics"
df$match_group[df$match_polsim==1 & df$match_nonpolsim==0] <- "Same opinion, different characteristics"
df$match_group[df$match_polsim==0 & df$match_nonpolsim==1] <- "Different opinion, same characteristics"
df$match_group[df$match_polsim==0 & df$match_nonpolsim==0] <- "Different opinion, different characteristics"

# match_profile_export = random selection of match_group
set.seed(42)
df_random_match <- df %>% group_by_at("lfdn") %>% filter(!is.na(match_group)) %>%
    slice_sample(n = 1) %>% select(c(lfdn, match_lfdn)) %>%
    mutate(match_profile_export = 1)
df <- merge(df, df_random_match, by=c("lfdn", "match_lfdn"), all.x=TRUE, all.y=FALSE)

# save (might be HUGE if match no. is not restricted!)
write_feather(df, paste0(wd, "/data/post_match.feather"))