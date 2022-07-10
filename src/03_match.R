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
'

### FUNCTIONS ###

matcher <- function(
        df,
        idcol="lfdn", # person id, numeric
        matchparams, # dict containing all var-specific parameters
        valuesNA, # vector of values that should not be matched
        simlow=300, # simscore value below which similarity is low
        simhigh=600, # simscore value above which similarity is high
        maxmatches_simlow=5, # max no of low-similarity matches allowed per person
        maxmatches_simhigh=5 # max no of high-similarity matches allowed per person
        ) {

    ### Step 1: Extract row of one person with all the variables we match on
    matchvars <- c()
    matchvars <- unlist(sapply(
      unlist(matchparams$keys()),
      function(x) { matchvars <- c(matchvars, matchparams$get(x)$get("items")$keys()) }
      ))
    df_matchsubset <- df[, c(idcol, matchvars)]
    df_match_all <- df_matchsubset %>%
        pmap_dfr(function(...) {

            # data of person 1 for whom we search matches
            row_p1 <- tibble(...)

            # dataframe containing all others, shuffled
            df_p2 <- df_matchsubset[
              !df_matchsubset[idcol]==as.numeric(row_p1[idcol])
              ,]
            df_p2 <- df_p2[sample(nrow(df_p2)),]

            # maximum no of matches allowed
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
                        row_scores <- pmap_dfr(
                            list(row_p1, row_p2, names(row_p2)),
                            function(p1, p2, matchvar) {

                                # do not match on id
                                # check if cells contain something
                                if (matchvar != idcol
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
                                    score <- match_values(
                                        p1, p2,
                                        matchvar, matchvarparams,
                                        valuesNA,
                                        matchvarparams$get("split", FALSE),
                                        matchvarparams$get("fuzzy", FALSE),
                                        matchvarparams$get("fuzzymaxdist", 4)
                                        )

                                    }
                                }
                            )

                        # sum scores, return match only if similarity high or low
                        simscore <- rowSums(row_scores)
                        if (simscore<simlow || simscore>simhigh) {
                            # count
                            if (simscore<simlow) {
                                nmatches_simlow <<- nmatches_simlow + 1
                            } else {
                                nmatches_simhigh <<- nmatches_simhigh + 1
                                }
                            # return person id (for merge), match id, match simscore
                            row_matched <- data.frame (
                                row_p1[idcol],
                                row_p2[idcol],
                                simscore
                                )
                            # naming quirk when dfs passed
                            colnames(row_matched) <- c(
                                idcol,
                                paste0("match_", idcol),
                                "match_simscore")
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
    }

match_values <- function(
        p1,
        p2,
        matchvar,
        matchvarparams,
        valuesNA=valuesNA,
        split,
        fuzzy,
        fuzzymaxdist
        ) {
    '
    Matches two persons on each variable passed.

    - p1, p2: values of persons 1 and 2 for the compared variable
    - matchvar: name of variable compared
    - split: indicates if variable value is actually a vector of values (boolean)
    - fuzzy: indicates if fuzzy matching is required (boolean)
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
            # fuzzy matching requires string input!
            matchpos <- amatch(p1, p2, maxDist=fuzzymaxdist, matchNA=FALSE)
            matches <- p2[matchpos]
        } else {
            matches <- intersect(p1,p2)
            }

        # score each match
        if (length(matches)>0 && !any(is.na(matches))) {
            score <- match_score(matchvar, matchvarparams, matches)
        } else {
            score <- 0
            }
    } else {
        score <- 0
        }
    return(score)
    }

match_score <- function(matchvar, matchvarparams, matches) {
    '
    Returns the score (weight) associated with a specific match:
    - Weights can be constants or functions
    - If function, pass on value of match and match vector length (values have
      particular scores, vector elements get the same score per variable)
    - Values for which no return is specified throw a message

    The call mirrors the weights.js call of the Baliettia paper (as far as I can
    judge without the original matching function).
    '
    weight <- matchvarparams$get("weight") # throws error if not set
    if (is.function(weight)) {
        score <- weight(value=matches[1], common=length(matches))
        if (length(score)==0) {
            # debug missing scores
            message(paste0("No score found for ", matchvar, " [", matches[1], "]. Score set to 0."))
            score <- 0
            }
    } else if (is.numeric(weight)) {
        score <- weight
        }
    return(score)
    }

### RUN ###
df_matched <- matcher(
    df=df,
    matchparams=matchparams,
    valuesNA=c("-99", "-66", ".", "")
    )

### MERGE ###
'
At the moment the merging forces a long format (every match gets a row),
which is easier to deal with for the export, because you can implement some
selection rules based on the match(row)-specific similarity cores.

You could also merge it in wide format, though, an approach for which is
commented out below.
'
df_merged <- merge(df, df_matched, by="lfdn", all=TRUE)
# ### wide format
# # index matches by person
# df_matched <- df_matched %>% group_by(lfdn) %>% mutate(match_no = row_number(lfdn))
# # reshape
# df_matched <- df_matched %>%
#     pivot_wider(
#         names_from = match_no,
#         values_from = c(match_lfdn, match_simscore))
# # rename (pattern: match[no]_var instead of match_var_[no]) and order
# colnames(df_matched) <- gsub("(match)(_\\w+)_([0-9]+)", "\\1\\3\\2", colnames(df_matched))
# df_matched <- df_matched[,order(colnames(df_matched))]
# # merge
# df_merged <- merge(df, df_matched, by="lfdn", all=TRUE)
