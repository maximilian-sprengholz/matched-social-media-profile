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
'

### FUNCTIONS ###

matcher <- function(
        df,
        idcol="lfdn", # person id, numeric
        matchvarparams, # dict containing all var-specific parameters
        valuesNA, # vector of values that should not be matched
        keepunmatchedinfo, # vector of variables for which info should be kept for profile header even if unmatched
        maxmatches=30, # maximum no of matches allowed per person NOT IMPLEMENTED YET
        simlow=300, # simscore value below which similarity is low
        simhigh=600 # simscore value above which similarity is high
        ) {
    '
    - Wrapper function
    - Input: df input, single row per person
    - Output: long format, multiple rows per person for each (non-)match
    '
    # Step 1: Extract row of one person, match to all other rows (persons)
    df_matchsubset <- df[, c(idcol, unlist(matchvarparams$keys()))]
    df_match_all <- df_matchsubset %>%
        pmap_dfr(function(...) {

            # data of person 1 for whom we search matches
            row_p1 <- tibble(...)

            # dataframe containing all others, shuffled
            df_p2 <- df_matchsubset[
              !df_matchsubset[idcol]==as.numeric(row_p1[idcol])
              ,]
            df_p2 <- df_p2[sample(nrow(df_p2)),]

            # Step 2: Match rows between p1 and all p2
            df_match_person <- df_p2 %>%
                pmap_dfr(function(...) {

                    # defaults
                    simscore <- 0 # start value
                    row_p2 <- tibble(...) # person 2 row from matching set

                    # Step 3: match variables, add match scores
                    row_matched <- pmap_dfr(
                        list(row_p1, row_p2, names(row_p2)),
                        function(p1, p2, matchvar) {

                            # do not match on id
                            # check if cells contain something
                            if (matchvar != idcol
                                && length(p1)==1 && length(p2)==1
                                && !is.na(p1) && !is.na(p2)) {

                                # match and score
                                matchresults <- match_values(
                                    p1, p2, matchvar,
                                    valuesNA, keepunmatchedinfo, simscore,
                                    matchvarparams$get(matchvar)$get("split", FALSE),
                                    matchvarparams$get(matchvar)$get("fuzzy", FALSE),
                                    matchvarparams$get(matchvar)$get("fuzzymaxdist", 4)
                                    )

                                simscore <<- simscore + matchresults[[2]]
                                matches <- matchresults[[1]]

                                }
                            }
                        )

                    # return only if similarity high or low
                    if (simscore<simlow || simscore>simhigh) {
                        # add header info (displayed also when unmatched)
                        for (col in keepunmatchedinfo) {
                            row_matched[paste("profileheader", col, sep="_")] <- row_p2[col]
                        }
                        # add simscore and match id
                        row_matched$simscore <- simscore
                        row_matched[idcol] <- row_p2[idcol]
                        # rename columns
                        colnames(row_matched) <- c(
                            paste("match", colnames(row_matched), sep="_")
                            )
                        # add original id for merging
                        row_matched[idcol] <- row_p1[idcol]
                        # return
                        row_matched
                        }

                    })
            }
        )
    }

match_values <- function(
        p1,
        p2,
        matchvar,
        valuesNA=valuesNA,
        keepunmatchedinfo=keepunmatchedinfo,
        simscore,
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
            score <- match_score(matchvar, matches)
            if (is.character(split)) {
                # collapse again if vector
                matches <- paste(matches, collapse=split)
                }
            matchresults <- list(matches, score)
        } else {
            matchresults <- list(NA, 0)
            }
    } else {
        matchresults <- list(NA, 0)
        }
    return(matchresults)
    }

match_score <- function(matchvar, matches) {
    '
    Returns the score (weight) associated with a specific match:
    - Weights can be constants or functions
    - If function, pass on value of match and match vector length (values have
      particular scores, vector elements get the same score per variable)
    - Values for which no return is specified throw a message

    The call mirrors the weights.js call of the Baliettia paper (as far as I can
    judge without the original matching function).
    '
    weight <- matchvarparams$get(matchvar)$get("weight") # throws error if not set
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

### RUN AND MERGE ###
df_match_all <- matcher(
    df=df,
    matchvarparams=matchvarparams,
    valuesNA=c("-99", "-66", ".", ""),
    keepunmatchedinfo=c("age", "initials", "gender", "currentstate")
    )
df_merged <- merge(df, df_match_all, by="lfdn", all=TRUE)
