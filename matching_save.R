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
  - Do not match on everything that resembles NA (-99, "", ...)
  - Implement skip when items not set (e.g. music when musiclisten=0/"unquoted")
'

### FUNCTIONS ###

match_values <- function(p1, p2, matchvar, valuesNA=valuesNA, simscore, split, fuzzy) {
    '
    Matches two persons on each variable passed.

    - p1, p2: values of persons 1 and 2 for the compared variable
    - matchvar: name of variable compared
    - split: indicates if variable value is actually a vector of values (boolean)
    - fuzzy: indicates if fuzzy matching is required (boolean)
    '

    # vectorize
    if (split==1) {
        p1 <- str_split(p1, split=", ")
        p2 <- str_split(p2, split=", ")
    } else {
        p1 <- c(p1)
        p2 <- c(p2)
        }

    # pop NA elements, continue only if vector length>0
    p1 <- setdiff(unlist(p1), valuesNA)
    p2 <- setdiff(unlist(p2), valuesNA)
    if (length(p1)>0 && length(p2)>0) {

        # match
        if (fuzzy==1) {
            # fuzzy matching requires string input!
            #amatch(pres, pres_df$President, maxDist = 10)
            matches <- intersect(p1,p2)
        } else {
            matches <- intersect(p1,p2)
            }

        # score each match
        if (length(matches)>0) {
            simscore <<- simscore + match_score(matchvar, matches)
            matches
        } else {
            NA
            }
    } else {
        NA
        }
    }

match_score <- function(matchvar, matches) {
    '
    Returns the score (weight) associated with a specific match:
    - Weights can be constants or functions
    - If function, pass on value of match and match vector length (values have
      particular scores, vector elements get the same score per variable)

    The call mirrors the weights.js call of the Baliettia paper (as far as I can
    judge).
    '
    # weighter <- weights$get(matchvar)
    # score <- ifelse (
    #     is.function(weighter),
    #     weighter(value=matches[1], common=length(matches)),
    #     weighter
    #     )
    1
    }

matcher <- function(
        df,
        idcol="lfdn", # person id, numeric
        matchvars=matchvars, # vector of all variables used for matching
        matchvars_fuzzy=matchvars_fuzzy, # vector of variable names for fuzzy matching
        valuesNA = c("-99", "-66", ".", ""), # vector of values that should not be matched
        maxmatches=30, # maximum no of matches allowed per person
        simlow=300, # simscore value below which similarity is low
        simhigh=600 # simscore value above which similarity is high
        ) {
    '
    input: df input, single row per person
    output: long format, multiple rows per person for each (non-)match
    '
    # Step 1: Extract row of one person, match to all other rows (persons)
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

                            # check if cells contain something
                            if (length(p1)==1 && length(p2)==1
                                && !is.na(p1) && !is.na(p2)) {

                                # match_values options
                                # split into vector when multiple comma-separated values
                                split <- ifelse (matchvar %in% matchvars_vector, 1, 0)
                                # exact vs. fuzzy matching
                                fuzzy <- ifelse (matchvar %in% matchvars_fuzzy, 1, 0)

                                # match and score
                                matches <- match_values(
                                    p1, p2, matchvar, valuesNA, simscore, split, fuzzy
                                    )

                                }
                            }
                        )

                    # return only if similarity high or low, add score and ids
                    if (simscore<simlow | simscore>simhigh) {
                        row_matched$simscore <- simscore
                        row_matched[idcol] <- row_p1[idcol]
                        row_matched[paste("match", idcol, sep="_")] <- row_p2[idcol]
                        row_matched
                        }

                    })
            }
        )
    }

### INPUT ###

# variables to match on
matchvars <- c(
    "age", "initials", "gender", "eyes", "othereyes", "righthanded", "language",
    "otherlanguage", "totlanguage", "currentstate", "currentzip", "currentrural",
    "grownup_ger", "homestate_ger", "hometown_ger", "homezip_ger", "homestate_foreign",
    "hometown_foreign", "homezip_foreign", "homerural", "samestate", "samezip",
    "secondgen", "secondgencountry", "marital", "parentsdivorced", "children",
    "childrenbracket", "siblings", "siblingsbracket", "military", "militarybranch",
    "education", "college", "gayfriends", "lossfriend", "caregiver", "pets",
    "otherpets", "employment", "ownhouse", "owncar", "studentdebt", "income",
    "incomebracket", "incomeclassPastDirection", "incomeclassFutureDirection",
    "incomeclasschild", "incomeclass", "incomeclassfuture", "workorplay", "energetic",
    "competitive", "perfectionist", "patient", "messy", "carebody", "confrontational",
    "fascination", "fairies", "snooze", "streetfurniture", "giveaway",
    "stoleglass", "foodback", "giftrecycle", "profanelanguage", "readhoroscope",
    "color", "othercolor", "food", "otherfood", "spicyfood", "vegetarian",
    "countriesvisited", "vacation", "socialmedia", "fashion", "smoke", "sportdo",
    "othersportdo", "museums", "dance", "musiclisten", "music", "othermusic",
    "bestmusician", "moviefan", "movie", "othermovie", "bestmovie", "bestactor",
    "sportfan", "sportfollow", "othersportfollow", "bestteam", "watchtv", "tvshows",
    "readbooks", "books", "playvideogames", "videogames", "followwebchannels",
    "webchannels", "docreative", "creative", "otherfun"
    )

# variables which require fuzzy matching
matchvars_fuzzy <- c()

# variables which contain a comma-separated string to be treated as vector
matchvars_vector <- c()

# scoring weight dictionary (approximate translation of JS object)
weights <- dict(list(
  var0 = "1",
  var1 = function(value="", common=0) {
    if (value==1) {
      return("ifyes")
    } else {
      return("ifno")
    }},
  var2 = function(common=0) {
    return(common*20)
  }
))




### RUN ###
#df <- read.csv(paste(wd, "testdata_clean.csv", sep="/"), fileEncoding = "UTF-8")
df_match_all <- matcher(df=df, matchvars=matchvars, matchvars_fuzzy = matchvars_fuzzy)
df_match_all
df_match_all$
