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

matcher <- function(
        df,
        idcol="lfdn", # person id, numeric
        matchvarparams, # dict containing all var-specific parameters
        valuesNA, # vector of values that should not be matched
        keepunmatchedinfo, # vector of variables for which info should be kept for profile header even if unmatched
        maxmatches=30, # maximum no of matches allowed per person
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

### INPUT ###
'
The dictionary contains all info necessary to determine the way to match and
score. Can be extended to contain any other parameters you think are necessary.

split (str/logical): separator if strings are to be treated as vector
fuzzy (logical): TRUE if variable requires fuzzy matching
fuzzymaxdist (int): maxDist parameter passend on to amatch() in match_values()
weight: value or function (similar to Baliettia weight.js)

Dict evaluation is smart: If key is not set, a default is assumed. FALSE for
split and fuzzy, 4 for fuzzymaxdist (please play around); see matcher().

Comments:
- Open answers under "Andere, und zwar:" can be vectors, but in the Baliettia
  paper the match vector length does not matter typically (so there is just one
  score irrespective of the number of matches). Can be a problem when the open
  answers contain a lot of elements, because the total weight these generate
  might be lower than the (max. 3 x weight) for the closed answer categories.
- Please see comments at specific vars for deviations or questions
- ENCODING ISSUES: Some string matches do not return score weights because
  (I assume) the category strings I provided are somehow different to the ones
  in the data (e.g. "Würzig, aber nicht zu viel"). Other Umlauts are matched,
  though, so no idea what the issue is exactly. Needs some digging.
'

matchvarparams <- dict(list(

    # Demography
    age = dict(list(
    	weight = function(value="", common=0) {
            # slightly diff. bc of unused gender info (15 if geder matched too)
    		return(12)
    		}
    	)),
    initials = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
    		}
    	)),
    gender = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
    		}
    	)),
    eyes = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Blau") return(20)
            return(5)
    		}
    	)),
    othereyes = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(15)
    		}
    	)),
    righthanded = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Rechtshänder") return(5)
            if (value == "Linkshänder") return(20)
            return(30)
    		}
    	)),
    language = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
            if (value != "Deutsch") return(18)
            return(2)
    		}
    	)),
    otherlanguage = dict(list(
    	vector = 1,
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 15)
    		}
    	)),
    totlanguage = dict(list(
    	weight = function(value="", common=0) {
            if (value == 1) return(5)
            if (value == 2) return(10)
            if (value < 5) return(18)
            return(30)
    		}
    	)),

    # Location
    currentstate = dict(list(
    	weight = function(value="", common=0) {
    		return(10)
    		}
    	)),
    currentzip = dict(list(
        # exact sensible?
    	weight = function(value="", common=0) {
    		return(40)
    		}
    	)),
    currentrural = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
    		}
    	)),
    grownup_ger = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(15)
            if (value == "Nein") return(5)
    		}
    	)),
    homestate_ger = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
    		}
    	)),
    hometown_ger = dict(list(
    	fuzzy = TRUE,
        fuzzymaxdist = 2,
    	weight = function(value="", common=0) {
    		return(25)
    		}
    	)),
    homezip_ger = dict(list(
    	# exact sensible?
    	weight = function(value="", common=0) {
    		return(45)
    		}
    	)),
    homestate_foreign = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(20)
    		}
    	)),
    hometown_foreign = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
    		}
    	)),
    homezip_foreign = dict(list(
        # exact sensible?
    	weight = function(value="", common=0) {
    		return(50)
    		}
    	)),
    homerural = dict(list(
    	weight = function(value="", common=0) {
    		return(10)
    		}
    	)),
    samestate = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(5)
    		if (value == "Nein") return(0)
    		}
    	)),
    samezip = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(30)
    		if (value == "Nein") return(0)
    		}
    	)),
    secondgen = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Nein") return(0)
            if (value == "Ja, Eltern") return(25)
            if (value == "Ja, Großeltern") return(15)
    		}
    	)),
    secondgencountry = dict(list(
        split = ", ",
        fuzzy = TRUE,
    	weight = function(value="", common=0) {
            return(common * 30)
    		}
    	)),

    # Family
    marital = dict(list(
    	weight = function(value="", common=0) {
    		return(10)
    		}
    	)),
    parentsdivorced = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(25)
    		if (value == "Nein") return(13)
    		if (value == "Sie waren nie verheiratet") return(18)
    		}
    	)),
    children = dict(list(
    	weight = function(value="", common=0) {
            if (value == 0) return(0)
            if (value < 3) return(10)
            if (value < 6) return(20)
            return(30)
    		}
    	)),
    childrenbracket = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Mehr als 4") return(35)
            if (value == "3-4") return(25)
            if (value == "1-2") return(15)
            if (value == "Keine") return(5)
    		}
    	)),
    siblings = dict(list(
    	weight = function(value="", common=0) {
            if (value == 0) return(5)
            if (value < 3) return(10)
            if (value < 6) return(20)
            return(30)
    		}
    	)),
    siblingsbracket = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Mehr als 4") return(35)
            if (value == "3-4") return(25)
            if (value == "1-2") return(15)
            if (value == "Keine") return(5)
    		}
    	)),
    military = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(25)
    		return(5)
    		}
    	)),
    militarybranch = dict(list(
    	weight = function(value="", common=0) {
    		return(35)
    		}
    	)),
    education = dict(list(
        # open answers perhaps problematic
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(10)
    		}
    	)),
    college = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(35)
    		}
    	)),
    gayfriends = dict(list(
    	weight = function(value="", common=0) {
    		return(30)
    		}
    	)),
    lossfriend = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(60)
            return(2)
    		}
    	)),
    caregiver = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(50)
            return(2)
    		}
    	)),
    pets = dict(list(
    	split = ", ",
    	weight = function(value="", common=0) {
    		return(common * 20)
    		}
    	)),
    otherpets = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 30)
    		}
    	)),

    # Finance
    employment = dict(list(
        # combine ?
    	weight = function(value="", common=0) {
    		return(10)
    		}
    	)),
    ownhouse = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
    		}
    	)),
    owncar = dict(list(
    	weight = function(value="", common=0) {
    		return(10)
    		}
    	)),
    studentdebt = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja, und sie sind groß") return(30)
            if (value == "Ja, aber sie sind überschaubar") return(20)
            if (value == "Nein, ich habe sie abbezahlt") return(15)
            if (value == "Nein, ich hatte nie welche") return(8)
    		}
    	)),
    income = dict(list(
        # exact, but value rounded to next multiple of 100
    	weight = function(value="", common=0) {
    		return(30)
    		}
    	)),
    incomebracket = dict(list(
        # 1 cat. more, check weights
    	weight = function(value="", common=0) {
            if (value == "unter 1000 Euro") return(30)
            if (value == "1000 Euro bis unter 2000 Euro") return(25)
            if (value == "2000 Euro bis unter 3000 Euro") return(15)
            if (value == "3000 Euro bis unter 4000 Euro") return(15)
            if (value == "4000 Euro bis unter 5000 Euro") return(25)
            if (value == "5000 Euro und mehr") return(30)
    		}
    	)),
    incomeclassPastDirection = dict(list(
    	weight = function(value="", common=0) {
            if (value != "Gleich") return(15)
            return(5)
    		}
    	)),
    incomeclassFutureDirection = dict(list(
    	weight = function(value="", common=0) {
            if (value != "Gleich") return(15)
            return(5)
    		}
    	)),
    incomeclasschild = dict(list(
    	# other cat., check weights (also for next 2)
    	weight = function(value="", common=0) {
    		if (value == "Unterschicht") return(40)
    		if (value == "untere Mittelschicht") return(25)
    		if (value == "Mittelschicht") return(15)
    		if (value == "obere Mittelschicht") return(30)
    		if (value == "Oberschicht") return(50)
    		}
    	)),
    incomeclass = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Unterschicht") return(40)
            if (value == "untere Mittelschicht") return(25)
            if (value == "Mittelschicht") return(15)
            if (value == "obere Mittelschicht") return(30)
            if (value == "Oberschicht") return(50)
    		}
    	)),
    incomeclassfuture = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Unterschicht") return(40)
            if (value == "untere Mittelschicht") return(25)
            if (value == "Mittelschicht") return(15)
            if (value == "obere Mittelschicht") return(30)
            if (value == "Oberschicht") return(50)
    		}
    	)),

    # Personality
    workorplay = dict(list(
    	weight = function(value="", common=0) {
    		return(20)
    		}
    	)),
    energetic = dict(list(
    	weight = function(value="", common=0) {
    		return(20)
    		}
    	)),
    competitive = dict(list(
    	weight = function(value="", common=0) {
    		return(20)
    		}
    	)),
    perfectionist = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(30)
            return(15)
    		}
    	)),
    patient = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
    		}
    	)),
    messy = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja, sehr chaotisch") return(30)
            if (value == "Ja, ein bisschen") return(15)
            return(20)
    		}
    	)),
    carebody = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
    		}
    	)),
    confrontational = dict(list(
    	weight = function(value="", common=0) {
    		return(20)
    		}
    	)),
    fascination = dict(list(
    	weight = function(value="", common=0) {
    		return(25)
    		}
    	)),
    fairies = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(30)
            return(5)
    		}
    	)),

    # Behavior
    snooze = dict(list(
    	weight = function(value="", common=0) {
    		return(30)
    		}
    	)),
    streetfurniture = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(30)
            return(10)
    		}
    	)),
    giveaway = dict(list(
    	weight = function(value="", common=0) {
    		return(20)
    		}
    	)),
    stoleglass = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(40)
            return(10)
    		}
    	)),
    foodback = dict(list(
    	weight = function(value="", common=0) {
    		return(30)
    		}
    	)),
    giftrecycle = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(30)
            return(20)
    		}
    	)),
    profanelanguage = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Nie") return(25)
            if (value == "Gelegentlich") return(20)
            if (value == "Oft") return(20)
            if (value == "Regelmäßig") return(30)
    		}
    	)),
    readhoroscope = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Täglich") return(30)
            if (value == "Wöchentlich") return(15)
            if (value == "Gelegentlich") return(8)
            if (value == "Nie") return(10)
    		}
    	)),

    # Taste
    color = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
    		}
    	)),
    othercolor = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(25)
    		}
    	)),
    food = dict(list(
    	weight = function(value="", common=0) {
    		return(20)
    		}
    	)),
    otherfood = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
    		}
    	)),
    spicyfood = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ich mag kein scharfes Essen") return(12)
            if (value == "Würzig, aber nicht zu viel") return(10)
            if (value == "Scharf") return(15)
            if (value == "Sehr scharf") return(22)
    		}
    	)),
    vegetarian = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(25)
            if (value == "Nein") return(8)
    		}
    	)),
    countriesvisited = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Keines") return(15)
            if (value == "Zwischen 1 und 2") return(10)
            if (value == "Zwischen 3 und 5") return(15)
            if (value == "Zwischen 6 und 10") return(20)
            if (value == "Mehr als 10") return(28)
    		}
    	)),
    vacation = dict(list(
        split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(25)
    		}
    	)),
    socialmedia = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ich bin sehr aktiv") return(20)
            if (value == "Ich bin etwas aktiv") return(15)
            if (value == "Ich bin kaum aktiv") return(12)
            if (value == "Ich bin nie aktiv") return(25)
    		}
    	)),
    fashion = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Sehr") return(20)
            if (value == "Etwas") return(15)
            if (value == "Nicht viel") return(12)
    		}
    	)),
    smoke = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(20)
            if (value == "Ja, aber nur in Gesellschaft") return(20)
            if (value == "Nein") return(20)
            if (value == "Nein, ich habe aufgehört") return(25)
    		}
    	)),
    sportdo = dict(list(
    	split = ", ",
    	weight = function(value="", common=0) {
    		return(common * 20)
    		}
    	)),
    othersportdo = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
    		}
    	)),
    museums = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja, ich liebe es") return(25)
            if (value == "Ja, manchmal") return(12)
            if (value == "Nein") return(8)
    		}
    	)),
    dance = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja, ich liebe es") return(25)
            if (value == "Ja, manchmal") return(12)
            if (value == "Nein") return(8)
    		}
    	)),
    musiclisten = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Nein") return(15)
            return(5)
    		}
    	)),
    music = dict(list(
        split = ", ",
    	weight = function(value="", common=0) {
    		return(common * 20)
    		}
    	)),
    othermusic = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(20)
    		}
    	)),
    bestmusician = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
    		}
    	)),
    moviefan = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Nein") return(15)
            return(5)
    		}
    	)),
    movie = dict(list(
    	split = ", ",
    	weight = function(value="", common=0) {
    		return(common * 20)
    		}
    	)),
    othermovie = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(20)
    		}
    	)),
    bestmovie = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
    		}
    	)),
    bestactor = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
    		}
    	)),
    sportfan = dict(list(
    	split = ", ",
    	fuzzy = 0,
    	weight = function(value="", common=0) {
            if (value == "Nein") return(15)
            return(5)
    		}
    	)),
    sportfollow = dict(list(
        # Implementation in Baliettia does not use the common vector length!
        # Makes no sense compared to the other items, please consider
    	split = ", ",
    	weight = function(value="", common=0) {
    		return(common * 20)
    		}
    	)),
    othersportfollow = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
    		}
    	)),
    bestteam = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(40)
    		}
    	)),
    watchtv = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Nein") return(15)
            return(5)
    		}
    	)),
    tvshows = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 20)
    		}
    	)),
    readbooks = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(15)
            return(10)
    		}
    	)),
    books = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 25)
    		}
    	)),
    playvideogames = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(20)
            return(5)
    		}
    	)),
    videogames = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 30)
    		}
    	)),
    followwebchannels = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(15)
            return(5)
    		}
    	)),
    webchannels = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 40)
    		}
    	)),
    docreative = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(20)
            return(8)
    		}
    	)),
    creative = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 22)
    		}
    	)),
    otherfun = dict(list(
        # what can be a match here?
    	split = ", ",
    	fuzzy = TRUE,
        fuzzymaxdist = 20,
    	weight = function(value="", common=0) {
    		return(common * 35)
    		}
    	))
))

# variables to match on (in case you need the list ;-))
# matchvars <- c(
#     "age", "initials", "gender", "eyes", "othereyes", "righthanded", "language",
#     "otherlanguage", "totlanguage", "currentstate", "currentzip", "currentrural",
#     "grownup_ger", "homestate_ger", "hometown_ger", "homezip_ger", "homestate_foreign",
#     "hometown_foreign", "homezip_foreign", "homerural", "samestate", "samezip",
#     "secondgen", "secondgencountry", "marital", "parentsdivorced", "children",
#     "childrenbracket", "siblings", "siblingsbracket", "military", "militarybranch",
#     "education", "college", "gayfriends", "lossfriend", "caregiver", "pets",
#     "otherpets", "employment", "ownhouse", "owncar", "studentdebt", "income",
#     "incomebracket", "incomeclassPastDirection", "incomeclassFutureDirection",
#     "incomeclasschild", "incomeclass", "incomeclassfuture", "workorplay", "energetic",
#     "competitive", "perfectionist", "patient", "messy", "carebody", "confrontational",
#     "fascination", "fairies", "snooze", "streetfurniture", "giveaway",
#     "stoleglass", "foodback", "giftrecycle", "profanelanguage", "readhoroscope",
#     "color", "othercolor", "food", "otherfood", "spicyfood", "vegetarian",
#     "countriesvisited", "vacation", "socialmedia", "fashion", "smoke", "sportdo",
#     "othersportdo", "museums", "dance", "musiclisten", "music", "othermusic",
#     "bestmusician", "moviefan", "movie", "othermovie", "bestmovie", "bestactor",
#     "sportfan", "sportfollow", "othersportfollow", "bestteam", "watchtv", "tvshows",
#     "readbooks", "books", "playvideogames", "videogames", "followwebchannels",
#     "webchannels", "docreative", "creative", "otherfun"
#     )


### RUN AND MERGE ###
df_match_all <- matcher(
    df=df,
    matchvarparams=matchvarparams,
    valuesNA=c("-99", "-66", ".", ""),
    keepunmatchedinfo=c("age", "gender", "currentstate")
    )
df_merged <- merge(df, df_match_all, by="lfdn", all=TRUE)
