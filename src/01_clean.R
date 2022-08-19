################################################################################
#
#   MATCHING VAR CLEANING
#
################################################################################
'
We need a specific set and form of variables for the matching. All column names
correspond to the keys in the JS weighting file, except "US" -> "GER", and some
variables are unavailable in our data (see below).

DIFF TO ORIGINAL:
----------------

[u] unavailable in dataset
[r] redundant/unused
[m] merged with other (main) variable

birthdayDate [u]
birthyear [r]
birthmonth [u]
birthday [u]
zodiac [u]
agegroup [r]
othergender [u]
race [u]
god [u]
currenttown [u]
sametown [u]

Update 2022-08-19:
- Dataset has been pre-cleaned so that matching variables are all available
  (I keep the comments just to keep track and correspondence to matchparams.R)
- multi_answer_merger() unused
- This file just implements further cleaning and transforms the variables so
  that they can be fed to the matcher
'

### Functions ###

multi_answer_merger <- function(df, stditemtuples, checkedvalue="quoted") {
    '
    This function creates a list of all the options ticked for a specific
    question, so that multiple variables are merged into one string which can
    be evaluated to a list later.

    df = dataframe
    stditemtuples = list of tuples c("colname", "Label")
    checkedvalue = value when TRUE
    '
    df$x <- ""
    # concatenate standard items
    for (tuple in stditemtuples) {
        df$x[df[tuple[[1]]]==checkedvalue] <- paste(
            df$x[df[tuple[[1]]]==checkedvalue],
            tuple[[2]],
            sep=", "
            )
        }
    # remove ", " from string start
    df$x <- gsub("^,\\s", "", df$x)
    # df$x is returned
}

### DEMOGRAPHICS ###

# age

# initials

# gender / othergender

# eyes

# othereyes
'
- Open questions can be cleaned fairly well automatically
- Note for matching:
  - It seems that in the Balietti paper, there was no option to choose different
    (combinations of) colors outside of the open answer
  - Fuzzy matching: Does order matter?
'
df <- rename(df, othereyes_orig = othereyes)
df$othereyes <- sapply(df$othereyes_orig, function(str) {
  matchlist <- str_extract_all(str, regex("(braun|blau|grün|grau|gelb)", ignore_case = T))
  matchlist <- lapply(matchlist, function(match) str_to_lower(match, locale = "de-DE"))
  matchlist <- matchlist[order(matchlist)] # alphabetically
  str <- sapply(matchlist, paste, collapse = "-")
  return(str)
})
df <- df %>% mutate(eyes = ifelse(othereyes %in% c("braun", "grün", "blau"), othereyes, eyes))
df$othereyes[df$othereyes %in% c("Braun", "Grün", "Blau")] <- ""

# righthanded

# language / otherlanguage
'
- Cleaning is based on the "isch" suffix and some other languages. Will not
  cover further mistakes, foreign terms, and languages not in list.
- Returns string separated by commas to be used as a list for the matching.
'
df <- rename(df, language_orig = language)
df$language <- sapply(df$language_orig, function(str) {
  matchlist <- str_extract_all(
    str, "([a-züäößA-ZÜÄÖ]+isch|[a-züäößA-ZÜÄÖ]+ich|[a-züäößA-ZÜÄÖ]+ish|[a-züäößA-ZÜÄÖ]+eutsch|[fF]arsi|[lL]atein|[aA]frikaans)"
    )
  matchlist <- lapply(matchlist, function(match) str_to_title(match, locale = "de-DE"))
  str <- sapply(matchlist, paste, collapse = ", ")
  return(str)
})

df <- rename(df, otherlanguage_orig = otherlanguage)
df$otherlanguage <- sapply(df$otherlanguage_orig, function(str) {
  matchlist <- str_extract_all(
    str, "([a-züäößA-ZÜÄÖ]+isch|[a-züäößA-ZÜÄÖ]+ich|[a-züäößA-ZÜÄÖ]+ish|[a-züäößA-ZÜÄÖ]+eutsch|[fF]arsi|[lL]atein|[aA]frikaans)"
    )
  matchlist <- lapply(matchlist, function(match) str_to_title(match, locale = "de-DE"))
  str <- sapply(matchlist, paste, collapse = ", ")
  return(str)
})


# totlanguage (first language + other languages)
df$totlanguage <- sapply(df$otherlanguage, function(str) {
  int = str_count(str, pattern = ", ") + 1
  return(int)
})


### LOCATION ###

# currentstate

# currentzip
df <- rename(df, currentzip = v_18)

# currentrural

# grownup_ger
'
REPLACED!
'
df$grownup_ger <- df$v_697

'
Note for matching: I assume that the higher scores for state/town/zip when
foreign are no absolute bonuses to the us/ger scores. So, a match on
homestate_foreign (weight=20) gives 20 points, not 35 (homestate_ger weight=15).
Might be different, but impossible to know without the matching function from
the Balietti paper.
'

# homestate_ger

# hometown_ger
'
Set to empty if not grown up in Germany (no matches on empty strings).
'
df$hometown_ger <- df$v_700
df <- df %>% mutate(hometown_ger = ifelse(grownup_ger == "Ja" , hometown_ger, ""))

# homezip_ger
'
"Do not know" category important?
Set to empty if not grown up in Germany (no matches on empty strings).
'
df$homezip_ger <- df$v_701
df <- df %>% mutate(homezip_ger = ifelse(grownup_ger == "Ja" , homezip_ger, ""))

# homestate_foreign
df <- rename(df, homestate_foreign = v_699)

# hometown_foreign
'
Set to empty if grown up in Germany (no matches on empty strings).
'
df$hometown_foreign <- df$v_700
df <- df %>% mutate(hometown_foreign = ifelse(grownup_ger == "Nein" , hometown_foreign, ""))

# homezip_foreign
'
"Do not know" category important?
Set to empty if grown up in Germany (no matches on empty strings).
'
df$homezip_foreign <- df$v_701
df <- df %>% mutate(homezip_foreign = ifelse(grownup_ger == "Nein" , homezip_foreign, ""))

# homerural (combines homerural_ger and homerural_foreign)

# samestate
'
Note for matching: Can only be matched for those who grew up in Germany, but is
"Nein" for foreigners anyhow.
'
df$samestate <- ""
df <- df %>% mutate(samestate = ifelse(currentstate == homestate_ger, "Ja", "Nein"))
df$samestate[df$currentstate==""] <- ""
df$samestate[df$homestate_ger==""] <- ""

# samezip
'
Note for matching: Can only be matched for those who grew up in Germany, but is
"Nein" for foreigners anyhow.
'
df$samezip <- ""
df <- df %>% mutate(samezip = ifelse(currentzip == homezip_ger, "Ja", "Nein"))
df$samezip[df$currentzip==""] <- ""
df$samezip[df$homezip_ger==""] <- ""

# secondgen

# secondgencountry
'
- Note for matching: Implement fuzzy logic (would it work, e.g., for German vs.
  English names of the same origin?)
- Some more cleaning necessary! Avoid fuzzy matches on gibberish.
'
df <- rename(df, secondgencountry_orig = secondgencountry)
df$secondgencountry <- sapply(df$secondgencountry_orig, function(str) {
  matchlist <- str_extract_all(str, "([a-züäößA-ZÜÄÖ]+)")
  matchlist <- lapply(matchlist, function(match) str_to_title(match, locale = "de-DE"))
  str <- sapply(matchlist, paste, collapse = ", ")
  return(str)
})


### FAMILY ###

# marital

# parentsdivorced

# children

# childrenbracket
df$childrenbracket <- ""
df$childrenbracket[df$v_144==0] <- "Keine"
df$childrenbracket[df$v_144>=1 & df$v_144<=2] <- "1-2"
df$childrenbracket[df$v_144>=3 & df$v_144<=4] <- "3-4"
df$childrenbracket[df$v_144>4] <- "Mehr als 4"

# siblings

# siblingsbracket
df$siblingsbracket <- ""
df$siblingsbracket[df$v_707==0] <- "Keine"
df$siblingsbracket[df$v_707>=1 & df$v_707<=2] <- "1-2"
df$siblingsbracket[df$v_707>=3 & df$v_707<=4] <- "3-4"
df$siblingsbracket[df$v_707>4] <- "Mehr als 4"

# military

# militarybranch

# education

# college

# gayfriends

# lossfriend

# caregiver

# pets

# otherpets

# employment
'
Note for matching: Very detailed, which levels to use?
'
df <- rename(df, employment = v_108)

# ownhouse

# owncar

# studentdebt
df <- rename(df, studentdebt = v_732)

# income
'
Note for matching: Matching on nearest multiple of 100 seems sensible, not sure
how Balietti et al. implemented this (the weights.js indicates a TODO).
'
df <- rename(df, income = v_299)
df$income[df$income<50 | df$income>1000000] <- NA
df$income <- ceiling(df$income/100)*100

# incomebracket
df <- rename(df, incomebracket = v_298)
df$incomebracket[df$income<1000] <- "unter 1000 Euro"
df$incomebracket[df$income>=1000 & df$income<2000] <- "1000 Euro bis unter 2000 Euro"
df$incomebracket[df$income>=2000 & df$income<3000] <- "2000 Euro bis unter 3000 Euro"
df$incomebracket[df$income>=3000 & df$income<4000] <- "3000 Euro bis unter 4000 Euro"
df$incomebracket[df$income>=4000 & df$income<5000] <- "4000 Euro bis unter 5000 Euro"
df$incomebracket[df$income>=5000] <- "5000 Euro und mehr"

# incomeclasschild

# incomeclass

# incomeclassfuture

# incomeclassPastDirection

# incomeclassFutureDirection


### PERSONALITY I + II ###

# workorplay

# energetic

# competitive

# perfectionist

# patient

# messy

# carebody

# confrontational

# fascination

# fairies


### BEHAVIOR ###

# snooze

# streetfurniture

# giveaway

# stoleglass

# foodback
df <- rename(df, foodback = v_752)

# giftrecycle

# profanelanguage

# readhoroscope


### TASTE ###

# color

# othercolor

# food

# otherfood
'
Needs cleaning. Perhaps to an extent automatable using a regex patterns and
"isch" suffix (see otherlanguage), but it needs to be more flexible, e.g. for
those who eat "alles" and so on...
'

# spicyfood

# vegetarian

# countriesvisited

# vacation
'
- Needs cleaning (for profile export at least)
- Note for matching: Test fuzziness necessary to match (even w/o cleaning?)
'

### THINGS YOU DO ###

# socialmedia

# fashion

# smoke

# sportdo

# othersportdo
'
- Needs Cleaning
'

# museums

# dance


### HOBBIES AND FREE TIME ###

# musiclisten

# music

# othermusic
'
- Needs Cleaning
'

# bestmusician
'
- Needs Cleaning
'

# moviefan

# movie

# othermovie
'
- Needs Cleaning
'

# bestmovie
'
- Needs Cleaning
'

# bestactor
'
- Needs Cleaning
'

# sportfan

# sportfollow

# othersportfollow
'
- Needs Cleaning
'

# sportfan

# watchtv

# tvshows
'
- Needs Cleaning
'

# readbooks

# books
'
- Needs Cleaning
'

# playvideogames

# videogames
'
- Needs Cleaning
'

# followwebchannels

# webchannels
'
- Needs Cleaning
'

# docreative

# creative
'
- Needs Cleaning
'

# otherfun
'
- Needs Cleaning
'


### GENERAL STRING CLEANING ###
'
Perhaps also encoding stuff can be done here (or at the beginning of this file).
'
df %>%
  mutate_if(is.character, str_trim)


### SAVE ###
write_feather(df, paste0(wd, "/data/pre_match.feather"))
