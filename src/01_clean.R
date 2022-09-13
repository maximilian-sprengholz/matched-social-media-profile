################################################################################
#
#   MATCHING VAR CLEANING
#
################################################################################
'
- Dataset has been pre-cleaned so that matching variables are all available. 
  Comments keep track and correspondence to matchparams.R
- This file just implements further cleaning and transforms the variables so
  that they can be fed to the matcher.
'

### FUNCTIONS ###
'
Cleaning functions to clean open answers:
- General
  - general cleaning (remove everything like ["und", "oder", "der", "die", "das", ...])
  - convert all interpunction to comma and split strings
- Question specific:
  -
'

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
# df <- rename(df, othereyes_orig = othereyes)
# df$othereyes <- sapply(df$othereyes_orig, function(str) {
#   matchlist <- str_extract_all(str, regex("(braun|blau|grün|grau|gelb)", ignore_case = T))
#   matchlist <- lapply(matchlist, function(match) str_to_lower(match, locale = "de-DE"))
#   matchlist <- matchlist[order(matchlist)] # alphabetically
#   str <- sapply(matchlist, paste, collapse = "-")
#   return(str)
# })
# df <- df %>% mutate(eyes = ifelse(othereyes %in% c("braun", "grün", "blau"), othereyes, eyes))
# df$othereyes[df$othereyes %in% c("Braun", "Grün", "Blau")] <- ""

# righthanded

# language / otherlanguage
'
- Cleaning is based on the "isch" suffix and some other languages. Will not
  cover further mistakes, foreign terms, and languages not in list.
- Returns string separated by commas to be used as a list for the matching.
'
# df <- rename(df, language_orig = language)
# df$language <- sapply(df$language_orig, function(str) {
#   matchlist <- str_extract_all(
#     str, "([a-züäößA-ZÜÄÖ]+isch|[a-züäößA-ZÜÄÖ]+ich|[a-züäößA-ZÜÄÖ]+ish|[a-züäößA-ZÜÄÖ]+eutsch|[fF]arsi|[lL]atein|[aA]frikaans)"
#     )
#   matchlist <- lapply(matchlist, function(match) str_to_title(match, locale = "de-DE"))
#   str <- sapply(matchlist, paste, collapse = ", ")
#   return(str)
# })

# df <- rename(df, otherlanguage_orig = otherlanguage)
# df$otherlanguage <- sapply(df$otherlanguage_orig, function(str) {
#   matchlist <- str_extract_all(
#     str, "([a-züäößA-ZÜÄÖ]+isch|[a-züäößA-ZÜÄÖ]+ich|[a-züäößA-ZÜÄÖ]+ish|[a-züäößA-ZÜÄÖ]+eutsch|[fF]arsi|[lL]atein|[aA]frikaans)"
#     )
#   matchlist <- lapply(matchlist, function(match) str_to_title(match, locale = "de-DE"))
#   str <- sapply(matchlist, paste, collapse = ", ")
#   return(str)
# })


# totlanguage (first language + other languages)
# df$totlanguage <- sapply(df$otherlanguage, function(str) {
#   int = str_count(str, pattern = ", ") + 1
#   return(int)
# })


### LOCATION ###

# currentstate

# currentzip

# currentrural

# grownup_ger

# homestate_ger

# hometown_ger

# homezip_ger
'
Some values exist despite not predominantly grown up in GER.
'
# df$homezip_ger <- df$v_701
# df <- df %>% mutate(homezip_ger = ifelse(grownup_ger == "Ja" , homezip_ger, ""))

# hometown_foreign

# homerural (combines homerural_ger and homerural_foreign)

# samestate
'
Is currently not, but can be set to "Nein" if grownup_ger=="Nein".
'

# samezip
'
Is currently not, but can be set to "Nein" if grownup_ger=="Nein". 
Some values are set because homezip_ger (incorrectly) set for immigrants.
'

# secondgen

# secondgencountry
'
- Note for matching: Implement fuzzy logic (would it work, e.g., for German vs.
  English names of the same origin?)
- Some more cleaning necessary! Avoid fuzzy matches on gibberish.
'
# df <- rename(df, secondgencountry_orig = secondgencountry)
# df$secondgencountry <- sapply(df$secondgencountry_orig, function(str) {
#   matchlist <- str_extract_all(str, "([a-züäößA-ZÜÄÖ]+)")
#   matchlist <- lapply(matchlist, function(match) str_to_title(match, locale = "de-DE"))
#   str <- sapply(matchlist, paste, collapse = ", ")
#   return(str)
# })


### FAMILY ###

# marital

# parentsdivorced

# children

# childrenbracket

# siblings

# siblingsbracket

# military

# militarybranch

# education

# college

# gayfriends

# lossfriend

# caregiver

# pets

# otherpets

# ownhouse

# owncar

# studentdebt

# income
'
Note for matching: Matching on nearest multiple of 100 seems sensible, not sure
how Balietti et al. implemented this (the weights.js indicates a TODO).
'
df$income[df$income<50 | df$income>1000000] <- NA
df$income <- ceiling(df$income/100)*100

# incomebracket

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
