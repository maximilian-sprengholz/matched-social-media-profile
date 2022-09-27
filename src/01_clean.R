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

### PARAMETERS ###
set.seed(42)

### FUNCTIONS ###

clean_open_answers <- function(dfcol, replace_pattern, to_comma_pattern) {

  # replace
  dfcol <- gsub(replace_pattern, "", dfcol, ignore.case = TRUE)
  # empty brackets
  dfcol <- gsub("[()]", "", dfcol)
  # insert commas as delimiters
  dfcol <- gsub(to_comma_pattern, ",", dfcol, ignore.case = TRUE)
  # remove whitespace
  dfcol <- str_trim(dfcol, "both")
  dfcol <- gsub("\\s\\s+", " ", dfcol)
  # remove superfluous commas
  dfcol <- gsub(",,+|,\\s,|\\s,\\s", ",", dfcol)
  dfcol <- gsub("^,\\s|^,|,\\s$|,$|\\s,", "", dfcol)

  }

### DEMOGRAPHICS ###

# age

# agegroup
'
 We only do fuzzy string matches, but matching on an age range would be nice too.
 To keep the system we have, we just assign a comma-separated age list to the person with
 all the age values
'
df$agegroup <- unlist(lapply(df$age, function(x) {
  str <- paste(c(x - 2.5, seq(x - 2, x + 2, by = 1), x + 2.5), collapse = ",")
  return(str)
}))

# initials

# gender / othergender

# eyes / othereyes
'
- It seems that in the Balietti paper, there was no option to choose different
  (combinations of) colors outside of the open answer
- Fuzzy matching: Does order matter?
'
df <- rename(df, othereyes_orig = othereyes)
df$othereyes <- mapply(
  function(e, oe) {
    # othereyes cleaning
    matchlist <- str_extract_all(oe, regex("(braun|blau|grün|grau|gelb)", ignore_case = TRUE))
    matchlist <- unlist(lapply(matchlist, function(match) str_to_lower(match, locale = "de-DE")))
    # check othereyes is actually a combination of eyes and othereyes
    if (e %in% c("braun", "grün", "blau") & !e %in% c(matchlist) & !all(is.na(matchlist))) {
      matchlist <- c(e, matchlist)
      }
    # order and paste together
    if (!all(is.na(matchlist))) {
      matchlist <- matchlist[order(matchlist)]
      str <- paste(matchlist, collapse = "-")
      return(str)
    } else {
      return(NA)
      }
    },
  df$eyes,
  df$othereyes_orig
  )
df <- df %>% mutate(eyes = ifelse(othereyes %in% c("braun", "grün", "blau"), othereyes, eyes))
df$othereyes[df$othereyes %in% c("braun", "grün", "blau")] <- NA
df$eyes[!is.na(df$othereyes)] <- NA

# righthanded

# language / otherlanguage
'
- Cleaning is based on the "isch" suffix and some other languages. Will not
  cover further mistakes, foreign terms, and languages not in list.
- Returns string separated by commas to be used as a list for the matching.
'
df <- rename(df, language_orig = language)
# general cleaning
replace_pattern <- c(
  "vn", "Xxx", "möchte nicht sagen", "nicht relevant für diese umfrage!", "K.A",
  "dumme umfragen beantworten"
  )
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|[&]|[+]|\\bbzw.\\b|\\bund\\b|\\boder\\b|[:]|[;]|[.]|[-]"
df$language <- clean_open_answers(df$language_orig, replace_pattern, to_comma_pattern)
# extract strings
# extract_pattern <- c(
#   "\\S+isch", "\\S+ich", "\\S+ish", "\\S+eutsch", "farsi", "latein",
#   "hindi", "afrikaans", "mandingo", "marathi", "urdu", "pashtu", "yoruba", "twi", "tamil",
#   "castellano"
#   )
# extract_pattern <- paste(paste0("\\b", extract_pattern, "([a-z]+)?\\b"), collapse = "|")
# df$language <- unlist(lapply(df$language, function(str) {
#   matchlist <- str_extract_all(str, regex(extract_pattern, ignore_case = TRUE))
#   str <- sapply(matchlist, paste, collapse = ", ")
#   return(str)
# }))

df <- rename(df, otherlanguage_orig = otherlanguage)
# general cleaning
replace_pattern <- c(
  "vn", "Xxx", "möchte nicht sagen", "nicht relevant für diese umfrage!", "K.A",
  "dumme umfragen beantworten"
  )
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|[&]|[+]|\\bbzw.\\b|\\bund\\b|\\boder\\b|[:]|[;]|[.]|[-]"
df$otherlanguage <- clean_open_answers(df$otherlanguage_orig, replace_pattern, to_comma_pattern)
# extract strings
extract_pattern <- c(
  "\\S+isch", "\\S+ich", "\\S+ish", "\\S+eutsch", "farsi", "latein",
  "hindi", "afrikaans", "mandingo", "marathi", "urdu", "pashtu", "yoruba", "twi", "tamil",
  "castellano"
  )
extract_pattern <- paste(paste0("\\b", extract_pattern, "([a-z]+)?\\b"), collapse = "|")
df$otherlanguage <- unlist(lapply(df$otherlanguage, function(str) {
  matchlist <- str_extract_all(str, regex(extract_pattern, ignore_case = TRUE))
  str <- sapply(matchlist, paste, collapse = ", ")
  return(str)
}))

# totlanguage (first language + other languages)
df$totlanguage <- unlist(lapply(df$otherlanguage, function(str) {
  int <- str_count(str, pattern = ", ") + 1
  return(int)
}))


### LOCATION ###

# currentstate

# currentzip
'
First 3 digits used.
'
df <- rename(df, currentzip_orig = currentzip)
df <- df %>% mutate(currentzip = strtrim(
    as.character(ifelse(
      currentzip_orig < 10000,
      paste0(0, currentzip_orig),
      currentzip_orig
      )),
    3))

# currentrural

# grownup_ger

# homestate_ger

# hometown_ger
df <- rename(df, hometown_ger_orig = hometown_ger)
# general cleaning
replace_pattern <- "K[.\\s]+A[.]|K[.]A[.]|Keine.*|\\b[0-9]+\\b|\\b-\\b"
to_comma_pattern <- "/|\\bund\\b|[&]|[+]|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$hometown_ger <- clean_open_answers(df$hometown_ger_orig, replace_pattern, to_comma_pattern)

# homezip_ger
'
First 3 digits used.
'
df <- rename(df, homezip_ger_orig = homezip_ger)
df <- df %>% mutate(homezip_ger = strtrim(
    as.character(ifelse(
      homezip_ger_orig < 10000,
      paste0(0, homezip_ger_orig),
      homezip_ger_orig
      )),
    3))

# hometown_foreign
df <- rename(df, hometown_foreign_orig = hometown_foreign)
# general cleaning
replace_pattern <- c(
  "möchte nicht sagen", "keine angabe", "geht sie nichts an", "geht keinen etwas an", "K.A.", "-"
  )
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[.]|[(]|[)]|\\bund\\b|[&]|[+]|\\boder\\b|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$hometown_foreign <- clean_open_answers(df$hometown_foreign_orig, replace_pattern, to_comma_pattern)

# homerural (combines homerural_ger and homerural_foreign)

# samestate

# samezip

# secondgen

# secondgencountry
df$secondgencountry_orig <- df$secondgencountry
# general cleaning
replace_pattern <- c(
  "kam", "mein", "aus", "einer", "mein", "ist", "er", "sie", "nur", "vater", "mutter",
  "opa", "oma", "großvater", "großmutter", "beide", "eltern\\S+", "haben", "zeitweise",
  "damals", "wohl", "in", "im", "von", "stammt", "noch", "dem", "der",
  "die", "heut\\S+", "kolonie", "ehe\\S+", "hierher", "eingewandert", "früher", "meine"
  )
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[.]|[(]|[)]|\\bund\\b|[&]|[+]|\\boder\\b|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$secondgencountry <- clean_open_answers(df$secondgencountry, replace_pattern, to_comma_pattern)
# make spaces commas
'
 This works because we only need one match to get a score, the numer of matches is irrelevant.
 So, even though "Puerto Rico" would be split, it does not matter if we match on both parts
 separately.
'
df$secondgencountry <- gsub("([a-züäößA-ZÜÄÖ])(\\s)([a-züäößA-ZÜÄÖ])", "\\1, \\3", df$secondgencountry)


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
df <- rename(df, education_orig = education)
# remove print context
df$education <- gsub("^Mein höchster beruflicher Schulabschluss ist ", "", df$education_orig)
df$education <- gsub("^ei[ne]+ |[.]$", "", df$education)

# college
df <- rename(df, college_orig = college)
# remove print context
df$college <- gsub("^Studiert habe ich hier: |[.]$", "", df$college_orig)

# gayfriends

# lossfriend

# caregiver

# pets

# otherpets
df <- rename(df, otherpets_orig = otherpets)
# general cleaning
replace_pattern <- c(
  "ihr", "habtn", "[0-9]+"
  )
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[.]|[(]|[)]|\\bund\\b|[&]|[+]|\\boder\\b|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$otherpets <- clean_open_answers(df$otherpets_orig, replace_pattern, to_comma_pattern)

# ownhouse

# owncar

# studentdebt

# income

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
df$color <- gsub("andere, und zwar:", "", df$color)

# othercolor
df <- rename(df, othercolor_orig = othercolor)
replace_pattern <- c(
  "gedeckte", "allgemein", "pudrige", "warme", "farb\\S+", "liebling\\S+", "mal", "dann", "töne",
  "mehrere", "mit", "gerne", "alle", "viele", "zum", "tragen", "wohnen", "diverse",
  "eindeutig", "zarte", "keine", "mischtöne", "manchmal", "ich", "habe", "eine", "phase"
  )
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[.]|[(]|[)]|\\bund\\b|[&]|[+]|\\boder\\b|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$othercolor <- clean_open_answers(df$othercolor_orig, replace_pattern, to_comma_pattern)

# food
df$food <- gsub("Anderes, und zwar:", "", df$food)

# otherfood
extract_pattern <- c(
  "\\S+isch", "\\S+ich", "\\S+ish", "all\\S+", "medi\\S+", "Tha\\S+", "Veg\\S+"
  )
extract_pattern <- paste(paste0("\\b", extract_pattern, "\\b"), collapse = "|")
df <- rename(df, otherfood_orig = otherfood)
df$otherfood <- unlist(lapply(df$otherfood_orig, function(str) {
  matchlist <- str_extract_all(str, regex(extract_pattern, ignore_case = TRUE))
  str <- sapply(matchlist, paste, collapse = ", ")
  return(str)
}))

# spicyfood

# vegetarian

# countriesvisited

# vacation
df <- rename(df, vacation_orig = vacation)
# general cleaning
replace_pattern <- c(
  "derzeit", "keines", "der", "die", "das", "ganz", "klar", "ich", "würde", "gerne", "nach",
  "reis\\S+", "kommt", "drauf", "darauf", "an", "wie", "viele", "wochen", "ähnlich", "auf", "durch",
  "panam", "lieber", "wei[sß]+", "nicht", "wo", "kann", "meinen", "hund", "mitnehmen", "doch",
  "wäre", "nett"
  )
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[.]|[(]|[)]|\\bund\\b|[&]|[+]|\\boder\\b|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$vacation <- clean_open_answers(df$vacation_orig, replace_pattern, to_comma_pattern)
# make spaces commas 
'
 This works because we only need one match to get a score, the numer of matches is irrelevant.
 So, even though "Puerto Rico" would be split, it does not matter if we match on both parts
 separately.
'
df$vacation <- gsub("([a-züäößA-ZÜÄÖ])(\\s)([a-züäößA-ZÜÄÖ])", "\\1, \\3", df$vacation)


### THINGS YOU DO ###

# socialmedia

# fashion

# smoke

# sportdo

# othersportdo
'
 In this case it seems the cleaner option is to NOT split by space after cleaning despite
 the fact that number of matches do not matter (bc. of many splits like "Rollerskates fahren")
'
df <- rename(df, othersportdo_orig = othersportdo)
# general cleaning
replace_pattern <- c(
  "etwas", "im", "in", "täglich", "übungen", "medizi\\S+", "für", "der", "die", "das",
  "genannten", "zu", "alt", "gelegent\\S+", "funktio\\S+", "wenn", "[0-9]+", "km",
  "manchmal", "berittenes"
  )
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[.]|[(]|[)]|\\bund\\b|[&]|[+]|\\boder\\b|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$othersportdo <- clean_open_answers(df$othersportdo_orig, replace_pattern, to_comma_pattern)

# museums

# dance


### HOBBIES AND FREE TIME ###

# musiclisten

# music

# othermusic
'
 Evaluated as vector, but single match is scored. 
 
 To not inflate the score, we assign an empty string to persons stating to like "everything"  
 as the common match vector length of music is already counted. Does also fix problems with
 those liking "everything but..."
'
df <- rename(df, othermusic_orig = othermusic)
# clean year/decade
df$othermusic <- gsub("(19)([0-9]0)", "\\2", df$othermusic_orig)
df$othermusic <- gsub("(\\b[1-9]0er\\b)", "\\1,", df$othermusic)
df$othermusic <- gsub("(\\b[1-9]0\\b)", ",\\1,", df$othermusic)
# general cleaning
replace_pattern <- c(
  "jahre", "deutsch[er]+", "bis", "die", "der", "das", "eigentlich", "gemischt", "ab",
  "Robert", "Schumann", "musik", "aktuelles", "hits", "us[w.]+", "von", "genr\\S+", "aus",
  "den", "lieder"
  )
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[(]|[)]|[+]|\\boder\\b|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$othermusic <- clean_open_answers(df$othermusic_orig, replace_pattern, to_comma_pattern)
# set to empty when "everything"
df$othermusic <- gsub("\\S+alle\\S+", "", df$othermusic, ignore.case = TRUE)

# bestmusician
'
 Evaluated as vector, but single match is scored. 
'
df <- rename(df, bestmusician_orig = bestmusician)
# general cleaning
replace_pattern <- c(
  "----", "_", "momentan", "habe ich nicht"
  )
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[(]|[)]|[+]|\\boder\\b|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$bestmusician <- clean_open_answers(df$bestmusician_orig, replace_pattern, to_comma_pattern)

# moviefan

# movie

# othermovie
'
 Evaluated as vector, but single match is scored. Just a few empty strings in dataset.
'

# bestmovie
'
 Evaluated as vector, but single match is scored.  
'
df <- rename(df, bestmovie_orig = bestmovie)
# delete everything in parentheses and after dashes
df$bestmovie <- gsub("\\s[-–]\\s.*|^[-–]\\s|[(].*[)]", "", df$bestmovie_orig, ignore.case = TRUE)
# general cleaning
replace_pattern <- c(
  "komm grad nicht drauf", "weiß nicht", "filme", "alle teile"
  )
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[(]|[)]|[+]|\\boder\\b|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$bestmovie <- clean_open_answers(df$bestmovie, replace_pattern, to_comma_pattern)

# bestactor
'
 Evaluated as vector, but single match is scored. 
'
df <- rename(df, bestactor_orig = bestactor)
# delete everything in parentheses and after dashes
df$bestactor <- gsub("\\s[-–]\\s.*|^[-–]\\s|[(].*[)]", "", df$bestactor_orig, ignore.case = TRUE)
# general cleaning
replace_pattern <- c(
  "komm grad nicht drauf", "weiß nicht", "filme", "alle teile"
  )
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[(]|[)]|\\bund\\b|[&]|[+]|\\boder\\b|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$bestactor <- clean_open_answers(df$bestactor, replace_pattern, to_comma_pattern)

# sportfan

# sportfollow

# othersportfollow
'
 Evaluated as vector, but single match is scored. Empty in dataset.
'

# bestteam
'
 Evaluated as vector, but single match is scored. 
'
df <- rename(df, bestteam_orig = bestteam)
# delete everything in parentheses and after dashes
df$bestteam <- gsub("\\s[-–]\\s.*|^[-–]\\s|[(].*[)]", "", df$bestteam_orig, ignore.case = TRUE)
# general cleaning
replace_pattern <- c(
  "da gibt es mehrere wie", "Es gibt keine Mannschaften beim Reiten"
  )
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[(]|[)]|\\bund\\b|[&]|[+]|\\boder\\b|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$bestteam <- clean_open_answers(df$bestteam, replace_pattern, to_comma_pattern)

# watchtv

# tvshows
'
 Common vector length is scored.
'
df <- rename(df, tvshows_orig = tvshows)
# remove print context
df$tvshows <- gsub("^Im Moment gefällt mir im Fernsehen am besten '|'[.]$", "", df$tvshows_orig)
# delete everything in parentheses and after dashes
df$tvshows <- gsub("\\s[-–]\\s.*|^[-–]\\s|[(].*[)]", "", df$tvshows, ignore.case = TRUE)
# general cleaning
replace_pattern <- c("----")
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[(]|[)]|[+]|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$tvshows <- clean_open_answers(df$tvshows, replace_pattern, to_comma_pattern)

# readbooks

# books
'
 Common vector length is scored.
'
df <- rename(df, books_orig = books)
# remove print context
df$books <- gsub("^Besonders gern lese ich im Moment '|'[.]$", "", df$books_orig)
# delete everything in parentheses and after dashes
df$books <- gsub("\\s[-–]\\s.*|^[-–]\\s|[(].*[)]", "", df$books, ignore.case = TRUE)
# general cleaning
replace_pattern <- c("----")
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[(]|[)]|[+]|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$books <- clean_open_answers(df$books, replace_pattern, to_comma_pattern)

# playvideogames

# videogames
'
 Common vector length is scored.
'
df <- rename(df, videogames_orig = videogames)
# remove print context
df$videogames <- gsub("^Mein liebstes Videospiel ist momentan '|'[.]$", "", df$videogames_orig)
# delete all version numbers
df$videogames <- gsub("\\b[0-9]+\\b|\\b[xvi]+\\b", "", df$videogames, ignore.case = TRUE)
# delete everything in parentheses and after dashes
df$videogames <- gsub("\\s[-–]\\s.*|^[-–]\\s|[(].*[)]", "", df$videogames, ignore.case = TRUE)
# general cleaning
replace_pattern <- c("----")
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[(]|[)]|[+]|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$videogames <- clean_open_answers(df$videogames, replace_pattern, to_comma_pattern)

# followwebchannels

# webchannels
'
 Common vector length is scored.
'
df <- rename(df, webchannels_orig = webchannels)
# remove print context
df$webchannels <- gsub(
  "^Bei den Webchannels interessiere ich mich insbesondere für '|'[.]$", "",
  df$webchannels_orig
  )
# delete everything in parentheses and after dashes
df$webchannels <- gsub("\\s[-–]\\s.*|^[-–]\\s|[(].*[)]", "", df$webchannels, ignore.case = TRUE)
# general cleaning
replace_pattern <- c("----")
replace_pattern <- paste(paste0("\\b", replace_pattern, "\\b"), collapse = "|")
to_comma_pattern <- "/|\\s-\\s|[(]|[)]|[+]|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$webchannels <- clean_open_answers(df$webchannels, replace_pattern, to_comma_pattern)

# docreative

# creative
'
 Common vector length is scored.
'
df <- rename(df, creative_orig = creative)
# remove print context
df$creative <- gsub(
  "^Viel Freude macht mir auch '|'[.]$", "",
  df$creative_orig
  )
# delete everything in parentheses and after dashes
df$creative <- gsub("\\s[-–]\\s.*|^[-–]\\s|[(].*[)]", "", df$creative, ignore.case = TRUE)
# general cleaning
replace_pattern <- "ich|spielen|machen"
to_comma_pattern <- "/|\\s-\\s|[.]|[(]|[)]|\\bund\\b|[&]|[+]|\\boder\\b|\\bbzw.\\b|[?]|[!]|[:]|[;]"
df$creative <- clean_open_answers(df$creative, replace_pattern, to_comma_pattern)


### GENERAL STRING CLEANING ###
df %>%
  mutate(across(where(~is.character(.)), ~gsub("\\s+", " ", .))) %>%
  mutate(across(where(~is.character(.)), ~str_trim(.)))

### HELPER VARS ###
df <- df %>% mutate(matchable = ifelse(useable == "1 Yes", 1, 0))

### SAVE ###
write_feather(df, paste0(wd, "/data/pre_match.feather"))