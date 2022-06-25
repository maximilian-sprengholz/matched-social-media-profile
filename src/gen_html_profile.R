#
# This file generates a standalone HTML social media profile page
# based on individual information passed.
#

### prep
#install.packages("R6")
#install.packages("https://cloud.r-project.org/src/contrib/rlang_1.0.2.tar.gz", repos = NULL, type="source")
#install.packages("tidyverse")
library(R6)
library(tidyverse)

### sample data
df <- read.csv("C:/Users/sprenmax/Seafile/Social-Media-Page/testdata.csv", fileEncoding = "UTF-8")


################################################################################
#
#   MATCHING VAR CLEANING
#
################################################################################
'
We need a specific set and form of variables for the matching, and a subset of
them for the social media profile. Best to clean from the beginning. At the
moment quick and dirty, has to be refined with the full data.

- Assumption: Cleaning will be good for any other operation, so:
    - columns for which values do not need to be transformed will be renamed
    - new columns will be created if information is aggregated/concatenated/...
- All column names correspond to the keys in the JS weighting file, except
  "US" -> "GER"
'


### DEMOGRAPHICS ###

# age
df <- rename(df, age = v_15)

# initials
df <- rename(df, initials = v_686)

# gender / othergender
'
Note for matching: Implement extra score for "othergender" based on eyes value
(! Männlich, Weiblich).
'
df <- rename(df, gender = v_16)
#df$gender <- tolower(df$gender) # case?

# eyes / othereyes
'
- Open questions can be cleaned fairly well automatically.
- Note for matching:
  - Implement extra score for "othereyes" based on eyes value
    (! braun, blau, grün)? It seems that in the Bialetti paper, there was no
    option to choose different colors outside of the open answer?
  - Fuzzy matching: Order should not matter (Grau-Blau = Blau-Grau)
'
df$eyes <- ""
df <- df %>% mutate(eyes = ifelse(v_687 == "quoted", paste(eyes, "Braun"), eyes))
df <- df %>% mutate(eyes = ifelse(v_688 == "quoted", paste(eyes, "Blau"), eyes))
df <- df %>% mutate(eyes = ifelse(v_689 == "quoted", paste(eyes, "Grün"), eyes))
df <- df %>% mutate(eyes = ifelse(v_881 == "quoted", paste(eyes, v_882) , eyes))
df$eyes
df$eyes <- sapply(df$eyes, function(str) {
  matchlist <- str_extract_all(str, regex("(braun|blau|grün|grau|gelb)", ignore_case = T))
  matchlist <- lapply(matchlist, function(match) str_to_title(match, locale = "de-DE"))
  str <- sapply(matchlist, paste, collapse = "-")
  return(str)
})

# righthanded
df <- rename(df, righthanded = v_693)

# language
df$language <- ""
df <- df %>% mutate(language = ifelse(v_883 != "Deutsch", v_884, v_883))

# otherlanguage
'
- Cleaning is based on the "isch" suffix. Will not cover mistakes, foreign terms,
  and languages with a different suffix in German (e.g. Latein, Afrikaans).
  Please extend.
- Returns string separated by commas to be used as a list for the matching.
'
df$otherlanguage <- sapply(df$v_695, function(str) {
  matchlist <- str_extract_all(str, "([a-züäößA-ZÜÄÖ]+isch)")
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
'
Needs labels.
'
df <- rename(df, currentstate = v_23)

# currentzip
df <- rename(df, currentzip = v_18)

# currentrural
df <- rename(df, currentrural = v_696)

# grownup_ger
df <- rename(df, grownup_ger = v_697)

'
Note for matching: I assume that the higher scores for state/town/zip when
foreign are no absolute bonuses to the us/ger scores. So, a match on
homestate_foreign (weight=20) gives 20 points, not 35 (homestate_ger weight=15).
Might be different, but impossible to know without the matching function from
the Bialetti paper.
'

# homestate_ger
df <- rename(df, homestate_ger = v_698)

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
'
Note for matching: Scores are the same for domestic/foreign (10), so no need to
distinguish (assumes that homerural can be matched despite different origins,
might be different in the Bialetti paper).
'
df <- rename(df, homerural = v_702)

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
'
Note for matching: Can only be matched for those who grew up in Germany, but is
"Nein" for foreigners anyhow.
'
df <- rename(df, secondgen = v_703)

# secondgencountry
'
- Note for matching: Implement fuzzy logic (would it work, e.g., for German vs.
  English names of the same origin?)
- Some more cleaning necessary! Avoid fuzzy matches on gibberish.
'
df$secondgencountry <- sapply(df$v_704, function(str) {
  matchlist <- str_extract_all(str, "([a-züäößA-ZÜÄÖ]+)")
  matchlist <- lapply(matchlist, function(match) str_to_title(match, locale = "de-DE"))
  str <- sapply(matchlist, paste, collapse = ", ")
  return(str)
})


### FAMILY ###

# marital
df <- rename(df, marital = v_705)

# parentsdivorced
df <- rename(df, parentdivorced = v_706)

# children
'
Note for matching: Both are matched and there is a (small) bonus for an exact
match beyond the bracket match.
'
df <- rename(df, children = v_144)

# childrenbracket
df$childrenbracket <- ""
df$childrenbracket[df$children==0] <- "Keine"
df$childrenbracket[df$children>=1 & df$children<=2] <- "1-2"
df$childrenbracket[df$children>=3 & df$children<=4] <- "3-4"
df$childrenbracket[df$children>4] <- "Mehr als 4"

# siblings
'
Note for matching: Both are matched and there is a (small) bonus for an exact
match beyond the bracket match.
'
df <- rename(df, siblings = v_707)

# siblingsbracket
df$siblingsbracket <- ""
df$siblingsbracket[df$siblings==0] <- "Keine"
df$siblingsbracket[df$siblings>=1 & df$siblings<=2] <- "1-2"
df$siblingsbracket[df$siblings>=3 & df$siblings<=4] <- "3-4"
df$siblingsbracket[df$siblings>4] <- "Mehr als 4"

# military
df <- rename(df, military = v_722)

# militarybranch
df <- rename(df, militarybranch = v_723)

# education
'
Note for matching:
- What do we do with the open answers? At the moment used as additional
  categories which would allow for fuzzy matching?
- Scores: Bonus for having the same "other" edu?
'
df$education <- ""
df <- df %>% mutate(education = ifelse(v_724 != "anderer Abschluss, und zwar:", v_724, v_725))

# college
df <- rename(df, college = v_726)

# gayfriends
df <- rename(df, gayfriends = v_727)

# lossfriend
df <- rename(df, lossfriend = v_729)

# caregiver
df <- rename(df, caregiver = v_728)

# pets / otherpets
'
- Open answers need cleaning! From what I can judge, this is cumbersome because
  some answers belong to the presented categories, some are so weird that a
  standard regex would not be of much help.
- Note for matching:
  - Similar to the list of languages spoken, I concatenate the answers in a
    string separated by commas. This string can be evaluated as a list in the
    matcher to determine (fuzzy) matches among its elements between persons
  - For the scores only the number of matches is relevant, but I do not know if
    "no pets" also means to have 1 in common. Treated as 0 score.
  - Note for matching: Implement extra score for "otherpets" based on pets value
    (! Katze, Hund, Fisch, Reptilie, Vogel, Nagetier).
'
df$pets <- ""
df <- df %>% mutate(pets = ifelse(
    v_714 == "quoted", ifelse(pets == "", "Katze", paste(pets, "Katze", sep=", ")), pets))
df <- df %>% mutate(pets = ifelse(
    v_715 == "quoted", ifelse(pets == "", "Hund", paste(pets, "Hund", sep=", ")), pets))
df <- df %>% mutate(pets = ifelse(
    v_716 == "quoted", ifelse(pets == "", "Fisch", paste(pets, "Fisch", sep=", ")), pets))
df <- df %>% mutate(pets = ifelse(
    v_717 == "quoted", ifelse(pets == "", "Reptilie", paste(pets, "Reptilie", sep=", ")), pets))
df <- df %>% mutate(pets = ifelse(
    v_718 == "quoted", ifelse(pets == "", "Vogel", paste(pets, "Vogel", sep=", ")), pets))
df <- df %>% mutate(pets = ifelse(
    v_719 == "quoted", ifelse(pets == "", "Nagetier", paste(pets, "Nagetier", sep=", ")), pets))
df <- df %>% mutate(pets = ifelse(
    v_890 == "quoted", ifelse(pets == "", v_891, paste(pets, v_891, sep=", ")), pets))

# employment
'
Note for matching: Very detailed, which levels to use?
'
df <- rename(df, employment = v_108)

# ownhouse
df <- rename(df, ownhouse = v_730)

# owncar
df <- rename(df, owncar = v_731)

# studentdebt
df <- rename(df, studentdebt = v_732)

# income
'
Note for matching: Matching on nearest multiple of 100 seems sensible, not sure
how Bialetti et al. implemented this (the weights.js indicates a TODO).
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
df <- rename(df, incomeclasschild = v_734)
df$incomeclasschild <- factor(df$incomeclasschild,
    levels = c("Unterschicht", "untere Mittelschicht", "Mittelschicht",
    "obere Mittelschicht", "Oberschicht"), ordered=TRUE)

# incomeclass
df <- rename(df, incomeclass = v_733)
df$incomeclass <- factor(df$incomeclass,
    levels = c("Unterschicht", "untere Mittelschicht", "Mittelschicht",
    "obere Mittelschicht", "Oberschicht"), ordered=TRUE)

# incomeclassfuture
df <- rename(df, incomeclassfuture = v_735)
df$incomeclassfuture <- factor(df$incomeclassfuture,
    levels = c("Unterschicht", "untere Mittelschicht", "Mittelschicht",
    "obere Mittelschicht", "Oberschicht"), ordered=TRUE)

# incomeclassPastDirection
df$incomeclassPastDirection <- ""
df <- df %>% mutate(incomeclassPastDirection = ifelse(
    incomeclasschild <= incomeclass,
    ifelse(incomeclasschild == incomeclass, "Gleich", "Abstieg"),
    "Aufstieg")
    )

# incomeclassFutureDirection
df$incomeclassFutureDirection <- ""
df <- df %>% mutate(incomeclassFutureDirection = ifelse(
    incomeclassfuture <= incomeclass,
    ifelse(incomeclassfuture == incomeclass, "Gleich", "Abstieg"),
    "Aufstieg")
    )


### PERSONALITY I + II ###
'
Note for profiles: Define combination rules to keep it short.
'

# workorplay
df <- rename(df, workorplay = v_738)

# energetic
df <- rename(df, energetic = v_739)

# competetive
df <- rename(df, competetive = v_740)

# perfectionist
df <- rename(df, perfectionist = v_741)

# patient
df <- rename(df, patient = v_742)

# messy
df <- rename(df, messy = v_743)

# carebody
df <- rename(df, carebody = v_744)

# confrontational
df <- rename(df, confrontational = v_745)

# fascination
df <- rename(df, fascination = v_746)

# fairies
df <- rename(df, fairies = v_747)


### BEHAVIOR ###
'
Note for profiles: Any way to combine?
'

# snooze
df <- rename(df, snooze = v_748)

# streetfurniture
df <- rename(df, streetfurniture = v_749)

# giveaway
df <- rename(df, giveaway = v_750)

# stoleglass
df <- rename(df, stoleglass = v_751)

# foodback
df <- rename(df, foodback = v_752)

# giftrecycle
df <- rename(df, giftrecycle = v_753)

# profanelanguage
df <- rename(df, profanelanguage = v_754)

# readhoroscope
df <- rename(df, readhoroscope = v_755)


### TASTE ###
'
Note for profiles: Define combination rules to keep it short.
'

# color / othercolor
'
- Some (manual) cleaning necessary for the open answers.
- Note for matching: Implement extra score for "othercolor" based on color value
  (! in colorlist).
'
df$color <- ""
df <- df %>% mutate(color = ifelse(
    v_756 != "Andere, und zwar:", v_756, str_to_title(v_893, locale = "de-DE")))

# food / otherfood
'
- Some (manual) cleaning necessary for the open answers. Perhaps to an extent
  automatable using a regex patterns and "isch" suffix (see otherlanguage),
  but it needs to be more flexible, e.g. for those who eat "alles" and so on...
- Note for matching: Implement extra score for "otherfood" based on food value
  (! in foodlist).
'
df$food <- ""
df <- df %>% mutate(food = ifelse(
    v_758 != "Anderes, und zwar:", v_758, str_to_title(v_894, locale = "de-DE")))

# spicyfood
df <- rename(df, spicyfood = v_760)

# vegetarian
df <- rename(df, vegetarian = v_761)

# countriesvisited
df <- rename(df, countriesvisited = v_762)

# vacation
'
- Needs cleaning (for profile export at least)
- Note for matching: Test fuzzyness necessary to match (even w/o cleaning?)
'
df <- rename(df, vacation = v_763)

### THINGS YOU DO ###



'
ORIGINAL:
---------

[u] unavailable in dataset
[r] redundant/unused
[?] open question

birthdayDate [u]
birthyear [r]
birthmonth [u]
birthday [u]
zodiac [u]
age
agegroup [r]
initials
gender
othergender [?]
eyes
othereyes
righthanded
language
otherlanguage
totlanguage [?]
race [u]
god [u]

currentstate
currenttown [u]
currentzip
currentrural
grownup_us
homestate_us
hometown_us
homezip_us
homerural_us
homestate_foreign
hometown_foreign
homezip_foreign
homerural_foreign
samestate
sametown [u]
samezip
secondgen
secondgencountry

marital
parentsdivorced
children
childrenbracket
siblings
siblingsbracket
military
militarybranch
education
college [?, studienort?]
gayfriends
lossfriend
caregiver
pets
otherpets

employment
ownhouse
owncar
studentdebt
income
incomebracket
incomeclassPastDirection
incomeclassFutureDirection
incomeclasschild
incomeclass
incomeclassfuture

workorplay
energetic
competitive
perfectionist
patient
messy
carebody
confrontational
fascination
fairies

snooze
streetfurniture
giveaway
stoleglass
foodback
giftrecycle
profanelanguage
readhoroscope

color
othercolor
food
otherfood
spicyfood
vegetarian
countriesvisited
vacation

socialmedia
fashion
smoke
sportdo
othersportdo
museums
dance
musiclisten
music
othermusic
bestmusician
moviefan
movie
othermovie
bestmovie
bestactor
sportfan
sportfollow
othersportfollow
bestteam
watchtv
tvshows
readbooks
books
playvideogames
videgames
followwebchannels
webchannels
docreative
creative
otherfun
'


### match


### export social media profiles

# CLASS TEST
# - fix structure
# - require string input (data cleaning before!)
# - provide generic method with conditions to meet field specific requirements
# - print function: what to return if empty/set? first/middle/last?

# fetch data from match (in groups to be fed to class)
#

Profile <- R6Class("Profile", list(
  # profile head
  gender = NULL, # perhaps avatar cue instead
  age = NULL, # bialetti: birthday; age group, zodiac as alternatives
  currentzip = NULL, # not in Bialetti header
  currentstate = NULL
  # demographics
  initials = NULL,
  gender = NULL,
  eyes = NULL,
  righthanded = NULL,
  language = NULL,
  otherlanguage = NULL, # list type!
  spirituality = NULL,
  # location
  currentrural = NULL,
  grownup_ger = NULL,
  homestate_ger = NULL, # Bundesland!
  homestate_foreign = NULL =
  hometown_state = NULL,
  currentrural = NULL,
  currentrural = NULL,
  currentrural = NULL,
  currentrural = NULL,
  currentrural = NULL,
  currentrural = NULL,

))



# check field names against matching function in JS (keep everything consistent);
#

# not matched
'
birthdayDate: 35,
birthyear: 15,
birthmonth: 10,
birthday: function(value, p1, p2) {
    if (p1.birthmonth === p2.birthmonth) return 10;
    return 5;
},
'
