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
- Open questions can be cleaned fairly well automatically
- Note for matching:
  - It seems that in the Baliettia paper, there was no option to choose different
    (combinations of) colors outside of the open answer
  - Fuzzy matching: Does order matter?
'
df$eyes <- ""
df <- df %>% mutate(eyes = ifelse(v_687 == "quoted", paste(eyes, "Braun"), eyes))
df <- df %>% mutate(eyes = ifelse(v_688 == "quoted", paste(eyes, "Blau"), eyes))
df <- df %>% mutate(eyes = ifelse(v_689 == "quoted", paste(eyes, "Grün"), eyes))
df <- df %>% mutate(eyes = ifelse(v_881 == "quoted", paste(eyes, v_882) , eyes))
df$eyes <- sapply(df$eyes, function(str) {
  matchlist <- str_extract_all(str, regex("(braun|blau|grün|grau|gelb)", ignore_case = T))
  matchlist <- lapply(matchlist, function(match) str_to_title(match, locale = "de-DE"))
  str <- sapply(matchlist, paste, collapse = "-")
  return(str)
})
df$othereyes[df$eyes %!in% c("Braun", "Grün", "Blau")] <- df$eyes[df$eyes %!in% c("Braun", "Grün", "Blau")]
df$eyes[df$eyes %!in% c("Braun", "Grün", "Blau")] <- ""

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
the Baliettia paper.
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
might be different in the Baliettia paper).
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
df <- rename(df, parentsdivorced = v_706)

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
- In the Baliettia paper no bonus for other education
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
  - Implement extra score for "otherpets" based on pets value
    (! Katze, Hund, Fisch, Reptilie, Vogel, Nagetier).
'
petvars <- list(
    c("v_714", "Katze"), c("v_715", "Hund"), c("v_716", "Fisch"),
    c("v_717", "Reptilie"), c("v_718", "Vogel"), c("v_719", "Nagetier")
    )
df$pets <- multi_answer_merger(df=df, stditemtuples=petvars)

# otherpets
'
Needs cleaning
'
df <- rename(df, otherpets = v_891)

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
how Baliettia et al. implemented this (the weights.js indicates a TODO).
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

# competitive
df <- rename(df, competitive = v_740)

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

# color
df <- rename(df, color= v_756)

# othercolor
'
- Needs cleaning.
'
df <- rename(df, othercolor= v_893)

# food
df <- rename(df, food = v_758)

# otherfood
'
Needs cleaning. Perhaps to an extent automatable using a regex patterns and
"isch" suffix (see otherlanguage), but it needs to be more flexible, e.g. for
those who eat "alles" and so on...
'
df <- rename(df, otherfood = v_894)

# spicyfood
df <- rename(df, spicyfood = v_760)

# vegetarian
df <- rename(df, vegetarian = v_761)

# countriesvisited
df <- rename(df, countriesvisited = v_762)

# vacation
'
- Needs cleaning (for profile export at least)
- Note for matching: Test fuzziness necessary to match (even w/o cleaning?)
'
df <- rename(df, vacation = v_763)


### THINGS YOU DO ###

# socialmedia
df <- rename(df, socialmedia = v_764)

# fashion
df <- rename(df, fashion = v_765)

# smoke
df <- rename(df, smoke = v_766)

# sportdo
sportvars <- list(
    c("v_772", "Fußball"), c("v_773", "Baseball"), c("v_774", "Basketball"),
    c("v_775", "Volleyball"), c("v_776", "Tennis"), c("v_777", "Eishockey"),
    c("v_778", "Cricket"), c("v_779", "American Football"), c("v_780", "Feldhockey"),
    c("v_781", "Radfahren"), c("v_782", "Leichtathletik"), c("v_783", "Tischtennis"),
    c("v_784", "Laufen"), c("v_785", "Kampfsport"), c("v_786", "Klettern"),
    c("v_787", "Skifahren"), c("v_788", "Yoga"), c("v_789", "Schwimmen"),
    c("v_790", "Angeln"), c("v_792", "Keine")
    )
df$sportdo <- multi_answer_merger(df=df, stditemtuples=sportvars)

# othersportdo
'
- Needs Cleaning
'
df <- rename(df, othersportdo = v_897)

# museums
df <- rename(df, museums = v_794)

# dance
df <- rename(df, dance = v_795)


### HOBBIES AND FREE TIME ###

# musiclisten
df <- rename(df, musiclisten = v_796)

# music
musicvars <- list(
    c("v_802", "Blues"), c("v_803", "Klassik"), c("v_804", "Country"),
    c("v_805", "Rock"), c("v_806", "Hip-Hop"), c("v_807", "Latin"),
    c("v_808", "Pop"), c("v_809", "Religiös"), c("v_810", "Funk"),
    c("v_811", "R&B"), c("v_812", "Rap"), c("v_813", "Elektronisch"),
    c("v_814", "Folk"), c("v_815", "Jazz"), c("v_816", "New Age"),
    c("v_817", "Reggae"), c("v_819", "Keine")
    )
df$music <- multi_answer_merger(df=df, stditemtuples=musicvars)

# othermusic
'
- Needs Cleaning
'
df <- rename(df, othermusic = v_900)

# bestmusician
'
- Needs Cleaning
'
df <- rename(df, bestmusician = v_821)

# moviefan
df <- rename(df, moviefan = v_822)

# movie / othermovie
movievars <- list(
    c("v_823", "Action"), c("v_824", "Abenteuer"), c("v_825", "Komödie"),
    c("v_826", "Krimi"), c("v_827", "Drama"), c("v_828", "Fantasy"),
    c("v_829", "Historisch"), c("v_830", "Horror"), c("v_831", "Mystery"),
    c("v_832", "Politisch"), c("v_833", "Romantik"), c("v_834", "SciFi"),
    c("v_835", "Thriller"), c("v_836", "Krieg"), c("v_837", "Western"),
    c("v_838", "Surreal"), c("v_840", "Keine")
    )
df$movie <- multi_answer_merger(df=df, stditemtuples=movievars)

# othermovie
'
- Needs Cleaning
'
df <- rename(df, othermovie = v_903)

# bestmovie
'
- Needs Cleaning
'
df <- rename(df, bestmovie = v_842)

# bestactor
'
- Needs Cleaning
'
df <- rename(df, bestactor = v_843)

# sportfan
df <- rename(df, sportfan = v_844)

# sportfollow /
'
- Note for matching: take care of othermovie bonus
'
sportfollowvars <- list(
    c("v_845", "Golf"), c("v_846", "Fußball"), c("v_847", "Baseball"),
    c("v_848", "Basketball"), c("v_849", "Volleyball"), c("v_850", "Tennis"),
    c("v_851", "Eishockey"), c("v_852", "Cricket"), c("v_853", "American Football"),
    c("v_854", "Feldhockey"), c("v_855", "Nascar"), c("v_856", "Formel 1"),
    c("v_857", "Radfahren"), c("v_858", "Darts"), c("v_859", "Snooker"),
    c("v_860", "Boxen"), c("v_862", "Keine")
    )
df$sportfollow <- multi_answer_merger(df=df, stditemtuples=sportfollowvars)

# othersportfollow
'
- Needs Cleaning
'
df <- rename(df, othersportfollow = v_906)

# sportfan
'
- Needs Cleaning
'
df <- rename(df, bestteam = v_864)

# watchtv
df <- rename(df, watchtv = v_865)

# tvshows
'
- Needs Cleaning
'
df <- rename(df, tvshows = v_866)

# readbooks
df <- rename(df, readbooks = v_867)

# books
'
- Needs Cleaning
'
df <- rename(df, books = v_868)

# playvideogames
df <- rename(df, playvideogames = v_871)

# videogames
'
- Needs Cleaning
'
df <- rename(df, videogames = v_872)

# followwebchannels
df <- rename(df, followwebchannels = v_869)

# webchannels
'
- Needs Cleaning
'
df <- rename(df, webchannels = v_870)

# docreative
df <- rename(df, docreative = v_873)

# creative
'
- Needs Cleaning
'
df <- rename(df, creative = v_874)

# otherfun
'
- Needs Cleaning
'
df <- rename(df, otherfun = v_875)

### GENERAL STRING CLEANING ###
'
Perhaps also encoding stuff can be done here (or at the beginning of this file).
'
df %>%
  mutate_if(is.character, str_trim)
