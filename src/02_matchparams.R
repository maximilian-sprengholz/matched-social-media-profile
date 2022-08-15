'
The dictionary contains all info necessary to:

(1) Group items and print group headlines
------------------------------------------

hl (list): list the label and icon (fa ref) displayed
           (id of group div = group key, e.g. demography)
items (dict): list of all the variables belonging to that group;
              all items also a dict


(2) Determine the way to match and score
----------------------------------------

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
- ENCODING ISSUES: I had some Umlaut issues when editing in Atom and running in
  R Studio (e.g. "WÃ¼rzig, aber nicht zu viel"). Some Umlauts could be matched,
  though, so no idea what the issue is exactly. At the moment runs fine after
  inputting everzthing i R Studio...

The dict can be extended to contain any other parameters you think are necessary.
'

matchparams <- dict(list(

    # Demography
    demography = dict(list(
        hl = list(label="Allgemein", icon="fa-user"),
        items = dict(list(
            age = dict(list(
                weight = function(value=NA, common=0) {
                    # slightly diff. bc of unused gender info (15 if geder matched too)
                    return(12)
                    }
                )),
            initials = dict(list(
                weight = function(value=NA, common=0) {
                    return(15)
                    }
                )),
            gender = dict(list(
                weight = function(value=NA, common=0) {
                    return(15)
                    }
                )),
            eyes = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Blau") return(20)
                    return(5)
                    }
                )),
            othereyes = dict(list(
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(15)
                    }
                )),
            righthanded = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Rechtshänder") return(5)
                    if (value == "Linkshänder") return(20)
                    return(30)
                    }
                )),
            language = dict(list(
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    if (value != "Deutsch") return(18)
                    return(2)
                    }
                )),
            otherlanguage = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(common * 15)
                    }
                )),
            totlanguage = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == 1) return(5)
                    if (value == 2) return(10)
                    if (value < 5) return(18)
                    return(30)
                    }
                ))
            ))
        )),

    # Location
    location = dict(list(
        hl = list(label="Ort und Herkunft", icon="fa-house-chimney-window"),
        items = dict(list(
            currentstate = dict(list(
                weight = function(value=NA, common=0) {
                    return(10)
                    }
                )),
            currentzip = dict(list(
                # exact sensible?
                weight = function(value=NA, common=0) {
                    return(40)
                    }
                )),
            currentrural = dict(list(
                weight = function(value=NA, common=0) {
                    return(15)
                    }
                )),
            grownup_ger = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(15)
                    if (value == "Nein") return(5)
                    }
                )),
            homestate_ger = dict(list(
                weight = function(value=NA, common=0) {
                    return(15)
                    }
                )),
            hometown_ger = dict(list(
                fuzzy = TRUE,
                fuzzymaxdist = 2,
                weight = function(value=NA, common=0) {
                    return(25)
                    }
                )),
            homezip_ger = dict(list(
                # exact sensible?
                weight = function(value=NA, common=0) {
                    return(45)
                    }
                )),
            homestate_foreign = dict(list(
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(20)
                    }
                )),
            hometown_foreign = dict(list(
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(30)
                    }
                )),
            homezip_foreign = dict(list(
                # exact sensible?
                weight = function(value=NA, common=0) {
                    return(50)
                    }
                )),
            homerural = dict(list(
                weight = function(value=NA, common=0) {
                    return(10)
                    }
                )),
            samestate = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(5)
                    if (value == "Nein") return(0)
                    }
                )),
            samezip = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(30)
                    if (value == "Nein") return(0)
                    }
                )),
            secondgen = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Nein") return(0)
                    if (value == "Ja, Eltern") return(25)
                    if (value == "Ja, Großeltern") return(15)
                    }
                )),
            secondgencountry = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(common * 30)
                    }
                ))
            ))
        )),

    # Family
    family = dict(list(
        hl = list(label="Familie", icon="fa-users"),
        items = dict(list(
            marital = dict(list(
                weight = function(value=NA, common=0) {
                    return(10)
                    }
                )),
            parentsdivorced = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(25)
                    if (value == "Nein") return(13)
                    if (value == "Sie waren nie verheiratet") return(18)
                    }
                )),
            children = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == 0) return(0)
                    if (value < 3) return(10)
                    if (value < 6) return(20)
                    return(30)
                    }
                )),
            childrenbracket = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Mehr als 4") return(35)
                    if (value == "3-4") return(25)
                    if (value == "1-2") return(15)
                    if (value == "Keine") return(5)
                    }
                )),
            siblings = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == 0) return(5)
                    if (value < 3) return(10)
                    if (value < 6) return(20)
                    return(30)
                    }
                )),
            siblingsbracket = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Mehr als 4") return(35)
                    if (value == "3-4") return(25)
                    if (value == "1-2") return(15)
                    if (value == "Keine") return(5)
                    }
                )),
            military = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(25)
                    return(5)
                    }
                )),
            militarybranch = dict(list(
                weight = function(value=NA, common=0) {
                    return(35)
                    }
                )),
            education = dict(list(
                # open answers perhaps problematic
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(10)
                    }
                )),
            college = dict(list(
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(35)
                    }
                )),
            gayfriends = dict(list(
                weight = function(value=NA, common=0) {
                    return(30)
                    }
                )),
            lossfriend = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(60)
                    return(2)
                    }
                )),
            caregiver = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(50)
                    return(2)
                    }
                )),
            pets = dict(list(
                split = ", ",
                weight = function(value=NA, common=0) {
                    return(common * 20)
                    }
                )),
            otherpets = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(common * 30)
                    }
                ))
            ))
        )),

    # Finance
    finance = dict(list(
        hl = list(label="Finanzen", icon="fa-money-check-dollar"),
        items = dict(list(
            employment = dict(list(
                # combine ?
                weight = function(value=NA, common=0) {
                    return(10)
                    }
                )),
            ownhouse = dict(list(
                weight = function(value=NA, common=0) {
                    return(15)
                    }
                )),
            owncar = dict(list(
                weight = function(value=NA, common=0) {
                    return(10)
                    }
                )),
            studentdebt = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja, und sie sind groß") return(30)
                    if (value == "Ja, aber sie sind überschaubar") return(20)
                    if (value == "Nein, ich habe sie abbezahlt") return(15)
                    if (value == "Nein, ich hatte nie welche") return(8)
                    }
                )),
            income = dict(list(
                # exact, but value rounded to next multiple of 100
                weight = function(value=NA, common=0) {
                    return(30)
                    }
                )),
            incomebracket = dict(list(
                # 1 cat. more, check weights
                weight = function(value=NA, common=0) {
                    if (value == "unter 1000 Euro") return(30)
                    if (value == "1000 Euro bis unter 2000 Euro") return(25)
                    if (value == "2000 Euro bis unter 3000 Euro") return(15)
                    if (value == "3000 Euro bis unter 4000 Euro") return(15)
                    if (value == "4000 Euro bis unter 5000 Euro") return(25)
                    if (value == "5000 Euro und mehr") return(30)
                    }
                )),
            incomeclassPastDirection = dict(list(
                weight = function(value=NA, common=0) {
                    if (value != "Gleich") return(15)
                    return(5)
                    }
                )),
            incomeclassFutureDirection = dict(list(
                weight = function(value=NA, common=0) {
                    if (value != "Gleich") return(15)
                    return(5)
                    }
                )),
            incomeclasschild = dict(list(
                # other cat., check weights (also for next 2)
                weight = function(value=NA, common=0) {
                    if (value == "Unterschicht") return(40)
                    if (value == "untere Mittelschicht") return(25)
                    if (value == "Mittelschicht") return(15)
                    if (value == "obere Mittelschicht") return(30)
                    if (value == "Oberschicht") return(50)
                    }
                )),
            incomeclass = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Unterschicht") return(40)
                    if (value == "untere Mittelschicht") return(25)
                    if (value == "Mittelschicht") return(15)
                    if (value == "obere Mittelschicht") return(30)
                    if (value == "Oberschicht") return(50)
                    }
                )),
            incomeclassfuture = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Unterschicht") return(40)
                    if (value == "untere Mittelschicht") return(25)
                    if (value == "Mittelschicht") return(15)
                    if (value == "obere Mittelschicht") return(30)
                    if (value == "Oberschicht") return(50)
                    }
                ))
            ))
        )),

    # Personality
    personality = dict(list(
        hl = list(label="Persönlichkeit", icon="fa-face-smile"),
        items = dict(list(
            workorplay = dict(list(
                weight = function(value=NA, common=0) {
                    return(20)
                    }
                )),
            energetic = dict(list(
                weight = function(value=NA, common=0) {
                    return(20)
                    }
                )),
            competitive = dict(list(
                weight = function(value=NA, common=0) {
                    return(20)
                    }
                )),
            perfectionist = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(30)
                    return(15)
                    }
                )),
            patient = dict(list(
                weight = function(value=NA, common=0) {
                    return(15)
                    }
                )),
            messy = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja, sehr chaotisch") return(30)
                    if (value == "Ja, ein bisschen") return(15)
                    return(20)
                    }
                )),
            carebody = dict(list(
                weight = function(value=NA, common=0) {
                    return(15)
                    }
                )),
            confrontational = dict(list(
                weight = function(value=NA, common=0) {
                    return(20)
                    }
                )),
            fascination = dict(list(
                weight = function(value=NA, common=0) {
                    return(25)
                    }
                )),
            fairies = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(30)
                    return(5)
                    }
                ))
            ))
        )),

    # Behavior
    behavior = dict(list(
        hl = list(label="Verhalten", icon="fa-hands"),
        items = dict(list(
            snooze = dict(list(
                weight = function(value=NA, common=0) {
                    return(30)
                    }
                )),
            streetfurniture = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(30)
                    return(10)
                    }
                )),
            giveaway = dict(list(
                weight = function(value=NA, common=0) {
                    return(20)
                    }
                )),
            stoleglass = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(40)
                    return(10)
                    }
                )),
            foodback = dict(list(
                weight = function(value=NA, common=0) {
                    return(30)
                    }
                )),
            giftrecycle = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(30)
                    return(20)
                    }
                )),
            profanelanguage = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Nie") return(25)
                    if (value == "Gelegentlich") return(20)
                    if (value == "Oft") return(20)
                    if (value == "Regelmäßig") return(30)
                    }
                )),
            readhoroscope = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Täglich") return(30)
                    if (value == "Wöchentlich") return(15)
                    if (value == "Gelegentlich") return(8)
                    if (value == "Nie") return(10)
                    }
                ))
            ))
        )),

    # Taste
    taste = dict(list(
        hl = list(label="Geschmack", icon="fa-star"),
        items = dict(list(
            color = dict(list(
                weight = function(value=NA, common=0) {
                    return(15)
                    }
                )),
            othercolor = dict(list(
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(25)
                    }
                )),
            food = dict(list(
                weight = function(value=NA, common=0) {
                    return(20)
                    }
                )),
            otherfood = dict(list(
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(30)
                    }
                )),
            spicyfood = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ich mag kein scharfes Essen") return(12)
                    if (value == "Würzig, aber nicht zu viel") return(10)
                    if (value == "Scharf") return(15)
                    if (value == "Sehr scharf") return(22)
                    }
                )),
            vegetarian = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(25)
                    if (value == "Nein") return(8)
                    }
                )),
            countriesvisited = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Keines") return(15)
                    if (value == "Zwischen 1 und 2") return(10)
                    if (value == "Zwischen 3 und 5") return(15)
                    if (value == "Zwischen 6 und 10") return(20)
                    if (value == "Mehr als 10") return(28)
                    }
                ))
            ))
        )),

    # Things you do
    thingsyoudo = dict(list(
        hl = list(label="Dinge, die ich mache", icon="fa-person-running"),
        items = dict(list(
            vacation = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(25)
                    }
                )),
            socialmedia = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ich bin sehr aktiv") return(20)
                    if (value == "Ich bin etwas aktiv") return(15)
                    if (value == "Ich bin kaum aktiv") return(12)
                    if (value == "Ich bin nie aktiv") return(25)
                    }
                )),
            fashion = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Sehr") return(20)
                    if (value == "Etwas") return(15)
                    if (value == "Nicht viel") return(12)
                    }
                )),
            smoke = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(20)
                    if (value == "Ja, aber nur in Gesellschaft") return(20)
                    if (value == "Nein") return(20)
                    if (value == "Nein, ich habe aufgehört") return(25)
                    }
                )),
            sportdo = dict(list(
                split = ", ",
                weight = function(value=NA, common=0) {
                    return(common * 20)
                    }
                )),
            othersportdo = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(30)
                    }
                )),
            museums = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja, ich liebe es") return(25)
                    if (value == "Ja, manchmal") return(12)
                    if (value == "Nein") return(8)
                    }
                )),
            dance = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja, ich liebe es") return(25)
                    if (value == "Ja, manchmal") return(12)
                    if (value == "Nein") return(8)
                    }
                ))
            ))
        )),

    # Things i like
    thingsilike = dict(list(
        hl = list(label="Dinge, die ich mag", icon="fa-heart-pulse"),
        items = dict(list(
            musiclisten = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Nein") return(15)
                    return(5)
                    }
                )),
            music = dict(list(
                split = ", ",
                weight = function(value=NA, common=0) {
                    return(common * 20)
                    }
                )),
            othermusic = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(20)
                    }
                )),
            bestmusician = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(30)
                    }
                )),
            moviefan = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Nein") return(15)
                    return(5)
                    }
                )),
            movie = dict(list(
                split = ", ",
                weight = function(value=NA, common=0) {
                    return(common * 20)
                    }
                )),
            othermovie = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(20)
                    }
                )),
            bestmovie = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(30)
                    }
                )),
            bestactor = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(30)
                    }
                )),
            sportfan = dict(list(
                split = ", ",
                fuzzy = 0,
                weight = function(value=NA, common=0) {
                    if (value == "Nein") return(15)
                    return(5)
                    }
                )),
            sportfollow = dict(list(
                # Implementation in Baliettia does not use the common vector length!
                # Makes no sense compared to the other items, please consider
                split = ", ",
                weight = function(value=NA, common=0) {
                    return(common * 20)
                    }
                )),
            othersportfollow = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(30)
                    }
                )),
            bestteam = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(40)
                    }
                )),
            watchtv = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Nein") return(15)
                    return(5)
                    }
                )),
            tvshows = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(common * 20)
                    }
                )),
            readbooks = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(15)
                    return(10)
                    }
                )),
            books = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(common * 25)
                    }
                )),
            playvideogames = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(20)
                    return(5)
                    }
                )),
            videogames = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(common * 30)
                    }
                )),
            followwebchannels = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(15)
                    return(5)
                    }
                )),
            webchannels = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(common * 40)
                    }
                )),
            docreative = dict(list(
                weight = function(value=NA, common=0) {
                    if (value == "Ja") return(20)
                    return(8)
                    }
                )),
            creative = dict(list(
                split = ", ",
                fuzzy = TRUE,
                weight = function(value=NA, common=0) {
                    return(common * 22)
                    }
                ))
            ))
        )),

    # Quirk
    quirk = dict(list(
        hl = list(label="Ein interessanter Fakt über mich", icon="fa-face-surprise"),
        items = dict(list(
            otherfun = dict(list(
                # what can be a match here?
                split = ", ",
                fuzzy = TRUE,
                fuzzymaxdist = 20,
                weight = function(value=NA, common=0) {
                    return(common * 35)
                    }
                ))
            ))
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
