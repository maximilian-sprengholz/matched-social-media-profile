'
The dictionary contains all info necessary to:

(1) Group items and print group headlines
------------------------------------------

- hl (list): list the label and icon (fa ref) displayed
  (id of group div = group key, e.g. demography)
- printsubset (list): defines subsetting the printed items according to our rules:
    - subgroups (atomic vector): set if selection should be based on group of 
      items instead of single items
    - map (list): maps correspondence between print strings and matching vars, 
      allows to prioritize by sum of simscores when matchvars have been combined 
      in print strings
    - max: max number of items (or groups when subgroups!=NA) printed
- items (dict): list of all the variables belonging to that group; all items also a dict


(2) Determine the way to match and score
----------------------------------------

- split (str/logical): separator if strings are to be treated as vector
- fuzzy (logical): TRUE if variable requires fuzzy matching
- fuzzy_distperchar (float, default = 0.33): 
  - this is a factor to compute the maxDist parameter used by fuzzy amatch() in match_values() 
  - consider two strings to compare, one longer than the other, then:
    maxDist = floor(length(str_longer)*0.33)
  - maxDist = 3 would mean that 3 x char deletion, insertion, substitution and transposition, 
    are allowed for each comparison
- weight: value or function (similar to Balietti weight.js)

Dict evaluation is smart: If key is not set, a default is assumed. FALSE for
split and fuzzy, 0.33 for fuzzy_distperchar; see matcher().

Comments:
- Open answers under "Andere, und zwar:" can be vectors, but in the Baliettia
  paper the match vector length does not matter typically (so there is just one
  score irrespective of the number of matches). Can be a problem when the open
  answers contain a lot of elements, because the total weight these generate
  might be lower than the max. n x weight for the closed answer categories.
- Please see comments at specific vars for deviations or questions

The dict can be extended to contain any other parameters.


MATCHVAR DIFFERENCES TO BALIETTI ET AL.:
----------------------------------------

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
homezip_foreign [u]
homestate_foreign[u]
currenttown [u]
sametown [u]
employment [r]
quirk [r]

See also comments for specific keys.

'

matchparams <- dict(list(

    # Demography
    demography = dict(list(
        hl = list(label="Allgemein", icon="fa-user"),
        printsubset = list(
            subgroups = NA,
            map = list(
                demography1 = "eyes",
                demography2 = "righthanded",
                demography3 = "homestate_ger",
                demography4 = "language",
                demography5 = "otherlanguage",
                demography6 = "totlanguage"
                ),
            max = 3
            ),
        items = dict(list(
            age = dict(list(
                weight = function(value=NA, common=NA) {
                    # slightly diff. bc of unused gender info (15 if gender matched too)
                    return(12)
                    }
                )),
            initials = dict(list(
                weight = function(value=NA, common=NA) {
                    return(15)
                    }
                )),
            gender = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Divers") return(30)
                    return(15)
                    }
                )),
            eyes = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "blau") return(20)
                    return(5)
                    }
                )),
            othereyes = dict(list(
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(15)
                    }
                )),
            righthanded = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Rechtshänder") return(5)
                    if (value == "Linkshänder") return(20)
                    return(30)
                    }
                )),
            language = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    # German only get 2, every other match 18
                    if (length(common)==1 & common[1]=="Deutsch") return(2)
                    return(18)
                    }
                )),
            otherlanguage = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(length(common) * 15)
                    }
                )),
            totlanguage = dict(list(
                weight = function(value=NA, common=NA) {
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
        printsubset = list(
            subgroups = NA,
            map = list(
                location2 = "currentrural",
                location3 = "grownup_ger",
                location4 = "homerural",
                location5 = "secondgen",
                location6 = "secondgencountry",
                location7 = "samestate"
                ),
            max = 3
            ),
        items = dict(list(
            currentstate = dict(list(
                weight = function(value=NA, common=NA) {
                    return(10)
                    }
                )),
            currentzip = dict(list(
                # exact sensible?
                weight = function(value=NA, common=NA) {
                    return(40)
                    }
                )),
            currentrural = dict(list(
                weight = function(value=NA, common=NA) {
                    return(15)
                    }
                )),
            grownup_ger = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ja") return(15)
                    if (value == "Nein") return(5)
                    }
                )),
            homestate_ger = dict(list(
                weight = function(value=NA, common=NA) {
                    return(15)
                    }
                )),
            hometown_ger = dict(list(
                fuzzy = TRUE,
                fuzzy_distperchar = 0.25,
                weight = function(value=NA, common=NA) {
                    return(25)
                    }
                )),
            homezip_ger = dict(list(
                # exact sensible?
                weight = function(value=NA, common=NA) {
                    return(45)
                    }
                )),
            hometown_foreign = dict(list(
                fuzzy = TRUE,
                fuzzy_distperchar = 0.25,
                weight = function(value=NA, common=NA) {
                    return(30)
                    }
                )),
            homerural = dict(list(
                weight = function(value=NA, common=NA) {
                    return(10)
                    }
                )),
            samestate = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ja") return(5)
                    if (value == "Nein") return(0)
                    }
                )),
            samezip = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ja") return(30)
                    if (value == "Nein") return(0)
                    }
                )),
            secondgen = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Eltern") return(25)
                    if (value == "Großeltern") return(15)
                    }
                )),
            secondgencountry = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(length(common) * 30)
                    }
                ))
            ))
        )),

    # Family
    family = dict(list(
        hl = list(label="Persönlicher Hintergrund", icon="fa-users"),
        printsubset = list(
            subgroups = NA,
            map = list(
                family1 = "marital",
                family2 = "parentsdivorced",
                family3 = "children",
                family4 = "siblings",
                family5 = c("pets", "otherpets"),
                family6 = "education",
                family7 = c("military", "militarybranch"),
                family8 = "caregiver",
                family9 = "lossfriend",
                family10 = "gayfriends"
                ),
            max = 4
            ),
        items = dict(list(
            marital = dict(list(
                weight = function(value=NA, common=NA) {
                    return(10)
                    }
                )),
            parentsdivorced = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "sind geschieden") return(25)
                    if (value == "sind nicht geschieden") return(13)
                    if (value == "waren nie verheiratet") return(18)
                    }
                )),
            children = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == 0) return(0)
                    if (value < 3) return(10)
                    if (value < 6) return(20)
                    return(30)
                    }
                )),
            childrenbracket = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Mehr als 4") return(35)
                    if (value == "3-4") return(25)
                    if (value == "1-2") return(15)
                    if (value == "Keine") return(5)
                    }
                )),
            siblings = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == 0) return(5)
                    if (value < 3) return(10)
                    if (value < 6) return(20)
                    return(30)
                    }
                )),
            siblingsbracket = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Mehr als 4") return(35)
                    if (value == "3-4") return(25)
                    if (value == "1-2") return(15)
                    if (value == "Keine") return(5)
                    }
                )),
            military = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ich habe beim Militär") return(25)
                    if (value == "Ich habe nicht beim Militär gedient.") return(5)
                    }
                )),
            militarybranch = dict(list(
                weight = function(value=NA, common=NA) {
                    return(35)
                    }
                )),
            education = dict(list(
                # open answers perhaps problematic
                fuzzy = TRUE,
                fuzzy_distperchar = 0.15,
                weight = function(value=NA, common=NA) {
                    return(10)
                    }
                )),
            college = dict(list(
                fuzzy = TRUE,
                fuzzy_distperchar = 0.2,
                weight = function(value=NA, common=NA) {
                    return(35)
                    }
                )),
            gayfriends = dict(list(
                weight = function(value=NA, common=NA) {
                    return(30)
                    }
                )),
            lossfriend = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "bereits den Verlust") return(60)
                    if (value == "bislang noch keinen Verlust") return(2)
                    }
                )),
            caregiver = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "eine Pflegetätigkeit") return(50)
                    if (value == "keine Pflegetätigkeit") return(2)
                    }
                )),
            pets = dict(list(
                split = ",",
                weight = function(value=NA, common=NA) {
                    return(length(common) * 20)
                    }
                )),
            otherpets = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(length(common) * 30)
                    }
                ))
            ))
        )),

    # Finance
    finance = dict(list(
        hl = list(label="Finanzen", icon="fa-money-check-dollar"),
        printsubset = list(
            subgroups = NA,
            map = list(
                finance1 = "incomeclass",
                finance2 = "incomeclasschild",
                finance3 = "incomeclassfuture",
                finance4 = "incomeclassPastDirection",
                finance5 = "incomeclassFutureDirection",
                finance6 = "owncar",
                finance7 = "ownhouse",
                finance8 = "studentdebt"
                ),
            max = 3
            ),
        items = dict(list(
            ownhouse = dict(list(
                weight = function(value=NA, common=NA) {
                    return(15)
                    }
                )),
            owncar = dict(list(
                weight = function(value=NA, common=NA) {
                    return(10)
                    }
                )),
            studentdebt = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ja, und sie sind groß") return(30)
                    if (value == "Ja, aber sie sind überschaubar") return(20)
                    if (value == "Nein, ich habe sie abbezahlt") return(15)
                    if (value == "Nein, ich hatte nie welche") return(8)
                    }
                )),
            income = dict(list(
                # exact, but value rounded to next multiple of 100
                weight = function(value=NA, common=NA) {
                    return(30)
                    }
                )),
            incomebracket = dict(list(
                # 1 cat. more, check weights
                weight = function(value=NA, common=NA) {
                    if (value == "unter 1000 Euro") return(30)
                    if (value == "1000 Euro bis unter 2000 Euro") return(25)
                    if (value == "2000 Euro bis unter 3000 Euro") return(15)
                    if (value == "3000 Euro bis unter 4000 Euro") return(15)
                    if (value == "4000 Euro bis unter 5000 Euro") return(25)
                    if (value == "5000 Euro und mehr") return(30)
                    }
                )),
            incomeclassPastDirection = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value != "keine Veränderung") return(15)
                    return(5)
                    }
                )),
            incomeclassFutureDirection = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value != "keine Veränderung") return(15)
                    return(5)
                    }
                )),
            incomeclasschild = dict(list(
                # other cat., check weights (also for next 2)
                weight = function(value=NA, common=NA) {
                    if (value == "Unterschicht") return(40)
                    if (value == "unteren Mittelschicht") return(25)
                    if (value == "Mittelschicht") return(15)
                    if (value == "oberen Mittelschicht") return(30)
                    if (value == "Oberschicht") return(50)
                    }
                )),
            incomeclass = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Unterschicht") return(40)
                    if (value == "unteren Mittelschicht") return(25)
                    if (value == "Mittelschicht") return(15)
                    if (value == "oberen Mittelschicht") return(30)
                    if (value == "Oberschicht") return(50)
                    }
                )),
            incomeclassfuture = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Unterschicht") return(40)
                    if (value == "unteren Mittelschicht") return(25)
                    if (value == "Mittelschicht") return(15)
                    if (value == "oberen Mittelschicht") return(30)
                    if (value == "Oberschicht") return(50)
                    }
                ))
            ))
        )),

    # Personality
    personality = dict(list(
        hl = list(label="Persönlichkeit", icon="fa-face-smile"),
        printsubset = list(
            subgroups = NA,
            map = list(
                personality1 = "patient",
                personality2 = "confrontational",
                personality3 = "messy",
                personality4 = "energetic",
                personality5 = "competitive",
                personality6 = "perfectionist",
                personality7 = "workorplay",
                personality8 = "carebody",
                personality9 = "fascination",
                personality10 = "fairies"
                ),
            max = 3
            ),
        items = dict(list(
            workorplay = dict(list(
                weight = function(value=NA, common=NA) {
                    return(20)
                    }
                )),
            energetic = dict(list(
                weight = function(value=NA, common=NA) {
                    return(20)
                    }
                )),
            competitive = dict(list(
                weight = function(value=NA, common=NA) {
                    return(20)
                    }
                )),
            perfectionist = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ansonsten würde ich mich selbst als ziemlichen Perfektionist beschreiben.") return(30)
                    if (value == "Ansonsten würde ich mich selbst nicht als Perfektionist beschreiben.") return(15)
                    }
                )),
            patient = dict(list(
                weight = function(value=NA, common=NA) {
                    return(15)
                    }
                )),
            messy = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "sehr chaotisch") return(30)
                    if (value == "eher chaotisch") return(15)
                    if (value == "überhaupt nicht chaotisch") return(15)
                    }
                )),
            carebody = dict(list(
                weight = function(value=NA, common=NA) {
                    return(15)
                    }
                )),
            confrontational = dict(list(
                weight = function(value=NA, common=NA) {
                    return(20)
                    }
                )),
            fascination = dict(list(
                weight = function(value=NA, common=NA) {
                    return(25)
                    }
                )),
            fairies = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Auch wenn ich weiß, dass es viele Menschen komisch finden, wünsche ich mir manchmal, dass fantastische Kreaturen real wären.") return(30)
                    if (value == "Mit fantastischen Kreaturen und solchen Dingen kann ich überhaupt nichts anfangen.") return(5)
                    }
                ))
            ))
        )),

    # Behavior
    behavior = dict(list(
        hl = list(label="Verhalten", icon="fa-hands"),
        printsubset = list(
            subgroups = NA,
            map = list(
                behavior1 = "foodback",
                behavior2 = "giveaway",
                behavior3 = "streetfurniture",
                behavior4 = "giftrecycle",
                behavior5 = "snooze",
                behavior6 = "stoleglass",
                behavior7 = "profanelanguage",
                behavior8 = "readhoroscope"
                ),
            max = 3
            ),
        items = dict(list(
            snooze = dict(list(
                weight = function(value=NA, common=NA) {
                    return(30)
                    }
                )),
            streetfurniture = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ich bin ein Mensch, der auch mal Möbel vom Sperrmüll mit nach Hause mit.") return(30)
                    if (value == "Ich bin kein Mensch, der Möbel vom Sperrmüll mit nach Hause nehmen würde.") return(10)
                    }
                )),
            giveaway = dict(list(
                weight = function(value=NA, common=NA) {
                    return(20)
                    }
                )),
            stoleglass = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ich finde, dass man es mit den Regeln nicht unbedingt immer ganz genau nehmen muss. Ich habe ehrlich gesagt auch schon mal ein Glas aus einer Bar gestohlen.") return(40)
                    if (value == "Ich finde, dass auch kleinere Diebstähle kein Kavaliersdelikt sind. Ich habe zum Beispiel noch nie ein Glas aus einer Bar gestohlen.") return(10)
                    }
                )),
            foodback = dict(list(
                weight = function(value=NA, common=NA) {
                    return(30)
                    }
                )),
            giftrecycle = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Wenn mir ein Geschenk selbst nicht gefällt, schenke ich es manchmal auch einfach weiter.") return(30)
                    if (value == "Auch wenn mir ein Geschenk selbst nicht gefällt, würde ich es niemals einfach weiter verschenken.") return(20)
                    }
                )),
            profanelanguage = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "ich nie") return(25)
                    if (value == "auch ich gelegentlich") return(20)
                    if (value == "auch ich durchaus oft") return(20)
                    if (value == "auch ich regelmäßig") return(30)
                    }
                )),
            readhoroscope = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "ich mir mein Horoskop schon gerne jeden Tag durchlese") return(30)
                    if (value == "ich mir mein Horoskop zumindest einmal die Woche schon ganz gerne durchlese") return(15)
                    if (value == "auch ich mir mein Horoskop schon mal gelegentlich ganz gerne durchlese") return(8)
                    if (value == "ich damit nichts anfangen kann und ich mir mein Horoskop wirklich nie durchlese") return(10)
                    }
                ))
            ))
        )),

    # Taste
    taste = dict(list(
        hl = list(label="Geschmack", icon="fa-star"),
        printsubset = list(
            subgroups = NA,
            map = list(
                taste0 = c("color", "othercolor"),
                taste1 = c("food", "otherfood"),
                taste2 = "spicyfood",
                taste3 = "vegetarian",
                taste4 = "countriesvisited",
                taste5 = "vacation"
                ),
            max = 3
            ),
        items = dict(list(
            color = dict(list(
                weight = function(value=NA, common=NA) {
                    return(15)
                    }
                )),
            othercolor = dict(list(
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(25)
                    }
                )),
            food = dict(list(
                weight = function(value=NA, common=NA) {
                    return(20)
                    }
                )),
            otherfood = dict(list(
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(30)
                    }
                )),
            spicyfood = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ich mag scharfes Essen grundsätzlich nicht.") return(12)
                    if (value == "Ich mag mein Essen gerne würzig, aber nicht zu viel.") return(10)
                    if (value == "Ich mag scharfes Essen.") return(15)
                    if (value == "Mir kann das Essen gar nicht scharf genug sein.") return(22)
                    }
                )),
            vegetarian = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ich esse grundsätzlich kein Fleisch.") return(25)
                    if (value == "Ich esse ganz gerne auch mal Gerichte mit Fleisch.") return(8)
                    }
                )),
            countriesvisited = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Bislang war ich noch nie außerhalb Deutschlands.") return(15)
                    if (value == "Ich habe bislang ein bis zwei andere Länder bereist.") return(10)
                    if (value == "Ich habe bislang drei bis fünf andere Länder bereist.") return(15)
                    if (value == "Ich habe bislang sechs bis zehn andere Länder bereist.") return(20)
                    if (value == "Ich habe bislang schon mehr als zehn andere Länder bereist.") return(28)
                    }
                ))
            ))
        )),

    # Things you do
    thingsyoudo = dict(list(
        hl = list(label="Dinge, die ich mache", icon="fa-person-running"),
        printsubset = list(
            subgroups = NA,
            map = list(
                thingsyoudo0 = "socialmedia",
                thingsyoudo1 = "fashion",
                thingsyoudo2 = "smoke",
                thingsyoudo3 = c("sportdo", "othersportdo"),
                thingsyoudo4 = "museums",
                thingsyoudo5 = "dance"
                ),
            max = 3
            ),
        items = dict(list(
            vacation = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(25)
                    }
                )),
            socialmedia = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "sehr aktiv") return(20)
                    if (value == "etwas aktiv") return(15)
                    if (value == "kaum aktiv") return(12)
                    if (value == "gar nicht aktiv") return(25)
                    }
                )),
            fashion = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "sehr") return(20)
                    if (value == "etwas") return(15)
                    if (value == "nicht viel") return(12)
                    }
                )),
            smoke = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ich bin Raucher.") return(20)
                    if (value == "Ich bin Raucher, aber nur in Gesellschaft.") return(20)
                    if (value == "Ich bin Nichtraucher.") return(20)
                    if (value == "Ich bin Nichtraucher, habe aber früher mal geraucht.") return(25)
                    }
                )),
            sportdo = dict(list(
                split = ",",
                weight = function(value=NA, common=NA) {
                    return(length(common) * 20)
                    }
                )),
            othersportdo = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(30)
                    }
                )),
            museums = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ich liebe es ins Museum zu gehen.") return(25)
                    if (value == "Ich gehe gern ins Museum, aber nur manchmal.") return(12)
                    if (value == "Ich gehe nicht gern ins Museum.") return(8)
                    }
                )),
            dance = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ich gehe gern tanzen, aber nur manchmal.") return(25)
                    if (value == "Ich gehe nicht gern tanzen.") return(12)
                    if (value == "Ich liebe es tanzen zu gehen.") return(8)
                    }
                ))
            ))
        )),

    # Things i like
    thingsilike = dict(list(
        hl = list(label="Dinge, die ich mag", icon="fa-heart-pulse"),
        printsubset = list(
            subgroups = list(
                music = c("thingsilike1", "thingsilike2"),
                movies = c("thingsilike3", "thingsilike4", "thingsilike5"),
                sport = c("thingsilike6", "thingsilike7"),
                tv = c("thingsilike8", "thingsilike9"),
                books = c("thingsilike10", "thingsilike11"),
                webchannels = c("thingsilike12", "thingsilike13"),
                videogames = c("thingsilike14", "thingsilike15"),
                creative = c("thingsilike16", "thingsilike17")
                ),
            map = list(
                thingsilike1 = c("musiclisten", "music", "othermusic"),
                thingsilike2 = "bestmusician",
                thingsilike3 = c("moviefan", "movie", "othermovie"),
                thingsilike4 = "bestmovie",
                thingsilike5 = "bestactor",
                thingsilike6 = c("sportfan", "sportfollow", "othersportfollow"),
                thingsilike7 = "bestteam",
                thingsilike8 = "watchtv",
                thingsilike9 = "tvshows",
                thingsilike10 = "readbooks",
                thingsilike11 = "books",
                thingsilike12 = "followwebchannels",
                thingsilike13 = "webchannels",
                thingsilike14 = "playvideogames",
                thingsilike15 = "videogames",
                thingsilike16 = "docreative",
                thingsilike17 = "creative"
                ),
            max = 4
            ),
        items = dict(list(
            musiclisten = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ich habe es gerne ruhig. Musik hören mag ich gar nicht so gern.") return(15)
                    return(5)
                    }
                )),
            music = dict(list(
                split = ",",
                weight = function(value=NA, common=NA) {
                    return(length(common) * 20)
                    }
                )),
            othermusic = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(20)
                    }
                )),
            bestmusician = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(30)
                    }
                )),
            moviefan = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Filme schaue ich nicht sonderlich gern.") return(15)
                    return(5)
                    }
                )),
            movie = dict(list(
                split = ",",
                weight = function(value=NA, common=NA) {
                    return(length(common) * 20)
                    }
                )),
            othermovie = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(20)
                    }
                )),
            bestmovie = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(30)
                    }
                )),
            bestactor = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(30)
                    }
                )),
            sportfan = dict(list(
                split = ",",
                fuzzy = 0,
                weight = function(value=NA, common=NA) {
                    if (value == "Als Fan bin ich für Sport nicht zu begeistern.") return(15)
                    return(5)
                    }
                )),
            sportfollow = dict(list(
                # Implementation in Baliettia does not use the common vector length!
                # Makes no sense compared to the other items, please consider
                split = ",",
                weight = function(value=NA, common=NA) {
                    return(length(common) * 20)
                    }
                )),
            othersportfollow = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(30)
                    }
                )),
            bestteam = dict(list(
                split = ",",
                fuzzy = TRUE,
                fuzzy_distperchar = 0.4,
                weight = function(value=NA, common=NA) {
                    return(40)
                    }
                )),
            watchtv = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Fernsehen schaue ich momentan eigentlich gar nicht.") return(15)
                    return(5)
                    }
                )),
            tvshows = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(length(common) * 20)
                    }
                )),
            readbooks = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ich genieße es auch sehr mich in ein gutes Buch zu vertiefen.") return(15)
                    return(10)
                    }
                )),
            books = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(length(common) * 25)
                    }
                )),
            playvideogames = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ab und zu spiele ich auch ganz gerne Videospiele.") return(20)
                    return(5)
                    }
                )),
            videogames = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(length(common) * 30)
                    }
                )),
            followwebchannels = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Neue Medien nutze ich auch sehr gerne und folge selbst auch einigen Webkanälen.") return(15)
                    return(5)
                    }
                )),
            webchannels = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(length(common) * 40)
                    }
                )),
            docreative = dict(list(
                weight = function(value=NA, common=NA) {
                    if (value == "Ich liebe es mich kreativ zu betätigen.") return(20)
                    return(8)
                    }
                )),
            creative = dict(list(
                split = ",",
                fuzzy = TRUE,
                weight = function(value=NA, common=NA) {
                    return(length(common) * 22)
                    }
                ))
            ))
        ))
    ))

# variables to match on
# matchvars <- c(
#     "age", "initials", "gender", "eyes", "othereyes", "righthanded", "language",
#     "otherlanguage", "totlanguage", "currentstate", "currentzip", "currentrural",
#     "grownup_ger", "homestate_ger", "hometown_ger", "homezip_ger",
#     "hometown_foreign", "homerural", "samestate", "samezip",
#     "secondgen", "secondgencountry", "marital", "parentsdivorced", "children",
#     "childrenbracket", "siblings", "siblingsbracket", "military", "militarybranch",
#     "education", "college", "gayfriends", "lossfriend", "caregiver", "pets",
#     "otherpets", "ownhouse", "owncar", "studentdebt", "income",
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
