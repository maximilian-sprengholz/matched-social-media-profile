'
The dictionary contains all info necessary to:

(1) Determine the way to match and score
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
- ENCODING ISSUES: Some string matches do not return score weights because
  (I assume) the category strings I provided are somehow different to the ones
  in the data (e.g. "Würzig, aber nicht zu viel"). Other Umlauts are matched,
  though, so no idea what the issue is exactly. Needs some digging.

(2) Export the information to the profiles
------------------------------------------

Can be extended to contain any other parameters you think are necessary.
'

matchvarparams <- dict(list(

    # Demography
    age = dict(list(
    	weight = function(value="", common=0) {
            # slightly diff. bc of unused gender info (15 if geder matched too)
    		return(12)
			},
		group = list(label="Demografie", id="demography"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    initials = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
			},
		group = list(label="Demografie", id="demography"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    gender = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
			},
		group = list(label="Demografie", id="demography"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    eyes = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Blau") return(20)
            return(5)
			},
		group = list(label="Demografie", id="demography"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    othereyes = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(15)
			},
		group = list(label="Demografie", id="demography"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    righthanded = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Rechtshänder") return(5)
            if (value == "Linkshänder") return(20)
            return(30)
			},
		group = list(label="Demografie", id="demography"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    language = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
            if (value != "Deutsch") return(18)
            return(2)
			},
		group = list(label="Demografie", id="demography"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    otherlanguage = dict(list(
    	vector = 1,
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 15)
			},
		group = list(label="Demografie", id="demography"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    totlanguage = dict(list(
    	weight = function(value="", common=0) {
            if (value == 1) return(5)
            if (value == 2) return(10)
            if (value < 5) return(18)
            return(30)
			},
		group = list(label="Demografie", id="demography"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),

    # Location
    currentstate = dict(list(
    	weight = function(value="", common=0) {
    		return(10)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    currentzip = dict(list(
        # exact sensible?
    	weight = function(value="", common=0) {
    		return(40)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    currentrural = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    grownup_ger = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(15)
            if (value == "Nein") return(5)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    homestate_ger = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    hometown_ger = dict(list(
    	fuzzy = TRUE,
        fuzzymaxdist = 2,
    	weight = function(value="", common=0) {
    		return(25)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    homezip_ger = dict(list(
    	# exact sensible?
    	weight = function(value="", common=0) {
    		return(45)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    homestate_foreign = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(20)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    hometown_foreign = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    homezip_foreign = dict(list(
        # exact sensible?
    	weight = function(value="", common=0) {
    		return(50)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    homerural = dict(list(
    	weight = function(value="", common=0) {
    		return(10)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    samestate = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(5)
    		if (value == "Nein") return(0)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    samezip = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(30)
    		if (value == "Nein") return(0)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    secondgen = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Nein") return(0)
            if (value == "Ja, Eltern") return(25)
            if (value == "Ja, Großeltern") return(15)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    secondgencountry = dict(list(
        split = ", ",
        fuzzy = TRUE,
    	weight = function(value="", common=0) {
            return(common * 30)
			},
		group = list(label="Ort und Herkunft", id="location"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),

    # Family
    marital = dict(list(
    	weight = function(value="", common=0) {
    		return(10)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    parentsdivorced = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(25)
    		if (value == "Nein") return(13)
    		if (value == "Sie waren nie verheiratet") return(18)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    children = dict(list(
    	weight = function(value="", common=0) {
            if (value == 0) return(0)
            if (value < 3) return(10)
            if (value < 6) return(20)
            return(30)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    childrenbracket = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Mehr als 4") return(35)
            if (value == "3-4") return(25)
            if (value == "1-2") return(15)
            if (value == "Keine") return(5)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    siblings = dict(list(
    	weight = function(value="", common=0) {
            if (value == 0) return(5)
            if (value < 3) return(10)
            if (value < 6) return(20)
            return(30)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    siblingsbracket = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Mehr als 4") return(35)
            if (value == "3-4") return(25)
            if (value == "1-2") return(15)
            if (value == "Keine") return(5)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    military = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(25)
    		return(5)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    militarybranch = dict(list(
    	weight = function(value="", common=0) {
    		return(35)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    education = dict(list(
        # open answers perhaps problematic
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(10)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    college = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(35)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    gayfriends = dict(list(
    	weight = function(value="", common=0) {
    		return(30)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    lossfriend = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(60)
            return(2)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    caregiver = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(50)
            return(2)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    pets = dict(list(
    	split = ", ",
    	weight = function(value="", common=0) {
    		return(common * 20)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    otherpets = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 30)
			},
		group = list(label="Familie", id="family"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),

    # Finance
    employment = dict(list(
        # combine ?
    	weight = function(value="", common=0) {
    		return(10)
			},
		group = list(label="Fianzen", id="finance"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    ownhouse = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
			},
		group = list(label="Fianzen", id="finance"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    owncar = dict(list(
    	weight = function(value="", common=0) {
    		return(10)
			},
		group = list(label="Fianzen", id="finance"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    studentdebt = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja, und sie sind groß") return(30)
            if (value == "Ja, aber sie sind überschaubar") return(20)
            if (value == "Nein, ich habe sie abbezahlt") return(15)
            if (value == "Nein, ich hatte nie welche") return(8)
			},
		group = list(label="Fianzen", id="finance"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    income = dict(list(
        # exact, but value rounded to next multiple of 100
    	weight = function(value="", common=0) {
    		return(30)
			},
		group = list(label="Fianzen", id="finance"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
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
			},
		group = list(label="Fianzen", id="finance"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    incomeclassPastDirection = dict(list(
    	weight = function(value="", common=0) {
            if (value != "Gleich") return(15)
            return(5)
			},
		group = list(label="Fianzen", id="finance"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    incomeclassFutureDirection = dict(list(
    	weight = function(value="", common=0) {
            if (value != "Gleich") return(15)
            return(5)
			},
		group = list(label="Fianzen", id="finance"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
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
			},
		group = list(label="Fianzen", id="finance"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    incomeclass = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Unterschicht") return(40)
            if (value == "untere Mittelschicht") return(25)
            if (value == "Mittelschicht") return(15)
            if (value == "obere Mittelschicht") return(30)
            if (value == "Oberschicht") return(50)
			},
		group = list(label="Fianzen", id="finance"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    incomeclassfuture = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Unterschicht") return(40)
            if (value == "untere Mittelschicht") return(25)
            if (value == "Mittelschicht") return(15)
            if (value == "obere Mittelschicht") return(30)
            if (value == "Oberschicht") return(50)
			},
		group = list(label="Fianzen", id="finance"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),

    # Personality
    workorplay = dict(list(
    	weight = function(value="", common=0) {
    		return(20)
			},
		group = list(label="Persönlichkeit", id="personality"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    energetic = dict(list(
    	weight = function(value="", common=0) {
    		return(20)
			},
		group = list(label="Persönlichkeit", id="personality"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    competitive = dict(list(
    	weight = function(value="", common=0) {
    		return(20)
			},
		group = list(label="Persönlichkeit", id="personality"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    perfectionist = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(30)
            return(15)
			},
		group = list(label="Persönlichkeit", id="personality"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    patient = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
			},
		group = list(label="Persönlichkeit", id="personality"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    messy = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja, sehr chaotisch") return(30)
            if (value == "Ja, ein bisschen") return(15)
            return(20)
			},
		group = list(label="Persönlichkeit", id="personality"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    carebody = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
			},
		group = list(label="Persönlichkeit", id="personality"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    confrontational = dict(list(
    	weight = function(value="", common=0) {
    		return(20)
			},
		group = list(label="Persönlichkeit", id="personality"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    fascination = dict(list(
    	weight = function(value="", common=0) {
    		return(25)
			},
		group = list(label="Persönlichkeit", id="personality"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    fairies = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Ja") return(30)
            return(5)
			},
		group = list(label="Persönlichkeit", id="personality"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),

    # Behavior
    snooze = dict(list(
    	weight = function(value="", common=0) {
    		return(30)
			},
		group = list(label="Verhalten", id="behavior"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    streetfurniture = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(30)
            return(10)
			},
		group = list(label="Verhalten", id="behavior"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    giveaway = dict(list(
    	weight = function(value="", common=0) {
    		return(20)
			},
		group = list(label="Verhalten", id="behavior"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    stoleglass = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(40)
            return(10)
			},
		group = list(label="Verhalten", id="behavior"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    foodback = dict(list(
    	weight = function(value="", common=0) {
    		return(30)
			},
		group = list(label="Verhalten", id="behavior"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    giftrecycle = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(30)
            return(20)
			},
		group = list(label="Verhalten", id="behavior"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    profanelanguage = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Nie") return(25)
            if (value == "Gelegentlich") return(20)
            if (value == "Oft") return(20)
            if (value == "Regelmäßig") return(30)
			},
		group = list(label="Verhalten", id="behavior"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    readhoroscope = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Täglich") return(30)
            if (value == "Wöchentlich") return(15)
            if (value == "Gelegentlich") return(8)
            if (value == "Nie") return(10)
			},
		group = list(label="Verhalten", id="behavior"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),

    # Taste
    color = dict(list(
    	weight = function(value="", common=0) {
    		return(15)
			},
		group = list(label="Geschmack", id="taste"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    othercolor = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(25)
			},
		group = list(label="Geschmack", id="taste"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    food = dict(list(
    	weight = function(value="", common=0) {
    		return(20)
			},
		group = list(label="Geschmack", id="taste"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    otherfood = dict(list(
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
			},
		group = list(label="Geschmack", id="taste"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    spicyfood = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ich mag kein scharfes Essen") return(12)
            if (value == "Würzig, aber nicht zu viel") return(10)
            if (value == "Scharf") return(15)
            if (value == "Sehr scharf") return(22)
			},
		group = list(label="Geschmack", id="taste"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    vegetarian = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(25)
            if (value == "Nein") return(8)
			},
		group = list(label="Geschmack", id="taste"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    countriesvisited = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Keines") return(15)
            if (value == "Zwischen 1 und 2") return(10)
            if (value == "Zwischen 3 und 5") return(15)
            if (value == "Zwischen 6 und 10") return(20)
            if (value == "Mehr als 10") return(28)
			},
		group = list(label="Geschmack", id="taste"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),

    # Things you do
    vacation = dict(list(
        split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(25)
			},
		group = list(label="Dinge, die ich mache", id="thingsyoudo"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    socialmedia = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ich bin sehr aktiv") return(20)
            if (value == "Ich bin etwas aktiv") return(15)
            if (value == "Ich bin kaum aktiv") return(12)
            if (value == "Ich bin nie aktiv") return(25)
			},
		group = list(label="Dinge, die ich mache", id="thingsyoudo"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    fashion = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Sehr") return(20)
            if (value == "Etwas") return(15)
            if (value == "Nicht viel") return(12)
			},
		group = list(label="Dinge, die ich mache", id="thingsyoudo"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    smoke = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(20)
            if (value == "Ja, aber nur in Gesellschaft") return(20)
            if (value == "Nein") return(20)
            if (value == "Nein, ich habe aufgehört") return(25)
			},
		group = list(label="Dinge, die ich mache", id="thingsyoudo"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    sportdo = dict(list(
    	split = ", ",
    	weight = function(value="", common=0) {
    		return(common * 20)
			},
		group = list(label="Dinge, die ich mache", id="thingsyoudo"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    othersportdo = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
			},
		group = list(label="Dinge, die ich mache", id="thingsyoudo"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    museums = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja, ich liebe es") return(25)
            if (value == "Ja, manchmal") return(12)
            if (value == "Nein") return(8)
			},
		group = list(label="Dinge, die ich mache", id="thingsyoudo"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    dance = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja, ich liebe es") return(25)
            if (value == "Ja, manchmal") return(12)
            if (value == "Nein") return(8)
			},
		group = list(label="Dinge, die ich mache", id="thingsyoudo"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),

    # Things I like
    musiclisten = dict(list(
    	weight = function(value="", common=0) {
    		if (value == "Nein") return(15)
            return(5)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    music = dict(list(
        split = ", ",
    	weight = function(value="", common=0) {
    		return(common * 20)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    othermusic = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(20)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    bestmusician = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    moviefan = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Nein") return(15)
            return(5)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    movie = dict(list(
    	split = ", ",
    	weight = function(value="", common=0) {
    		return(common * 20)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    othermovie = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(20)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    bestmovie = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    bestactor = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    sportfan = dict(list(
    	split = ", ",
    	fuzzy = 0,
    	weight = function(value="", common=0) {
            if (value == "Nein") return(15)
            return(5)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    sportfollow = dict(list(
        # Implementation in Baliettia does not use the common vector length!
        # Makes no sense compared to the other items, please consider
    	split = ", ",
    	weight = function(value="", common=0) {
    		return(common * 20)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    othersportfollow = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(30)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    bestteam = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(40)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    watchtv = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Nein") return(15)
            return(5)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    tvshows = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 20)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    readbooks = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(15)
            return(10)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    books = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 25)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    playvideogames = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(20)
            return(5)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    videogames = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 30)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    followwebchannels = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(15)
            return(5)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    webchannels = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 40)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    docreative = dict(list(
    	weight = function(value="", common=0) {
            if (value == "Ja") return(20)
            return(8)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),
    creative = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value="", common=0) {
    		return(common * 22)
			},
		group = list(label="Dinge, die ich mag", id="thingsilike"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
			}
    	)),

    # Quirk
    otherfun = dict(list(
        # what can be a match here?
    	split = ", ",
    	fuzzy = TRUE,
        fuzzymaxdist = 20,
    	weight = function(value="", common=0) {
    		return(common * 35)
			},
		group = list(label="Ein interessanter Fakt über mich", id="quirk"),
		print = function(value="") {
			return(paste0("<li>", value, "<li>"))
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
