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
    	weight = function(value=NA, common=0) {
            # slightly diff. bc of unused gender info (15 if geder matched too)
    		return(12)
			},
		group = list(id="demography", label="Demografie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
                return(list(track=t, print=paste0(value, " Jahre")))
			} else {
				p <- paste0("<li>Ich bin ", value, " Jahre alt.</li>")
                return(list(track=t, print=p))
				}
			}
    	)),
    initials = dict(list(
    	weight = function(value=NA, common=0) {
    		return(15)
			},
		group = list(id="demography", label="Demografie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
            # can never be called in current setup
			# if (header == TRUE) {
			# 	return(list(track=t, print=paste0(value)))
			# } else {
			# 	p <- paste0("<li>", value, "</li>")
            #   return(list(track=t, print=p))
			# 	}
			}
    	)),
    gender = dict(list(
    	weight = function(value=NA, common=0) {
    		return(15)
			},
		group = list(id="demography", label="Demografie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>Ich bin", tolower(value), ".</li>")
                return(list(track=t, print=p))
				}
			}
    	)),
    eyes = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Blau") return(20)
            return(5)
			},
		group = list(id="demography", label="Demografie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>Meine Augen sind ", tolower(value), ".</li>")
                return(list(track=t, print=p))
				}
			}
    	)),
    othereyes = dict(list(
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(15)
			},
		group = list(id="demography", label="Demografie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>Meine Augen sind ", tolower(value), ".</li>")
                return(list(track=t, print=p))
				}
			}
    	)),
    righthanded = dict(list(
    	weight = function(value=NA, common=0) {
    		if (value == "Rechtshänder") return(5)
            if (value == "Linkshänder") return(20)
            return(30)
			},
		group = list(id="demography", label="Demografie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>Ich bin ", value, ".</li>")
                return(list(track=t, print=p))
				}
			}
    	)),
    language = dict(list(
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
            if (value != "Deutsch") return(18)
            return(2)
			},
		group = list(id="demography", label="Demografie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
                # combination of items (only non-NA passed, check for colname)
                '
                Set returns based on the values of other variables. Because you
                print stuff before the columns are actually looped over, you
                have to return the additional items printed which can then be
                skipped.
                '
                if ("match_otherlanguage" %in% colnames(df)) {
                    p <- paste0("<li>Meine Muttersprache ist ", value,
                        " und ich spreche noch ", df[1,"match_otherlanguage"], ".</li>")
                    t <- c("match_otherlanguage")

                } else {
                    p <- paste0("<li>Meine Muttersprache ist ", value, ".</li>")
                    }
                return(list(track=t, print=p))
				}
			}
    	)),
    otherlanguage = dict(list(
    	vector = 1,
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(common * 15)
			},
		group = list(id="demography", label="Demografie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>Ich spreche ", value, ".</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    totlanguage = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == 1) return(5)
            if (value == 2) return(10)
            if (value < 5) return(18)
            return(30)
			},
		group = list(id="demography", label="Demografie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>Ich spreche insgesamt ", value, " Sprachen.</li>")
				return(list(track=t, print=p))
				}
			}
    	)),

    # Location
    currentstate = dict(list(
    	weight = function(value=NA, common=0) {
    		return(10)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>Ich wohne in ", value, ".</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    currentzip = dict(list(
        # exact sensible?
    	weight = function(value=NA, common=0) {
    		return(40)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    currentrural = dict(list(
    	weight = function(value=NA, common=0) {
    		return(15)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>Meine Region ist ", tolower(value), ".</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    grownup_ger = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja") return(15)
            if (value == "Nein") return(5)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
                if (value == "Ja") p <- paste0("<li>Ich bin in Deutschland aufgewachsen.</li>")
                if (value == "Nein") p <- paste0("<li>Ich bin nicht in Deutschland aufgewachsen.</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    homestate_ger = dict(list(
    	weight = function(value=NA, common=0) {
    		return(15)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    hometown_ger = dict(list(
    	fuzzy = TRUE,
        fuzzymaxdist = 2,
    	weight = function(value=NA, common=0) {
    		return(25)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    homezip_ger = dict(list(
    	# exact sensible?
    	weight = function(value=NA, common=0) {
    		return(45)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    homestate_foreign = dict(list(
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(20)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    hometown_foreign = dict(list(
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(30)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    homezip_foreign = dict(list(
        # exact sensible?
    	weight = function(value=NA, common=0) {
    		return(50)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    homerural = dict(list(
    	weight = function(value=NA, common=0) {
    		return(10)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    samestate = dict(list(
    	weight = function(value=NA, common=0) {
    		if (value == "Ja") return(5)
    		if (value == "Nein") return(0)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    samezip = dict(list(
    	weight = function(value=NA, common=0) {
    		if (value == "Ja") return(30)
    		if (value == "Nein") return(0)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    secondgen = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Nein") return(0)
            if (value == "Ja, Eltern") return(25)
            if (value == "Ja, Großeltern") return(15)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    secondgencountry = dict(list(
        split = ", ",
        fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
            return(common * 30)
			},
		group = list(id="location", label="Ort und Herkunft", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),

    # Family
    marital = dict(list(
    	weight = function(value=NA, common=0) {
    		return(10)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    parentsdivorced = dict(list(
    	weight = function(value=NA, common=0) {
    		if (value == "Ja") return(25)
    		if (value == "Nein") return(13)
    		if (value == "Sie waren nie verheiratet") return(18)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    children = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == 0) return(0)
            if (value < 3) return(10)
            if (value < 6) return(20)
            return(30)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    childrenbracket = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Mehr als 4") return(35)
            if (value == "3-4") return(25)
            if (value == "1-2") return(15)
            if (value == "Keine") return(5)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    siblings = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == 0) return(5)
            if (value < 3) return(10)
            if (value < 6) return(20)
            return(30)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    siblingsbracket = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Mehr als 4") return(35)
            if (value == "3-4") return(25)
            if (value == "1-2") return(15)
            if (value == "Keine") return(5)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    military = dict(list(
    	weight = function(value=NA, common=0) {
    		if (value == "Ja") return(25)
    		return(5)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    militarybranch = dict(list(
    	weight = function(value=NA, common=0) {
    		return(35)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    education = dict(list(
        # open answers perhaps problematic
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(10)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    college = dict(list(
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(35)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    gayfriends = dict(list(
    	weight = function(value=NA, common=0) {
    		return(30)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    lossfriend = dict(list(
    	weight = function(value=NA, common=0) {
    		if (value == "Ja") return(60)
            return(2)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    caregiver = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja") return(50)
            return(2)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    pets = dict(list(
    	split = ", ",
    	weight = function(value=NA, common=0) {
    		return(common * 20)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    otherpets = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(common * 30)
			},
		group = list(id="family", label="Familie", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),

    # Finance
    employment = dict(list(
        # combine ?
    	weight = function(value=NA, common=0) {
    		return(10)
			},
		group = list(id="finance", label="Finanzen", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    ownhouse = dict(list(
    	weight = function(value=NA, common=0) {
    		return(15)
			},
		group = list(id="finance", label="Finanzen", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    owncar = dict(list(
    	weight = function(value=NA, common=0) {
    		return(10)
			},
		group = list(id="finance", label="Finanzen", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    studentdebt = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja, und sie sind groß") return(30)
            if (value == "Ja, aber sie sind überschaubar") return(20)
            if (value == "Nein, ich habe sie abbezahlt") return(15)
            if (value == "Nein, ich hatte nie welche") return(8)
			},
		group = list(id="finance", label="Finanzen", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    income = dict(list(
        # exact, but value rounded to next multiple of 100
    	weight = function(value=NA, common=0) {
    		return(30)
			},
		group = list(id="finance", label="Finanzen", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
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
			},
		group = list(id="finance", label="Finanzen", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    incomeclassPastDirection = dict(list(
    	weight = function(value=NA, common=0) {
            if (value != "Gleich") return(15)
            return(5)
			},
		group = list(id="finance", label="Finanzen", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    incomeclassFutureDirection = dict(list(
    	weight = function(value=NA, common=0) {
            if (value != "Gleich") return(15)
            return(5)
			},
		group = list(id="finance", label="Finanzen", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
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
			},
		group = list(id="finance", label="Finanzen", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    incomeclass = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Unterschicht") return(40)
            if (value == "untere Mittelschicht") return(25)
            if (value == "Mittelschicht") return(15)
            if (value == "obere Mittelschicht") return(30)
            if (value == "Oberschicht") return(50)
			},
		group = list(id="finance", label="Finanzen", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    incomeclassfuture = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Unterschicht") return(40)
            if (value == "untere Mittelschicht") return(25)
            if (value == "Mittelschicht") return(15)
            if (value == "obere Mittelschicht") return(30)
            if (value == "Oberschicht") return(50)
			},
		group = list(id="finance", label="Finanzen", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),

    # Personality
    workorplay = dict(list(
    	weight = function(value=NA, common=0) {
    		return(20)
			},
		group = list(id="personality", label="Persönlichkeit", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    energetic = dict(list(
    	weight = function(value=NA, common=0) {
    		return(20)
			},
		group = list(id="personality", label="Persönlichkeit", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    competitive = dict(list(
    	weight = function(value=NA, common=0) {
    		return(20)
			},
		group = list(id="personality", label="Persönlichkeit", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    perfectionist = dict(list(
    	weight = function(value=NA, common=0) {
    		if (value == "Ja") return(30)
            return(15)
			},
		group = list(id="personality", label="Persönlichkeit", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    patient = dict(list(
    	weight = function(value=NA, common=0) {
    		return(15)
			},
		group = list(id="personality", label="Persönlichkeit", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    messy = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja, sehr chaotisch") return(30)
            if (value == "Ja, ein bisschen") return(15)
            return(20)
			},
		group = list(id="personality", label="Persönlichkeit", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    carebody = dict(list(
    	weight = function(value=NA, common=0) {
    		return(15)
			},
		group = list(id="personality", label="Persönlichkeit", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    confrontational = dict(list(
    	weight = function(value=NA, common=0) {
    		return(20)
			},
		group = list(id="personality", label="Persönlichkeit", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    fascination = dict(list(
    	weight = function(value=NA, common=0) {
    		return(25)
			},
		group = list(id="personality", label="Persönlichkeit", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    fairies = dict(list(
    	weight = function(value=NA, common=0) {
    		if (value == "Ja") return(30)
            return(5)
			},
		group = list(id="personality", label="Persönlichkeit", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),

    # Behavior
    snooze = dict(list(
    	weight = function(value=NA, common=0) {
    		return(30)
			},
		group = list(id="behavior", label="Verhalten", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    streetfurniture = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja") return(30)
            return(10)
			},
		group = list(id="behavior", label="Verhalten", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    giveaway = dict(list(
    	weight = function(value=NA, common=0) {
    		return(20)
			},
		group = list(id="behavior", label="Verhalten", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    stoleglass = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja") return(40)
            return(10)
			},
		group = list(id="behavior", label="Verhalten", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    foodback = dict(list(
    	weight = function(value=NA, common=0) {
    		return(30)
			},
		group = list(id="behavior", label="Verhalten", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    giftrecycle = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja") return(30)
            return(20)
			},
		group = list(id="behavior", label="Verhalten", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    profanelanguage = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Nie") return(25)
            if (value == "Gelegentlich") return(20)
            if (value == "Oft") return(20)
            if (value == "Regelmäßig") return(30)
			},
		group = list(id="behavior", label="Verhalten", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    readhoroscope = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Täglich") return(30)
            if (value == "Wöchentlich") return(15)
            if (value == "Gelegentlich") return(8)
            if (value == "Nie") return(10)
			},
		group = list(id="behavior", label="Verhalten", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),

    # Taste
    color = dict(list(
    	weight = function(value=NA, common=0) {
    		return(15)
			},
		group = list(id="taste", label="Geschmack", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    othercolor = dict(list(
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(25)
			},
		group = list(id="taste", label="Geschmack", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    food = dict(list(
    	weight = function(value=NA, common=0) {
    		return(20)
			},
		group = list(id="taste", label="Geschmack", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    otherfood = dict(list(
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(30)
			},
		group = list(id="taste", label="Geschmack", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    spicyfood = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ich mag kein scharfes Essen") return(12)
            if (value == "Würzig, aber nicht zu viel") return(10)
            if (value == "Scharf") return(15)
            if (value == "Sehr scharf") return(22)
			},
		group = list(id="taste", label="Geschmack", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    vegetarian = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja") return(25)
            if (value == "Nein") return(8)
			},
		group = list(id="taste", label="Geschmack", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    countriesvisited = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Keines") return(15)
            if (value == "Zwischen 1 und 2") return(10)
            if (value == "Zwischen 3 und 5") return(15)
            if (value == "Zwischen 6 und 10") return(20)
            if (value == "Mehr als 10") return(28)
			},
		group = list(id="taste", label="Geschmack", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),

    # Things you do
    vacation = dict(list(
        split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(25)
			},
		group = list(id="thingsyoudo", label="Dinge, die ich mache", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    socialmedia = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ich bin sehr aktiv") return(20)
            if (value == "Ich bin etwas aktiv") return(15)
            if (value == "Ich bin kaum aktiv") return(12)
            if (value == "Ich bin nie aktiv") return(25)
			},
		group = list(id="thingsyoudo", label="Dinge, die ich mache", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    fashion = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Sehr") return(20)
            if (value == "Etwas") return(15)
            if (value == "Nicht viel") return(12)
			},
		group = list(id="thingsyoudo", label="Dinge, die ich mache", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    smoke = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja") return(20)
            if (value == "Ja, aber nur in Gesellschaft") return(20)
            if (value == "Nein") return(20)
            if (value == "Nein, ich habe aufgehört") return(25)
			},
		group = list(id="thingsyoudo", label="Dinge, die ich mache", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    sportdo = dict(list(
    	split = ", ",
    	weight = function(value=NA, common=0) {
    		return(common * 20)
			},
		group = list(id="thingsyoudo", label="Dinge, die ich mache", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    othersportdo = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(30)
			},
		group = list(id="thingsyoudo", label="Dinge, die ich mache", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    museums = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja, ich liebe es") return(25)
            if (value == "Ja, manchmal") return(12)
            if (value == "Nein") return(8)
			},
		group = list(id="thingsyoudo", label="Dinge, die ich mache", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    dance = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja, ich liebe es") return(25)
            if (value == "Ja, manchmal") return(12)
            if (value == "Nein") return(8)
			},
		group = list(id="thingsyoudo", label="Dinge, die ich mache", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),

    # Things I like
    musiclisten = dict(list(
    	weight = function(value=NA, common=0) {
    		if (value == "Nein") return(15)
            return(5)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    music = dict(list(
        split = ", ",
    	weight = function(value=NA, common=0) {
    		return(common * 20)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    othermusic = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(20)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    bestmusician = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(30)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    moviefan = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Nein") return(15)
            return(5)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    movie = dict(list(
    	split = ", ",
    	weight = function(value=NA, common=0) {
    		return(common * 20)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    othermovie = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(20)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    bestmovie = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(30)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    bestactor = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(30)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    sportfan = dict(list(
    	split = ", ",
    	fuzzy = 0,
    	weight = function(value=NA, common=0) {
            if (value == "Nein") return(15)
            return(5)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    sportfollow = dict(list(
        # Implementation in Baliettia does not use the common vector length!
        # Makes no sense compared to the other items, please consider
    	split = ", ",
    	weight = function(value=NA, common=0) {
    		return(common * 20)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    othersportfollow = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(30)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    bestteam = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(40)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    watchtv = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Nein") return(15)
            return(5)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    tvshows = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(common * 20)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    readbooks = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja") return(15)
            return(10)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    books = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(common * 25)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    playvideogames = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja") return(20)
            return(5)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    videogames = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(common * 30)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    followwebchannels = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja") return(15)
            return(5)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    webchannels = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(common * 40)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    docreative = dict(list(
    	weight = function(value=NA, common=0) {
            if (value == "Ja") return(20)
            return(8)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),
    creative = dict(list(
    	split = ", ",
    	fuzzy = TRUE,
    	weight = function(value=NA, common=0) {
    		return(common * 22)
			},
		group = list(id="thingsyoulike", label="Dinge, die ich mag", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
			}
    	)),

    # Quirk
    otherfun = dict(list(
        # what can be a match here?
    	split = ", ",
    	fuzzy = TRUE,
        fuzzymaxdist = 20,
    	weight = function(value=NA, common=0) {
    		return(common * 35)
			},
		group = list(id="quirk", label="Ein interessanter Fakt über mich", icon="fa-user"),
		print = function(df=NA, value=NA, header=FALSE, t=c()) {
			if (header == TRUE) {
				return(list(track=t, print=paste0(value)))
			} else {
				p <- paste0("<li>", value, "</li>")
				return(list(track=t, print=p))
				}
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
