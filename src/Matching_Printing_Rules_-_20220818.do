***********************************************
***********************************************
***			COVID-19 Vaccination			***
***											***
***			Wave 6 - Printing Rules		    ***
***			18.08.2022						***
***											***
***********************************************
***********************************************



***    Globals    *******************************************
*************************************************************
global data 	"C:\Users\geisslef\_Work\04_Paper\Covid-19-Vaccination\Daten\Wave6"
global out 		"C:\Users\geisslef\_Work\04_Paper\Covid-19-Vaccination\Daten\Wave6\Out"


clear all

use "$data/W6_with_coded_essays_-_strings_recoded", clear

* ---------------------------------------------------------------------------- *

* Rename for merge
rename v_144 v_144_w6

* Merge children from w1 
merge m:1 ID using "C:\Users\geisslef\Dropbox\Covid Vaccine Allocation\4_data\wave_1\D-P21-13185 HU Berlin Impfskeptiker_W1_inkl Device.dta", keepusing(v_144)
drop if _merge == 2
drop _merge
rename v_144 v_144_w1

* Merge children from w4 (first refreshment)
merge m:1 ID using "C:\Users\geisslef\Dropbox\Covid Vaccine Allocation\4_data\wave_4\D-P21-13185 HU Berlin Impfskeptiker_W4_inkl Device---Combined.dta", keepusing(v_144)
drop if _merge == 2
drop _merge
rename v_144 v_144_w4

* Rename after merge
rename v_144_w6 v_144


* Gen combined v_144
gen v_144_merged = v_144
mvdecode v_144_merged, mv(-66 -99)
replace v_144_merged = v_144_w1 if v_144_merged == .
replace v_144_merged = v_144_w4 if v_144_merged == .



* ---------------------------------------------------------------------------- *


*** Header ***

* header 1
* initials 
cap drop initials 
gen str initials  = ""
replace initials  = upper(v_686_print)


* header2
* gender
cap drop gender 
decode v_16, gen(gender)
replace  gender = "Divers" if gender == "Sonstiges"

* currentstate 
cap drop currentstate 
decode v_23, gen(currentstate)

* age
cap drop age
tostring v_15, gen(age)


cap drop header2
gen str header2 = gender + "," + " " + age + " " + "Jahre" + "," + " " + currentstate



* ---------------------------------------------------------------------------- *

*** Allgemein ***
* <i class="fa-solid fa-user"></i>


* demography1
cap drop eyes 
gen str eyes = ""
replace eyes = "braun" if v_687 == 1
replace eyes = "blau" if v_688 == 1
replace eyes = "grün" if v_689 == 1

cap drop othereyes 
gen str othereyes = ""
replace othereyes = v_882_print
replace othereyes = ustrlower(othereyes)
replace othereyes = "" if v_882_print == " "
replace othereyes = "" if v_882_print == "-99"

cap drop eyes_help 
gen str eyes_help = eyes
replace eyes_help = othereyes if v_882_print != " "

cap drop demography1
gen str demography1 = ""
replace demography1 = "Meine Augenfarbe ist" + " " +  eyes_help + "." 
replace demography1 = "" if eyes_help == "" 



* demography2
cap drop righthanded 
decode v_693, gen(righthanded)

cap drop righthanded_help
decode v_693, gen(righthanded_help)
replace righthanded_help = "Linkshänderin" if righthanded_help == "Linkshänder" & v_16 == 1
replace righthanded_help = "Rechtshänderin" if righthanded_help == "Rechtshänder" & v_16 == 1
replace righthanded_help = "beidhändig" if righthanded_help == "Beidhändig" & v_16 == 1

cap drop demography2
gen str demography2 = ""
replace demography2 = "Ich bin" + " " + righthanded_help + "."




* demography3
cap drop homestate_ger 
gen str homestate_ger = ""
replace homestate_ger = "Ich komme aus Deutschland." if v_150 == 1
replace homestate_ger = "Ich komme nicht aus Deutschland." if v_150 == 2

cap drop demography3
gen str demography3 = ""
replace demography3 = homestate_ger




* demography4
cap drop language 
gen str language  = ""
replace language  = v_884_print
replace language  = ustrtitle(language)
replace language  = "Deutsch" if v_883 == 1

cap drop demography4
gen str demography4 = ""
replace demography4 = "Meine Muttersprache ist" + " " + language  + "."
replace demography4 = "" if language  == ""




* demography5
cap drop otherlanguage  
gen str otherlanguage  = ""
replace otherlanguage  = v_695_print
replace otherlanguage  = ustrtitle(otherlanguage)
replace otherlanguage = "ansonsten keine andere Fremdsprache" if v_695_print == " " | v_695_print == "" | v_695_print == "-99"

cap drop demography5
gen str demography5 = ""
replace demography5 = "Neben meiner Muttersprache spreche ich" + " " + otherlanguage  + "."
replace demography5 = "" if otherlanguage  == ""


* totlanguage?


* ---------------------------------------------------------------------------- *

*** Ort und Herkunft ***
* <i class="fa-solid fa-house-chimney-window"></i>


* location1
/*
cap drop currentstate  
decode v_698, gen(currentstate )

cap drop location1
gen str location1 = ""
replace location1 = "Momentan wohne ich in" + " " + currentstate  + "."
replace location1 = "" if currentstate  == ""
*/



* location2
cap drop currentrural  
decode v_696, gen(currentrural )
replace currentrural  = "eher ländlichen" if v_696 == 1
replace currentrural  = "eher städtischen" if v_696 == 2

cap drop location2
gen str location2 = ""
replace location2 = "Ich wohne in einer" + " " + currentrural  + " " + "Gegend."




* location3
cap drop grownup_ger 
gen str grownup_ger  = ""
replace grownup_ger  = ustrtitle(v_699_print)
replace grownup_ger  = "Deutschland" if v_697 == 1

cap drop location3
gen str location3 = ""
replace location3 = "Aufgewachsen bin ich in" + " " + grownup_ger  + "."
replace location3 = "" if grownup_ger  == " "




* location4
cap drop homerural  
decode v_702, gen(homerural )
replace homerural  = "eher ländlich" if v_702 == 1
replace homerural  = "eher städtisch" if v_702 == 2

cap drop location4
gen str location4 = ""
replace location4 = "Ich bin in einer Gegend aufgewachsen, die" + " " + homerural + " " + "ist."




* location5
cap drop secondgen 
gen str secondgen  = ""
replace secondgen  = "Eltern" if v_703 == 1
replace secondgen  = "Großltern" if v_703 == 2
replace secondgen  = "" if v_703 == 3

cap drop location5
gen str location5 = ""
replace location5 = "Ich habe auch eine Migrationsgeschichte: Meine" + " " + secondgen + " " + "sind damals nach Deutschland eingewandert."
replace location5 = "" if v_703 == 3 




* location6
cap drop secondgencountry  
gen str secondgencountry   = ""
replace secondgencountry   = ustrtitle(v_704_print)

cap drop location6
gen str location6 = ""
replace location6 = "Eingewandert sind meine" + " " + secondgen + " " + "aus" + " " + secondgencountry + "."
replace location6 = "" if secondgencountry  == " "
replace location6 = "" if secondgencountry  == "-66"


* ---------------------------------------------------------------------------- *

*** Persönlicher Hintergrund ***
* <i class="fa-solid fa-family"></i>


* family1
cap drop marital 
decode v_705, gen(marital )
replace marital  = "Ich bin verheiratet und lebe mit meinem Partner zusammen." if v_705 == 1
replace marital  = "Ich bin in eingetragener gleichgeschlechtlicher Lebenspartnerschaft und lebe mit meinem Partner zusammen." if v_705 == 2
replace marital  = "Ich bin verheiratet aber getrennt lebend." if v_705 == 3
replace marital  = "Ich bin ledig." if v_705 == 4
replace marital  = "Ich bin geschieden." if v_705 == 5
replace marital  = "Ich bin verwitwet." if v_705 == 6
replace marital  = "Ich bin in einer eingetragenen Lebenspartnerschaft aber getrennt lebend." if v_705 == 7
replace marital  = "Ich bin in einer eingetragenen Lebenspartnerschaft, die aufgehoben wurde." if v_705 == 8
replace marital  = "Ich bin in einer eingetragenen Lebenspartnerschaft, bei der mein Partner verstorben ist." if v_705 == 9

cap drop family1
gen str family1 = ""
replace family1 = marital



* family2
cap drop parentsdivorced 
decode v_706, gen(parentsdivorced )
replace parentsdivorced  = "sind geschieden" if v_706 == 1
replace parentsdivorced  = "sind nicht geschieden" if v_706 == 2
replace parentsdivorced  = "waren nie verheiratet" if v_706 == 3
replace parentsdivorced  = "" if v_706 == 99

cap drop family2
gen str family2 = ""
replace family2 = "Meine Eltern" + " " + parentsdivorced + "."
replace family2 = "" if v_706 == 99



* family3
cap drop children
tostring v_144_merged, gen(children)
replace children  = "keine" if v_144_merged == 0
replace children  = "zwei" if v_144_merged == 2
replace children  = "drei" if v_144_merged == 3
replace children  = "vier" if v_144_merged == 4
replace children  = "fünf" if v_144_merged == 5
replace children  = "sechs" if v_144_merged == 6
replace children  = "sieben" if v_144_merged == 7
replace children  = "acht" if v_144_merged == 8
replace children  = "neun" if v_144_merged == 9
replace children  = "zehn" if v_144_merged == 10
replace children  = "elf" if v_144_merged == 11
replace children  = "zwölf" if v_144_merged == 12
replace children = children + " " + "eigene Kinder"
replace children  = "ein eigenes Kind" if v_144_merged == 1

cap drop family3
gen str family3 = ""
replace family3 = "Ich habe" + " " + children + "."
replace family3 = "" if v_144_merged == .




* family4
cap drop siblings
tostring v_707, gen(siblings)
replace siblings  = "keine" if v_707 == 0
replace siblings  = "zwei" if v_707 == 2
replace siblings  = "drei" if v_707 == 3
replace siblings  = "vier" if v_707 == 4
replace siblings  = "fünf" if v_707 == 5
replace siblings  = "sechs" if v_707 == 6
replace siblings  = "sieben" if v_707 == 7
replace siblings  = "acht" if v_707 == 8
replace siblings  = "neun" if v_707 == 9
replace siblings  = "zehn" if v_707 == 10
replace siblings  = "elf" if v_707 == 11
replace siblings  = "zwölf" if v_707 == 12 
replace siblings = siblings + " " + "Geschwister."
replace siblings  = "ein Geschwisterkind." if v_707 == 1
replace siblings  = "" if v_707 == -66
replace siblings  = "" if v_707 == -99

cap drop family4
gen str family4 = ""
replace family4 = "Ich habe" + " " + siblings




* family5
cap drop pets 
cap drop pets_help
egen pets_help = anymatch(v_713-v_890), values(1)
gen str pets = ""
replace pets = "die folgenden Haustiere:" if pets_help == 1
replace pets = "keine Haustiere." if pets_help != 1

foreach pet in "Katze" "Hund" "Fische" "Reptilien" "Vögel" "Nagetiere" {
	cap drop pet_`pet'
	gen str pet_`pet' = ""
}

cap drop pet_Andere
gen str pet_Andere = ""

replace pet_Katze = "Katze; " if v_713 == 1
replace pet_Hund = "Hund; " if v_714 == 1
replace pet_Fische = "Fische; " if v_715 == 1
replace pet_Reptilien = "Reptilien; " if v_716 == 1
replace pet_Vögel = "Vögel; " if v_717 == 1
replace pet_Nagetiere = "Nagetiere; " if v_718 == 1
replace pet_Andere = v_891_print if v_890 == 1

cap drop pets
gen str pets = ""
replace pets = pet_Katze + pet_Hund + pet_Fische + pet_Reptilien + pet_Vögel + pet_Nagetiere
replace pets = subinstr(pets, ";", ",",.) 

cap drop otherpets
gen str otherpets = v_891_print if v_890 == 1

cap drop family5
gen str family5 = ""
replace family5 = "Ich habe" + " " + pets + " " + pet_Katze + pet_Hund + pet_Fische + pet_Reptilien + pet_Vögel + pet_Nagetiere + pet_Andere





* family6
cap drop education 
decode v_724, gen(education )
replace education  = "Mein höchster beruflicher Schulabschluss ist ein Berufsschulabschluss." if v_724 == 1
replace education  = "Mein höchster beruflicher Schulabschluss ist eine Lehre." if v_724 == 2
replace education  = "Mein höchster beruflicher Schulabschluss ist ein Meister-/Technikerabschluss." if v_724 == 3
replace education  = "Mein höchster beruflicher Schulabschluss ist ein Fachhochschulabschluss." if v_724 == 4
replace education  = "Mein höchster beruflicher Schulabschluss ist ein Hochschulabschluss." if v_724 == 5
replace education  = "Mein höchster beruflicher Schulabschluss ist" + " " + v_725_print + "." if v_724 == 6
replace education  = "Ich habe bislang keinen beruflichen Schulabschluss." if v_724 == 7

cap drop college 
gen str college  = ""
replace college  = "Studiert habe ich hier:" + " " + v_726_print
replace college  = " " if v_726_print == " " | v_726_print == "NA" | v_726_print == "-66"

cap drop family6
gen str family6 = ""
replace family6 = education + " " + college





* family7
cap drop military 
gen str military  = ""
replace military  = "Ich habe beim Militär" if v_722 == 1
replace military  = "Ich habe nicht beim Militär gedient." if v_722 == 2

cap drop militarybranch 
gen str militarybranch  = ""
replace militarybranch  = "im Heer gedient." if v_723 == 1
replace militarybranch  = "in der Marine gedient." if v_723 == 2
replace militarybranch  = "in der Luftwaffe gedient." if v_723 == 3
replace militarybranch  = "im Sänitätsdienst gedient." if v_723 == 4
replace militarybranch  = "in der Streitkräftebasis gedient." if v_723 == 5
replace militarybranch  = "im Bereich Cyber gedient." if v_723 == 6

cap drop family7
gen str family7 = ""
replace family7 = military  + " " + militarybranch 






* family8
cap drop caregiver 
gen str caregiver  = ""
replace caregiver  = "Pflegetätigkeit" if v_728 == 1
replace caregiver  = "keine Pflegetätigkeit" if v_728 == 2

cap drop family8
gen str family8 = ""
replace family8 = "Ich verrichte momentan" + " " + caregiver  + " " + "für einen alten, kranken oder behinderten Menschen."





* family9
cap drop lossfriend 
gen str lossfriend  = ""
replace lossfriend  = "bereits den Verlust" if v_729 == 1
replace lossfriend  = "bislang noch keinen Verlust" if v_729 == 2

cap drop family9
gen str family9 = ""
replace family9 = "In meinem Leben habe ich" + " " + lossfriend  + " " + "einer bedeutenden Person erlebt."





* family10
cap drop gayfriends 
gen str gayfriends  = ""
replace gayfriends  = "Ich habe Freunde in der LGBT-Community." if v_727 == 1
replace gayfriends  = "Ich habe keine Freunde in der LGBT-Community." if v_727 == 2
replace gayfriends  = "NA." if v_727 == 3

cap drop family10
gen str family10 = ""
replace family10 = gayfriends 
replace family10 = "" if v_727 == 3



* ---------------------------------------------------------------------------- *


*** Finanzen ***
* <i class="fa-solid fa-money-check-dollar"></i>

* finance1
cap drop incomeclass  
decode v_733, gen(incomeclass )
replace incomeclass  = "oberen Mittelschicht" if incomeclass  == "obere Mittelschicht"
replace incomeclass  = "unteren Mittelschicht" if incomeclass  == "untere Mittelschicht"

cap drop finance1
gen str finance1 = ""
replace finance1 = "Ich zähle mich aktuell zur" + " " + incomeclass  + "."



* finance2
decode v_734, gen(incomeclasschild  )
replace incomeclasschild   = "oberen Mittelschicht" if incomeclasschild   == "obere Mittelschicht"
replace incomeclasschild   = "unteren Mittelschicht" if incomeclasschild   == "untere Mittelschicht"

cap drop finance2
gen str finance2 = ""
replace finance2 = "In meiner Kindheit würde ich mein Elternhaus der" + " " + incomeclasschild + " " + "zuordnen."



* finance3
cap drop incomeclassfuture   
decode v_735, gen(incomeclassfuture )
replace incomeclassfuture  = "oberen Mittelschicht" if incomeclassfuture  == "obere Mittelschicht"
replace incomeclassfuture  = "unteren Mittelschicht" if incomeclassfuture  == "untere Mittelschicht"

cap drop finance3
gen str finance3 = ""
replace finance3 = "Für die Zukunft denke ich, dass ich mich in der" + " " + incomeclassfuture  + " " + "wiederfinden werde."




* finance4
cap drop incomeclassPastDirection 
gen str incomeclassPastDirection  = ""
replace incomeclassPastDirection  = "ein Aufstieg" if v_733 > v_734
replace incomeclassPastDirection  = "ein Abstieg" if v_734 > v_733
replace incomeclassPastDirection  = "keine Veränderung" if v_733 == v_734

cap drop finance4
gen str finance4 = ""
replace finance4 = "Im Vergleich zu meinen Eltern ist dies" + " " + incomeclassPastDirection + "."



* finance5
cap drop incomeclassFutureDirection  
gen str incomeclassFutureDirection   = ""
replace incomeclassFutureDirection   = "ein Aufstieg" if v_733 > v_735
replace incomeclassFutureDirection   = "ein Abstieg" if v_735 > v_733
replace incomeclassFutureDirection   = "keine Veränderung" if v_733 == v_735

cap drop finance5
gen str finance5 = ""
replace finance4 = "Im Vergleich zu meinen Eltern ist dies" + " " + incomeclassFutureDirection  + "."



* finance6
cap drop owncar  
decode v_731, gen(owncar )
replace owncar  = "ein eigenes Auto" if v_731 == 1
replace owncar  = "kein eigenes Auto" if v_731 == 2

cap drop finance6
gen str finance6 = ""
replace finance6 = "Ich besitze" + " " + owncar  + "." 



* finance7
cap drop ownhouse 
gen str ownhouse  = ""
replace ownhouse  = "Wohneigentum" if v_730 == 1
replace ownhouse  = "kein Wohneigentum" if v_730 == 2

cap drop finance7
gen str finance7 = ""
replace finance7 = "Ich besitze" + " " + ownhouse  + "." 



* ---------------------------------------------------------------------------- *


*** Persönlichkeit ***
* <i class="fa-solid fa-face-smile"></i>


* personality1
cap drop patient 
gen str patient  = ""
replace patient  = "ein geduldiger" if v_742 == 1
replace patient  = "kein geduldiger" if v_742 == 2

cap drop personality1
gen str personality1 = ""
replace personality1 = "Ich würde sagen, dass ich eher" + " " + patient + " " + "Mensch bin."




* personality2
cap drop confrontational 
gen str confrontational  = ""
replace confrontational  = "eher ein konfrontativer Mensch bin." if v_745 == 1
replace confrontational  = "eher kein konfrontativer Mensch bin." if v_745 == 2

cap drop personality2
gen str personality2 = ""
replace personality2 = "Man könnte sagen, dass ich" + " " + confrontational





* personality3
cap drop messy 
gen str messy  = ""
replace messy  = "sehr chaotisch" if v_743 == 1
replace messy  = "eher chaotisch" if v_743 == 2
replace messy  = "überhaupt nicht chaotisch" if v_743 == 3

cap drop personality3
gen str personality3 = ""
replace personality3 = "Meine Freunde und Familienmitglieder würden wahrscheinlich sagen, dass ich" + " " + messy  + " " + "bin."




* personality4
cap drop energetic 
gen str energetic  = ""
replace energetic  = "würde ich mich als hyperaktiv beschreiben." if v_739 == 1
replace energetic  = "würde ich mich als recht aktiv beschreiben." if v_739 == 2
replace energetic  = "bin ich nur gelegentlich aktiv." if v_739 == 3
replace energetic  = "bewege ich mich eigentlich eher wenig." if v_739 == 4

cap drop personality4
gen str personality4 = ""
replace personality4 = "Grundsätzlich" + " " + energetic




* personality5
cap drop competitive 
gen str competitive  = ""
replace competitive  = "An Wettkämpfen nehme ich generell sehr gerne teil." if v_740 == 1
replace competitive  = "An Wettkämpfen nehme ich generell überhaupt nicht gerne teil." if v_740 == 2 

cap drop personality5
gen str personality5 = ""
replace personality5 = competitive





* personality6
cap drop perfectionist 
gen str perfectionist  = ""
replace perfectionist  = "Ansonsten würde ich mich selbst als ziemlichen Perfektionist beschreiben." if v_741 == 1 & v_16 == 2
replace perfectionist  = "Ansonsten würde ich mich selbst als ziemliche Perfektionistin beschreiben." if v_741 == 1 & v_16 == 1
replace perfectionist  = "Ansonsten würde ich mich selbst nicht als Perfektionist beschreiben." if v_741 == 2 & v_16 == 2
replace perfectionist  = "Ansonsten würde ich mich selbst nicht als Perfektionistin beschreiben." if v_741 == 2 & v_16 == 1

cap drop personality6
gen str personality6 = ""
replace personality6 = perfectionist




* personality7
cap drop workorplay 
gen str workorplay  = ""
replace workorplay  = "Arbeiten" if v_738 == 1
replace workorplay  = "Freizeit" if v_738 == 2

cap drop personality7
gen str personality7 = ""
replace personality7 = "Wenn ich mich zwischen Arbeiten und Freizeit entscheiden müsste, würde ich ganz klar" + " " + workorplay  + " " + "wählen." 




* personality8
cap drop carebody 
gen str carebody  = ""
replace carebody  = "Ein gepflegtes Äußeres ist mir persönlich schon wichtig." if v_744 == 1
replace carebody  = "Ein gepflegtes Äußeres ist mir persönlich nicht allzu wichtig." if v_744 == 2

cap drop personality8
gen str personality8 = ""
replace personality8 = carebody 





* personality9
cap drop fascination 
gen str fascination  = ""
replace fascination  = "Sternen und Galaxien" if v_746 == 1
replace fascination  = "technologischem Fortschritt" if v_746 == 2
replace fascination  = "alten Zivilisationen" if v_746 == 3
replace fascination  = "der Natur und Tierwelt" if v_746 == 4

cap drop personality9
gen str personality9 = ""
replace personality9 = "Ich bin fasziniert von" + " " + fascination  + "."




* personality10
cap drop fairies 
gen str fairies  = ""
replace fairies  = "Auch wenn ich weiß, dass es viele Menschen komisch finden, wünsche ich mir manchmal, dass fantastische Kreaturen real wären." if v_747 == 1
replace fairies  = "Mit fantastischen Kreaturen und solchen Dingen kann ich überhaupt nichts anfangen." if v_747 == 2

cap drop personality10
gen str personality10 = ""
replace personality10 = fairies 



* ---------------------------------------------------------------------------- *


*** Verhalten ***
* <i class="fa-solid fa-head-side"></i>


* behavior1
cap drop foodback 
gen str foodback  = ""
replace foodback   = "Ich bin ein Mensch, der auch mal Essen im Restaurant zurückgeben lässt, wenn es nicht schmeckt." if v_752 == 1
replace foodback  = "Ich bin kein Mensch, der Essen im Restaurant zurückgeben lässt, wenn es nicht schmeckt." if v_752 == 2

cap drop behavior1
gen str behavior1 = ""
replace behavior1 = foodback 





* behavior2
cap drop giveaway 
gen str giveaway  = ""
replace giveaway  = "Ich habe kein Problem damit, Dinge einfach wegzuwerfen oder zu verschenken, wenn ich sie selbst nicht mehr verwende." if v_750 == 1
replace giveaway  = "Ich habe große Problem damit, Dinge wegzuwerfen oder zu verschenken, sogar dann, wenn ich sie selbst überhaupt nicht mehr verwende." if v_750 == 2

cap drop behavior2
gen str behavior2 = ""
replace behavior2 = giveaway 





* behavior3
cap drop streetfurniture  
gen str streetfurniture  = ""
replace streetfurniture  = "Ich bin ein Mensch, der auch mal Möbel vom Sperrmüll mit nach Hause mit." if v_749 == 1
replace streetfurniture  = "Ich bin kein Mensch, der Möbel vom Sperrmüll mit nach Hause nehmen würde." if v_749 == 2

cap drop behavior3
gen str behavior3 = ""
replace behavior3 = streetfurniture 






* behavior4
cap drop giftrecycle 
gen str giftrecycle  = ""
replace giftrecycle  = "Wenn mir ein Geschenk selbst nicht gefällt, schenke ich es manchmal auch einfach weiter." if v_753 == 1
replace giftrecycle  = "Auch wenn mir ein Geschenk selbst nicht gefällt, würde ich es niemals einfach weiter verschenken." if v_753 == 2

cap drop behavior4
gen str behavior4 = ""
replace behavior4 = giftrecycle 






* behavior5
cap drop snooze 
gen str snooze  = ""
replace snooze  = "null Mal betätige, sondern sofort aufstehe" if v_748 == 1
replace snooze  = "ein oder zwei Mal betätige bevor ich aufstehe" if v_748 == 2
replace snooze  = "dreimal oder häufiger betätige bevor ich aufstehe" if v_748 == 3
replace snooze  = "" if v_748 == 99

cap drop behavior5
gen str behavior5 = ""
replace behavior5 = "Die Schlummertaste beim Wecker kann ja Fluch und Segen zugleich sein. Bei mir ist es so, dass ich die Schlummertaste meistens" + " " + snooze + "."
replace behavior5 = "" if v_748 == 99 





* behavior6
cap drop stoleglass 
gen str stoleglass  = ""
replace stoleglass  = "Ich finde, dass man es mit den Regeln nicht unbedingt immer ganz genau nehmen muss. Ich habe ehrlich gesagt auch schon mal ein Glas aus einer Bar gestohlen." if v_751 == 1
replace stoleglass  = "Ich finde, dass auch kleinere Diebstähle kein Kavaliersdelikt sind. Ich habe zum Beispiel noch nie ein Glas aus einer Bar gestohlen." if v_751 == 2
replace stoleglass  = "" if v_751 == 3

cap drop behavior6
gen str behavior6 = ""
replace behavior6 = stoleglass 
replace behavior6 = "" if v_751 == 3






* behavior7
cap drop profanelanguage 
gen str profanelanguage  = ""
replace profanelanguage  = "ich nie" if v_754 == 1
replace profanelanguage  = "auch ich gelegentlich" if v_754 == 2
replace profanelanguage  = "auch ich durchaus oft" if v_754 == 3
replace profanelanguage  = "auch ich regelmäßig" if v_754 == 4

cap drop behavior7
gen str behavior7 = ""
replace behavior7 = "Für viele Menschen gehören Schimpfwörter zum normalen Sprachgebrauch einfach dazu. Bei mir ist es so, dass" + " " + profanelanguage  + " " + "Schimpfwörter verwende."





* behavior8
cap drop readhoroscope 
gen str readhoroscope  = ""
replace readhoroscope  = "ich mir mein Horoskop schon gerne jeden Tag durchlese" if v_755 == 1
replace readhoroscope  = "ich mir mein Horoskop zumindest einmal die Woche schon ganz gerne durchlese" if v_755 == 2
replace readhoroscope  = "auch ich mir mein Horoskop schon mal gelegentlich ganz gerne durchlese" if v_755 == 3
replace readhoroscope  = "ich damit nichts anfangen kann und ich mir mein Horoskop wirklich nie durchlese" if v_755 == 4

cap drop behavior8
gen str behavior8 = ""
replace behavior8 = "Horoskope sind ja so eine Sache. Ich muss sagen, dass" + " " + readhoroscope  + "."



* ---------------------------------------------------------------------------- *

*** Dinge, die ich mag ***
* <i class="fa-solid fa-head-side-heart"></i>



* taste0
cap drop str_v_756
decode v_756, gen(str_v_756)
cap drop color 
gen str color = ""
replace color = lower(str_v_756)

cap drop othercolor 
gen str othercolor = ""
replace othercolor = lower(v_893_print)

cap drop color_help 
gen str color_help = ""
replace color_help = color
replace color_help = othercolor if v_756 == 12

cap drop taste0
gen str taste0 = ""
replace taste0 = "Meine Lieblingsfarbe ist:" + " " + color_help + "." 



* taste1
cap drop str_v_758
decode v_758, gen(str_v_758)
cap drop food 
gen str food  = ""
replace food  = str_v_758

cap drop otherfood 
gen str otherfood  = ""
replace otherfood = v_894_print
replace otherfood = "" if v_894_print == "-99"

cap drop food_help
gen str food_help  = ""
replace food_help = "Ich esse am liebsten" + " " + food + "." if v_758 != 16 &  v_758 != 99
replace food_help = "Am liebsten esse ich:" + " " +  otherfood + "." if v_758 == 16 &  v_758 != 99
replace food_help = "Ich habe kein bestimmtes Lieblingsessen." if v_758 == 99
replace food_help = "" if v_894_print == " "

cap drop taste1
gen str taste1 = ""
replace taste1 = food_help 




* taste2
cap drop spicyfood 
gen str spicyfood  = ""
replace spicyfood  = "Ich mag scharfes Essen grundsätzlich nicht." if v_760 ==1
replace spicyfood  = "Ich mag mein Essen gerne würzig, aber nicht zu viel." if v_760 ==2
replace spicyfood  = "Ich mag scharfes Essen." if v_760 ==3
replace spicyfood  = "Mir kann das Essen gar nicht scharf genug sein." if v_760 ==4

cap drop taste2
gen str taste2 = ""
replace taste2 = spicyfood 




* taste3
cap drop vegetarian 
gen str vegetarian  = ""
replace vegetarian  = "Ich esse grundsätzlich kein Fleisch." if v_761 == 1
replace vegetarian  = "Ich esse ganz gerne auch mal Gerichte mit Fleisch." if v_761 == 2

cap drop taste3
gen str taste3 = ""
replace taste3 = vegetarian 





* taste4
cap drop countriesvisited 
gen str countriesvisited  = ""
replace countriesvisited  = "Bislang war ich noch nie außerhalb Deutschlands." if v_762 == 0
replace countriesvisited  = "Ich habe bislang ein bis zwei andere Länder bereist." if v_762 == 1
replace countriesvisited  = "Ich habe bislang drei bis fünf andere Länder bereist." if v_762 == 2
replace countriesvisited  = "Ich habe bislang sechs bis zehn andere Länder bereist." if v_762 == 3
replace countriesvisited  = "Ich habe bislang schon mehr als zehn andere Länder bereist." if v_762 == 4

cap drop taste4
gen str taste4 = ""
replace taste4 = countriesvisited



* taste5
cap drop vacation 
gen str vacation  = ""
replace vacation  = v_763_print
replace vacation  = ustrtitle(vacation)

cap drop taste5
gen str taste5 = ""
replace taste5 = "Mein absolutes Traumland für eine Reise wäre:" + " " + vacation
replace taste5 = "" if vacation == " " 



* ---------------------------------------------------------------------------- *

*** Dinge, die ich mache ***
* <i class="fa-solid fa-head-side-heart"></i>


* thingsyoudo0
cap drop socialmedia 
gen str socialmedia  = ""
replace socialmedia  = "sehr aktiv" if v_764 == 1
replace socialmedia  = "etwas aktiv" if v_764 == 2
replace socialmedia  = "kaum aktiv" if v_764 == 3
replace socialmedia  = "gar nicht aktiv" if v_764 == 4

cap drop thingsyoudo0
gen str thingsyoudo0 = ""
replace thingsyoudo0 = "Ob ich aktiv Social Media verwende? Ich würde sagen, dass ich in Social Media" + " " + socialmedia  + " " + "bin."





* thingsyoudo1
cap drop fashion  
gen str fashion   = ""
replace fashion   = "sehr" if v_765 == 1
replace fashion   = "etwas" if v_765 == 2
replace fashion   = "nicht viel" if v_765 == 3

cap drop thingsyoudo1
gen str thingsyoudo1 = ""
replace thingsyoudo1 = "Für neue Modetrends oder schicke Klamotten und Accessoires interessiere ich mich" + " " + fashion + "."







* thingsyoudo2
cap drop smoke 
gen str smoke  = ""
replace smoke  = "Ich bin Raucher." if v_766 == 1 & v_16 == 2
replace smoke  = "Ich bin Raucherin." if v_766 == 1 & v_16 == 1
replace smoke  = "Ich bin Raucher, aber nur in Gesellschaft." if v_766 == 2 & v_16 == 2
replace smoke  = "Ich bin Raucherin, aber nur in Gesellschaft." if v_766 == 2 & v_16 == 1
replace smoke  = "Ich bin Nichtraucher." if v_766 == 3 & v_16 == 2
replace smoke  = "Ich bin Nichtraucherin." if v_766 == 3 & v_16 == 1
replace smoke  = "Ich bin Nichtraucher, habe aber früher mal geraucht." if v_766 == 4 & v_16 == 2
replace smoke  = "Ich bin Nichtraucherin, habe aber früher mal geraucht." if v_766 == 4 & v_16 == 1

cap drop thingsyoudo2
gen str thingsyoudo2 = ""
replace thingsyoudo2 = smoke 





* thingsyoudo3
cap drop sportdo 
gen str sportdo  = ""
replace sportdo  = "auch selbst aktiv Sport. Momentan vor allem:" if v_792 == 0
replace sportdo  = "selbst aktiv keinen Sport." if v_792 == 1


foreach sport in "Fußball" "Baseball" "Basketball" "Volleyball" "Tennis" "Eishockey" "Cricket" "Football" "Feldhockey" "Radfahren" "Leichtathletik" "Tischtennis" "Laufen" "Kampfsport" "Klettern" "Skifahren" "Yoga" "Schwimmen" "Angeln" "Sonstiges" {
	cap drop sport_`sport'
	gen str sport_`sport' = ""
}

cap drop sport_Andere
gen str sport_Andere = ""

replace sport_Fußball = "Fußball; " if v_772 == 1
replace sport_Baseball = "Baseball; " if v_773 == 1
replace sport_Basketball = "Basketball; " if v_774 == 1
replace sport_Volleyball = "Volleyball; " if v_775 == 1
replace sport_Tennis = "Tennis; " if v_776 == 1
replace sport_Eishockey = "Eishockey; " if v_777 == 1
replace sport_Cricket = "Cricket; " if v_778 == 1
replace sport_Football = "American Football; " if v_779 == 1
replace sport_Feldhockey = "Feldhockey; " if v_780 == 1
replace sport_Radfahren = "Radfahren; " if v_781 == 1
replace sport_Leichtathletik = "Leichtathletik; " if v_782 == 1
replace sport_Tischtennis = "Tischtennis; " if v_783 == 1
replace sport_Laufen = "Laufen; " if v_784 == 1
replace sport_Kampfsport = "Kampfsport; " if v_785 == 1
replace sport_Klettern = "Klettern; " if v_786 == 1
replace sport_Skifahren = "Skifahren; " if v_787 == 1
replace sport_Yoga = "Yoga; " if v_788 == 1
replace sport_Schwimmen = "Schwimmen; " if v_789 == 1
replace sport_Angeln = "Angeln; " if v_790 == 1
replace sport_Andere = v_897_print if v_896 == 1

cap drop sportdo
gen str sportdo = ""
replace sportdo = sport_Fußball + sport_Baseball + sport_Basketball + sport_Volleyball + sport_Tennis + sport_Eishockey + sport_Cricket + sport_Football + sport_Feldhockey + sport_Radfahren + sport_Leichtathletik + sport_Tischtennis + sport_Laufen + sport_Kampfsport + sport_Klettern + sport_Skifahren + sport_Yoga + sport_Schwimmen + sport_Angeln
replace sportdo = subinstr(sportdo, ";", ",",.) 

cap drop othersportdo 
gen str othersportdo  = v_897_print if v_896 == 1

cap drop thingsyoudo3
gen str thingsyoudo3 = ""
replace thingsyoudo3 = "Ich treibe" + " " + sportdo  + " " + sport_Fußball + sport_Baseball + sport_Basketball + sport_Volleyball + sport_Tennis + sport_Eishockey + sport_Cricket + sport_Football + sport_Feldhockey + sport_Radfahren + sport_Leichtathletik + sport_Tischtennis + sport_Laufen + sport_Kampfsport + sport_Klettern + sport_Skifahren + sport_Yoga + sport_Schwimmen + sport_Angeln + sport_Andere







* thingsyoudo4
cap drop museums 
gen str museums  = ""
replace museums  = "Ich liebe es ins Museum zu gehen." if v_794 == 1
replace museums  = "Ich gehe gern ins Museum, aber nur manchmal." if v_794 == 2
replace museums  = "Ich gehe nicht gern ins Museum." if v_794 == 3

cap drop thingsyoudo4
gen str thingsyoudo4 = ""
replace thingsyoudo4 = museums 






* thingsyoudo5
cap drop dance 
gen str dance  = ""
replace dance  = "Ich liebe es tanzen zu gehen." if v_795 == 1
replace dance  = "Ich gehe gern tanzen, aber nur manchmal." if v_795 == 2
replace dance  = "Ich gehe nicht gern tanzen." if v_795 == 3

cap drop thingsyoudo5
gen str thingsyoudo5 = ""
replace thingsyoudo5 = dance 




* ---------------------------------------------------------------------------- *

*** Dinge, die ich mag ***
* <i class="fa-solid fa-heart-pulse"></i>



* thingsilike1
cap drop musiclisten 
gen str musiclisten  = ""
replace musiclisten  = "Ich liebe es Musik zu hören. Am Liebsten mag ich:" if v_796 == 1 & v_819 != 1
replace musiclisten  = "Ich liebe es Musik zu hören. Eine bestimmte Lieblings-Musikrichtung habe ich aber nicht." if v_796 == 1 & v_819 == 1
replace musiclisten  = "Ich habe es gerne ruhig. Musik hören mag ich gar nicht so gern." if v_796 == 2


foreach musik in "Blues" "Klassik" "Country" "Rock" "HipHop" "Latin" "Pop" "Religiös" "Funk" "R_B" "Rap" "Elektronisch" "Folk" "Jazz" "New_Age" "Reggae" "Andere"  {
	cap drop musik_`musik'
	gen str musik_`musik' = ""
}

cap drop musik_Andere
gen str musik_Andere = ""

replace musik_Blues = "Blues; " if v_802 == 1
replace musik_Klassik = "Klassik; " if v_803 == 1
replace musik_Country = "Country; " if v_804 == 1
replace musik_Rock = "Rock; " if v_805 == 1
replace musik_HipHop = "Hip-Hop; " if v_806 == 1
replace musik_Latin = "Latin; " if v_807 == 1
replace musik_Pop = "Pop; " if v_808 == 1
replace musik_Religiös = "Religiös; " if v_809 == 1
replace musik_Funk = "Funk; " if v_810 == 1
replace musik_R_B = "R&B; " if v_811 == 1
replace musik_Rap = "Rap; " if v_812 == 1
replace musik_Elektronisch = "Elektronisch; " if v_813 == 1
replace musik_Folk = "Folk; " if v_814 == 1
replace musik_Jazz = "Jazz; " if v_815 == 1
replace musik_New_Age = "New Age; " if v_816 == 1
replace musik_Reggae = "Reggae; " if v_817 == 1
replace musik_Andere = v_900_print if v_899 == 1

cap confirm variable music, exact
if _rc==0 {
   drop music
}
gen str music  = ""
replace music  = musik_Blues + musik_Klassik + musik_Country + musik_Rock + musik_HipHop + musik_Latin + musik_Pop + musik_Religiös + musik_Funk + musik_R_B + musik_Rap + musik_Elektronisch + musik_Folk + musik_Jazz + musik_New_Age + musik_Reggae
replace music  = subinstr(music , ";", ",",.) 

cap drop othermusic  
gen str othermusic   = v_900_print if v_899 == 1

cap drop thingsilike1
gen str thingsilike1 = ""
replace thingsilike1 = musiclisten  + " " + musik_Blues + musik_Klassik + musik_Country + musik_Rock + musik_HipHop + musik_Latin + musik_Pop + musik_Religiös + musik_Funk + musik_R_B + musik_Rap + musik_Elektronisch + musik_Folk + musik_Jazz + musik_New_Age + musik_Reggae






* thingsilike2
cap drop bestmusician 
gen str bestmusician  = v_821_print

cap drop thingsilike2
gen str thingsilike2 = ""
replace thingsilike2 = "Ich bin großer Fan von:" + " " + bestmusician  + "."
replace thingsilike2 = "" if bestmusician  == " "
replace thingsilike2 = "" if bestmusician  == "-66"





* thingsilike3
cap drop moviefan 
gen str moviefan  = ""
replace moviefan  = "Ich bin ein großer Filmfan. Am Liebsten mag ich:" if v_822 == 1 & v_840 != 1
replace moviefan  = "Ich bin ein großer Filmfan. Ein bestimmtes Lieblings-Genre habe ich aber nicht." if v_822 == 1 & v_840 == 1
replace moviefan  = "Filme schaue ich nicht sonderlich gern." if v_822 == 2


foreach filme in "Action" "Abenteuer" "Komödie" "Krimi" "Drama" "Fantasy" "Historisch" "Horror" "Mystery" "Politisch" "Romantik" "SciFi" "Thriller" "Krieg" "Western" "Surreal" "Andere"  {
	cap drop filme_`filme'
	gen str filme_`filme' = ""
}

cap drop filme_Andere
gen str filme_Andere = ""

replace filme_Action = "Action; " if v_823 == 1
replace filme_Abenteuer = "Abenteuer; " if v_824 == 1
replace filme_Komödie = "Komödie; " if v_825 == 1
replace filme_Krimi = "Krimi; " if v_826 == 1
replace filme_Drama = "Drama; " if v_827 == 1
replace filme_Fantasy = "Fantasy; " if v_828 == 1
replace filme_Historisch = "Historisch; " if v_829 == 1
replace filme_Horror = "Horror; " if v_830 == 1
replace filme_Mystery = "Mystery; " if v_831 == 1
replace filme_Politisch = "Politisch; " if v_832 == 1
replace filme_Romantik = "Romantik; " if v_833 == 1
replace filme_SciFi = "SciFi; " if v_834 == 1
replace filme_Thriller = "Thriller; " if v_835 == 1
replace filme_Krieg = "Krieg; " if v_836 == 1
replace filme_Western = "Western; " if v_837 == 1
replace filme_Surreal = "Surreal; " if v_838 == 1

replace filme_Andere = v_903_print if v_902 == 1
replace filme_Andere = "" if v_903_print == " "
replace filme_Andere = "" if v_903_print == ""

cap confirm variable movie, exact
if _rc==0 {
   drop movie
}
gen str movie = ""
replace movie = filme_Action + filme_Abenteuer + filme_Komödie + filme_Krimi + filme_Drama + filme_Fantasy + filme_Historisch + filme_Horror + filme_Mystery + filme_Politisch + filme_Romantik + filme_SciFi + filme_Thriller + filme_Krieg + filme_Western + filme_Surreal
replace movie = subinstr(movie  , ";", ",",.) 

cap drop othermovie   
gen str othermovie = ""
replace othermovie = v_903_print if v_903_print == " "
replace othermovie = v_903_print if v_903_print == ""

cap drop thingsilike3
gen str thingsilike3 = ""
replace thingsilike3 = moviefan  + " " + filme_Action + filme_Abenteuer + filme_Komödie + filme_Krimi + filme_Drama + filme_Fantasy + filme_Historisch + filme_Horror + filme_Mystery + filme_Politisch + filme_Romantik + filme_SciFi + filme_Thriller + filme_Krieg + filme_Western + filme_Surreal





* thingsilike4
cap drop bestmovie 
gen str bestmovie  = v_842_print
replace bestmovie  = "" if v_842_print == "-66"
replace bestmovie  = "" if v_842_print == "-99"

cap drop thingsilike4
gen str thingsilike4 = ""
replace thingsilike4 = "Mein absoluter Lieblingsfilm ist" + " " + "'" + bestmovie + "'" + "."
replace thingsilike4 = "" if bestmovie == " "




* thingsilike5
cap drop bestactor 
gen str bestactor  = v_843_print
replace bestactor  = "" if v_843_print == "-66"
replace bestactor  = "" if v_843_print == "-99"

cap drop thingsilike5
gen str thingsilike5 = ""
replace thingsilike5 = "Am liebsten sehe ich Filme mit" + " " + bestactor + "."
replace thingsilike5 = "" if bestactor == " "
replace thingsilike5 = "" if bestactor == ""





* thingsilike6
cap drop sportfan 
gen str sportfan  = ""
replace sportfan  = "Ich bin ein riesiger Sportfan. Am meisten kann ich mich begeistern für:" if v_844 == 1 & v_862 != 1
replace sportfan  = "Ich bin ein riesiger Sportfan. Es gibt aber jetzt nicht die eine Sportart, die ich besonders gerne mag." if v_844 == 1 & v_862 == 1
replace sportfan  = "Als Fan bin ich für Sport nicht zu begeistern." if v_844 == 2


foreach fan in"Golf" "Fußball" "Baseball" "Basketball" "Volleyball" "Tennis" "Eishockey" "Cricket" "American_Football" "Feldhockey" "Nascar" "Formel_1" "Radfahren" "Darts" "Snooker" "Boxen" "Andere"  {
	cap drop fan_`fan'
	gen str fan_`fan' = ""
}

cap drop fan_Andere
gen str fan_Andere = ""

replace fan_Golf = "Golf; " if v_845 == 1
replace fan_Fußball = "Fußball; " if v_846 == 1
replace fan_Baseball = "Baseball; " if v_847 == 1
replace fan_Basketball = "Basketball; " if v_848 == 1
replace fan_Volleyball = "Volleyball; " if v_849 == 1
replace fan_Tennis = "Tennis; " if v_850 == 1
replace fan_Eishockey = "Eishockey; " if v_851 == 1
replace fan_Cricket = "Cricket; " if v_852 == 1
replace fan_American_Football = "American Football; " if v_853 == 1
replace fan_Feldhockey = "Feldhockey; " if v_854 == 1
replace fan_Nascar = "Nascar; " if v_855 == 1
replace fan_Formel_1 = "Formel 1; " if v_856 == 1
replace fan_Radfahren = "Radfahren; " if v_857 == 1
replace fan_Darts = "Darts; " if v_858 == 1
replace fan_Snooker = "Snooker; " if v_859 == 1
replace fan_Boxen = "Boxen; " if v_860 == 1

replace fan_Andere = v_906_print if v_905 == 1
replace fan_Andere = "" if v_906_print == " "
replace fan_Andere = "" if v_906_print == ""

cap drop sportfollow   
gen str sportfollow  = ""
replace sportfollow  = fan_Golf + fan_Fußball + fan_Baseball + fan_Basketball + fan_Volleyball + fan_Tennis + fan_Eishockey + fan_Cricket + fan_American_Football + fan_Feldhockey + fan_Nascar + fan_Formel_1 + fan_Radfahren + fan_Darts + fan_Snooker + fan_Boxen
replace sportfollow  = subinstr(sportfollow, ";", ",",.) 

cap drop othersportfollow    
gen str othersportfollow = ""
replace othersportfollow = v_906_print if v_906_print == " "
replace othersportfollow = v_906_print if v_906_print == ""

cap drop thingsilike6
gen str thingsilike6 = ""
replace thingsilike6 = sportfan + " " + fan_Golf + fan_Fußball + fan_Baseball + fan_Basketball + fan_Volleyball + fan_Tennis + fan_Eishockey + fan_Cricket + fan_American_Football + fan_Feldhockey + fan_Nascar + fan_Formel_1 + fan_Radfahren + fan_Darts + fan_Snooker + fan_Boxen + fan_Andere




* thingsilike7
cap drop bestteam 
gen str bestteam  = v_864_print

cap drop thingsilike7
gen str thingsilike7 = ""
replace thingsilike7 = "Ich bin großer Fan von" + " " + bestteam  + "."
replace thingsilike7 = "" if bestteam  == "-66"
replace thingsilike7 = "" if bestteam  == " "






* thingsilike8
cap drop watchtv 
gen str watchtv  = ""
replace watchtv  = "Ich schaue auch gerne mal Fernsehen." if v_865 == 1 
replace watchtv  = "Fernsehen schaue ich momentan eigentlich gar nicht." if v_865 == 2

cap drop thingsilike8
gen str thingsilike8 = ""
replace thingsilike8 = watchtv 




* thingsilike9
cap drop tvshows 
gen str tvshows  = v_866_print
replace tvshows  = "Im Moment gefällt mir im Fernsehen am besten" + " " + "'" + tvshows + "'" + "."
replace tvshows  = "" if v_866_print == "-66"
replace tvshows = "" if v_866_print == ""
replace tvshows = "" if v_866_print == " "

cap drop thingsilike9
gen str thingsilike9 = ""
replace thingsilike9 = tvshows 




* thingsilike10
cap drop readbooks 
gen str readbooks  = ""
replace readbooks  = "Ich genieße es auch sehr mich in ein gutes Buch zu vertiefen." if v_867 == 1 
replace readbooks  = "Bücher lese ich momentan so gut wie gar nicht." if v_867 == 2

cap drop thingsilike10
gen str thingsilike10 = ""
replace thingsilike10 = readbooks 




* thingsilike11
cap drop books 
gen str books  = v_868_print
replace books  = "Besonders gern lese ich im Moment" + " " + "'" + books + "'" + "."
replace books  = "" if v_868_print == "-66"
replace books  = "" if v_868_print == ""
replace books  = "" if v_868_print == " "

cap drop thingsilike11
gen str thingsilike11 = ""
replace thingsilike11 = books 




* thingsilike12
cap drop followwebchannels 
gen str followwebchannels  = ""
replace followwebchannels  = "Neue Medien nutze ich auch sehr gerne und folge selbst auch einigen Webkanälen." if v_869 == 1 
replace followwebchannels  = "Der Hype um diese ganzen neuen Webkanäle und Plattformen lässt mich ziemlich kalt. Ich folge auf jeden Fall niemandem." if v_869 == 2

cap drop thingsilike12
gen str thingsilike12 = ""
replace thingsilike12 = followwebchannels 




* thingsilike13
cap drop webchannels 
gen str webchannels  = v_870_print
replace webchannels  = "Bei den Webchannels interessiere ich mich insbesondere für" + " " + "'" + webchannels + "'" + "."
replace webchannels  = "" if v_870_print == "-66"
replace webchannels  = "" if v_870_print == " "
replace webchannels  = "" if v_870_print == ""

cap drop thingsilike13
gen str thingsilike13 = ""
replace thingsilike13 = webchannels 




* thingsilike14
cap drop playvideogames  
gen str playvideogames   = ""
replace playvideogames   = "Ab und zu spiele ich auch ganz gerne Videospiele." if v_871 == 1 
replace playvideogames   = "Videospiele und solchen Sachen sind nichts für mich." if v_871 == 2

cap drop thingsilike14
gen str thingsilike14 = ""
replace thingsilike14 = playvideogames




* thingsilike15
cap drop videogames 
gen str videogames  = v_872_print
replace videogames  = "Mein liebstes Videospiel ist momentan" + " " + "'" + videogames + "'" + "."
replace videogames  = "" if v_872_print == "-66"
replace videogames  = "" if v_872_print == " "
replace videogames  = "" if v_872_print == ""

cap drop thingsilike15
gen str thingsilike15 = ""
replace thingsilike15 = videogames 



* thingsilike16
cap drop docreative 
gen str docreative  = ""
replace docreative  = "Ich liebe es mich kreativ zu betätigen." if v_873 == 1 
replace docreative  = "Kreative Tätigkeiten wie Malen oder Singen sind nicht gerade mein Ding." if v_873 == 2

cap drop thingsilike16
gen str thingsilike16 = ""
replace thingsilike16 = docreative




* thingsilike17
cap drop creative 
gen str creative  = v_874_print
replace creative  = "Viel Freude macht mir auch" + " " + "'" + creative  + "'" + "."
replace creative  = "" if v_874_print == "-66"
replace creative  = "" if v_874_print == ""
replace creative  = "" if v_874_print == " "

cap drop thingsilike17
gen str thingsilike17 = ""
replace thingsilike17 = creative 



* ---------------------------------------------------------------------------- *

*** Ein interessanter Fakt über mich ***
* <i class="fa-solid fa-face-surprise"></i>

* quirk1
cap drop quirk1 
gen str quirk1 = v_876
replace quirk1 = "" if v_876 == "-66"
replace quirk1 = "" if v_876 == "-99"


* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *

* Identifier same political view
cap drop essay_vac_opinion_prior
gen essay_vac_opinion_prior = .
replace essay_vac_opinion_prior = 1 if v_633_NEU <= 5 & (c_0115 == "3" | c_0115 == "4")
replace essay_vac_opinion_prior = 2 if v_633_NEU > 5 & (c_0115 == "3" | c_0115 == "4")
label var essay_vac_opinion_prior "Essay-Vaccination - opinion"
label define essay_vac_opinion_prior 1 "In Favor" 2 "Against"
label val  essay_vac_opinion_prior essay_opinion_prior


cap drop essay_red_opinion_prior
gen essay_red_opinion_prior = .
replace essay_red_opinion_prior = 1 if v_926 > 3 & v_926 != . & (c_0115 == "1" | c_0115 == "2")
replace essay_red_opinion_prior = 2 if v_926 <= 3 & v_926 != . & (c_0115 == "1" | c_0115 == "2")
label var essay_red_opinion_prior "Essay-Vaccination - opinion"
label define essay_red_opinion_prior 1 "In Favor" 2 "Against"
label val  essay_red_opinion_prior essay_opinion_prior


cap drop essay_opinion_prior
gen essay_opinion_prior = .
replace essay_opinion_prior = essay_vac_opinion_prior if essay_vac_opinion_prior != .
replace essay_opinion_prior = essay_red_opinion_prior if essay_red_opinion_prior != .
label var essay_opinion_prior "Essay-topic - same opinion"
label define essay_opinion_prior 1 "In Favor" 2 "Against"
label val  essay_opinion_prior essay_opinion_prior



* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *

foreach var of varlist header* demography* location* family* finance* personality* behavior* taste* thingsyoudo* thingsilike* quirk* {
	replace `var' = "" if useable != "1 Yes"
}

save "$data/Data_with_printing_rules_20220818", replace
