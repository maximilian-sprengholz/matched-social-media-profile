***********************************************
***********************************************
***			COVID-19 Vaccination			***
***											***
***			Wave 6 - Printing Rules		    ***
***			08.07.2022						***
***											***
***********************************************
***********************************************



***    Globals    *******************************************
*************************************************************
global data 	"C:\Users\geisslef\_Work\04_Paper\Covid-19-Vaccination\Daten\Wave6"
global out 		"C:\Users\geisslef\_Work\04_Paper\Covid-19-Vaccination\Daten\Wave6\Out"



clear all
set scheme plottigblind

use "C:\Users\sprenmax\Seafile\Social-Media-Page\DE_82803 D-P21-13185 HU Berlin Impfskeptiker_Welle 6_Testdatensatz_2.dta", clear

* ---------------------------------------------------------------------------- *

*** Header ***

* header1
cap drop header1
gen str header1 = ""
replace header1 = v_686


* header2
cap drop str_gender str_county str_age
decode v_16, gen(str_gender)
//decode v_23, gen(str_county)
gen str_county = v_23
tostring v_15, gen(str_age)

cap drop header2
gen str header2 = str_gender + "," + " " + str_age + " " + "Jahre" + "," + " " + str_county



* ---------------------------------------------------------------------------- *

*** Allgemein ***
* <i class="fa-solid fa-user"></i>


* demography1
cap drop str_eyecolor
gen str str_eyecolor = ""
replace str_eyecolor = v_882
replace str_eyecolor = ustrlower(str_eyecolor)
replace str_eyecolor = "braun" if v_687 == 1
replace str_eyecolor = "blau" if v_688 == 1
replace str_eyecolor = "grün" if v_689 == 1
replace str_eyecolor = "NA" if v_882 == " "

cap drop str_hand
decode v_693, gen(str_hand)
replace str_hand = "Linkshänderin" if str_hand == "Linkshänder" & v_16 == 1
replace str_hand = "Rechtshänderin" if str_hand == "Rechtshänder" & v_16 == 1

cap drop demography1
gen str demography1 = ""
replace demography1 = "Meine Augenfarbe ist" + " " +  str_eyecolor + " " + "und ich bin" + " "+ str_hand + "."



* demography2
cap drop str_land
gen str str_land = ""
replace str_land = "Ich komme aus Deutschland." if v_150 == 1
replace str_land = "Ich komme nicht aus Deutschland." if v_150 == 2


cap drop str_language
gen str str_language = ""
replace str_language = v_884
replace str_language = proper(str_language)
replace str_language = "Deutsch" if v_883 == 1


cap drop str_other_languages
gen str str_other_languages = ""
replace str_other_languages = v_695
replace str_other_languages = proper(str_other_languages)
replace str_other_languages = "ansonsten keine andere Fremdsprache" if v_695 == " " | v_695 == "-99"

cap drop demography3
gen str demography3 = ""
replace demography3 = str_land + " " + "Meine Muttersprache ist" + " " + str_language + " " + "und ich spreche" + " " + str_other_languages + "."





* ---------------------------------------------------------------------------- *

*** Ort und Herkunft ***
* <i class="fa-solid fa-house-chimney-window"></i>


* location1
cap drop str_bundesland
decode v_698, gen(str_bundesland)

cap drop str_stadt_land
decode v_696, gen(str_stadt_land)
replace str_stadt_land = "eher ländlichen" if v_696 == 1
replace str_stadt_land = "eher städtischen" if v_696 == 2

cap drop location1
gen str location1 = ""
replace location1 = "Momentan wohne ich in" + " " + str_bundesland + " " + "in einer" + " " + str_stadt_land + " " + "Gegend."




* location2
cap drop str_aufgewachsen
gen str str_aufgewachsen = ""
replace str_aufgewachsen = v_699
replace str_aufgewachsen = "Deutschland" if v_697 == 1


cap drop str_stadt_land
decode v_696, gen(str_stadt_land_raised)


cap drop location2
gen str location2 = ""
replace location2 = "Ich bin in" + " " + str_aufgewachsen + " " + "in einer Gegend, die" + " " + str_stadt_land_raised + " " + "ist."




* location3
cap drop str_immigration
gen str str_immigration = ""
replace str_immigration = "Eltern" if v_703 == 1
replace str_immigration = "Großltern" if v_703 == 2
replace str_immigration = "NA" if v_703 == 3



cap drop location3
gen str location3 = ""
replace location3 = "Ich habe auch eine Migrationsgeschichte: Meine" + " " + str_immigration + " " + "sind damals nach Deutschland eingewandert."
replace location3 = "NA" if v_703 == 3


* ---------------------------------------------------------------------------- *

*** Familie ***
* <i class="fa-solid fa-family"></i>


* family1
cap drop str_famstand
decode v_705, gen(str_famstand)
replace str_famstand = "verheiratet und lebe mit meinem Partner zusammen" if v_705 == 1
replace str_famstand = "in eingetragener gleichgeschlechtlicher Lebenspartnerschaft und lebe miz meinem Partner zusammen" if v_705 == 2
replace str_famstand = "verheiratet aber getrennt lebend" if v_705 == 3
replace str_famstand = "ledig" if v_705 == 4
replace str_famstand = "geschieden" if v_705 == 5
replace str_famstand = "verwitwet" if v_705 == 6
replace str_famstand = "in einer eingetragenen Lebenspartnerschaft aber getrennt lebend" if v_705 == 7
replace str_famstand = "in einer eingetragenen Lebenspartnerschaft, die aufgehoben wurde" if v_705 == 8
replace str_famstand = "in einer eingetragenen Lebenspartnerschaft, bei der mein Partner verstorben ist" if v_705 == 9

cap drop family1
gen str family1 = ""
replace family1 = "Ich bin" + " " + str_famstand + "."



* family2
cap drop str_divorce
decode v_706, gen(str_divorce)
replace str_divorce = "sind geschieden" if v_706 == 1
replace str_divorce = "sind nicht geschieden" if v_706 == 2
replace str_divorce = "waren nie verheiratet" if v_706 == 3
replace str_divorce = "NA" if v_706 == 99

cap drop family2
gen str family2 = ""
replace family2 = "Meine Eltern" + " " + str_divorce + "."
replace family2 = "NA" if v_706 == 99



* family3
tostring v_144, gen(str_kids)
replace str_kids = "keine" if v_144 == 0
tostring v_707, gen(str_siblings)
replace str_siblings = "keine" if v_707 == 0

cap drop family3
gen str family3 = ""
replace family3 = "Ich habe " + " " + str_kids + " " + "eigene Kinder und" + " " + str_siblings + " " + "Geschwister."




* family4
cap drop str_pet
cap drop str_pet_help
egen str_pet_help = anymatch(v_713-v_890), values(1)
gen str str_pet = ""
replace str_pet = "die folgenden Haustiere:" if str_pet_help == 1
replace str_pet = "keine Haustiere." if str_pet_help != 1

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
replace pet_Andere = v_891 if v_890 == 1


cap drop family4
gen str family4 = ""
replace family4 = "Ich habe" + " " + str_pet + " " + pet_Katze + pet_Hund + pet_Fische + pet_Reptilien + pet_Vögel + pet_Nagetiere + pet_Andere





* family5
cap drop str_edu_voc
decode v_724, gen(str_edu_voc)
replace str_edu_voc = "Mein höchster beruflicher Schulabschluss ist ein Berufsschulabschluss." if v_724 == 1
replace str_edu_voc = "Mein höchster beruflicher Schulabschluss ist eine Lehre." if v_724 == 2
replace str_edu_voc = "Mein höchster beruflicher Schulabschluss ist ein Meister-/Technikerabschluss." if v_724 == 3
replace str_edu_voc = "Mein höchster beruflicher Schulabschluss ist ein Fachhochschulabschluss." if v_724 == 4
replace str_edu_voc = "Mein höchster beruflicher Schulabschluss ist ein Hochschulabschluss." if v_724 == 5
replace str_edu_voc = "Mein höchster beruflicher Schulabschluss ist" + " " + v_725 + "." if v_724 == 6
replace str_edu_voc = "Ich habe bislang keinen beruflichen Schulabschluss." if v_724 == 7

cap drop str_edu_ort
gen str str_edu_ort = ""
replace str_edu_ort = "Studiert habe ich hier:" + " " + v_726
replace str_edu_ort = " " if v_726 == " " | v_726 == "NA" | v_726 == "-66"

cap drop family5
gen str family5 = ""
replace family5 = str_edu_voc + " " + str_edu_ort







* family6
cap drop str_military
gen str str_military = ""
replace str_military = "Ich habe beim Militär" if v_722 == 1
replace str_military = "Ich habe nicht beim Militär gedient." if v_722 == 2

cap drop str_branch
gen str str_branch = ""
replace str_branch = "im Heer gedient." if v_723 == 1
replace str_branch = "in der Marine gedient." if v_723 == 2
replace str_branch = "in der Luftwaffe gedient." if v_723 == 3
replace str_branch = "im Sänitätsdienst gedient." if v_723 == 4
replace str_branch = "in der Streitkräftebasis gedient." if v_723 == 5
replace str_branch = "im Bereich Cyber gedient." if v_723 == 6



cap drop family6
gen str family6 = ""
replace family6 = str_military + " " + str_branch







* family7
cap drop str_pflege
gen str str_pflege = ""
replace str_pflege = "Pflegetätigkeit" if v_728 == 1
replace str_pflege = "keine Pflegetätigkeit" if v_728 == 2

cap drop family7
gen str family7 = ""
replace family7 = "Ich verrichte momentan" + " " + str_pflege + " " + "für einen alten, kranken oder behinderten Menschen."





* family8
cap drop str_loss
gen str str_loss = ""
replace str_loss = "bereits den Verlust" if v_729 == 1
replace str_loss = "bislang noch keinen Verlust" if v_729 == 2

cap drop family8
gen str family8 = ""
replace family8 = "In meinem Leben habe ich" + " " + str_loss + " " + "einer bedeutenden Person erlebt."





* family9
cap drop str_lgbt
gen str str_lgbt = ""
replace str_lgbt = "Ich habe Freunde in der LGBT-Community." if v_727 == 1
replace str_lgbt = "Ich habe keine Freunde in der LGBT-Community." if v_727 == 2
replace str_lgbt = "NA." if v_727 == 3

cap drop family9
gen str family9 = ""
replace family9 = str_lgbt
replace family9 = "" if v_727 == 3



* ---------------------------------------------------------------------------- *


*** Finanzen ***
* <i class="fa-solid fa-money-check-dollar"></i>

* finance1
cap drop str_schicht_aktuell
decode v_733, gen(str_schicht_aktuell)
replace str_schicht_aktuell = "oberen Mittelschicht" if str_schicht_aktuell == "obere Mittelschicht"
replace str_schicht_aktuell = "unteren Mittelschicht" if str_schicht_aktuell == "untere Mittelschicht"

cap drop str_schicht_change
gen str str_schicht_change = ""
replace str_schicht_change = "ein Aufstieg" if v_733 > v_734
replace str_schicht_change = "ein Abstieg" if v_734 > v_733
replace str_schicht_change = "keine Veränderung" if v_733 == v_734

cap drop str_schicht_zukunft
decode v_735, gen(str_schicht_zukunft)
replace str_schicht_zukunft = "oberen Mittelschicht" if str_schicht_zukunft == "obere Mittelschicht"
replace str_schicht_zukunft = "unteren Mittelschicht" if str_schicht_zukunft == "untere Mittelschicht"


cap drop finance1
gen str finance1 = ""
replace finance1 = "Ich zähle mich zur" + " " + str_schicht_aktuell + "." + " " + "Im Vergleich zu meinen Eltern ist dies" + " " + str_schicht_change + "." + " " + "Für die Zukunft denke ich, dass ich mich in der" + " " + str_schicht_zukunft + " " + "wiederfinden werde."




* finance2
cap drop str_auto
decode v_731, gen(str_auto)
replace str_auto = "ein eigenes Auto" if v_731 == 1
replace str_auto = "kein eigenes Auto" if v_731 == 2


cap drop finance2
gen str finance2 = ""
replace finance2 = "Ich besitze" + " " + str_auto + "."



* finance3
cap drop str_wohneigentum
gen str str_wohneigentum = ""
replace str_wohneigentum = "eigenes Wohneigentum" if v_730 == 1
replace str_wohneigentum = "kein eigenes Wohneigentum" if v_730 == 2

cap drop finance3
gen str finance3 = ""
replace finance3 = "Ich besitze" + " " + str_wohneigentum + "."



* ---------------------------------------------------------------------------- *


*** Persönlichkeit ***
* <i class="fa-solid fa-face-smile"></i>


* personality1
cap drop str_geduld
gen str str_geduld = ""
replace str_geduld = "ein geduldiger" if v_742 == 1
replace str_geduld = "kein geduldiger" if v_742 == 2

cap drop str_konfrontativ
gen str str_konfrontativ = ""
replace str_konfrontativ = "konfrontativer" if v_745 == 1
replace str_konfrontativ = "nicht konfrontativer" if v_745 == 2

cap drop str_chaotisch
gen str str_chaotisch = ""
replace str_chaotisch = "sehr chaotisch" if v_743 == 1
replace str_chaotisch = "eher chaotisch" if v_743 == 2
replace str_chaotisch = "überhaupt nicht chaotisch" if v_743 == 3

cap drop personality1
gen str personality1 = ""
replace personality1 = "Sich selbst zu beschreiben ist ja oftmals gar nicht so einfach. Ich würde aber sagen, dass ich eher" + " " + str_geduld + " " + "und ein" + " " + str_konfrontativ + " " + "Mensch bin." + " " + "Meine Freunde und Familienmitglieder würden wahrscheinlich sagen, dass ich" + " " + str_chaotisch + " " + "bin."






* personality2
cap drop str_wettkampf
gen str str_wettkampf = ""
replace str_wettkampf = "mag es sehr" if v_740 == 1
replace str_wettkampf = "mag es überhaupt nicht" if v_740 == 2

cap drop str_perfekt
gen str str_perfekt = ""
replace str_perfekt = "als Perfektionist" if v_741 == 1 & v_16 == 2
replace str_perfekt = "als Perfektionistin" if v_741 == 1 & v_16 == 1
replace str_perfekt = "nicht als Perfektionist" if v_741 == 2 & v_16 == 2
replace str_perfekt = "nicht als Perfektionistin" if v_741 == 2 & v_16 == 1

cap drop str_energie
gen str str_energie = ""
replace str_energie = "bin ich hyperaktiv." if v_739 == 1
replace str_energie = "bin ich recht aktiv." if v_739 == 2
replace str_energie = "bin ich nur gelegentlich aktiv." if v_739 == 3
replace str_energie = "bewege ich mich eher wenig." if v_739 == 4

cap drop personality2
gen str personality2 = ""
replace personality2 = "Ich" + " " + str_wettkampf + " " + "an Wettkämpfen teilzunehmen und würde mich" + " " + str_perfekt + " " + "bezeichnen." + " " + "Grundsätzlich" + " " + str_energie






* personality3
cap drop str_workplay
gen str str_workplay = ""
replace str_workplay = "Arbeiten" if v_738 == 1
replace str_workplay = "Freizeit" if v_738 == 2

cap drop personality3
gen str personality3 = ""
replace personality3 = "Wenn ich mich zwischen Arbeiten und Freizeit entscheiden müsste, würde ich ganz klar" + " " + str_workplay + " " + "wählen."




* personality4
cap drop str_pflege
gen str str_pflege = ""
replace str_pflege = "Ein gepflegtes Äußeres ist mir persönlich schon wichtig." if v_744 == 1
replace str_pflege = "Ein gepflegtes Äußeres ist mir persönlich nicht so wichtig. Es zählen doch vor allem die inneren Werte." if v_744 == 2

cap drop personality4
gen str personality4 = ""
replace personality4 = str_pflege





* personality5
cap drop str_faszination
gen str str_faszination = ""
replace str_faszination = "Sternen und Galaxien" if v_746 == 1
replace str_faszination = "technologischem Fortschritt" if v_746 == 2
replace str_faszination = "alten Zivilisationen" if v_746 == 3
replace str_faszination = "Natur und der Tierwelt" if v_746 == 4

cap drop str_fantasie
gen str str_fantasie = ""
replace str_fantasie = "Auch wenn ich weiß, dass viele Menschen das komisch finden, wünsche ich mir manchmal, dass fantatsische Kreaturen real wären." if v_747 == 1
replace str_fantasie = "Mit fantastischen Kreaturen und solchen Dingen kann ich jedoch überhaupt nichts anfangen." if v_747 == 2

cap drop personality5
gen str personality5 = ""
replace personality5 = "Ich bin fasziniert von" + " " + str_faszination + "." + " " + str_fantasie



* ---------------------------------------------------------------------------- *


*** behavior ***
* <i class="fa-solid fa-head-side"></i>


* behavior1
cap drop str_return
gen str str_return = ""
replace str_return = "Ich bin ein Mensch, der auch mal Essen im Restaurant zurückgeben lässt, wenn es nicht schmeckt." if v_752 == 1
replace str_return = "Ich bin kein Mensch, der Essen im Restaurant zurückgeben lässt, wenn es nicht schmeckt." if v_752 == 2

cap drop behavior1
gen str behavior1 = ""
replace behavior1 = str_return





* behavior2
cap drop str_wegwerfen
gen str str_wegwerfen = ""
replace str_wegwerfen = "Ich habe kein Problem damit, Dinge einfach wegzuwerfen oder zu verschenken, wenn ich sie selbst nicht mehr verwende." if v_750 == 1
replace str_wegwerfen = "Ich habe große Problem damit, Dinge wegzuwerfen oder zu verschenken, sogar dann, wenn ich sie selbst überhaupt nicht mehr verwende." if v_750 == 2

cap drop behavior2
gen str behavior2 = ""
replace behavior2 = str_wegwerfen





* behavior3
cap drop str_sperrmüll
gen str str_sperrmüll = ""
replace str_sperrmüll = "Ich finde Sachen, die eine Person nicht mehr brauchen kann, trotzdem noch einen Mehrwert haben können. Ich habe auch selbst schon mal Möbel vom Sperrmüll mit nach Hause genommen." if v_749 == 1
replace str_sperrmüll = "Ich finde der Recycling-Trend geht manchmal etwas zu weit. Ich würde zumindest nie Möbel vom Sperrmüll mit nach Hause nehmen." if v_749 == 2

cap drop behavior3
gen str behavior3 = ""
replace behavior3 = str_sperrmüll






* behavior4
cap drop str_geschenk
gen str str_geschenk = ""
replace str_geschenk = "Wenn mir ein Geschenk selbst nicht gefällt, schenke ich es manchmal auch einfach weiter." if v_753 == 1
replace str_geschenk = "Auch wenn mir ein Geschenk selbst nicht gefällt, würde ich es niemals einfach weiter verschenken." if v_753 == 2

cap drop behavior4
gen str behavior4 = ""
replace behavior4 = str_geschenk






* behavior5
cap drop str_snoozing
gen str str_snoozing = ""
replace str_snoozing = "null Mal betätige, sondern sofort aufstehe" if v_748 == 1
replace str_snoozing = "ein oder zwei Mal betätige bevor ich aufstehe" if v_748 == 2
replace str_snoozing = "dreimal oder häufiger Mal betätige bevor ich aufstehe" if v_748 == 3
replace str_snoozing = "NA" if v_748 == 99

cap drop behavior5
gen str behavior5 = ""
replace behavior5 = "Die Schlummertaste beim Wecker kann ja Fluch und Segen zugleich sein. Bei mir ist es so, dass ich die Schlummertaste normalerweise" + " " + str_snoozing + "."
replace behavior5 = "" if v_748 == 99





* behavior6
cap drop str_diebstahl
gen str str_diebstahl = ""
replace str_diebstahl = "Ich finde, dass man es mit den Regeln nicht unbedingt immer ganz genau nehmen muss. Ich habe ehrlich gesagt auch schon mal ein Glas aus einer Bar gestohlen." if v_751 == 1
replace str_diebstahl = "Ich finde, dass auch kleinere Diebstähle kein Kavaliersdelikt sind. Ich habe zum Beispiel noch nie ein Glas aus einer Bar gestohlen." if v_751 == 2
replace str_diebstahl = "NA." if v_751 == 3

cap drop behavior6
gen str behavior6 = ""
replace behavior6 = str_diebstahl
replace behavior6 = "" if v_751 == 3






* behavior7
cap drop str_fluchen
gen str str_fluchen = ""
replace str_fluchen = "ich nie" if v_754 == 1
replace str_fluchen = "auch ich gelegentlich" if v_754 == 2
replace str_fluchen = "auch ich durchaus oft" if v_754 == 3
replace str_fluchen = "auch ich regelmäßig" if v_754 == 4

cap drop behavior7
gen str behavior7 = ""
replace behavior7 = "Für viele Menschen gehören Schimpfwörter mittlerweile zum Sprachgebrauch dazu. Bei mir ist es so, dass" + " " + str_fluchen + " " + "Schimpfwörter verwende."





* behavior8
cap drop str_horoskop
gen str str_horoskop = ""
replace str_horoskop = "ich mir mein Horoskop schon gerne jeden Tag durchlese" if v_755 == 1
replace str_horoskop = "ich mir mein Horoskop zumindest einmal die Woche schon ganz gerne durchlese" if v_755 == 2
replace str_horoskop = "auch ich mir mein Horoskop gelegentlich schon mal ganz gerne durchlese" if v_755 == 3
replace str_horoskop = "ich damit nichts anfangen kann und ich mir mein Horoskop wirklich nie durchlese" if v_755 == 4

cap drop behavior8
gen str behavior8 = ""
replace behavior8 = "Horoskope sind ja so eine Sache. Ich muss sagen, dass" + " " + str_horoskop + "."


* ---------------------------------------------------------------------------- *

*** Geschmack ***
* <i class="fa-solid fa-star"></i>

// items missing


* ---------------------------------------------------------------------------- *

*** Dinge, die ich mache ***
* <i class="fa-solid fa-head-side-heart"></i>


* thingsyoudo1
cap drop str_social_media
gen str str_social_media = ""
replace str_social_media = "sehr aktiv" if v_764 == 1
replace str_social_media = "etwas aktiv" if v_764 == 2
replace str_social_media = "kaum aktiv" if v_764 == 3
replace str_social_media = "gar nicht aktiv" if v_764 == 4

cap drop thingsyoudo1
gen str thingsyoudo1 = ""
replace thingsyoudo1 = "Ob ich Social Media verwende? Ich würde sagen, dass ich" + " " + str_social_media + " " + "bin."






* thingsyoudo2
cap drop str_smoke
gen str str_smoke = ""
replace str_smoke = "Ich bin Raucher." if v_766 == 1 & v_16 == 2
replace str_smoke = "Ich bin Raucherin." if v_766 == 1 & v_16 == 1
replace str_smoke = "Ich bin Raucher, aber nur in Gesellschaft." if v_766 == 2 & v_16 == 2
replace str_smoke = "Ich bin Raucherin, aber nur in Gesellschaft." if v_766 == 2 & v_16 == 1
replace str_smoke = "Ich bin Nichtraucher." if v_766 == 3 & v_16 == 2
replace str_smoke = "Ich bin Nichtraucherin." if v_766 == 3 & v_16 == 1
replace str_smoke = "Ich bin Nichtraucher, habe aber früher mal geraucht." if v_766 == 4 & v_16 == 2
replace str_smoke = "Ich bin Nichtraucherin, habe aber früher mal geraucht." if v_766 == 4 & v_16 == 1

cap drop thingsyoudo2
gen str thingsyoudo2 = ""
replace thingsyoudo2 = str_smoke





* thingsyoudo3
cap drop str_sport
gen str str_sport = ""
replace str_sport = "auch selbst aktiv Sport. Momentan vor allem:" if v_792 == 0
replace str_sport = "selbst aktiv keinen Sport." if v_792 == 1


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


replace sport_Andere = v_897 if v_896 == 1


cap drop thingsyoudo3
gen str thingsyoudo3 = ""
replace thingsyoudo3 = "Ich treibe" + " " + str_sport + " " + sport_Fußball + sport_Baseball + sport_Basketball + sport_Volleyball + sport_Tennis + sport_Eishockey + sport_Cricket + sport_Football + sport_Feldhockey + sport_Radfahren + sport_Leichtathletik + sport_Tischtennis + sport_Laufen + sport_Kampfsport + sport_Klettern + sport_Skifahren + sport_Yoga + sport_Schwimmen + sport_Angeln + sport_Andere







* thingsyoudo4
cap drop str_museum
gen str str_museum = ""
replace str_museum = "Ich liebe es ins Museum zu gehen." if v_794 == 1
replace str_museum = "Ich gehe gern ins Museum, aber nur manchmal." if v_794 == 2
replace str_museum = "Ich gehe nicht gern ins Museum." if v_794 == 3

cap drop thingsyoudo4
gen str thingsyoudo4 = ""
replace thingsyoudo4 = str_museum






* thingsyoudo5
cap drop str_tanzen
gen str str_tanzen = ""
replace str_tanzen = "Ich liebe es tanzen zu gehen." if v_795 == 1
replace str_tanzen = "Ich gehe gern tanzen, aber nur manchmal." if v_795 == 2
replace str_tanzen = "Ich gehe nicht gern tanzen." if v_795 == 3

cap drop thingsyoudo5
gen str thingsyoudo5 = ""
replace thingsyoudo5 = str_tanzen




* ---------------------------------------------------------------------------- *

*** Dinge, die ich mag ***
* <i class="fa-solid fa-heart-pulse"></i>



* thingsilike1
cap drop str_musik
gen str str_musik = ""
replace str_musik = "Ich liebe es Musik zu hören. Am Liebsten mag ich:" if v_796 == 1 & v_819 != 1
replace str_musik = "Ich liebe es Musik zu hören. Eine bestimmte Lieblings-Musikrichtung habe ich aber nicht." if v_796 == 1 & v_819 == 1
replace str_musik = "Ich habe es gerne ruhig. Musik hören mag ich gar nicht so gern." if v_796 == 2


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

replace musik_Andere = v_900 if v_899 == 1


cap drop thingsilike1
gen str thingsilike1 = ""
replace thingsilike1 = str_musik + " " + musik_Blues + musik_Klassik + musik_Country + musik_Rock + musik_HipHop + musik_Latin + musik_Pop + musik_Religiös + musik_Funk + musik_R_B + musik_Rap + musik_Elektronisch + musik_Folk + musik_Jazz + musik_New_Age + musik_Reggae






* thingsilike2
cap drop str_musik_liebling
gen str str_musik_liebling = v_821

cap drop thingsilike2
gen str thingsilike2 = ""
replace thingsilike2 = "Ich bin großer Fan von " + " " + str_musik_liebling + "."





* thingsilike3
cap drop str_filme
gen str str_filme = ""
replace str_filme = "Ich bin ein großer Filmfan. Am Liebsten mag ich Filme aus dem Genre:" if v_822 == 1 & v_840 != 1
replace str_filme = "Ich bin ein großer Filmfan. Ein bestimmtes Lieblings-Genre habe ich aber nicht." if v_822 == 1 & v_840 == 1
replace str_filme = "Filme schauen mag ich nicht so gern." if v_822 == 2


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

replace filme_Andere = v_903 if v_902 == 1


cap drop thingsilike3
gen str thingsilike3 = ""
replace thingsilike3 = str_filme + " " + filme_Action + filme_Abenteuer + filme_Komödie + filme_Krimi + filme_Drama + filme_Fantasy + filme_Historisch + filme_Horror + filme_Mystery + filme_Politisch + filme_Romantik + filme_SciFi + filme_Thriller + filme_Krieg + filme_Western + filme_Surreal





* thingsilike4
cap drop str_filme_liebling
gen str str_filme_liebling = v_842

cap drop str_schauspieler_liebling
gen str str_schauspieler_liebling = v_843

cap drop thingsilike4
gen str thingsilike4 = ""
replace thingsilike4 = "Mein absoluter Lieblingsfilm ist" + " " + "'" + str_filme_liebling + "'" + " " + "und am liebsten sehe ich Filme mit" + " " + str_schauspieler_liebling + "."











* thingsilike5
cap drop str_fan
gen str str_fan = ""
replace str_fan = "Ich bin ein riesiger Sportfan. Am meisten kann ich mich begeistern für:" if v_844 == 1 & v_862 != 1
replace str_fan = "Ich bin ein riesiger Sportfan. Es gibt aber jetzt nicht die eine Sportart, die ich besonders gerne mag." if v_844 == 1 & v_862 == 1
replace str_fan = "Als Fan bin ich für Sport nicht zu begeistern." if v_844 == 2


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

replace fan_Andere = v_906 if v_905 == 1


cap drop thingsilike5
gen str thingsilike5 = ""
replace thingsilike5 = str_fan + " " + fan_Golf + fan_Fußball + fan_Baseball + fan_Basketball + fan_Volleyball + fan_Tennis + fan_Eishockey + fan_Cricket + fan_American_Football + fan_Feldhockey + fan_Nascar + fan_Formel_1 + fan_Radfahren + fan_Darts + fan_Snooker + fan_Boxen + fan_Andere




* thingsilike6
cap drop str_fan_liebling
gen str str_fan_liebling = v_864

cap drop thingsilike6
gen str thingsilike6 = ""
replace thingsilike6 = "Ich bin großer Fan von" + " " + str_fan_liebling + "."
replace thingsilike6 = "" if str_fan_liebling == "-66"






* thingsilike7
cap drop str_fernsehen
gen str str_fernsehen = ""
replace str_fernsehen = "Ich schaue auch gerne mal Fernsehen." if v_865 == 1
replace str_fernsehen = "Fernsehen schaue ich momentan eigentlich gar nicht." if v_865 == 2

cap drop str_fernsehen_liebling
gen str str_fernsehen_liebling = v_866
replace str_fernsehen_liebling = "Am besten gefällt mir im Moment" + " " + "'" + str_fernsehen_liebling + "'" + "."
replace str_fernsehen_liebling = "" if v_866 == "-66"

cap drop thingsilike7
gen str thingsilike7 = ""
replace thingsilike7 = str_fernsehen + " " + str_fernsehen_liebling







* thingsilike8
cap drop str_buch
gen str str_buch = ""
replace str_buch = "Ich genieße es auch sehr mich in ein gutes Buch zu vertiefen." if v_867 == 1
replace str_buch = "Bücher lese ich momentan so gut wie gar nicht." if v_867 == 2

cap drop str_buch_liebling
gen str str_buch_liebling = v_868
replace str_buch_liebling = "Besonders gut gefällt mir im Moment" + " " + "'" + str_buch_liebling + "'" + "."
replace str_buch_liebling = "" if v_868 == "-66"

cap drop thingsilike8
gen str thingsilike8 = ""
replace thingsilike8 = str_buch + " " + str_buch_liebling








* thingsilike9
cap drop str_web
gen str str_web = ""
replace str_web = "Neue Medien nutze ich auch sehr gerne und folge selbst auch einigen Webkanälen." if v_869 == 1
replace str_web = "Dieser Hype um diese ganzen neuen Webanäle und Plattformen lässt mich ziemlich kalt. Ich folge auf jeden Fall niemandem." if v_869 == 2

cap drop str_web_liebling
gen str str_web_liebling = v_870
replace str_web_liebling = "Insbesondere interessiere ich mich für" + " " + "'" + str_web_liebling + "'" + "."
replace str_web_liebling = "" if v_870 == "-66"

cap drop thingsilike9
gen str thingsilike9 = ""
replace thingsilike9 = str_web + " " + str_web_liebling







* thingsilike10
cap drop str_video
gen str str_video = ""
replace str_video = "Ab und zu spiele ich auch ganz gerne Videospiele." if v_871 == 1
replace str_video = "Videospiele und solchen Sachen sind nichts für mich." if v_871 == 2

cap drop str_video_liebling
gen str str_video_liebling = v_872
replace str_video_liebling = "Am meisten Spaß macht mir" + " " + "'" + str_video_liebling + "'" + "."
replace str_video_liebling = "" if v_872 == "-66"

cap drop thingsilike10
gen str thingsilike10 = ""
replace thingsilike10 = str_video + " " + str_video_liebling







* thingsilike11
cap drop str_kreativ
gen str str_kreativ = ""
replace str_kreativ = "Ich liebe es mich kreativ zu betätigen." if v_873 == 1
replace str_kreativ = "Kreative Tätigkeiten wie malen oder singen sind nicht gerade mein Ding." if v_873 == 2

cap drop str_kreativ_liebling
gen str str_kreativ_liebling = v_874
replace str_kreativ_liebling = "Am meisten Freude macht mir" + " " + "'" + str_kreativ_liebling + "'" + "."
replace str_kreativ_liebling = "" if v_874 == "-66"

cap drop thingsilike11
gen str thingsilike11 = ""
replace thingsilike11 = str_kreativ + " " + str_kreativ_liebling



* ---------------------------------------------------------------------------- *

*** Ein interessanter Fakt über mich ***
* <i class="fa-solid fa-face-surprise"></i>

* quirk1
cap drop quirk1
gen str quirk1 = v_876



* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
export delimited using "C:/Users/sprenmax/Seafile/projects/matched-social-media-profile/testdata.csv", replace
//save "$data/Data_with_printing_rules_20220708", replace
