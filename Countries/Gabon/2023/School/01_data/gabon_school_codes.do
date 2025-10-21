*Purpose: prepare the data for Gabon for processing
*Output: pre-processed raw files that are later used for cleaning

if "`c(username)'" == "wb631589" {
	global dir "C:\Users\wb631589\OneDrive - WBG\"
}

use "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\School\EPDash.dta", clear

foreach v of varlist _all {
	cap replace `v' = "" if inlist(`v', "##N/A##", "NA")
}


*now drop empty surveys with Information a completer and let's do some basic matching based on this information
egen total_modules = rowtotal(modules__*), miss
drop if school_code_preload == "Information a completer" & missing(m1s0q1) & missing(total_modules) // 30-59-05-34, 47-96-77-12, 45-62-10-31, 76-03-20-17

replace school_name_preload = "ENSET" if (m1s0q1_comments == "L'ECOLE ENSET/A , PROVINCE DE L'ESTUAIRE, VILLE LIBREVILLE" | m1s0q1_comments == "École ENSET .A , PROVINCE DE L'ESTUAIRE, VILLE DE LIBREVILLE") & school_name_preload == "Information a completer" // based on comments, apparently we only have the data from one module for this school
replace school_emis_preload = "0101301045X512" if (m1s0q1_comments == "L'ECOLE ENSET/A , PROVINCE DE L'ESTUAIRE, VILLE LIBREVILLE" | m1s0q1_comments == "École ENSET .A , PROVINCE DE L'ESTUAIRE, VILLE DE LIBREVILLE") // emis code from the weights file

replace school_name_preload = "FRANCO ARABE NASROU ISLAM" if (m1s0q1_comments == "COMPLEXE SCOLAIRE FRANCO-ARABE A OZANGUE, DANS LA PROVINCE DE L ESTUAIRE ET LE DISTRIC DE LIBREVILLE AVEC LE CODE DE L'ECOLE 0101305188X524 ET LE CODE EMIS 0101305188x524" | m1s0q1_comments == "ECOLE FRACO-ARABE CATIER OZANGUE DANS LA PRIVINCE DE L'ESTUAIRE LE CODE DE L'ECOLE 0101305188X524 ET LE CODE EMIS 0101305188X524") & school_name_preload == "Information a completer"
replace school_emis_preload = "0101305188x524" if (m1s0q1_comments == "COMPLEXE SCOLAIRE FRANCO-ARABE A OZANGUE, DANS LA PROVINCE DE L ESTUAIRE ET LE DISTRIC DE LIBREVILLE AVEC LE CODE DE L'ECOLE 0101305188X524 ET LE CODE EMIS 0101305188x524" | m1s0q1_comments == "ECOLE FRACO-ARABE CATIER OZANGUE DANS LA PRIVINCE DE L'ESTUAIRE LE CODE DE L'ECOLE 0101305188X524 ET LE CODE EMIS 0101305188X524") & school_emis_preload == "Information a completer"
replace school_code_preload = "0101305188X524" if (m1s0q1_comments == "COMPLEXE SCOLAIRE FRANCO-ARABE A OZANGUE, DANS LA PROVINCE DE L ESTUAIRE ET LE DISTRIC DE LIBREVILLE AVEC LE CODE DE L'ECOLE 0101305188X524 ET LE CODE EMIS 0101305188x524" | m1s0q1_comments == "ECOLE FRACO-ARABE CATIER OZANGUE DANS LA PRIVINCE DE L'ESTUAIRE LE CODE DE L'ECOLE 0101305188X524 ET LE CODE EMIS 0101305188X524") & school_code_preload == "Information a completer" // based on the comments and the name from the weights file

replace school_name_preload = "LES MESANGES BLEUES SITUE" if (m1s0q1_comments == "NOUS VISITONS LES MESANGES BLEUES SITUE DANS LA PROVINCE  DE L'ESTUAIRE ET LE DISTRIC DE LIBREVILLE, AVEC LE CODE DE L' ECOLE 0101302040X514 ET LE CODE EMIS 0101302040X514" | m1s0q1_comments == "ECOLE LES MESANGES SITUÉ A STFO,LIBREVILLE DANS LA PROVINCE DE L'ESTUAIRE,AVEC ID 0101302040X514 et L'ÉMIS 0101302040X514") & school_name_preload == "Information a completer"
replace school_emis_preload = "0101302040X514" if (m1s0q1_comments == "NOUS VISITONS LES MESANGES BLEUES SITUE DANS LA PROVINCE  DE L'ESTUAIRE ET LE DISTRIC DE LIBREVILLE, AVEC LE CODE DE L' ECOLE 0101302040X514 ET LE CODE EMIS 0101302040X514" | m1s0q1_comments == "ECOLE LES MESANGES SITUÉ A STFO,LIBREVILLE DANS LA PROVINCE DE L'ESTUAIRE,AVEC ID 0101302040X514 et L'ÉMIS 0101302040X514")  & school_emis_preload == "Information a completer"
replace school_code_preload = "0101302040X514" if (m1s0q1_comments == "NOUS VISITONS LES MESANGES BLEUES SITUE DANS LA PROVINCE  DE L'ESTUAIRE ET LE DISTRIC DE LIBREVILLE, AVEC LE CODE DE L' ECOLE 0101302040X514 ET LE CODE EMIS 0101302040X514" | m1s0q1_comments == "ECOLE LES MESANGES SITUÉ A STFO,LIBREVILLE DANS LA PROVINCE DE L'ESTUAIRE,AVEC ID 0101302040X514 et L'ÉMIS 0101302040X514")  & school_code_preload == "Information a completer" // based on the comments + weights file

replace school_name_preload = "ECOLE PUBLIQUE COMMUNALE C DE TCHIBANGA" if (m1s0q1_comments == "ECOLE PUBLIQUE COMMUNALE C, à l'adresse ,LA CARRIERE,dans la province NYANGA, et le district, MOUGOUTSI, avec le code de l'ecole, 0501401009X505 et le code EMIS 0501401009X505 .") & school_name_preload == "Information a completer"
replace school_emis_preload = "0501401009X505" if (m1s0q1_comments == "ECOLE PUBLIQUE COMMUNALE C, à l'adresse ,LA CARRIERE,dans la province NYANGA, et le district, MOUGOUTSI, avec le code de l'ecole, 0501401009X505 et le code EMIS 0501401009X505 .")  & school_emis_preload == "Information a completer"
replace school_code_preload = "0501401009X505" if (m1s0q1_comments == "ECOLE PUBLIQUE COMMUNALE C, à l'adresse ,LA CARRIERE,dans la province NYANGA, et le district, MOUGOUTSI, avec le code de l'ecole, 0501401009X505 et le code EMIS 0501401009X505 .")  & school_code_preload == "Information a completer" // based on comments + school info correct


replace school_name_preload = "COMPLEXE SCOLAIRE BILINGUE LES THEOPHILES" if (m1s0q1_comments ==  "nous visitons l,ecole nommee, le complexe scolaire les theophiles, dans la province de L'ESTUAIRE et le District avec le code de l'ecole 0106401054X506 et le code emis 0106401054x506" | m1s0q1_comments ==  "ECOLE PRIVEE LEA THÉOPHILES AU QUARTIER AWOUGOU DANS LA PROVINCE DE L'ESTUAIRE") & school_name_preload == "Information a completer"
replace school_emis_preload = "0106401054x506" if (m1s0q1_comments ==  "nous visitons l,ecole nommee, le complexe scolaire les theophiles, dans la province de L'ESTUAIRE et le District avec le code de l'ecole 0106401054X506 et le code emis 0106401054x506" | m1s0q1_comments ==  "ECOLE PRIVEE LEA THÉOPHILES AU QUARTIER AWOUGOU DANS LA PROVINCE DE L'ESTUAIRE")  & school_emis_preload == "Information a completer"
replace school_code_preload = "0106401054x506" if (m1s0q1_comments ==  "nous visitons l,ecole nommee, le complexe scolaire les theophiles, dans la province de L'ESTUAIRE et le District avec le code de l'ecole 0106401054X506 et le code emis 0106401054x506" | m1s0q1_comments ==  "ECOLE PRIVEE LEA THÉOPHILES AU QUARTIER AWOUGOU DANS LA PROVINCE DE L'ESTUAIRE")  & school_code_preload == "Information a completer" // based on comments + weights file

replace school_name_preload = "EAPEC" if (m1s0q1_comments == "ÉCOLE PRIVÉE EAPEC DANS LA PROVINCE DE L'ESTUAIRE, DANS LE QUARTIER AMBOWE,et le DISTRICT DE LIBREVILLE AVEC LE CODE DE L'ECOLE 0101305188X524 ET LE CODE EMIS 0101305188X524" | m1s0q1_comments == "ECOLE NOMMEE EAPEC,SITUER A AMBOW,DANS LA PROVINCE DE L'ESTUAIRE,ET LE DISTRICT DE LIBREVILLE, AVEC LE CODE DE L' ECOLE ...?..ET LE CODE EMIS .....?..")
replace school_code_preload = "0101301130X505" if (m1s0q1_comments == "ÉCOLE PRIVÉE EAPEC DANS LA PROVINCE DE L'ESTUAIRE, DANS LE QUARTIER AMBOWE,et le DISTRICT DE LIBREVILLE AVEC LE CODE DE L'ECOLE 0101305188X524 ET LE CODE EMIS 0101305188X524" | m1s0q1_comments == "ECOLE NOMMEE EAPEC,SITUER A AMBOW,DANS LA PROVINCE DE L'ESTUAIRE,ET LE DISTRICT DE LIBREVILLE, AVEC LE CODE DE L' ECOLE ...?..ET LE CODE EMIS .....?..")
replace school_emis_preload = "0101301130X505" if (m1s0q1_comments == "ÉCOLE PRIVÉE EAPEC DANS LA PROVINCE DE L'ESTUAIRE, DANS LE QUARTIER AMBOWE,et le DISTRICT DE LIBREVILLE AVEC LE CODE DE L'ECOLE 0101305188X524 ET LE CODE EMIS 0101305188X524" | m1s0q1_comments == "ECOLE NOMMEE EAPEC,SITUER A AMBOW,DANS LA PROVINCE DE L'ESTUAIRE,ET LE DISTRICT DE LIBREVILLE, AVEC LE CODE DE L' ECOLE ...?..ET LE CODE EMIS .....?..") // based on the school name provided in comments and the emis code that corresponds to that name + location information in the weights file

replace school_name_preload = m1s0q2_name if school_info_correct == 0 & !missing(m1s0q2_name) & school_name_preload == "Information a completer"
replace school_code_preload = m1s0q2_code if school_info_correct == 0 & !missing(m1s0q2_code) & school_code_preload == "Information a completer"
replace school_emis_preload = m1s0q2_emis if school_info_correct == 0 & !missing(m1s0q2_emis) & school_emis_preload == "Information a completer"

replace school_name_preload = m1s0q1_comments if school_name_preload == "Information a completer" & !inlist(m1s0q1_comments, "NA", "##N/A##", "Nous avons été très bien reçu", "")

replace school_name_preload = strlower(school_name_preload)
replace school_name_preload = subinstr(school_name_preload, `"""',  "", .)

*this part is after the manual comparing of schools across all observations and making sure that the teachers from the roster in one survey match the teacher(s) from the assessment/questionnaire/observation
replace school_name_preload = "ecole publique de belle-vue 3" if school_name_preload == "belle vue 3" // now we have 3 observations, it is clearly the same school  

replace school_name_preload = "COMPLEXE SCOLAIRE LA RENAISSANCE SA FRA" if inlist(school_name_preload, "complexe scolaire la renaissance sa fra", "ecole privee la renaissance sa-fra")

*replace school_name_preload = "complexe scolaire bilingue les theophiles" if school_name_preload == "complexe scolaire les theophiles" // already made this change above in the modified version

replace school_name_preload = "COMPLEXE SCOLAIRE PRIVE LES ALEVINS" if inlist(school_name_preload, "complexe scolaire prive les alevins", "complexe scolaire privÉ les alevins") // confirmed
replace school_emis_preload = "0101301045X512" if school_name_preload == "COMPLEXE SCOLAIRE PRIVE LES ALEVINS" // there is a typo in one of the codes, I am using the old emis code from the weights file // confirmed

replace school_name_preload = "ECOLE BILINGUE MARTIN LUTHER KING" if inlist(school_name_preload, "ecole  martin luther  king", "martin luter king")

replace school_name_preload = "ECOLE CATHOLIQUE SAINT RICHARD" if inlist(school_name_preload, "ecole catholique saint richard d'oyem1", "ecole catholique  saint richard d'oyem1") | school_emis_preload == "0901401027X502"

replace school_name_preload = "ECOLE PRIVEE BILINGUE LES RELAYEURS" if inlist(school_name_preload, "ecole privee les relayeurs", "ecole privee bilingue les relayeurs")

replace school_name_preload = "ECOLE PUBLIQUE COMMUNALE C DE TCHIBANGA" if inlist(school_name_preload, "École publique communale  c  de tchibanga", "ecole publique communale")

replace school_name_preload = "COMPLEXE SCOLAIRE LA PATIENCE RIMA" if inlist(school_name_preload, "École privÉe laÏc la patience rima", "école privée laïque la patience rima")
replace school_emis_preload = "0101302078X504" if school_name_preload == "COMPLEXE SCOLAIRE LA PATIENCE RIMA" // seems to have duplicates modules

replace school_name_preload = "COMPLEXE SCOLAIRE PRIVE ALBATROS" if inlist(school_name_preload, "groupe scolaire albatros", "groupe scolaire privÉ l'albatros") // confirmed
replace school_emis_preload = "0101306019X501" if school_name_preload == "COMPLEXE SCOLAIRE PRIVE ALBATROS" // confirmed

replace school_name_preload = "ECOLE BILINGUE PRIVE GABON EMERGENT" if school_name_preload == "complexe scolaire bilingue les theophiles" & interview__id == "cae4bf783ce140879804b27f0ee09c86" // per comment + the fact that we already have 2 observations for that school with different teacher names
replace school_emis_preload = "0101302070X504" if interview__id == "cae4bf783ce140879804b27f0ee09c86"

replace school_name_preload = "ECOLE PUBLIQUE COMMUNALE DE BAKELE" if interview__id == "b70d08ab52414b8c962c2b2a3b8c2a3b" | interview__id == "67c9e0789b6d44fd9996ded54cc76e01" | interview__id == "5d4a770b589f4f518fde8f14a0c1a606" // based on comments from the enumerators + duplicated entries with identical names for some modules, for the last survey, teacher name is the same as the name in the other surveys
replace school_emis_preload = "0701401008X502" if interview__id == "b70d08ab52414b8c962c2b2a3b8c2a3b" | interview__id == "67c9e0789b6d44fd9996ded54cc76e01" | interview__id == "5d4a770b589f4f518fde8f14a0c1a606"

replace school_name_preload = "ECOLE PUBLIQUE DE BELLE-VUE 3" if interview__id == "13ae31e266e84b4890ffc7c755b1be46" | interview__id == "80aed6d7e2954cddafe321994dccd09b" | interview__id == "1f16cd5389984e60ad35798a43765ace" // in two interviews, teacher names are identical. for the third interview, I can match the teacher names from individual modules to the other surveys + comments
replace school_emis_preload = "0101303213X502" if interview__id == "13ae31e266e84b4890ffc7c755b1be46" | interview__id == "80aed6d7e2954cddafe321994dccd09b" | interview__id == "1f16cd5389984e60ad35798a43765ace" 

replace school_name_preload = "unidentified 1" if interview__id == "c283907aeea247d2964db0edc3ceb5d1" | interview__id == "a73baa8ba1314627a5ace1b3c171f03b" // rejected though

replace school_name_preload = "COMPLEXE SCOLAIRE MAISON DE DAVID 3" if interview__id == "2627a5e26408448db430a49db297968c" | interview__id == "dd04414ab05240c98879a94271a4125f" // based on the name of one of them, the school code corresponding to the name of the other one, and the perfect match between modules for two enumerators
replace school_emis_preload = "0101301002X504" if interview__id == "2627a5e26408448db430a49db297968c" | interview__id == "dd04414ab05240c98879a94271a4125f" 

replace school_name_preload = "ECOLE BILINGUE PRIVE GABON EMERGENT" if interview__id == "8dd3857af71e4843aa40c2ac14f7b9a7" | interview__id == "cae4bf783ce140879804b27f0ee09c86"
replace school_emis_preload = "0101302070X504" if interview__id == "8dd3857af71e4843aa40c2ac14f7b9a7" | interview__id == "cae4bf783ce140879804b27f0ee09c86" // based on enumerator comments + same teacher names across these two observations

replace school_name_preload = "OLIVIERS VERDOYANTS" if interview__id == "4274441f49b84aa59a58bd6f7f514422" | interview__id == "e1501441ce6f4fc994de3cef337f79ed"
replace school_emis_preload = "0101306005x510" if interview__id == "4274441f49b84aa59a58bd6f7f514422" | interview__id == "e1501441ce6f4fc994de3cef337f79ed" // based on the comments from both enumerators


*these four observations have the same school name but they are in fact two different schools with the same name but different code and located in different places. matched them based on the location information and teacher names
replace school_name_preload = "COMPLEXE SCOLAIRE LES PETITS COEURS" if inlist(interview__id, "6e0d3588e58d4cdd8102875d7d7cadda", "2f5242d896cc4f749dc7d4f6f8ab7394", "b25bdaf4af55460fb0452e121f09bd8e", "d840969806df4de4b75cec248a8fae1b")
replace school_emis_preload = "0101303165X513" if interview__id == "2f5242d896cc4f749dc7d4f6f8ab7394" | interview__id == "b25bdaf4af55460fb0452e121f09bd8e" // for the one in belle vue
replace school_emis_preload = "0101302008X503" if interview__id == "6e0d3588e58d4cdd8102875d7d7cadda" | interview__id == "d840969806df4de4b75cec248a8fae1b" // for the ones in cite

replace school_name_preload = "ECOLE PUBLIQUE DE NFOULAYONG" if interview__id == "7c7602bdfd77449a9d804d806b6c2eb0"
replace school_emis_preload = "0102401002X503" if interview__id == "7c7602bdfd77449a9d804d806b6c2eb0" // from the weights file based on the name

replace school_name_preload = "ECOLE PRIVEE ISLAMIQUE AL MOUMININE" if interview__id == "09aeef2c89e14f429d12cca5d94e96b4" | interview__id == "84768dc744024363998b4fed99edbf48" // per comment
replace school_emis_preload = "0101302157X502" if interview__id == "09aeef2c89e14f429d12cca5d94e96b4" | interview__id == "84768dc744024363998b4fed99edbf48" // per comment

replace school_name_preload = "ecole privee laÃque  la conquÃŠte" if interview__id == "3c51b4627c2042dfb53f49350a267268" | interview__id == "f37f795fcfcd423cb16b967a008b5b89" // do not know what the correct name is, but this two observations are one school might be ECOLE PRIVEE LAIQUE TRIOMPHALE based on the sampling file. the second observation is matched based on teacher names

replace school_name_preload = "ECOLE PUBLIQUE BELLE VUE 2" if interview__id == "a2160eefd39c4955ab535a1dbe417e0b" | interview__id == "b7ffe25687b44375af7ab44b684eedd1"
replace school_emis_preload = "0101303167X512" if interview__id == "a2160eefd39c4955ab535a1dbe417e0b" | interview__id == "b7ffe25687b44375af7ab44b684eedd1" // name is taken from one observation and I found the other one using teacher names

replace school_name_preload = "ECOLE PUBLIQUE DE BATAVEA 1" if interview__id == "d83ceb0b09124fabb75d1987aa512027" | interview__id == "03e5cf8cf4734ffe91390f6ce767eec8"
replace school_emis_preload = "0101304029X503" if interview__id == "d83ceb0b09124fabb75d1987aa512027" | interview__id == "03e5cf8cf4734ffe91390f6ce767eec8" // based on the comment from one of the enumerators and teacher name match

replace school_name_preload = "ECOLE PUBLIQUE DE BATAVEA 3" if interview__id == "5f6a83372fad43cb99964f757a4d4916"
replace school_emis_preload = "0101304029X503" if interview__id == "5f6a83372fad43cb99964f757a4d4916"

replace school_name_preload = "ECOLE PUBLIC DE MVENGUE VILLAGE" if inlist(interview__id, "124bc50342b34e28a602b2d313cc9ca9", "d619d6f6738d45d59ab51b04b7cfcfab", "71394654a701484fb23c6c7a55b4d596")
replace school_emis_preload = "0201102012X501" if inlist(interview__id, "124bc50342b34e28a602b2d313cc9ca9", "d619d6f6738d45d59ab51b04b7cfcfab", "71394654a701484fb23c6c7a55b4d596") // based on the enumerator comments and name matching for teachers
drop if interview__id == "71394654a701484fb23c6c7a55b4d596" // almost a perfect duplicate of the other one but since it did not have the correct name I believe the new one was entered? same applies to all other modules so safe to drop

replace school_emis_preload = "0202201003X503" if interview__id == "0e6099a10b7343e2ad3076e4fd848083" | interview__id == "fb00a555ab26463b978c3acca313aa21" // typo in the code, consulted with the sampling frame

replace school_name_preload = "GROUPE SCOLAIRE MODERNE DE L ESTUAIRE" if interview__id == "c28f90651a7c47a186e6ca4fb9a9a97b" | interview__id == "d78e48cbf2c24e15b5b798c53f2c105b"
replace school_emis_preload = "0101303154X513" if interview__id == "c28f90651a7c47a186e6ca4fb9a9a97b" | interview__id == "d78e48cbf2c24e15b5b798c53f2c105b" // based on enumerator comments

drop if interview__id == "1c54c0a1f3f644489ddb6acd8bc1a624" // fully missing, school closed

replace school_name_preload = "LA PEPINIERE DES CHAMPIONS" if interview__id == "0239e9c3b14f49ed8b8503803497af86" | interview__id == "ed9dc1fbd1be49b09071533e1e3ba4b9"
replace school_emis_preload = "0102502028X506" if interview__id == "0239e9c3b14f49ed8b8503803497af86" | interview__id == "ed9dc1fbd1be49b09071533e1e3ba4b9"

replace school_name_preload = "LES MESANGES BLEUS" if interview__id == "95ca1dd39c184a14bd9e41303aa6ab3e" | interview__id == "4afda8f59a474c81a01c4af8bb0a3c0c"
replace school_emis_preload = "0101302040X514" if interview__id == "95ca1dd39c184a14bd9e41303aa6ab3e" | interview__id == "4afda8f59a474c81a01c4af8bb0a3c0c" // just making sure that the name and the school code are consistent with the master sampling file

replace school_name_preload = "COMPLEXE SCOLAIRE L ELITE AFRICAINE" if interview__id == "a930c848854c4b318427aae1ebdb3cec"
replace school_emis_preload = "0101304028X524" if interview__id == "a930c848854c4b318427aae1ebdb3cec" // two schools in the sampling file with the same name, I picked the one with the same location as in the name of this school

replace school_name_preload = "EAPEC" if interview__id == "9a5bd1f8fafc4313bef83832145c5f3d" | interview__id == "b85df4d82fca404aa24c557eef136e5e"
replace school_emis_preload = "0101301130X505" if interview__id == "9a5bd1f8fafc4313bef83832145c5f3d" | interview__id == "b85df4d82fca404aa24c557eef136e5e" // location and enumerator note about the school name indicate it is this school

replace school_name_preload = "ECOLE PUBLIQUE DE GROS BOUQUET 3" if interview__id == "5be82c7322d74c4490440f20ed4f3a24" | interview__id == "122baf164c84466f813df4f430cc6465"
replace school_emis_preload = "0101301064X502" if interview__id == "5be82c7322d74c4490440f20ed4f3a24" | interview__id == "122baf164c84466f813df4f430cc6465" // based on the note and the sampling frame

replace school_name_preload = "ECOLE PUBLIQUE MONT BOUET 1" if interview__id == "5d5533e43aa94062a3b03ca4c71dae0d" | interview__id == "3d27876d34fd4703b74d5e8bfad059b2" | interview__id == "6a6ce045696f40299d059afa39c40a88"
replace school_emis_preload = "0101303105X508" if interview__id == "5d5533e43aa94062a3b03ca4c71dae0d" | interview__id == "3d27876d34fd4703b74d5e8bfad059b2" | interview__id == "6a6ce045696f40299d059afa39c40a88" // clearly the same school based on the duplicated modules

*this part attempts to address duplicates + fixing the remaining issues with school codes/names

/*
foreach v of varlist m5* {
	replace `v' = "" if interview__id == "54a16c9242714d36ae403cf785f5c095" // duplicated entries for this section or the teachers I cannot match to the roster :(
}
*/

foreach v of varlist numEligible* {
	replace `v' = . if interview__id == "c6fea949076740438a26c16fe6637155" // currently have 0 teachers, replacing to avoid decimal points
}

replace school_name_preload = "ECOLE DE L ALLIANCE CHRETIENNE A" if interview__id == "12bdbcb24e94407bb039633b290fb144" | interview__id == "af4f885b5cd4408893292e86fb46e9df"
replace school_emis_preload = "0202402001X510" if interview__id == "12bdbcb24e94407bb039633b290fb144" | interview__id == "af4f885b5cd4408893292e86fb46e9df" // based on enumerators comments + teacher names

replace school_name_preload = "COMPLEXE SCOLAIRE MANETTE" if interview__id == "9edd062942da4025883d7284089822e7" | interview__id == "91dfa81f1e5a45088ba4b038348d2fa1"
replace school_emis_preload = "0202402005X505" if interview__id == "9edd062942da4025883d7284089822e7" | interview__id == "91dfa81f1e5a45088ba4b038348d2fa1" // based on enumerators comments and teacher match

replace m4saq1_number = 12 if interview__id == "12bdbcb24e94407bb039633b290fb144" // making sure it it consistent with the roster of af4f885b5cd4408893292e86fb46e9df  that we are going to keep due to duplicates, yabina Angoulou
replace m6_teacher_code = 4 if interview__id == "12bdbcb24e94407bb039633b290fb144" // ngoussanga Sandrine
replace m8_teacher_code = 12  if interview__id == "12bdbcb24e94407bb039633b290fb144" // oyabina angoulou diane


foreach v of varlist numEligible m1* available* name* grade* m1* i1-i5 m2* teacher_phone_number* {
	
	cap confirm numeric variable `v'
	
	if !_rc {
		 replace `v' = . if interview__id == "12bdbcb24e94407bb039633b290fb144" // all changes are addressed above and we do not need these names
	}
	else {
		replace `v' = "" if interview__id == "12bdbcb24e94407bb039633b290fb144"
	}
}


replace m6_teacher_code = 1 if interview__id == "8229b2ba32914daeae66ae45003b55aa" // to be consistent witht the teachers from the roster we keep OBONE  ONDO ALEXIA FRIDOLINE
replace m4saq1_number = 4 if interview__id == "8229b2ba32914daeae66ae45003b55aa" // ZOGO EDANG GABRIEL


 foreach v of varlist numEligible available* name* grade* m2* i1-i5 teacher_phone_number*  {
	
	cap confirm numeric variable `v'
	
	if !_rc {
		 replace `v' = . if interview__id == "8229b2ba32914daeae66ae45003b55aa" // all changes are addressed above and we do not need these names
	}
	else {
		replace `v' = "" if interview__id == "8229b2ba32914daeae66ae45003b55aa"
	}
}

*just removing roster entries for this observation because the other ones were used in the survey
 foreach v of varlist numEligible available* name* grade* m2* i1-i5 teacher_phone_number*  {
	
	cap confirm numeric variable `v'
	
	if !_rc {
		 replace `v' = . if interview__id == "7e51032df14a4ccda9431bd4eba23b00" 
	}
	else {
		replace `v' = "" if interview__id == "7e51032df14a4ccda9431bd4eba23b00"
	}
}

**# Bookmark #3
*these two surveys have multiple duplicated sections but a lot of them have perfect matches in values. i corrected the interview id in the teacher questionnaire file to make sure we only get one survey, setting other values to missing
 foreach v of varlist numEligible* m1* available* name* grade* m2* m7* m3* i1-i5 teacher_phone_number* randomization teacher_etri_list_photo list_total needed_total  {
	
	cap confirm numeric variable `v'
	
	if !_rc {
		 replace `v' = . if interview__id == "99d2079d60a64b9e87a85b7e0783bc8a" 
	}
	else {
		replace `v' = "" if interview__id == "99d2079d60a64b9e87a85b7e0783bc8a"
	}
}

*m5 is in the first observation only, m6 is in the second only 
*m4 is in the second only + the observation module + m8 is in the second only


*these four observations (four schools) were apparently swapped, most likely because the names are very similar. i am confident that the name must be changed because of the coordinates + match of the names across observations + same date time 
replace school_name_preload = "ECOLE PUBLIQUE COMMUNALE D LEYIMADE MOANDA" if interview__id == "d6e0746b770843c2922ca6a1086bea27" | interview__id == "598991e04d734ca289b4ea8dcc3e7f7f"
replace school_emis_preload = "0202402016X524" if interview__id == "d6e0746b770843c2922ca6a1086bea27" | interview__id == "598991e04d734ca289b4ea8dcc3e7f7f" 

replace school_name_preload = "ECOLE COMMUNALE A MOANDA" if interview__id == "cf35ba34723b4210a59ffecf217a8c3b" | interview__id == "f9b0503ec13e4369a7842ea1a560c573"
replace school_emis_preload = "0202401023X508" if interview__id == "cf35ba34723b4210a59ffecf217a8c3b" | interview__id == "f9b0503ec13e4369a7842ea1a560c573"

*there is only one variable with the differences across these two duplicates, assigning the netral i do not know value to both 
replace m1scq9a_imon_etri = 98 if interview__id == "9d3656a88dbd4194b676e5e6918fee6e" | interview__id == "0ac808db3cb34278a27fa2769345985f"

replace available5 = 1 if interview__id == "123cef55f64f474387dd2a1445ab2af1" // otherwise perfect duplicate, just replacing it here to be able to collapse without decimals

replace school_name_preload = "La Conquête" if inlist(interview__id, "3c51b4627c2042dfb53f49350a267268", "f37f795fcfcd423cb16b967a008b5b89")
replace school_emis_preload = "blank" if inlist(interview__id, "3c51b4627c2042dfb53f49350a267268", "f37f795fcfcd423cb16b967a008b5b89") // the name is coming from the notes + GPS, but I cannot find any appropriate match in the frame :(

*for this one, I saved the survey from module 3 by changing the interview_id (though I guess it was not necessary), m8 is empty. these are the two duplicated sections
 foreach v of varlist numEligible* m3* m8*  {
	
	cap confirm numeric variable `v'
	
	if !_rc {
		 replace `v' = . if interview__id == "b7ffe25687b44375af7ab44b684eedd1" 
	}
	else {
		replace `v' = "" if interview__id == "b7ffe25687b44375af7ab44b684eedd1"
	}
}

replace m1sbq11_infr = 0 if interview__id == "38ab9b15f5f14340bc39859040bed15c" // just based on previous responses, it does not look like they have access to electricity, and that is the only varable with differences for the duplicated section for this school

 foreach v of varlist m1*  {
	
	cap confirm numeric variable `v'
	
	if !_rc {
		 replace `v' = . if interview__id == "38ab9b15f5f14340bc39859040bed15c" 
	}
	else {
		replace `v' = "" if interview__id == "38ab9b15f5f14340bc39859040bed15c"
	}
}

*the only variables that are missing in the other observation for this school that I want to save from the observation that I am about to drop 
replace m8_bilingual_school = 0 if interview__id == "5f6a83372fad43cb99964f757a4d4916"
replace m8_refugee = 0 if interview__id == "5f6a83372fad43cb99964f757a4d4916"

drop if interview__id == "fc3165a89085497a825ee8d369b15d51" // apart from the changes above, it is a full duplicate

*I am going to drop the observation that was entered later for this set of school since they mostly seem to be duplicates and it is unclear why it was entered later. i just want to save the variables on the remote learning opportunities since maybe those were added later
foreach v of varlist m7covq5-m7covq9_other {
	cap confirm numeric variable `v'
	if !_rc {
		bysort school_name_preload school_emis_preload: egen min_`v' = min(`v')
		replace `v' = min_`v' if interview__id == "6a6ce045696f40299d059afa39c40a88"
		drop min_`v'
	}
}

replace m7covq7_other = "classes de 5eme" if interview__id == "6a6ce045696f40299d059afa39c40a88"
replace m7covq8_other = "maths français  eveil" if interview__id == "6a6ce045696f40299d059afa39c40a88"
replace m7covq9_other = "les media" if interview__id == "6a6ce045696f40299d059afa39c40a88"

replace m8_teacher_code = 10 if interview__id == "6a6ce045696f40299d059afa39c40a88" // per roster
replace m6_teacher_code = 1 if interview__id == "6a6ce045696f40299d059afa39c40a88" // per roster, we do have 2 g1 teachers for this school...

drop if interview__id == "5d5533e43aa94062a3b03ca4c71dae0d" // now lets drop the duplicate


replace school_name_preload = "ECOLE PUBLIQUE PK8/3" if interview__id == "194727cfee4e4ba9b901c669f2411a0d" | interview__id == "f813c4b8415e4dd580a2bfc61ec62082" // the only school with this code 

*there is only one duplicated section (classroom observation). i will set the values for the one where the video part is missing + reject by supervisor as missing for that section
foreach v of varlist m4* s1_* s2_* {
	
	cap confirm numeric variable `v'
	
	if !_rc {
		 replace `v' = . if interview__id == "2eac40bd2bed4827b285a2d850554917" 
	}
	else {
		replace `v' = "" if interview__id == "2eac40bd2bed4827b285a2d850554917"
	}
}


****confirmed to be droped from all other sections
drop if interview__id == "203a6d80c42542cabf8381f680718f50" // duplicated entried in the same school code, keeping the one that seems to have more complete entries
drop if interview__id == "67c9e0789b6d44fd9996ded54cc76e01" // repeated roster information, other sections are empty
drop if interview__id == "9aea81feba494b0f9165c7331071c95c" // unclear what school it belongs to (most likely to the same school as 80aed6d7e2954cddafe321994dccd09b but we already have 3 observations for that school) 
drop if interview__id == "13ae31e266e84b4890ffc7c755b1be46" // less complete but see the notes below 

replace school_name_preload = "ECOLE PUBLIQUE D'ANGONDJE" if inlist(interview__id, "214ed16e6bfc4cf1a6bf9e7fd77a1856", "81318ef89c34488cad7f012543e34358") // based on the gps coordinates and our replacement file, otherwise we would drop it
replace school_emis_preload = "0102502040X502" if inlist(interview__id, "214ed16e6bfc4cf1a6bf9e7fd77a1856", "81318ef89c34488cad7f012543e34358") // based on the gps coordinates and our replacement file, otherwise we would drop it

*same with this school
replace school_name_preload = "ECOLE PUBLIQUE D'AKEBE 2" if inlist(interview__id, "c283907aeea247d2964db0edc3ceb5d1", "a73baa8ba1314627a5ace1b3c171f03b", "4e61bc3f3ab048d28acac1234c7370ad")
replace school_emis_preload = "0101304038X509" if inlist(interview__id, "c283907aeea247d2964db0edc3ceb5d1", "a73baa8ba1314627a5ace1b3c171f03b", "4e61bc3f3ab048d28acac1234c7370ad")


*cannot match this one to anything in our sampling or replacement file based on the coordinates
replace school_name_preload = "unidentified 4" if interview__id == "98db1f003ce947bebed28a3f4eff2abd"
replace school_emis_preload = "blank" if interview__id == "98db1f003ce947bebed28a3f4eff2abd"

replace school_name_preload = "unidentified 3" if inlist(interview__id, "a730996a68d84671abe674a045ea2b7f", "7ef0ee89725847bbb489440b91d001b6", "f1c2653e0c284737bf2047d619e5a5df")
replace school_emis_preload = "blank" if inlist(interview__id, "a730996a68d84671abe674a045ea2b7f", "7ef0ee89725847bbb489440b91d001b6", "f1c2653e0c284737bf2047d619e5a5df")

replace school_name_preload = "unidentified 5" if interview__id == "a968e663a1a6421ba37c5e5995c08836"
replace school_emis_preload = "blank" if interview__id == "a968e663a1a6421ba37c5e5995c08836"

*many modules with double entries are just missing, corrected teacher ids above and removing the missing modules here
foreach v of varlist numEligible m1* available* name* grade* i1-i5 m2* teacher_phone_number* m7* m6* {
		cap confirm numeric variable `v'
	
	if !_rc {
		 replace `v' = . if interview__id == "a73baa8ba1314627a5ace1b3c171f03b" 
	}
	else {
		replace `v' = "" if interview__id == "a73baa8ba1314627a5ace1b3c171f03b"
	}
}

*not sure what these are, so I am dropping them :(
drop if interview__id == "b0b06da7f59945ddb83abcf77fb329d2"

replace school_name_preload = "no school" if interview__id == "f92a00a7458e46dab90c84081f70b530" // felt too bad to drop this one since we have some other surveys attached to it 
replace school_emis_preload = "blank" if interview__id == "f92a00a7458e46dab90c84081f70b530"

*now bring in weights
replace school_name_preload = strupper(school_name_preload)
replace school_emis_preload = strupper(school_emis_preload)
gen school_code = school_name_preload + " _ " + school_emis_preload
replace school_code = subinstr(school_code, "'", " ", .)

preserve 
	import delimited "C:\Users\wb631589\OneDrive - WBG\GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\sampling\GEPD_GAB_weights_revised_2023-08-09.csv", varnames(1) clear 
	
	isid school_code
	tempfile weights
	save `weights'
restore

merge m:1 school_code using `weights', keepus(school_name_preload) 

*do fuzzy match for names for the ones that were not merged in
preserve 
	keep if _merge == 1 
	keep school_name_preload school_emis_preload school_code
	duplicates drop
	
	tempfile find_name
	save `find_name'
	
	gen idmaster = _n
	rename school_name_preload txtmaster
	
	tempfile master_fuzzy
	save `master_fuzzy'
restore

preserve 
	keep if _merge == 2
	keep school_name_preload school_emis_preload school_code
	duplicates drop
	
	gen idusing = _n
	rename school_name_preload txtusing
	rename school_code school_code_weights
	
	tempfile find_code
	save `find_code'
	
	tempfile using_fuzzy
	save `using_fuzzy'
restore

preserve
	use `master_fuzzy', clear

	matchit idmaster txtmaster using `using_fuzzy', idusing(idusing) txtusing(txtusing) override
	bysort idmaster: egen double max_score = max(similscore)
	keep if similscore == max_score & !inlist(idusing, 1085, 1052, 809)

	merge 1:1 idusing using `find_code', keep(3) keepus(school_code_weights)
	rename txtmaster school_name_preload
	drop txtusing 
	
	*for the school that was matched twice, only keep the one with the relevant school code
	drop if school_code_weights == "ECOLE PUBLIQUE PK8 1 _ 0101305152X510" & school_name_preload == "ECOLE PUBLIQUE PK8/3"

	tempfile for_merge
	save `for_merge'
restore

drop if _merge == 2
merge m:1 school_name_preload using `for_merge', gen(second_merge)

replace school_code = school_code_weights if second_merge == 3 & !missing(second_merge)


replace school_code = "ECOLE PUBLIQUE COMMUNALE D DE TCHIBANGA _ 0501402011X512" if school_name_preload == "ECOLE PUBLIQUE COMMUNALE D DE TCHIBANGA"
replace school_code = "ECOLE D APPLICATION DE L ENSET A _ 0101301045X512" if school_name_preload == "ENSET"

drop _merge idmaster similscore max_score school_code_weights second_merge

order school_code, after(interview__id)

save "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data_2024\EPDash.dta", replace

****drop from other sections that make sure that our merge is smooth with the main EPDash file
*drops from teacher dta

use "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\School\TEACHERS.dta", clear

local to_drop 12bdbcb24e94407bb039633b290fb144 8229b2ba32914daeae66ae45003b55aa 7e51032df14a4ccda9431bd4eba23b00 99d2079d60a64b9e87a85b7e0783bc8a e28e85116fa446cd98599af9fe8e74bb a73baa8ba1314627a5ace1b3c171f03b 13ae31e266e84b4890ffc7c755b1be46 5d5533e43aa94062a3b03ca4c71dae0d 203a6d80c42542cabf8381f680718f50 67c9e0789b6d44fd9996ded54cc76e01 b0b06da7f59945ddb83abcf77fb329d2 9d3656a88dbd4194b676e5e6918fee6e 38ab9b15f5f14340bc39859040bed15c 2ba982e7629946e0a0bc9b58d4b55fb7

foreach v of local to_drop {
	drop if interview__id == "`v'"
}

save "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data_2024\TEACHERS.dta", replace

merge m:1 interview__id using "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data_2024\EPDash.dta" // no master only


*questionnaire roster
use "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\School\questionnaire_roster.dta", clear

***TO SAVE TWO QUESTIONNAIRES FROM questionnaire_roster, WE NEED TO INCLUDE THE FOLLOWING LINES  and then we can drop 13ae31e266e84b4890ffc7c755b1be46
replace interview__id = "1f16cd5389984e60ad35798a43765ace" if interview__id == "13ae31e266e84b4890ffc7c755b1be46" & inlist(questionnaire_roster__id, 1, 2)
replace interview__key = "59-77-85-26" if interview__id == "13ae31e266e84b4890ffc7c755b1be46" & inlist(questionnaire_roster__id, 1, 2)

replace m3sb_tnumber = 11 if m3sb_troster == "NZE" & interview__id == "1f16cd5389984e60ad35798a43765ace"
replace m3sb_tnumber = 18 if m3sb_troster == "Nzahou" & interview__id == "1f16cd5389984e60ad35798a43765ace"
drop if interview__id == "13ae31e266e84b4890ffc7c755b1be46"

*second change in questionnaire roster. for these two surveys, just combine them into one 
replace interview__id = "904a5746a994450ebbf17a4bc4e25f89" if interview__id == "99d2079d60a64b9e87a85b7e0783bc8a" & inlist(questionnaire_roster__id, 5, 1, 2)
replace interview__key = "20-34-70-08" if interview__id == "99d2079d60a64b9e87a85b7e0783bc8a"
drop if interview__id == "99d2079d60a64b9e87a85b7e0783bc8a"
drop if interview__id == "904a5746a994450ebbf17a4bc4e25f89" & m3s0q1 != 1

*also need to drop this of the questionnair roster 71394654a701484fb23c6c7a55b4d596, fully duplicated
drop if interview__id == "71394654a701484fb23c6c7a55b4d596"

*next change to the roster is 
*drop if interview__id == "b7ffe25687b44375af7ab44b684eedd1" // this person seems to be interviewed in the other one as well. updated -- since the answers are similar keep this person but still a bit confused

*next change to the roster is 
drop if interview__id == "5d5533e43aa94062a3b03ca4c71dae0d" // all teachers are either unavailable or have duplicated names with the survey completed, one of the teachers with the same name has a fully survey in both entries but the entries are different, dropping this one because this interview id looks more suspicious

drop if interview__id == "9aea81feba494b0f9165c7331071c95c" // unclear what school it belongs to (most likely to the same school as 80aed6d7e2954cddafe321994dccd09b but we already have 3 observations for that school) 

save "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data_2024\questionnaire_roster.dta", replace

 merge m:1 interview__id using "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data_2024\EPDash.dta" // no master only

*teacher assessment
use "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\School\teacher_assessment_answers.dta", clear

 drop if interview__id == "5d5533e43aa94062a3b03ca4c71dae0d" // fully duplicated 
 drop if interview__id == "71394654a701484fb23c6c7a55b4d596"

*sadly i cannot match the match teachers from this survey to the main roster, so I think we have to drop them all to avoid matching them with wrong people (there is one person I can match but he alrealy seems to have the survey)? so from this file teacher_assessment_answers we need to drop. 
*this teacher seems to be entered twice but with different entries, remove some of their answers
drop if interview__id == "d3a8984382ad4d13bd42ee6fe01d6dd1" & teacher_assessment_answers__id == 5
drop if interview__id == "d3a8984382ad4d13bd42ee6fe01d6dd1" & teacher_assessment_answers__id == 3
*in the assessment file
replace m5sb_tnum = 9 if interview__id == "a73baa8ba1314627a5ace1b3c171f03b" & m5sb_tnum == 2 // per roster

save "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data_2024\teacher_assessment_answers.dta", replace

 merge m:1 interview__id using "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data_2024\EPDash.dta" // no master only
 
*ecd assessment 
use "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\School\ecd_assessment.dta", clear

drop if interview__id == "71394654a701484fb23c6c7a55b4d596"

save "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data_2024\ecd_assessment.dta", replace

merge m:1 interview__id using "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data_2024\EPDash.dta" // no master only

*fourth grade assessment 
use "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data\raw\School\fourth_grade_assessment.dta", clear

drop if interview__id == "fc3165a89085497a825ee8d369b15d51" // full duplicate
drop if interview__id == "9aea81feba494b0f9165c7331071c95c" // unclear what school it belongs to :(
 
save "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data_2024\fourth_grade_assessment.dta", replace

 merge m:1 interview__id using "$dir/GEPD-Confidential\CNT\GAB\GAB_2023_GEPD\GAB_2023_GEPD_v01_RAW\Data_2024\EPDash.dta" // no master only
