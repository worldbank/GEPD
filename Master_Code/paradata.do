
gen timestamp1=subinstr(timestamp, "T"," ",.)

gen double tm=clock(timestamp1, "YMD hms")
format tm %tC
levelsof interview__id , local (res)
local counter=1
gen resp_id=.
foreach var in `res' {
replace resp_id=`counter' if interview__id=="`var'"
local counter=`counter'+1

}
tsset resp_id order
gen time_length=f.tm-tm

replace time_length=(f.tm-tm)/1000

*cleaning to split parameters field into question, module, response
split parameters, gen(mod) parse("||")
rename mod1 question
gen module = substr(question, 1,4)
rename mod2 response


*Tabulate time of each module
egen module_time=total(time_length), by(interview__id module)
gen module_time_min=module_time/60

tabstat module_time_min if parameters!="", by(module) 