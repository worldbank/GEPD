*Code gender for principal
clear
use "C:\Users\wb469649\WBG\Ezequiel Molina - Dashboard (Team Folder)\Country_Work\Peru\2019\Data\raw\School\EPDash.dta" 
gsort -m1saq1_first

gen m7saq10=.

*add in gender based on name
replace m7saq10=1 if _n==1
replace m7saq10=2 if _n==2
replace m7saq10=1 if _n==3
replace m7saq10=1 if _n==4
replace m7saq10=2 if _n==5
replace m7saq10=2 if _n==6
replace m7saq10=1 if _n==7
replace m7saq10=2 if _n==8
replace m7saq10=2 if _n==9
replace m7saq10=1 if _n==10

replace m7saq10=1 if _n==11
replace m7saq10=2 if _n==12
replace m7saq10=2 if _n==13
replace m7saq10=1 if _n==14
replace m7saq10=1 if _n==15
replace m7saq10=2 if _n==16
replace m7saq10=2 if _n==17
replace m7saq10=1 if _n==18
replace m7saq10=2 if _n==19
replace m7saq10=1 if _n==20

replace m7saq10=1 if _n==21
replace m7saq10=1 if _n==22
replace m7saq10=1 if _n==23
replace m7saq10=1 if _n==24
replace m7saq10=1 if _n==25
replace m7saq10=1 if _n==26
replace m7saq10=1 if _n==27
replace m7saq10=2 if _n==28
replace m7saq10=1 if _n==29
replace m7saq10=1 if _n==30

replace m7saq10=1 if _n==31
replace m7saq10=1 if _n==32
replace m7saq10=1 if _n==33
replace m7saq10=1 if _n==34
replace m7saq10=2 if _n==35
replace m7saq10=1 if _n==36
replace m7saq10=2 if _n==37
replace m7saq10=2 if _n==38
replace m7saq10=2 if _n==39
replace m7saq10=2 if _n==40

replace m7saq10=2 if _n==41
replace m7saq10=2 if _n==42
replace m7saq10=1 if _n==43
replace m7saq10=2 if _n==44
replace m7saq10=2 if _n==45
replace m7saq10=2 if _n==46
replace m7saq10=1 if _n==47
replace m7saq10=1 if _n==48
replace m7saq10=1 if _n==49
replace m7saq10=2 if _n==50

replace m7saq10=2 if _n==51
replace m7saq10=1 if _n==52
replace m7saq10=1 if _n==53
replace m7saq10=2 if _n==54
replace m7saq10=1 if _n==55
replace m7saq10=1 if _n==56
replace m7saq10=2 if _n==57
replace m7saq10=2 if _n==58
replace m7saq10=1 if _n==59
replace m7saq10=2 if _n==60

replace m7saq10=1 if _n==61
replace m7saq10=1 if _n==62
replace m7saq10=1 if _n==63
replace m7saq10=1 if _n==64
replace m7saq10=1 if _n==65
replace m7saq10=1 if _n==66
replace m7saq10=1 if _n==67
replace m7saq10=2 if _n==68
replace m7saq10=2 if _n==69
replace m7saq10=1 if _n==70

replace m7saq10=1 if _n==71
replace m7saq10=1 if _n==72
replace m7saq10=1 if _n==73
replace m7saq10=2 if _n==74
replace m7saq10=2 if _n==75
replace m7saq10=1 if _n==76
replace m7saq10=2 if _n==77
replace m7saq10=2 if _n==78
replace m7saq10=2 if _n==79
replace m7saq10=2 if _n==80

replace m7saq10=1 if _n==81
replace m7saq10=2 if _n==82
replace m7saq10=2 if _n==83
replace m7saq10=2 if _n==84
replace m7saq10=2 if _n==85
replace m7saq10=2 if _n==86
replace m7saq10=2 if _n==87
replace m7saq10=1 if _n==88
replace m7saq10=2 if _n==89
replace m7saq10=1 if _n==90

replace m7saq10=1 if _n==91
replace m7saq10=2 if _n==92
replace m7saq10=2 if _n==93
replace m7saq10=2 if _n==94
replace m7saq10=2 if _n==95
replace m7saq10=1 if _n==96
replace m7saq10=1 if _n==97
replace m7saq10=1 if _n==98
replace m7saq10=2 if _n==99
replace m7saq10=1 if _n==100

replace m7saq10=1 if _n==101
replace m7saq10=1 if _n==102
replace m7saq10=1 if _n==103
replace m7saq10=1 if _n==104
replace m7saq10=1 if _n==105
replace m7saq10=2 if _n==106
replace m7saq10=2 if _n==107
replace m7saq10=1 if _n==108
replace m7saq10=2 if _n==109
replace m7saq10=2 if _n==110

replace m7saq10=1 if _n==111
replace m7saq10=1 if _n==112
replace m7saq10=1 if _n==113
replace m7saq10=1 if _n==114
replace m7saq10=1 if _n==115
replace m7saq10=1 if _n==116
replace m7saq10=1 if _n==117
replace m7saq10=1 if _n==118
replace m7saq10=1 if _n==119
replace m7saq10=1 if _n==120

replace m7saq10=1 if _n==121
replace m7saq10=1 if _n==122
replace m7saq10=1 if _n==123
replace m7saq10=1 if _n==124
replace m7saq10=1 if _n==125
replace m7saq10=2 if _n==126
replace m7saq10=1 if _n==127
replace m7saq10=2 if _n==128
replace m7saq10=2 if _n==129
replace m7saq10=1 if _n==130

replace m7saq10=1 if _n==131
replace m7saq10=1 if _n==132
replace m7saq10=1 if _n==133
replace m7saq10=1 if _n==134
replace m7saq10=1 if _n==135
replace m7saq10=1 if _n==136
replace m7saq10=2 if _n==137
replace m7saq10=1 if _n==138
replace m7saq10=2 if _n==139
replace m7saq10=1 if _n==140

replace m7saq10=1 if _n==141
replace m7saq10=1 if _n==142
replace m7saq10=1 if _n==143
replace m7saq10=2 if _n==144
replace m7saq10=2 if _n==145
replace m7saq10=2 if _n==146
replace m7saq10=1 if _n==147
replace m7saq10=1 if _n==148
replace m7saq10=1 if _n==149
replace m7saq10=2 if _n==150

replace m7saq10=1 if _n==151
replace m7saq10=1 if _n==152
replace m7saq10=1 if _n==153
replace m7saq10=1 if _n==154
replace m7saq10=1 if _n==155
replace m7saq10=2 if _n==156
replace m7saq10=2 if _n==157
replace m7saq10=2 if _n==158
replace m7saq10=1 if _n==159
replace m7saq10=1 if _n==160

replace m7saq10=2 if _n==161
replace m7saq10=1 if _n==162
replace m7saq10=1 if _n==163
replace m7saq10=2 if _n==164
replace m7saq10=1 if _n==165
replace m7saq10=2 if _n==166
replace m7saq10=2 if _n==167
replace m7saq10=2 if _n==168
replace m7saq10=2 if _n==169
replace m7saq10=2 if _n==170

replace m7saq10=2 if _n==171
replace m7saq10=1 if _n==172
replace m7saq10=1 if _n==173
replace m7saq10=1 if _n==174
replace m7saq10=1 if _n==175
replace m7saq10=2 if _n==176
replace m7saq10=2 if _n==177
replace m7saq10=2 if _n==178
replace m7saq10=1 if _n==179
replace m7saq10=2 if _n==180

replace m7saq10=1 if _n==181
replace m7saq10=2 if _n==182
replace m7saq10=1 if _n==183
replace m7saq10=2 if _n==184
replace m7saq10=1 if _n==185
replace m7saq10=1 if _n==186
replace m7saq10=2 if _n==187
replace m7saq10=1 if _n==188
replace m7saq10=1 if _n==189
replace m7saq10=2 if _n==190

replace m7saq10=2 if _n==191
replace m7saq10=2 if _n==192
replace m7saq10=2 if _n==193
replace m7saq10=1 if _n==194
replace m7saq10=1 if _n==195
replace m7saq10=1 if _n==196
replace m7saq10=2 if _n==197
replace m7saq10=1 if _n==198
replace m7saq10=1 if _n==199
replace m7saq10=1 if _n==200

replace m7saq10=2 if _n==201
replace m7saq10=1 if _n==202
replace m7saq10=1 if _n==203
replace m7saq10=1 if _n==204
replace m7saq10=2 if _n==205
replace m7saq10=2 if _n==206

save "C:\Users\wb469649\WBG\Ezequiel Molina - Dashboard (Team Folder)\Country_Work\Peru\2019\Data\raw\School\EPDash_v2.dta" , replace
