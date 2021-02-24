
# Preparacion de bbdd -----------------------------------------------------

library("here")
library(dplyr)


cl90 <- sjlabelled::read_stata(path = here("data","rawdata","WV2_Data_Chile_1990_Stata_v20180912.dta"))
cl96 <- sjlabelled::read_stata(path = here("data","rawdata","WV3_Data_Chile_1996_Stata_v20180912.dta"))

# cl06  <- sjlabelled::read_stata(path = here("data","CEP52_2006.dta"))
# cl16a <- sjlabelled::read_stata(path = here("data","CEP77_2016.dta"))
# cl16b <- sjlabelled::read_stata(path = here("data","CEP78_2016.dta"))
# load(file = here("data","rawdata","ELSOC_W02_v2.10.RData"));   cl17<- elsoc_2017
# load(file = here("data","rawdata","ELSOC_W03_v1.00_R.RData")); cl18<- elsoc_2018


# save(cl90, file = here("data","wvs1990CL.RData"))
# save(cl96, file = here("data","wvs1996CL.RData"))
# save(cl06, file = here("data","issp2006CL.RData"))
# save(cl16a,file = here("data","issp2016_A.RData"))
# save(cl16b,file = here("data","issp2016_B.RData"))
# save(cl17,file = here("data","elsoc2017.RData"))
# save(cl18,file = here("data","elsoc2018.RData"))

rm(list=ls())
load(file = here("data","wvs1990CL.RData"))
load(file = here("data","wvs1996CL.RData"))
load(file = here("data","issp1999CL.RData"))
load(file = here("data","issp2006CL.RData"))
load(file = here("data","issp2009CL.RData"))
load(file = here("data","issp2016_A.RData"))
load(file = here("data","issp2016_B.RData"))
load(file = here("data","elsoc2017.RData"))
load(file = here("data","elsoc2018.RData"))
load(file = here("data","issp2019CL.RData"))


# sjPlot::view_df(x = cl99,show.frq = T)
# sjPlot::view_df(x = cl06)
# sjPlot::view_df(x = cl09)
# sjPlot::view_df(x = cl16a)
# sjPlot::view_df(x = cl16b)
# sjPlot::view_df(x = cl19)


# Estatus social subjetivo------------------------------------

cl99$V46<- sjmisc::rec(as.numeric(cl99$V46),rec="rev")
cl99$V46[cl99$V46 %in% c(88,99)] <- NA

cl06$dd19[cl06$dd19 %in% c(88,99)] <- NA
cl09$V44[cl09$V44 %in% c(98,99)] <- NA
cl16a$DS_P27[cl16a$DS_P27 %in% c(88,99)] <- NA
cl16b$DS_P13A[cl16b$DS_P13A %in% c(88,99)] <- NA
cl19$M2_P13A[cl19$M2_P13A %in% c(88,99)] <- NA

## nombre sustantivo: Estatus subjetivo

cl99  <- rename(cl99,ess=V46)
cl06  <- rename(cl06,ess=dd19)
cl09  <- rename(cl09,ess=V44)
cl16a <- rename(cl16a,ess=DS_P27)
cl16b <- rename(cl16b,ess=DS_P13A)
cl19  <- rename(cl19,ess=M2_P13A)

cl99$ess <- as.numeric(cl99$ess)
cl06$ess <- as.numeric(cl06$ess)
cl09$ess <- as.numeric(cl09$ess)
cl16a$ess <-as.numeric(cl16a$ess) 
cl16b$ess <-as.numeric(cl16b$ess) 
cl19$ess <- as.numeric(cl19$ess)

summary(cl99$ess)
summary(cl06$ess)
summary(cl09$ess)
summary(cl16a$ess)
summary(cl16b$ess)
summary(cl19$ess)

# Educacion ---------------------------------------------------------------

cl99$EDUCYRS[cl99$EDUCYRS==97] <- NA
cl99$DEGREE[cl99$DEGREE %in% c(8,9)] <- NA
summary(cl99$EDUCYRS)
table(cl99$DEGREE)

# 1 None
# 2 Incomplete primary
# 3 Primary completed
# 4 Incomplete secondary
# 5 Secondary completed
# 6 Incomplete university # Terciary higher
# 7 University completed  # Terciary Higher
# 8 Can't choose
# 9 No answer


cl09$EDUCYRS[cl09$EDUCYRS %in% c(97,99)] <- NA # year of education
cl09$DEGREE[cl09$DEGREE %in% c(0,99)] <- NA # formal qualification level (comparative)
cl09$CL_DEGR[cl09$CL_DEGR %in% c(0,99)] <- NA # specific Chile degree

summary(cl09$EDUCYRS)
table(cl09$DEGREE)
table(cl09$CL_DEGR)


# 0	No formal qualification	
# 1	Lowest formal qualification	
# 2	Above lowest qualification	
# 3	Higher secondary completed	
# 4	Above higher secondary level, other qualification	
# 5	University degree completed	
# 8	Dont know


# 0 NAP, other countries
# - 1 None, no formal schooling
# - 2 Incomplete primary
# - 3 Primary completed
# - 4 Incomplete secondary
# - 5 Secondary completed
# - 6 University incomplete # Terciary incomplete
# - 7 University completed  # Terciaty incomplete
# - 8 Incomplete non-university higher education
# - 9 Completed non-university higher education
# 99 No answer

cl09$CL_DEGR[cl09$CL_DEGR==8] <- 6 # Incomplete non-university  ->  Terciary incomplete
cl09$CL_DEGR[cl09$CL_DEGR==9] <- 7 # Completed non-university   ->  Terciary complete
table(cl09$CL_DEGR)

# year 2019
cl19$DS_P3[cl19$DS_P3==99] <- NA
cl19$DS_P4[cl19$DS_P4==99] <- NA

summary(cl19$DS_P3)
table(cl19$DS_P4)

# (0) 1.	No estudió
# (1) 2.	Educación básica incompleta	
# (2) 3.	Educación básica completa	
# (3) 4.	Educación media incompleta
# (4) 5.	Educación media completa	
# (5) 6.	Educación superior no universitaria incompleta	
# (6) 7.	Educación superior no universitaria completa	
# (7) 8.	Educación superior universitaria incompleta	
# (8) 9.	Educación superior universitaria completa	
# (9) 10.	Estudios de postgrado, máster, doctorado	

cl19$DS_P4 <- cl19$DS_P4+1
table(cl19$DS_P4)

cl19$DS_P4[cl19$DS_P4==8] <- 6 # 6 == Terciary incomplete
cl19$DS_P4[cl19$DS_P4 %in% c(7,9,10)] <- 7 # 7 =Terciary complete

# nombre sustantivo educacion ----------------------------------------------#

cl99 <- rename(cl99,educ=EDUCYRS,educat=DEGREE)
cl09 <- rename(cl09,educ=EDUCYRS,educat=CL_DEGR)
cl19 <- rename(cl19,educ=DS_P3,educat=DS_P4)

table(cl99$educat)# Ok, (CORRER TODO PARA QUE QUEDE BIEN!)
table(cl09$educat)# Ok, (CORRER TODO PARA QUE QUEDE BIEN!)
table(cl19$educat)# Ok, (CORRER TODO PARA QUE QUEDE BIEN!)

# Educación: Educacion terciaria 


cl99$edter <- ifelse(test = (cl99$educat %in% c(7)),yes = 1,no = 0) # Complete & Incomplete Terciary =1; Less than Terciary =0
cl09$edter <- ifelse(test = (cl09$educat %in% c(7)),yes = 1,no = 0) # Complete & Incomplete Terciary =1; Less than Terciary =0
cl19$edter <- ifelse(test = (cl19$educat %in% c(7)),yes = 1,no = 0) # Complete & Incomplete Terciary =1; Less than Terciary =0

table(cl99$edter)
table(cl09$edter)
table(cl19$edter)


# Ingresos ----------------------------------------------------------------

# - Ingresos en Deciles
# - Ingresos en log
# - Si voy a usar la variable ingreso como control, ¿debería usarla en zscore para mantener validez inter-temporal?

# Year 1999---------------------------------------------------------------------------------#
cl99$INCOME[cl99$INCOME%in% c(999997,999998,999999)]   <- NA # Family income
cl99$RINCOME[cl99$RINCOME%in% c(999997,999998,999999)] <- NA # Earnings of Respondent  
table(cl99$INCOME)
table(cl99$RINCOME)

cl99$INCREAL <- cl99$INCOME
cl99$INCREAL[cl99$INCREAL==1]<- (45000+90000)/2
cl99$INCREAL[cl99$INCREAL==2]<- (91000+120000)/2
cl99$INCREAL[cl99$INCREAL==3]<- (121000+150000)/2
cl99$INCREAL[cl99$INCREAL==4]<- (151000+180000)/2
cl99$INCREAL[cl99$INCREAL==5]<- (181000+210000)/2
cl99$INCREAL[cl99$INCREAL==6]<- (211000+240000)/2
cl99$INCREAL[cl99$INCREAL==7]<- (241000+290000)/2
cl99$INCREAL[cl99$INCREAL==8]<- (291000+390000)/2
cl99$INCREAL[cl99$INCREAL==9]<- (391000+600000)/2
cl99$INCREAL[cl99$INCREAL==10]<- (601000+1000000)/2
cl99$INCREAL[cl99$INCREAL==11]<- (1000001+1500000)/2
cl99$INCREAL[cl99$INCREAL==12]<- (1500001+2000000)/2
cl99$INCREAL[cl99$INCREAL==13]<- (2000001+3000000)/2
cl99$INCREAL[cl99$INCREAL==14]<- (3000000+3200000)/2

cl99$INCREALF <- cl99$RINCOME
cl99$INCREALF[cl99$INCREALF==0]<- (45000+90000)/2
cl99$INCREALF[cl99$INCREALF==1]<- (91000+120000)/2
cl99$INCREALF[cl99$INCREALF==2]<- (121000+150000)/2
cl99$INCREALF[cl99$INCREALF==3]<- (151000+180000)/2
cl99$INCREALF[cl99$INCREALF==4]<- (181000+210000)/2
cl99$INCREALF[cl99$INCREALF==5]<- (211000+240000)/2
cl99$INCREALF[cl99$INCREALF==6]<- (241000+290000)/2
cl99$INCREALF[cl99$INCREALF==7]<- (291000+390000)/2
cl99$INCREALF[cl99$INCREALF==8]<- (391000+600000)/2
cl99$INCREALF[cl99$INCREALF==9]<- (601000+1000000)/2
cl99$INCREALF[cl99$INCREALF==10]<- (1000001+1500000)/2
cl99$INCREALF[cl99$INCREALF==11]<- (1500001+2000000)/2
cl99$INCREALF[cl99$INCREALF==12]<- (2000001+3000000)/2
cl99$INCREALF[cl99$INCREALF==14]<- (3000000+3200000)/2
summary(cl99$INCREAL) # NAs=213
summary(cl99$INCREALF)# NAs=147

cl99$D10  <- ntile(x = cl99$INCREAL, n = 10) # Crear Deciles con ingresos individuales
cl99$D10F <- ntile(x = cl99$INCREALF,n = 10) # Crear Deciles con Ingresos Familiares 
table(cl99$D10 ) # OK
table(cl99$D10F) # OK

cl99$zinc  <- as.numeric(scale(cl99$INCREAL ,center = TRUE)) # zscore de income individual
cl99$zincf <- as.numeric(scale(cl99$INCREALF,center = TRUE)) # zscore de income familiar
summary(cl99$zinc )# OK
summary(cl99$zincf)# OK


cl99$loginc  <- log(cl99$INCREAL )
cl99$logincf <- log(cl99$INCREALF)


# Deciles que vienen por defecto en la base
cl99$INCOMER[cl99$INCOMER%in% c(0,97,98,99)]   <- NA   # Family income Decile
cl99$RINCOMER[cl99$RINCOMER%in% c(0,97,98,99)]   <- NA # # Earnings of Respondent Decile
table(cl99$INCOMER)
table(cl99$RINCOMER)

# Year 2009---------------------------------------------------------------------------------#
cl09$CL_INC[cl09$CL_INC %in% c(9999990,9999998,9999999)]   <- NA # Family income
cl09$CL_RINC[cl09$CL_RINC %in% c(9999990,9999998,9999999)] <- NA # Earnings of Respondent
table(cl09$CL_INC)
table(cl09$CL_RINC)


cl09$D10  <- ntile(x = cl09$CL_INC, n = 10) # Crear Deciles con ingresos individuales
cl09$D10F <- ntile(x = cl09$CL_RINC,n = 10) # Crear Deciles con Ingresos Familiares 
table(cl09$D10 ) # OK
table(cl09$D10F) # OK

cl09$zinc  <- as.numeric(scale(cl09$CL_INC,center = TRUE)) # zscore de income individual
cl09$zincf <- as.numeric(scale(cl09$CL_RINC,center = TRUE)) # zscore de income familiar
summary(cl09$zinc )# OK
summary(cl09$zincf)# OK


cl09$loginc  <- log(cl09$CL_INC)
cl09$logincf <- log(cl09$CL_RINC)



# Year 2019---------------------------------------------------------------------------------#
cl19$DS_P38[cl19$DS_P38 %in% c(88,99)] <- NA # Ingreo individual
cl19$DS_P39[cl19$DS_P39 %in% c(88,99)] <- NA # Ingreso del Hogar
table(cl19$DS_P38)
table(cl19$DS_P39)


cl19$DS_P38[cl19$DS_P38==1]<- (32000+35000 /2)
cl19$DS_P38[cl19$DS_P38==2]<- (35001+56000 /2)
cl19$DS_P38[cl19$DS_P38==3]<- (56001+78000 /2)
cl19$DS_P38[cl19$DS_P38==4]<- (78001+101000 /2)
cl19$DS_P38[cl19$DS_P38==5]<- (101001+134000 /2)
cl19$DS_P38[cl19$DS_P38==6]<- (134001+179000 /2)
cl19$DS_P38[cl19$DS_P38==7]<- (179001+224000 /2)
cl19$DS_P38[cl19$DS_P38==8]<- (224001+291000 /2)
cl19$DS_P38[cl19$DS_P38==9]<- (291001+358000 /2)
cl19$DS_P38[cl19$DS_P38==10]<-(358001+448000 /2)
cl19$DS_P38[cl19$DS_P38==11]<-(448001+1000000 	/2)
cl19$DS_P38[cl19$DS_P38==12]<-(1000001+2000000 /2)
cl19$DS_P38[cl19$DS_P38==13]<-(2000001+3000000 /2)
cl19$DS_P38[cl19$DS_P38==14]<-(3000000+3200000/2)


cl19$DS_P39[cl19$DS_P39==1]<- (32000+35000 /2)
cl19$DS_P39[cl19$DS_P39==2]<- (35001+56000 /2)
cl19$DS_P39[cl19$DS_P39==3]<- (56001+78000 /2)
cl19$DS_P39[cl19$DS_P39==4]<- (78001+101000 /2)
cl19$DS_P39[cl19$DS_P39==5]<- (101001+134000 /2)
cl19$DS_P39[cl19$DS_P39==6]<- (134001+179000 /2)
cl19$DS_P39[cl19$DS_P39==7]<- (179001+224000 /2)
cl19$DS_P39[cl19$DS_P39==8]<- (224001+291000 /2)
cl19$DS_P39[cl19$DS_P39==9]<- (291001+358000 /2)
cl19$DS_P39[cl19$DS_P39==10]<-(358001+448000 /2)
cl19$DS_P39[cl19$DS_P39==11]<-(448001+1000000 	/2)
cl19$DS_P39[cl19$DS_P39==12]<-(1000001+2000000 /2)
cl19$DS_P39[cl19$DS_P39==13]<-(2000001+3000000 /2)
cl19$DS_P39[cl19$DS_P39==14]<-(3000000+3200000/2)

summary(cl19$DS_P38)
summary(cl19$DS_P39)

cl19$D10  <- ntile(x = cl19$DS_P38, n = 10) # Crear Deciles con ingresos individuales
cl19$D10F <- ntile(x = cl19$DS_P39,n = 10) # Crear Deciles con Ingresos Familiares 
table(cl19$D10 ) # OK
table(cl19$D10F) # OK

cl19$zinc  <- as.numeric(scale(cl19$DS_P38,center = TRUE)) # zscore de income individual
cl19$zincf <- as.numeric(scale(cl19$DS_P39,center = TRUE)) # zscore de income familiar
summary(cl19$zinc )# OK
summary(cl19$zincf)# OK


cl19$loginc  <- log(cl19$DS_P38)
cl19$logincf <- log(cl19$DS_P39)


# Ocupacion ---------------------------------------------------------------

# year 1999
cl99$isei<- cl99$ISCO88_4
cl99$isei[cl99$isei %in% c(0,9996,9997,9998,9999) ] <- NA
# ISEI individual
{
  cl99$isei[cl99$ISCO88_4==1000]=55 
  cl99$isei[cl99$ISCO88_4==1100]=70 
  cl99$isei[cl99$ISCO88_4==1110]=77 
  cl99$isei[cl99$ISCO88_4==1120]=77 
  cl99$isei[cl99$ISCO88_4==1130]=66 
  cl99$isei[cl99$ISCO88_4==1140]=58 
  cl99$isei[cl99$ISCO88_4==1141]=58 
  cl99$isei[cl99$ISCO88_4==1142]=58 
  cl99$isei[cl99$ISCO88_4==1143]=58 
  cl99$isei[cl99$ISCO88_4==1200]=68 
  cl99$isei[cl99$ISCO88_4==1210]=70 
  cl99$isei[cl99$ISCO88_4==1220]=67 
  cl99$isei[cl99$ISCO88_4==1221]=67 
  cl99$isei[cl99$ISCO88_4==1222]=67 
  cl99$isei[cl99$ISCO88_4==1223]=67 
  cl99$isei[cl99$ISCO88_4==1224]=59 
  cl99$isei[cl99$ISCO88_4==1225]=59 
  cl99$isei[cl99$ISCO88_4==1226]=59 
  cl99$isei[cl99$ISCO88_4==1227]=87 
  cl99$isei[cl99$ISCO88_4==1228]=59 
  cl99$isei[cl99$ISCO88_4==1229]=67 
  cl99$isei[cl99$ISCO88_4==1230]=61 
  cl99$isei[cl99$ISCO88_4==1231]=69 
  cl99$isei[cl99$ISCO88_4==1232]=69 
  cl99$isei[cl99$ISCO88_4==1233]=56 
  cl99$isei[cl99$ISCO88_4==1234]=69 
  cl99$isei[cl99$ISCO88_4==1235]=69 
  cl99$isei[cl99$ISCO88_4==1236]=69 
  cl99$isei[cl99$ISCO88_4==1237]=69 
  cl99$isei[cl99$ISCO88_4==1239]=69 
  cl99$isei[cl99$ISCO88_4==1240]=58 
  cl99$isei[cl99$ISCO88_4==1250]=64 
  cl99$isei[cl99$ISCO88_4==1251]=70 
  cl99$isei[cl99$ISCO88_4==1252]=60 
  cl99$isei[cl99$ISCO88_4==1300]=51 
  cl99$isei[cl99$ISCO88_4==1310]=51 
  cl99$isei[cl99$ISCO88_4==1311]=43 
  cl99$isei[cl99$ISCO88_4==1312]=56 
  cl99$isei[cl99$ISCO88_4==1313]=51 
  cl99$isei[cl99$ISCO88_4==1314]=49 
  cl99$isei[cl99$ISCO88_4==1315]=44 
  cl99$isei[cl99$ISCO88_4==1316]=51 
  cl99$isei[cl99$ISCO88_4==1317]=51 
  cl99$isei[cl99$ISCO88_4==1318]=51 
  cl99$isei[cl99$ISCO88_4==1319]=51 
  cl99$isei[cl99$ISCO88_4==2000]=70 
  cl99$isei[cl99$ISCO88_4==2100]=69 
  cl99$isei[cl99$ISCO88_4==2110]=74 
  cl99$isei[cl99$ISCO88_4==2111]=74 
  cl99$isei[cl99$ISCO88_4==2112]=74 
  cl99$isei[cl99$ISCO88_4==2113]=74 
  cl99$isei[cl99$ISCO88_4==2114]=74 
  cl99$isei[cl99$ISCO88_4==2120]=71 
  cl99$isei[cl99$ISCO88_4==2121]=71 
  cl99$isei[cl99$ISCO88_4==2122]=71 
  cl99$isei[cl99$ISCO88_4==2130]=71 
  cl99$isei[cl99$ISCO88_4==2131]=71 
  cl99$isei[cl99$ISCO88_4==2132]=71 
  cl99$isei[cl99$ISCO88_4==2139]=71 
  cl99$isei[cl99$ISCO88_4==2140]=73 
  cl99$isei[cl99$ISCO88_4==2141]=69 
  cl99$isei[cl99$ISCO88_4==2142]=69 
  cl99$isei[cl99$ISCO88_4==2143]=68 
  cl99$isei[cl99$ISCO88_4==2144]=68 
  cl99$isei[cl99$ISCO88_4==2145]=67 
  cl99$isei[cl99$ISCO88_4==2146]=71 
  cl99$isei[cl99$ISCO88_4==2147]=67 
  cl99$isei[cl99$ISCO88_4==2148]=56 
  cl99$isei[cl99$ISCO88_4==2149]=69 
  cl99$isei[cl99$ISCO88_4==2200]=80 
  cl99$isei[cl99$ISCO88_4==2210]=78 
  cl99$isei[cl99$ISCO88_4==2211]=77 
  cl99$isei[cl99$ISCO88_4==2212]=77 
  cl99$isei[cl99$ISCO88_4==2213]=79 
  cl99$isei[cl99$ISCO88_4==2220]=85 
  cl99$isei[cl99$ISCO88_4==2221]=88 
  cl99$isei[cl99$ISCO88_4==2222]=85 
  cl99$isei[cl99$ISCO88_4==2223]=83 
  cl99$isei[cl99$ISCO88_4==2224]=74 
  cl99$isei[cl99$ISCO88_4==2229]=85 
  cl99$isei[cl99$ISCO88_4==2230]=43 
  cl99$isei[cl99$ISCO88_4==2300]=69 
  cl99$isei[cl99$ISCO88_4==2310]=77 
  cl99$isei[cl99$ISCO88_4==2320]=69 
  cl99$isei[cl99$ISCO88_4==2321]=70 
  cl99$isei[cl99$ISCO88_4==2322]=66 
  cl99$isei[cl99$ISCO88_4==2330]=66 
  cl99$isei[cl99$ISCO88_4==2331]=66 
  cl99$isei[cl99$ISCO88_4==2332]=43 
  cl99$isei[cl99$ISCO88_4==2340]=66 
  cl99$isei[cl99$ISCO88_4==2350]=66 
  cl99$isei[cl99$ISCO88_4==2351]=70 
  cl99$isei[cl99$ISCO88_4==2352]=70 
  cl99$isei[cl99$ISCO88_4==2359]=65 
  cl99$isei[cl99$ISCO88_4==2400]=68 
  cl99$isei[cl99$ISCO88_4==2410]=69 
  cl99$isei[cl99$ISCO88_4==2411]=69 
  cl99$isei[cl99$ISCO88_4==2412]=69 
  cl99$isei[cl99$ISCO88_4==2419]=69 
  cl99$isei[cl99$ISCO88_4==2420]=85 
  cl99$isei[cl99$ISCO88_4==2421]=85 
  cl99$isei[cl99$ISCO88_4==2422]=90 
  cl99$isei[cl99$ISCO88_4==2429]=82 
  cl99$isei[cl99$ISCO88_4==2430]=65 
  cl99$isei[cl99$ISCO88_4==2431]=65 
  cl99$isei[cl99$ISCO88_4==2432]=65 
  cl99$isei[cl99$ISCO88_4==2440]=65 
  cl99$isei[cl99$ISCO88_4==2441]=78 
  cl99$isei[cl99$ISCO88_4==2442]=71 
  cl99$isei[cl99$ISCO88_4==2443]=71 
  cl99$isei[cl99$ISCO88_4==2444]=65 
  cl99$isei[cl99$ISCO88_4==2445]=71 
  cl99$isei[cl99$ISCO88_4==2446]=51 
  cl99$isei[cl99$ISCO88_4==2450]=61 
  cl99$isei[cl99$ISCO88_4==2451]=65 
  cl99$isei[cl99$ISCO88_4==2452]=54 
  cl99$isei[cl99$ISCO88_4==2453]=64 
  cl99$isei[cl99$ISCO88_4==2454]=64 
  cl99$isei[cl99$ISCO88_4==2455]=64 
  cl99$isei[cl99$ISCO88_4==2460]=53 
  cl99$isei[cl99$ISCO88_4==3000]=54 
  cl99$isei[cl99$ISCO88_4==3100]=50 
  cl99$isei[cl99$ISCO88_4==3110]=49 
  cl99$isei[cl99$ISCO88_4==3111]=45 
  cl99$isei[cl99$ISCO88_4==3112]=45 
  cl99$isei[cl99$ISCO88_4==3113]=46 
  cl99$isei[cl99$ISCO88_4==3114]=46 
  cl99$isei[cl99$ISCO88_4==3115]=54 
  cl99$isei[cl99$ISCO88_4==3116]=54 
  cl99$isei[cl99$ISCO88_4==3117]=54 
  cl99$isei[cl99$ISCO88_4==3118]=51 
  cl99$isei[cl99$ISCO88_4==3119]=53 
  cl99$isei[cl99$ISCO88_4==3120]=52 
  cl99$isei[cl99$ISCO88_4==3121]=52 
  cl99$isei[cl99$ISCO88_4==3122]=52 
  cl99$isei[cl99$ISCO88_4==3123]=52 
  cl99$isei[cl99$ISCO88_4==3130]=52 
  cl99$isei[cl99$ISCO88_4==3131]=48 
  cl99$isei[cl99$ISCO88_4==3132]=57 
  cl99$isei[cl99$ISCO88_4==3133]=57 
  cl99$isei[cl99$ISCO88_4==3139]=52 
  cl99$isei[cl99$ISCO88_4==3140]=57 
  cl99$isei[cl99$ISCO88_4==3141]=52 
  cl99$isei[cl99$ISCO88_4==3142]=52 
  cl99$isei[cl99$ISCO88_4==3143]=69 
  cl99$isei[cl99$ISCO88_4==3144]=69 
  cl99$isei[cl99$ISCO88_4==3145]=50 
  cl99$isei[cl99$ISCO88_4==3150]=50 
  cl99$isei[cl99$ISCO88_4==3151]=50 
  cl99$isei[cl99$ISCO88_4==3152]=50 
  cl99$isei[cl99$ISCO88_4==3200]=48 
  cl99$isei[cl99$ISCO88_4==3210]=50 
  cl99$isei[cl99$ISCO88_4==3211]=50 
  cl99$isei[cl99$ISCO88_4==3212]=50 
  cl99$isei[cl99$ISCO88_4==3213]=50 
  cl99$isei[cl99$ISCO88_4==3220]=55 
  cl99$isei[cl99$ISCO88_4==3221]=51 
  cl99$isei[cl99$ISCO88_4==3222]=51 
  cl99$isei[cl99$ISCO88_4==3223]=51 
  cl99$isei[cl99$ISCO88_4==3224]=60 
  cl99$isei[cl99$ISCO88_4==3225]=51 
  cl99$isei[cl99$ISCO88_4==3226]=60 
  cl99$isei[cl99$ISCO88_4==3227]=51 
  cl99$isei[cl99$ISCO88_4==3228]=51 
  cl99$isei[cl99$ISCO88_4==3229]=51 
  cl99$isei[cl99$ISCO88_4==3230]=38 
  cl99$isei[cl99$ISCO88_4==3231]=38 
  cl99$isei[cl99$ISCO88_4==3232]=38 
  cl99$isei[cl99$ISCO88_4==3240]=49 
  cl99$isei[cl99$ISCO88_4==3241]=51 
  cl99$isei[cl99$ISCO88_4==3242]=38 
  cl99$isei[cl99$ISCO88_4==3300]=38 
  cl99$isei[cl99$ISCO88_4==3310]=38 
  cl99$isei[cl99$ISCO88_4==3320]=38 
  cl99$isei[cl99$ISCO88_4==3330]=38 
  cl99$isei[cl99$ISCO88_4==3340]=38 
  cl99$isei[cl99$ISCO88_4==3400]=55 
  cl99$isei[cl99$ISCO88_4==3410]=55 
  cl99$isei[cl99$ISCO88_4==3411]=61 
  cl99$isei[cl99$ISCO88_4==3412]=54 
  cl99$isei[cl99$ISCO88_4==3413]=59 
  cl99$isei[cl99$ISCO88_4==3414]=56 
  cl99$isei[cl99$ISCO88_4==3415]=56 
  cl99$isei[cl99$ISCO88_4==3416]=50 
  cl99$isei[cl99$ISCO88_4==3417]=56 
  cl99$isei[cl99$ISCO88_4==3419]=55 
  cl99$isei[cl99$ISCO88_4==3420]=55 
  cl99$isei[cl99$ISCO88_4==3421]=55 
  cl99$isei[cl99$ISCO88_4==3422]=55 
  cl99$isei[cl99$ISCO88_4==3423]=55 
  cl99$isei[cl99$ISCO88_4==3429]=55 
  cl99$isei[cl99$ISCO88_4==3430]=54 
  cl99$isei[cl99$ISCO88_4==3431]=54 
  cl99$isei[cl99$ISCO88_4==3432]=59 
  cl99$isei[cl99$ISCO88_4==3433]=51 
  cl99$isei[cl99$ISCO88_4==3434]=61 
  cl99$isei[cl99$ISCO88_4==3439]=54 
  cl99$isei[cl99$ISCO88_4==3440]=56 
  cl99$isei[cl99$ISCO88_4==3441]=56 
  cl99$isei[cl99$ISCO88_4==3442]=57 
  cl99$isei[cl99$ISCO88_4==3443]=56 
  cl99$isei[cl99$ISCO88_4==3444]=46 
  cl99$isei[cl99$ISCO88_4==3449]=56 
  cl99$isei[cl99$ISCO88_4==3450]=56 
  cl99$isei[cl99$ISCO88_4==3451]=55 
  cl99$isei[cl99$ISCO88_4==3452]=56 
  cl99$isei[cl99$ISCO88_4==3460]=43 
  cl99$isei[cl99$ISCO88_4==3470]=52 
  cl99$isei[cl99$ISCO88_4==3471]=53 
  cl99$isei[cl99$ISCO88_4==3472]=64 
  cl99$isei[cl99$ISCO88_4==3473]=50 
  cl99$isei[cl99$ISCO88_4==3474]=50 
  cl99$isei[cl99$ISCO88_4==3475]=54 
  cl99$isei[cl99$ISCO88_4==3480]=38 
  cl99$isei[cl99$ISCO88_4==4000]=45 
  cl99$isei[cl99$ISCO88_4==4100]=45 
  cl99$isei[cl99$ISCO88_4==4110]=51 
  cl99$isei[cl99$ISCO88_4==4111]=51 
  cl99$isei[cl99$ISCO88_4==4112]=50 
  cl99$isei[cl99$ISCO88_4==4113]=50 
  cl99$isei[cl99$ISCO88_4==4114]=51 
  cl99$isei[cl99$ISCO88_4==4115]=53 
  cl99$isei[cl99$ISCO88_4==4120]=51 
  cl99$isei[cl99$ISCO88_4==4121]=51 
  cl99$isei[cl99$ISCO88_4==4122]=51 
  cl99$isei[cl99$ISCO88_4==4130]=36 
  cl99$isei[cl99$ISCO88_4==4131]=32 
  cl99$isei[cl99$ISCO88_4==4132]=43 
  cl99$isei[cl99$ISCO88_4==4133]=45 
  cl99$isei[cl99$ISCO88_4==4140]=39 
  cl99$isei[cl99$ISCO88_4==4141]=39 
  cl99$isei[cl99$ISCO88_4==4142]=39 
  cl99$isei[cl99$ISCO88_4==4143]=39 
  cl99$isei[cl99$ISCO88_4==4144]=39 
  cl99$isei[cl99$ISCO88_4==4190]=39 
  cl99$isei[cl99$ISCO88_4==4200]=49 
  cl99$isei[cl99$ISCO88_4==4210]=48 
  cl99$isei[cl99$ISCO88_4==4211]=53 
  cl99$isei[cl99$ISCO88_4==4212]=46 
  cl99$isei[cl99$ISCO88_4==4213]=40 
  cl99$isei[cl99$ISCO88_4==4214]=40 
  cl99$isei[cl99$ISCO88_4==4215]=40 
  cl99$isei[cl99$ISCO88_4==4220]=52 
  cl99$isei[cl99$ISCO88_4==4221]=52 
  cl99$isei[cl99$ISCO88_4==4222]=52 
  cl99$isei[cl99$ISCO88_4==4223]=52 
  cl99$isei[cl99$ISCO88_4==5000]=40 
  cl99$isei[cl99$ISCO88_4==5100]=38 
  cl99$isei[cl99$ISCO88_4==5110]=34 
  cl99$isei[cl99$ISCO88_4==5111]=34 
  cl99$isei[cl99$ISCO88_4==5112]=34 
  cl99$isei[cl99$ISCO88_4==5113]=34 
  cl99$isei[cl99$ISCO88_4==5120]=32 
  cl99$isei[cl99$ISCO88_4==5121]=30 
  cl99$isei[cl99$ISCO88_4==5122]=30 
  cl99$isei[cl99$ISCO88_4==5123]=34 
  cl99$isei[cl99$ISCO88_4==5130]=25 
  cl99$isei[cl99$ISCO88_4==5131]=25 
  cl99$isei[cl99$ISCO88_4==5132]=25 
  cl99$isei[cl99$ISCO88_4==5133]=25 
  cl99$isei[cl99$ISCO88_4==5139]=25 
  cl99$isei[cl99$ISCO88_4==5140]=30 
  cl99$isei[cl99$ISCO88_4==5141]=29 
  cl99$isei[cl99$ISCO88_4==5142]=19 
  cl99$isei[cl99$ISCO88_4==5143]=54 
  cl99$isei[cl99$ISCO88_4==5149]=19 
  cl99$isei[cl99$ISCO88_4==5150]=43 
  cl99$isei[cl99$ISCO88_4==5151]=43 
  cl99$isei[cl99$ISCO88_4==5152]=43 
  cl99$isei[cl99$ISCO88_4==5160]=47 
  cl99$isei[cl99$ISCO88_4==5161]=42 
  cl99$isei[cl99$ISCO88_4==5162]=50 
  cl99$isei[cl99$ISCO88_4==5163]=40 
  cl99$isei[cl99$ISCO88_4==5164]=40 
  cl99$isei[cl99$ISCO88_4==5169]=40 
  cl99$isei[cl99$ISCO88_4==5200]=43 
  cl99$isei[cl99$ISCO88_4==5210]=43 
  cl99$isei[cl99$ISCO88_4==5220]=43 
  cl99$isei[cl99$ISCO88_4==5230]=37 
  cl99$isei[cl99$ISCO88_4==6000]=23 
  cl99$isei[cl99$ISCO88_4==6100]=23 
  cl99$isei[cl99$ISCO88_4==6110]=23 
  cl99$isei[cl99$ISCO88_4==6111]=23 
  cl99$isei[cl99$ISCO88_4==6112]=23 
  cl99$isei[cl99$ISCO88_4==6113]=23 
  cl99$isei[cl99$ISCO88_4==6114]=23 
  cl99$isei[cl99$ISCO88_4==6120]=23 
  cl99$isei[cl99$ISCO88_4==6121]=23 
  cl99$isei[cl99$ISCO88_4==6122]=23 
  cl99$isei[cl99$ISCO88_4==6123]=23 
  cl99$isei[cl99$ISCO88_4==6124]=23 
  cl99$isei[cl99$ISCO88_4==6129]=23 
  cl99$isei[cl99$ISCO88_4==6130]=23 
  cl99$isei[cl99$ISCO88_4==6131]=23 
  cl99$isei[cl99$ISCO88_4==6132]=27 
  cl99$isei[cl99$ISCO88_4==6133]=28 
  cl99$isei[cl99$ISCO88_4==6134]=28 
  cl99$isei[cl99$ISCO88_4==6140]=22 
  cl99$isei[cl99$ISCO88_4==6141]=22 
  cl99$isei[cl99$ISCO88_4==6142]=22 
  cl99$isei[cl99$ISCO88_4==6150]=28 
  cl99$isei[cl99$ISCO88_4==6151]=28 
  cl99$isei[cl99$ISCO88_4==6152]=28 
  cl99$isei[cl99$ISCO88_4==6153]=28 
  cl99$isei[cl99$ISCO88_4==6154]=28 
  cl99$isei[cl99$ISCO88_4==6200]=16 
  cl99$isei[cl99$ISCO88_4==6210]=16 
  cl99$isei[cl99$ISCO88_4==7000]=34 
  cl99$isei[cl99$ISCO88_4==7100]=31 
  cl99$isei[cl99$ISCO88_4==7110]=30 
  cl99$isei[cl99$ISCO88_4==7111]=30 
  cl99$isei[cl99$ISCO88_4==7112]=30 
  cl99$isei[cl99$ISCO88_4==7113]=27 
  cl99$isei[cl99$ISCO88_4==7120]=30 
  cl99$isei[cl99$ISCO88_4==7121]=29 
  cl99$isei[cl99$ISCO88_4==7122]=29 
  cl99$isei[cl99$ISCO88_4==7123]=26 
  cl99$isei[cl99$ISCO88_4==7124]=29 
  cl99$isei[cl99$ISCO88_4==7129]=30 
  cl99$isei[cl99$ISCO88_4==7130]=34 
  cl99$isei[cl99$ISCO88_4==7131]=19 
  cl99$isei[cl99$ISCO88_4==7132]=30 
  cl99$isei[cl99$ISCO88_4==7133]=31 
  cl99$isei[cl99$ISCO88_4==7134]=34 
  cl99$isei[cl99$ISCO88_4==7135]=26 
  cl99$isei[cl99$ISCO88_4==7136]=33 
  cl99$isei[cl99$ISCO88_4==7137]=37 
  cl99$isei[cl99$ISCO88_4==7140]=29 
  cl99$isei[cl99$ISCO88_4==7141]=29 
  cl99$isei[cl99$ISCO88_4==7142]=32 
  cl99$isei[cl99$ISCO88_4==7143]=29 
  cl99$isei[cl99$ISCO88_4==7200]=34 
  cl99$isei[cl99$ISCO88_4==7210]=31 
  cl99$isei[cl99$ISCO88_4==7211]=29 
  cl99$isei[cl99$ISCO88_4==7212]=30 
  cl99$isei[cl99$ISCO88_4==7213]=33 
  cl99$isei[cl99$ISCO88_4==7214]=30 
  cl99$isei[cl99$ISCO88_4==7215]=30 
  cl99$isei[cl99$ISCO88_4==7216]=30 
  cl99$isei[cl99$ISCO88_4==7220]=35 
  cl99$isei[cl99$ISCO88_4==7221]=33 
  cl99$isei[cl99$ISCO88_4==7222]=40 
  cl99$isei[cl99$ISCO88_4==7223]=34 
  cl99$isei[cl99$ISCO88_4==7224]=24 
  cl99$isei[cl99$ISCO88_4==7230]=34 
  cl99$isei[cl99$ISCO88_4==7231]=34 
  cl99$isei[cl99$ISCO88_4==7232]=42 
  cl99$isei[cl99$ISCO88_4==7233]=33 
  cl99$isei[cl99$ISCO88_4==7234]=23 
  cl99$isei[cl99$ISCO88_4==7240]=40 
  cl99$isei[cl99$ISCO88_4==7241]=40 
  cl99$isei[cl99$ISCO88_4==7242]=39 
  cl99$isei[cl99$ISCO88_4==7243]=41 
  cl99$isei[cl99$ISCO88_4==7244]=40 
  cl99$isei[cl99$ISCO88_4==7245]=38 
  cl99$isei[cl99$ISCO88_4==7300]=34 
  cl99$isei[cl99$ISCO88_4==7310]=38 
  cl99$isei[cl99$ISCO88_4==7311]=38 
  cl99$isei[cl99$ISCO88_4==7312]=38 
  cl99$isei[cl99$ISCO88_4==7313]=38 
  cl99$isei[cl99$ISCO88_4==7320]=28 
  cl99$isei[cl99$ISCO88_4==7321]=27 
  cl99$isei[cl99$ISCO88_4==7322]=29 
  cl99$isei[cl99$ISCO88_4==7323]=29 
  cl99$isei[cl99$ISCO88_4==7324]=29 
  cl99$isei[cl99$ISCO88_4==7330]=29 
  cl99$isei[cl99$ISCO88_4==7331]=29 
  cl99$isei[cl99$ISCO88_4==7332]=29 
  cl99$isei[cl99$ISCO88_4==7340]=40 
  cl99$isei[cl99$ISCO88_4==7341]=40 
  cl99$isei[cl99$ISCO88_4==7342]=40 
  cl99$isei[cl99$ISCO88_4==7343]=42 
  cl99$isei[cl99$ISCO88_4==7344]=40 
  cl99$isei[cl99$ISCO88_4==7345]=37 
  cl99$isei[cl99$ISCO88_4==7346]=38 
  cl99$isei[cl99$ISCO88_4==7400]=33 
  cl99$isei[cl99$ISCO88_4==7410]=30 
  cl99$isei[cl99$ISCO88_4==7411]=30 
  cl99$isei[cl99$ISCO88_4==7412]=31 
  cl99$isei[cl99$ISCO88_4==7413]=30 
  cl99$isei[cl99$ISCO88_4==7414]=30 
  cl99$isei[cl99$ISCO88_4==7415]=30 
  cl99$isei[cl99$ISCO88_4==7416]=30 
  cl99$isei[cl99$ISCO88_4==7420]=33 
  cl99$isei[cl99$ISCO88_4==7421]=33 
  cl99$isei[cl99$ISCO88_4==7422]=33 
  cl99$isei[cl99$ISCO88_4==7423]=33 
  cl99$isei[cl99$ISCO88_4==7424]=33 
  cl99$isei[cl99$ISCO88_4==7430]=36 
  cl99$isei[cl99$ISCO88_4==7431]=29 
  cl99$isei[cl99$ISCO88_4==7432]=29 
  cl99$isei[cl99$ISCO88_4==7433]=45 
  cl99$isei[cl99$ISCO88_4==7434]=36 
  cl99$isei[cl99$ISCO88_4==7435]=36 
  cl99$isei[cl99$ISCO88_4==7436]=33 
  cl99$isei[cl99$ISCO88_4==7437]=28 
  cl99$isei[cl99$ISCO88_4==7440]=31 
  cl99$isei[cl99$ISCO88_4==7441]=31 
  cl99$isei[cl99$ISCO88_4==7442]=31 
  cl99$isei[cl99$ISCO88_4==7500]=42 
  cl99$isei[cl99$ISCO88_4==7510]=42 
  cl99$isei[cl99$ISCO88_4==7520]=39 
  cl99$isei[cl99$ISCO88_4==7530]=26 
  cl99$isei[cl99$ISCO88_4==8000]=31 
  cl99$isei[cl99$ISCO88_4==8100]=30 
  cl99$isei[cl99$ISCO88_4==8110]=35 
  cl99$isei[cl99$ISCO88_4==8111]=35 
  cl99$isei[cl99$ISCO88_4==8112]=35 
  cl99$isei[cl99$ISCO88_4==8113]=35 
  cl99$isei[cl99$ISCO88_4==8120]=30 
  cl99$isei[cl99$ISCO88_4==8121]=31 
  cl99$isei[cl99$ISCO88_4==8122]=30 
  cl99$isei[cl99$ISCO88_4==8123]=28 
  cl99$isei[cl99$ISCO88_4==8124]=30 
  cl99$isei[cl99$ISCO88_4==8130]=22 
  cl99$isei[cl99$ISCO88_4==8131]=22 
  cl99$isei[cl99$ISCO88_4==8139]=22 
  cl99$isei[cl99$ISCO88_4==8140]=27 
  cl99$isei[cl99$ISCO88_4==8141]=27 
  cl99$isei[cl99$ISCO88_4==8142]=27 
  cl99$isei[cl99$ISCO88_4==8143]=27 
  cl99$isei[cl99$ISCO88_4==8150]=35 
  cl99$isei[cl99$ISCO88_4==8151]=35 
  cl99$isei[cl99$ISCO88_4==8152]=35 
  cl99$isei[cl99$ISCO88_4==8153]=35 
  cl99$isei[cl99$ISCO88_4==8154]=35 
  cl99$isei[cl99$ISCO88_4==8155]=35 
  cl99$isei[cl99$ISCO88_4==8159]=35 
  cl99$isei[cl99$ISCO88_4==8160]=32 
  cl99$isei[cl99$ISCO88_4==8161]=33 
  cl99$isei[cl99$ISCO88_4==8162]=27 
  cl99$isei[cl99$ISCO88_4==8163]=33 
  cl99$isei[cl99$ISCO88_4==8170]=26 
  cl99$isei[cl99$ISCO88_4==8171]=26 
  cl99$isei[cl99$ISCO88_4==8172]=26 
  cl99$isei[cl99$ISCO88_4==8200]=32 
  cl99$isei[cl99$ISCO88_4==8210]=36 
  cl99$isei[cl99$ISCO88_4==8211]=36 
  cl99$isei[cl99$ISCO88_4==8212]=30 
  cl99$isei[cl99$ISCO88_4==8220]=30 
  cl99$isei[cl99$ISCO88_4==8221]=30 
  cl99$isei[cl99$ISCO88_4==8222]=30 
  cl99$isei[cl99$ISCO88_4==8223]=30 
  cl99$isei[cl99$ISCO88_4==8224]=30 
  cl99$isei[cl99$ISCO88_4==8229]=30 
  cl99$isei[cl99$ISCO88_4==8230]=30 
  cl99$isei[cl99$ISCO88_4==8231]=30 
  cl99$isei[cl99$ISCO88_4==8232]=30 
  cl99$isei[cl99$ISCO88_4==8240]=29 
  cl99$isei[cl99$ISCO88_4==8250]=38 
  cl99$isei[cl99$ISCO88_4==8251]=38 
  cl99$isei[cl99$ISCO88_4==8252]=38 
  cl99$isei[cl99$ISCO88_4==8253]=38 
  cl99$isei[cl99$ISCO88_4==8260]=30 
  cl99$isei[cl99$ISCO88_4==8261]=29 
  cl99$isei[cl99$ISCO88_4==8262]=29 
  cl99$isei[cl99$ISCO88_4==8263]=32 
  cl99$isei[cl99$ISCO88_4==8264]=24 
  cl99$isei[cl99$ISCO88_4==8265]=32 
  cl99$isei[cl99$ISCO88_4==8266]=32 
  cl99$isei[cl99$ISCO88_4==8269]=32 
  cl99$isei[cl99$ISCO88_4==8270]=29 
  cl99$isei[cl99$ISCO88_4==8271]=29 
  cl99$isei[cl99$ISCO88_4==8272]=29 
  cl99$isei[cl99$ISCO88_4==8273]=29 
  cl99$isei[cl99$ISCO88_4==8274]=29 
  cl99$isei[cl99$ISCO88_4==8275]=29 
  cl99$isei[cl99$ISCO88_4==8276]=29 
  cl99$isei[cl99$ISCO88_4==8277]=29 
  cl99$isei[cl99$ISCO88_4==8278]=29 
  cl99$isei[cl99$ISCO88_4==8279]=29 
  cl99$isei[cl99$ISCO88_4==8280]=31 
  cl99$isei[cl99$ISCO88_4==8281]=30 
  cl99$isei[cl99$ISCO88_4==8282]=34 
  cl99$isei[cl99$ISCO88_4==8283]=34 
  cl99$isei[cl99$ISCO88_4==8284]=30 
  cl99$isei[cl99$ISCO88_4==8285]=30 
  cl99$isei[cl99$ISCO88_4==8286]=30 
  cl99$isei[cl99$ISCO88_4==8290]=26 
  cl99$isei[cl99$ISCO88_4==8300]=32 
  cl99$isei[cl99$ISCO88_4==8310]=36 
  cl99$isei[cl99$ISCO88_4==8311]=41 
  cl99$isei[cl99$ISCO88_4==8312]=32 
  cl99$isei[cl99$ISCO88_4==8320]=34 
  cl99$isei[cl99$ISCO88_4==8321]=30 
  cl99$isei[cl99$ISCO88_4==8322]=30 
  cl99$isei[cl99$ISCO88_4==8323]=30 
  cl99$isei[cl99$ISCO88_4==8324]=34 
  cl99$isei[cl99$ISCO88_4==8330]=26 
  cl99$isei[cl99$ISCO88_4==8331]=26 
  cl99$isei[cl99$ISCO88_4==8332]=26 
  cl99$isei[cl99$ISCO88_4==8333]=28 
  cl99$isei[cl99$ISCO88_4==8334]=28 
  cl99$isei[cl99$ISCO88_4==8340]=32 
  cl99$isei[cl99$ISCO88_4==8400]=24 
  cl99$isei[cl99$ISCO88_4==9000]=20 
  cl99$isei[cl99$ISCO88_4==9100]=25 
  cl99$isei[cl99$ISCO88_4==9110]=29 
  cl99$isei[cl99$ISCO88_4==9111]=29 
  cl99$isei[cl99$ISCO88_4==9112]=28 
  cl99$isei[cl99$ISCO88_4==9113]=29 
  cl99$isei[cl99$ISCO88_4==9120]=28 
  cl99$isei[cl99$ISCO88_4==9130]=16 
  cl99$isei[cl99$ISCO88_4==9131]=16 
  cl99$isei[cl99$ISCO88_4==9132]=16 
  cl99$isei[cl99$ISCO88_4==9133]=16 
  cl99$isei[cl99$ISCO88_4==9140]=23 
  cl99$isei[cl99$ISCO88_4==9141]=23 
  cl99$isei[cl99$ISCO88_4==9142]=23 
  cl99$isei[cl99$ISCO88_4==9150]=27 
  cl99$isei[cl99$ISCO88_4==9151]=25 
  cl99$isei[cl99$ISCO88_4==9152]=27 
  cl99$isei[cl99$ISCO88_4==9153]=27 
  cl99$isei[cl99$ISCO88_4==9160]=23 
  cl99$isei[cl99$ISCO88_4==9161]=23 
  cl99$isei[cl99$ISCO88_4==9162]=23 
  cl99$isei[cl99$ISCO88_4==9200]=16 
  cl99$isei[cl99$ISCO88_4==9210]=16 
  cl99$isei[cl99$ISCO88_4==9211]=16 
  cl99$isei[cl99$ISCO88_4==9212]=16 
  cl99$isei[cl99$ISCO88_4==9213]=16 
  cl99$isei[cl99$ISCO88_4==9300]=23 
  cl99$isei[cl99$ISCO88_4==9310]=21 
  cl99$isei[cl99$ISCO88_4==9311]=21 
  cl99$isei[cl99$ISCO88_4==9312]=21 
  cl99$isei[cl99$ISCO88_4==9313]=21 
  cl99$isei[cl99$ISCO88_4==9320]=20 
  cl99$isei[cl99$ISCO88_4==9321]=20 
  cl99$isei[cl99$ISCO88_4==9322]=24 
  cl99$isei[cl99$ISCO88_4==9330]=29 
  cl99$isei[cl99$ISCO88_4==9331]=22 
  cl99$isei[cl99$ISCO88_4==9332]=22 
  cl99$isei[cl99$ISCO88_4==9333]=30
  cl99$isei[cl99$ISCO88_4==110 ]=70}
cl99$isei <- as.numeric(cl99$isei)
summary(cl99$isei)
table(cl99$isei)

# ISEI individual + pareja
cl99$SPIS88_4[cl99$SPIS88_4 %in% c(0,9996,9997,9998,9999) ] <- NA
cl99$isei_sp <- ifelse(test = (is.na(cl99$isei)),yes = cl99$SPIS88_4,no = cl99$isei)


# View(cl99[,c("ISCO88_4","SPIS88_4","isei","isei_sp")]) 

# year 2009
cl09$isei<- cl09$ISCO88
cl09$isei[cl09$isei %in% c(0,9996,9997,9998,9999) ] <- NA
# ISEI individual
{
  cl09$isei[cl09$ISCO88_4==1000]=55 
  cl09$isei[cl09$ISCO88_4==1100]=70 
  cl09$isei[cl09$ISCO88_4==1110]=77 
  cl09$isei[cl09$ISCO88_4==1120]=77 
  cl09$isei[cl09$ISCO88_4==1130]=66 
  cl09$isei[cl09$ISCO88_4==1140]=58 
  cl09$isei[cl09$ISCO88_4==1141]=58 
  cl09$isei[cl09$ISCO88_4==1142]=58 
  cl09$isei[cl09$ISCO88_4==1143]=58 
  cl09$isei[cl09$ISCO88_4==1200]=68 
  cl09$isei[cl09$ISCO88_4==1210]=70 
  cl09$isei[cl09$ISCO88_4==1220]=67 
  cl09$isei[cl09$ISCO88_4==1221]=67 
  cl09$isei[cl09$ISCO88_4==1222]=67 
  cl09$isei[cl09$ISCO88_4==1223]=67 
  cl09$isei[cl09$ISCO88_4==1224]=59 
  cl09$isei[cl09$ISCO88_4==1225]=59 
  cl09$isei[cl09$ISCO88_4==1226]=59 
  cl09$isei[cl09$ISCO88_4==1227]=87 
  cl09$isei[cl09$ISCO88_4==1228]=59 
  cl09$isei[cl09$ISCO88_4==1229]=67 
  cl09$isei[cl09$ISCO88_4==1230]=61 
  cl09$isei[cl09$ISCO88_4==1231]=69 
  cl09$isei[cl09$ISCO88_4==1232]=69 
  cl09$isei[cl09$ISCO88_4==1233]=56 
  cl09$isei[cl09$ISCO88_4==1234]=69 
  cl09$isei[cl09$ISCO88_4==1235]=69 
  cl09$isei[cl09$ISCO88_4==1236]=69 
  cl09$isei[cl09$ISCO88_4==1237]=69 
  cl09$isei[cl09$ISCO88_4==1239]=69 
  cl09$isei[cl09$ISCO88_4==1240]=58 
  cl09$isei[cl09$ISCO88_4==1250]=64 
  cl09$isei[cl09$ISCO88_4==1251]=70 
  cl09$isei[cl09$ISCO88_4==1252]=60 
  cl09$isei[cl09$ISCO88_4==1300]=51 
  cl09$isei[cl09$ISCO88_4==1310]=51 
  cl09$isei[cl09$ISCO88_4==1311]=43 
  cl09$isei[cl09$ISCO88_4==1312]=56 
  cl09$isei[cl09$ISCO88_4==1313]=51 
  cl09$isei[cl09$ISCO88_4==1314]=49 
  cl09$isei[cl09$ISCO88_4==1315]=44 
  cl09$isei[cl09$ISCO88_4==1316]=51 
  cl09$isei[cl09$ISCO88_4==1317]=51 
  cl09$isei[cl09$ISCO88_4==1318]=51 
  cl09$isei[cl09$ISCO88_4==1319]=51 
  cl09$isei[cl09$ISCO88_4==2000]=70 
  cl09$isei[cl09$ISCO88_4==2100]=69 
  cl09$isei[cl09$ISCO88_4==2110]=74 
  cl09$isei[cl09$ISCO88_4==2111]=74 
  cl09$isei[cl09$ISCO88_4==2112]=74 
  cl09$isei[cl09$ISCO88_4==2113]=74 
  cl09$isei[cl09$ISCO88_4==2114]=74 
  cl09$isei[cl09$ISCO88_4==2120]=71 
  cl09$isei[cl09$ISCO88_4==2121]=71 
  cl09$isei[cl09$ISCO88_4==2122]=71 
  cl09$isei[cl09$ISCO88_4==2130]=71 
  cl09$isei[cl09$ISCO88_4==2131]=71 
  cl09$isei[cl09$ISCO88_4==2132]=71 
  cl09$isei[cl09$ISCO88_4==2139]=71 
  cl09$isei[cl09$ISCO88_4==2140]=73 
  cl09$isei[cl09$ISCO88_4==2141]=69 
  cl09$isei[cl09$ISCO88_4==2142]=69 
  cl09$isei[cl09$ISCO88_4==2143]=68 
  cl09$isei[cl09$ISCO88_4==2144]=68 
  cl09$isei[cl09$ISCO88_4==2145]=67 
  cl09$isei[cl09$ISCO88_4==2146]=71 
  cl09$isei[cl09$ISCO88_4==2147]=67 
  cl09$isei[cl09$ISCO88_4==2148]=56 
  cl09$isei[cl09$ISCO88_4==2149]=69 
  cl09$isei[cl09$ISCO88_4==2200]=80 
  cl09$isei[cl09$ISCO88_4==2210]=78 
  cl09$isei[cl09$ISCO88_4==2211]=77 
  cl09$isei[cl09$ISCO88_4==2212]=77 
  cl09$isei[cl09$ISCO88_4==2213]=79 
  cl09$isei[cl09$ISCO88_4==2220]=85 
  cl09$isei[cl09$ISCO88_4==2221]=88 
  cl09$isei[cl09$ISCO88_4==2222]=85 
  cl09$isei[cl09$ISCO88_4==2223]=83 
  cl09$isei[cl09$ISCO88_4==2224]=74 
  cl09$isei[cl09$ISCO88_4==2229]=85 
  cl09$isei[cl09$ISCO88_4==2230]=43 
  cl09$isei[cl09$ISCO88_4==2300]=69 
  cl09$isei[cl09$ISCO88_4==2310]=77 
  cl09$isei[cl09$ISCO88_4==2320]=69 
  cl09$isei[cl09$ISCO88_4==2321]=70 
  cl09$isei[cl09$ISCO88_4==2322]=66 
  cl09$isei[cl09$ISCO88_4==2330]=66 
  cl09$isei[cl09$ISCO88_4==2331]=66 
  cl09$isei[cl09$ISCO88_4==2332]=43 
  cl09$isei[cl09$ISCO88_4==2340]=66 
  cl09$isei[cl09$ISCO88_4==2350]=66 
  cl09$isei[cl09$ISCO88_4==2351]=70 
  cl09$isei[cl09$ISCO88_4==2352]=70 
  cl09$isei[cl09$ISCO88_4==2359]=65 
  cl09$isei[cl09$ISCO88_4==2400]=68 
  cl09$isei[cl09$ISCO88_4==2410]=69 
  cl09$isei[cl09$ISCO88_4==2411]=69 
  cl09$isei[cl09$ISCO88_4==2412]=69 
  cl09$isei[cl09$ISCO88_4==2419]=69 
  cl09$isei[cl09$ISCO88_4==2420]=85 
  cl09$isei[cl09$ISCO88_4==2421]=85 
  cl09$isei[cl09$ISCO88_4==2422]=90 
  cl09$isei[cl09$ISCO88_4==2429]=82 
  cl09$isei[cl09$ISCO88_4==2430]=65 
  cl09$isei[cl09$ISCO88_4==2431]=65 
  cl09$isei[cl09$ISCO88_4==2432]=65 
  cl09$isei[cl09$ISCO88_4==2440]=65 
  cl09$isei[cl09$ISCO88_4==2441]=78 
  cl09$isei[cl09$ISCO88_4==2442]=71 
  cl09$isei[cl09$ISCO88_4==2443]=71 
  cl09$isei[cl09$ISCO88_4==2444]=65 
  cl09$isei[cl09$ISCO88_4==2445]=71 
  cl09$isei[cl09$ISCO88_4==2446]=51 
  cl09$isei[cl09$ISCO88_4==2450]=61 
  cl09$isei[cl09$ISCO88_4==2451]=65 
  cl09$isei[cl09$ISCO88_4==2452]=54 
  cl09$isei[cl09$ISCO88_4==2453]=64 
  cl09$isei[cl09$ISCO88_4==2454]=64 
  cl09$isei[cl09$ISCO88_4==2455]=64 
  cl09$isei[cl09$ISCO88_4==2460]=53 
  cl09$isei[cl09$ISCO88_4==3000]=54 
  cl09$isei[cl09$ISCO88_4==3100]=50 
  cl09$isei[cl09$ISCO88_4==3110]=49 
  cl09$isei[cl09$ISCO88_4==3111]=45 
  cl09$isei[cl09$ISCO88_4==3112]=45 
  cl09$isei[cl09$ISCO88_4==3113]=46 
  cl09$isei[cl09$ISCO88_4==3114]=46 
  cl09$isei[cl09$ISCO88_4==3115]=54 
  cl09$isei[cl09$ISCO88_4==3116]=54 
  cl09$isei[cl09$ISCO88_4==3117]=54 
  cl09$isei[cl09$ISCO88_4==3118]=51 
  cl09$isei[cl09$ISCO88_4==3119]=53 
  cl09$isei[cl09$ISCO88_4==3120]=52 
  cl09$isei[cl09$ISCO88_4==3121]=52 
  cl09$isei[cl09$ISCO88_4==3122]=52 
  cl09$isei[cl09$ISCO88_4==3123]=52 
  cl09$isei[cl09$ISCO88_4==3130]=52 
  cl09$isei[cl09$ISCO88_4==3131]=48 
  cl09$isei[cl09$ISCO88_4==3132]=57 
  cl09$isei[cl09$ISCO88_4==3133]=57 
  cl09$isei[cl09$ISCO88_4==3139]=52 
  cl09$isei[cl09$ISCO88_4==3140]=57 
  cl09$isei[cl09$ISCO88_4==3141]=52 
  cl09$isei[cl09$ISCO88_4==3142]=52 
  cl09$isei[cl09$ISCO88_4==3143]=69 
  cl09$isei[cl09$ISCO88_4==3144]=69 
  cl09$isei[cl09$ISCO88_4==3145]=50 
  cl09$isei[cl09$ISCO88_4==3150]=50 
  cl09$isei[cl09$ISCO88_4==3151]=50 
  cl09$isei[cl09$ISCO88_4==3152]=50 
  cl09$isei[cl09$ISCO88_4==3200]=48 
  cl09$isei[cl09$ISCO88_4==3210]=50 
  cl09$isei[cl09$ISCO88_4==3211]=50 
  cl09$isei[cl09$ISCO88_4==3212]=50 
  cl09$isei[cl09$ISCO88_4==3213]=50 
  cl09$isei[cl09$ISCO88_4==3220]=55 
  cl09$isei[cl09$ISCO88_4==3221]=51 
  cl09$isei[cl09$ISCO88_4==3222]=51 
  cl09$isei[cl09$ISCO88_4==3223]=51 
  cl09$isei[cl09$ISCO88_4==3224]=60 
  cl09$isei[cl09$ISCO88_4==3225]=51 
  cl09$isei[cl09$ISCO88_4==3226]=60 
  cl09$isei[cl09$ISCO88_4==3227]=51 
  cl09$isei[cl09$ISCO88_4==3228]=51 
  cl09$isei[cl09$ISCO88_4==3229]=51 
  cl09$isei[cl09$ISCO88_4==3230]=38 
  cl09$isei[cl09$ISCO88_4==3231]=38 
  cl09$isei[cl09$ISCO88_4==3232]=38 
  cl09$isei[cl09$ISCO88_4==3240]=49 
  cl09$isei[cl09$ISCO88_4==3241]=51 
  cl09$isei[cl09$ISCO88_4==3242]=38 
  cl09$isei[cl09$ISCO88_4==3300]=38 
  cl09$isei[cl09$ISCO88_4==3310]=38 
  cl09$isei[cl09$ISCO88_4==3320]=38 
  cl09$isei[cl09$ISCO88_4==3330]=38 
  cl09$isei[cl09$ISCO88_4==3340]=38 
  cl09$isei[cl09$ISCO88_4==3400]=55 
  cl09$isei[cl09$ISCO88_4==3410]=55 
  cl09$isei[cl09$ISCO88_4==3411]=61 
  cl09$isei[cl09$ISCO88_4==3412]=54 
  cl09$isei[cl09$ISCO88_4==3413]=59 
  cl09$isei[cl09$ISCO88_4==3414]=56 
  cl09$isei[cl09$ISCO88_4==3415]=56 
  cl09$isei[cl09$ISCO88_4==3416]=50 
  cl09$isei[cl09$ISCO88_4==3417]=56 
  cl09$isei[cl09$ISCO88_4==3419]=55 
  cl09$isei[cl09$ISCO88_4==3420]=55 
  cl09$isei[cl09$ISCO88_4==3421]=55 
  cl09$isei[cl09$ISCO88_4==3422]=55 
  cl09$isei[cl09$ISCO88_4==3423]=55 
  cl09$isei[cl09$ISCO88_4==3429]=55 
  cl09$isei[cl09$ISCO88_4==3430]=54 
  cl09$isei[cl09$ISCO88_4==3431]=54 
  cl09$isei[cl09$ISCO88_4==3432]=59 
  cl09$isei[cl09$ISCO88_4==3433]=51 
  cl09$isei[cl09$ISCO88_4==3434]=61 
  cl09$isei[cl09$ISCO88_4==3439]=54 
  cl09$isei[cl09$ISCO88_4==3440]=56 
  cl09$isei[cl09$ISCO88_4==3441]=56 
  cl09$isei[cl09$ISCO88_4==3442]=57 
  cl09$isei[cl09$ISCO88_4==3443]=56 
  cl09$isei[cl09$ISCO88_4==3444]=46 
  cl09$isei[cl09$ISCO88_4==3449]=56 
  cl09$isei[cl09$ISCO88_4==3450]=56 
  cl09$isei[cl09$ISCO88_4==3451]=55 
  cl09$isei[cl09$ISCO88_4==3452]=56 
  cl09$isei[cl09$ISCO88_4==3460]=43 
  cl09$isei[cl09$ISCO88_4==3470]=52 
  cl09$isei[cl09$ISCO88_4==3471]=53 
  cl09$isei[cl09$ISCO88_4==3472]=64 
  cl09$isei[cl09$ISCO88_4==3473]=50 
  cl09$isei[cl09$ISCO88_4==3474]=50 
  cl09$isei[cl09$ISCO88_4==3475]=54 
  cl09$isei[cl09$ISCO88_4==3480]=38 
  cl09$isei[cl09$ISCO88_4==4000]=45 
  cl09$isei[cl09$ISCO88_4==4100]=45 
  cl09$isei[cl09$ISCO88_4==4110]=51 
  cl09$isei[cl09$ISCO88_4==4111]=51 
  cl09$isei[cl09$ISCO88_4==4112]=50 
  cl09$isei[cl09$ISCO88_4==4113]=50 
  cl09$isei[cl09$ISCO88_4==4114]=51 
  cl09$isei[cl09$ISCO88_4==4115]=53 
  cl09$isei[cl09$ISCO88_4==4120]=51 
  cl09$isei[cl09$ISCO88_4==4121]=51 
  cl09$isei[cl09$ISCO88_4==4122]=51 
  cl09$isei[cl09$ISCO88_4==4130]=36 
  cl09$isei[cl09$ISCO88_4==4131]=32 
  cl09$isei[cl09$ISCO88_4==4132]=43 
  cl09$isei[cl09$ISCO88_4==4133]=45 
  cl09$isei[cl09$ISCO88_4==4140]=39 
  cl09$isei[cl09$ISCO88_4==4141]=39 
  cl09$isei[cl09$ISCO88_4==4142]=39 
  cl09$isei[cl09$ISCO88_4==4143]=39 
  cl09$isei[cl09$ISCO88_4==4144]=39 
  cl09$isei[cl09$ISCO88_4==4190]=39 
  cl09$isei[cl09$ISCO88_4==4200]=49 
  cl09$isei[cl09$ISCO88_4==4210]=48 
  cl09$isei[cl09$ISCO88_4==4211]=53 
  cl09$isei[cl09$ISCO88_4==4212]=46 
  cl09$isei[cl09$ISCO88_4==4213]=40 
  cl09$isei[cl09$ISCO88_4==4214]=40 
  cl09$isei[cl09$ISCO88_4==4215]=40 
  cl09$isei[cl09$ISCO88_4==4220]=52 
  cl09$isei[cl09$ISCO88_4==4221]=52 
  cl09$isei[cl09$ISCO88_4==4222]=52 
  cl09$isei[cl09$ISCO88_4==4223]=52 
  cl09$isei[cl09$ISCO88_4==5000]=40 
  cl09$isei[cl09$ISCO88_4==5100]=38 
  cl09$isei[cl09$ISCO88_4==5110]=34 
  cl09$isei[cl09$ISCO88_4==5111]=34 
  cl09$isei[cl09$ISCO88_4==5112]=34 
  cl09$isei[cl09$ISCO88_4==5113]=34 
  cl09$isei[cl09$ISCO88_4==5120]=32 
  cl09$isei[cl09$ISCO88_4==5121]=30 
  cl09$isei[cl09$ISCO88_4==5122]=30 
  cl09$isei[cl09$ISCO88_4==5123]=34 
  cl09$isei[cl09$ISCO88_4==5130]=25 
  cl09$isei[cl09$ISCO88_4==5131]=25 
  cl09$isei[cl09$ISCO88_4==5132]=25 
  cl09$isei[cl09$ISCO88_4==5133]=25 
  cl09$isei[cl09$ISCO88_4==5139]=25 
  cl09$isei[cl09$ISCO88_4==5140]=30 
  cl09$isei[cl09$ISCO88_4==5141]=29 
  cl09$isei[cl09$ISCO88_4==5142]=19 
  cl09$isei[cl09$ISCO88_4==5143]=54 
  cl09$isei[cl09$ISCO88_4==5149]=19 
  cl09$isei[cl09$ISCO88_4==5150]=43 
  cl09$isei[cl09$ISCO88_4==5151]=43 
  cl09$isei[cl09$ISCO88_4==5152]=43 
  cl09$isei[cl09$ISCO88_4==5160]=47 
  cl09$isei[cl09$ISCO88_4==5161]=42 
  cl09$isei[cl09$ISCO88_4==5162]=50 
  cl09$isei[cl09$ISCO88_4==5163]=40 
  cl09$isei[cl09$ISCO88_4==5164]=40 
  cl09$isei[cl09$ISCO88_4==5169]=40 
  cl09$isei[cl09$ISCO88_4==5200]=43 
  cl09$isei[cl09$ISCO88_4==5210]=43 
  cl09$isei[cl09$ISCO88_4==5220]=43 
  cl09$isei[cl09$ISCO88_4==5230]=37 
  cl09$isei[cl09$ISCO88_4==6000]=23 
  cl09$isei[cl09$ISCO88_4==6100]=23 
  cl09$isei[cl09$ISCO88_4==6110]=23 
  cl09$isei[cl09$ISCO88_4==6111]=23 
  cl09$isei[cl09$ISCO88_4==6112]=23 
  cl09$isei[cl09$ISCO88_4==6113]=23 
  cl09$isei[cl09$ISCO88_4==6114]=23 
  cl09$isei[cl09$ISCO88_4==6120]=23 
  cl09$isei[cl09$ISCO88_4==6121]=23 
  cl09$isei[cl09$ISCO88_4==6122]=23 
  cl09$isei[cl09$ISCO88_4==6123]=23 
  cl09$isei[cl09$ISCO88_4==6124]=23 
  cl09$isei[cl09$ISCO88_4==6129]=23 
  cl09$isei[cl09$ISCO88_4==6130]=23 
  cl09$isei[cl09$ISCO88_4==6131]=23 
  cl09$isei[cl09$ISCO88_4==6132]=27 
  cl09$isei[cl09$ISCO88_4==6133]=28 
  cl09$isei[cl09$ISCO88_4==6134]=28 
  cl09$isei[cl09$ISCO88_4==6140]=22 
  cl09$isei[cl09$ISCO88_4==6141]=22 
  cl09$isei[cl09$ISCO88_4==6142]=22 
  cl09$isei[cl09$ISCO88_4==6150]=28 
  cl09$isei[cl09$ISCO88_4==6151]=28 
  cl09$isei[cl09$ISCO88_4==6152]=28 
  cl09$isei[cl09$ISCO88_4==6153]=28 
  cl09$isei[cl09$ISCO88_4==6154]=28 
  cl09$isei[cl09$ISCO88_4==6200]=16 
  cl09$isei[cl09$ISCO88_4==6210]=16 
  cl09$isei[cl09$ISCO88_4==7000]=34 
  cl09$isei[cl09$ISCO88_4==7100]=31 
  cl09$isei[cl09$ISCO88_4==7110]=30 
  cl09$isei[cl09$ISCO88_4==7111]=30 
  cl09$isei[cl09$ISCO88_4==7112]=30 
  cl09$isei[cl09$ISCO88_4==7113]=27 
  cl09$isei[cl09$ISCO88_4==7120]=30 
  cl09$isei[cl09$ISCO88_4==7121]=29 
  cl09$isei[cl09$ISCO88_4==7122]=29 
  cl09$isei[cl09$ISCO88_4==7123]=26 
  cl09$isei[cl09$ISCO88_4==7124]=29 
  cl09$isei[cl09$ISCO88_4==7129]=30 
  cl09$isei[cl09$ISCO88_4==7130]=34 
  cl09$isei[cl09$ISCO88_4==7131]=19 
  cl09$isei[cl09$ISCO88_4==7132]=30 
  cl09$isei[cl09$ISCO88_4==7133]=31 
  cl09$isei[cl09$ISCO88_4==7134]=34 
  cl09$isei[cl09$ISCO88_4==7135]=26 
  cl09$isei[cl09$ISCO88_4==7136]=33 
  cl09$isei[cl09$ISCO88_4==7137]=37 
  cl09$isei[cl09$ISCO88_4==7140]=29 
  cl09$isei[cl09$ISCO88_4==7141]=29 
  cl09$isei[cl09$ISCO88_4==7142]=32 
  cl09$isei[cl09$ISCO88_4==7143]=29 
  cl09$isei[cl09$ISCO88_4==7200]=34 
  cl09$isei[cl09$ISCO88_4==7210]=31 
  cl09$isei[cl09$ISCO88_4==7211]=29 
  cl09$isei[cl09$ISCO88_4==7212]=30 
  cl09$isei[cl09$ISCO88_4==7213]=33 
  cl09$isei[cl09$ISCO88_4==7214]=30 
  cl09$isei[cl09$ISCO88_4==7215]=30 
  cl09$isei[cl09$ISCO88_4==7216]=30 
  cl09$isei[cl09$ISCO88_4==7220]=35 
  cl09$isei[cl09$ISCO88_4==7221]=33 
  cl09$isei[cl09$ISCO88_4==7222]=40 
  cl09$isei[cl09$ISCO88_4==7223]=34 
  cl09$isei[cl09$ISCO88_4==7224]=24 
  cl09$isei[cl09$ISCO88_4==7230]=34 
  cl09$isei[cl09$ISCO88_4==7231]=34 
  cl09$isei[cl09$ISCO88_4==7232]=42 
  cl09$isei[cl09$ISCO88_4==7233]=33 
  cl09$isei[cl09$ISCO88_4==7234]=23 
  cl09$isei[cl09$ISCO88_4==7240]=40 
  cl09$isei[cl09$ISCO88_4==7241]=40 
  cl09$isei[cl09$ISCO88_4==7242]=39 
  cl09$isei[cl09$ISCO88_4==7243]=41 
  cl09$isei[cl09$ISCO88_4==7244]=40 
  cl09$isei[cl09$ISCO88_4==7245]=38 
  cl09$isei[cl09$ISCO88_4==7300]=34 
  cl09$isei[cl09$ISCO88_4==7310]=38 
  cl09$isei[cl09$ISCO88_4==7311]=38 
  cl09$isei[cl09$ISCO88_4==7312]=38 
  cl09$isei[cl09$ISCO88_4==7313]=38 
  cl09$isei[cl09$ISCO88_4==7320]=28 
  cl09$isei[cl09$ISCO88_4==7321]=27 
  cl09$isei[cl09$ISCO88_4==7322]=29 
  cl09$isei[cl09$ISCO88_4==7323]=29 
  cl09$isei[cl09$ISCO88_4==7324]=29 
  cl09$isei[cl09$ISCO88_4==7330]=29 
  cl09$isei[cl09$ISCO88_4==7331]=29 
  cl09$isei[cl09$ISCO88_4==7332]=29 
  cl09$isei[cl09$ISCO88_4==7340]=40 
  cl09$isei[cl09$ISCO88_4==7341]=40 
  cl09$isei[cl09$ISCO88_4==7342]=40 
  cl09$isei[cl09$ISCO88_4==7343]=42 
  cl09$isei[cl09$ISCO88_4==7344]=40 
  cl09$isei[cl09$ISCO88_4==7345]=37 
  cl09$isei[cl09$ISCO88_4==7346]=38 
  cl09$isei[cl09$ISCO88_4==7400]=33 
  cl09$isei[cl09$ISCO88_4==7410]=30 
  cl09$isei[cl09$ISCO88_4==7411]=30 
  cl09$isei[cl09$ISCO88_4==7412]=31 
  cl09$isei[cl09$ISCO88_4==7413]=30 
  cl09$isei[cl09$ISCO88_4==7414]=30 
  cl09$isei[cl09$ISCO88_4==7415]=30 
  cl09$isei[cl09$ISCO88_4==7416]=30 
  cl09$isei[cl09$ISCO88_4==7420]=33 
  cl09$isei[cl09$ISCO88_4==7421]=33 
  cl09$isei[cl09$ISCO88_4==7422]=33 
  cl09$isei[cl09$ISCO88_4==7423]=33 
  cl09$isei[cl09$ISCO88_4==7424]=33 
  cl09$isei[cl09$ISCO88_4==7430]=36 
  cl09$isei[cl09$ISCO88_4==7431]=29 
  cl09$isei[cl09$ISCO88_4==7432]=29 
  cl09$isei[cl09$ISCO88_4==7433]=45 
  cl09$isei[cl09$ISCO88_4==7434]=36 
  cl09$isei[cl09$ISCO88_4==7435]=36 
  cl09$isei[cl09$ISCO88_4==7436]=33 
  cl09$isei[cl09$ISCO88_4==7437]=28 
  cl09$isei[cl09$ISCO88_4==7440]=31 
  cl09$isei[cl09$ISCO88_4==7441]=31 
  cl09$isei[cl09$ISCO88_4==7442]=31 
  cl09$isei[cl09$ISCO88_4==7500]=42 
  cl09$isei[cl09$ISCO88_4==7510]=42 
  cl09$isei[cl09$ISCO88_4==7520]=39 
  cl09$isei[cl09$ISCO88_4==7530]=26 
  cl09$isei[cl09$ISCO88_4==8000]=31 
  cl09$isei[cl09$ISCO88_4==8100]=30 
  cl09$isei[cl09$ISCO88_4==8110]=35 
  cl09$isei[cl09$ISCO88_4==8111]=35 
  cl09$isei[cl09$ISCO88_4==8112]=35 
  cl09$isei[cl09$ISCO88_4==8113]=35 
  cl09$isei[cl09$ISCO88_4==8120]=30 
  cl09$isei[cl09$ISCO88_4==8121]=31 
  cl09$isei[cl09$ISCO88_4==8122]=30 
  cl09$isei[cl09$ISCO88_4==8123]=28 
  cl09$isei[cl09$ISCO88_4==8124]=30 
  cl09$isei[cl09$ISCO88_4==8130]=22 
  cl09$isei[cl09$ISCO88_4==8131]=22 
  cl09$isei[cl09$ISCO88_4==8139]=22 
  cl09$isei[cl09$ISCO88_4==8140]=27 
  cl09$isei[cl09$ISCO88_4==8141]=27 
  cl09$isei[cl09$ISCO88_4==8142]=27 
  cl09$isei[cl09$ISCO88_4==8143]=27 
  cl09$isei[cl09$ISCO88_4==8150]=35 
  cl09$isei[cl09$ISCO88_4==8151]=35 
  cl09$isei[cl09$ISCO88_4==8152]=35 
  cl09$isei[cl09$ISCO88_4==8153]=35 
  cl09$isei[cl09$ISCO88_4==8154]=35 
  cl09$isei[cl09$ISCO88_4==8155]=35 
  cl09$isei[cl09$ISCO88_4==8159]=35 
  cl09$isei[cl09$ISCO88_4==8160]=32 
  cl09$isei[cl09$ISCO88_4==8161]=33 
  cl09$isei[cl09$ISCO88_4==8162]=27 
  cl09$isei[cl09$ISCO88_4==8163]=33 
  cl09$isei[cl09$ISCO88_4==8170]=26 
  cl09$isei[cl09$ISCO88_4==8171]=26 
  cl09$isei[cl09$ISCO88_4==8172]=26 
  cl09$isei[cl09$ISCO88_4==8200]=32 
  cl09$isei[cl09$ISCO88_4==8210]=36 
  cl09$isei[cl09$ISCO88_4==8211]=36 
  cl09$isei[cl09$ISCO88_4==8212]=30 
  cl09$isei[cl09$ISCO88_4==8220]=30 
  cl09$isei[cl09$ISCO88_4==8221]=30 
  cl09$isei[cl09$ISCO88_4==8222]=30 
  cl09$isei[cl09$ISCO88_4==8223]=30 
  cl09$isei[cl09$ISCO88_4==8224]=30 
  cl09$isei[cl09$ISCO88_4==8229]=30 
  cl09$isei[cl09$ISCO88_4==8230]=30 
  cl09$isei[cl09$ISCO88_4==8231]=30 
  cl09$isei[cl09$ISCO88_4==8232]=30 
  cl09$isei[cl09$ISCO88_4==8240]=29 
  cl09$isei[cl09$ISCO88_4==8250]=38 
  cl09$isei[cl09$ISCO88_4==8251]=38 
  cl09$isei[cl09$ISCO88_4==8252]=38 
  cl09$isei[cl09$ISCO88_4==8253]=38 
  cl09$isei[cl09$ISCO88_4==8260]=30 
  cl09$isei[cl09$ISCO88_4==8261]=29 
  cl09$isei[cl09$ISCO88_4==8262]=29 
  cl09$isei[cl09$ISCO88_4==8263]=32 
  cl09$isei[cl09$ISCO88_4==8264]=24 
  cl09$isei[cl09$ISCO88_4==8265]=32 
  cl09$isei[cl09$ISCO88_4==8266]=32 
  cl09$isei[cl09$ISCO88_4==8269]=32 
  cl09$isei[cl09$ISCO88_4==8270]=29 
  cl09$isei[cl09$ISCO88_4==8271]=29 
  cl09$isei[cl09$ISCO88_4==8272]=29 
  cl09$isei[cl09$ISCO88_4==8273]=29 
  cl09$isei[cl09$ISCO88_4==8274]=29 
  cl09$isei[cl09$ISCO88_4==8275]=29 
  cl09$isei[cl09$ISCO88_4==8276]=29 
  cl09$isei[cl09$ISCO88_4==8277]=29 
  cl09$isei[cl09$ISCO88_4==8278]=29 
  cl09$isei[cl09$ISCO88_4==8279]=29 
  cl09$isei[cl09$ISCO88_4==8280]=31 
  cl09$isei[cl09$ISCO88_4==8281]=30 
  cl09$isei[cl09$ISCO88_4==8282]=34 
  cl09$isei[cl09$ISCO88_4==8283]=34 
  cl09$isei[cl09$ISCO88_4==8284]=30 
  cl09$isei[cl09$ISCO88_4==8285]=30 
  cl09$isei[cl09$ISCO88_4==8286]=30 
  cl09$isei[cl09$ISCO88_4==8290]=26 
  cl09$isei[cl09$ISCO88_4==8300]=32 
  cl09$isei[cl09$ISCO88_4==8310]=36 
  cl09$isei[cl09$ISCO88_4==8311]=41 
  cl09$isei[cl09$ISCO88_4==8312]=32 
  cl09$isei[cl09$ISCO88_4==8320]=34 
  cl09$isei[cl09$ISCO88_4==8321]=30 
  cl09$isei[cl09$ISCO88_4==8322]=30 
  cl09$isei[cl09$ISCO88_4==8323]=30 
  cl09$isei[cl09$ISCO88_4==8324]=34 
  cl09$isei[cl09$ISCO88_4==8330]=26 
  cl09$isei[cl09$ISCO88_4==8331]=26 
  cl09$isei[cl09$ISCO88_4==8332]=26 
  cl09$isei[cl09$ISCO88_4==8333]=28 
  cl09$isei[cl09$ISCO88_4==8334]=28 
  cl09$isei[cl09$ISCO88_4==8340]=32 
  cl09$isei[cl09$ISCO88_4==8400]=24 
  cl09$isei[cl09$ISCO88_4==9000]=20 
  cl09$isei[cl09$ISCO88_4==9100]=25 
  cl09$isei[cl09$ISCO88_4==9110]=29 
  cl09$isei[cl09$ISCO88_4==9111]=29 
  cl09$isei[cl09$ISCO88_4==9112]=28 
  cl09$isei[cl09$ISCO88_4==9113]=29 
  cl09$isei[cl09$ISCO88_4==9120]=28 
  cl09$isei[cl09$ISCO88_4==9130]=16 
  cl09$isei[cl09$ISCO88_4==9131]=16 
  cl09$isei[cl09$ISCO88_4==9132]=16 
  cl09$isei[cl09$ISCO88_4==9133]=16 
  cl09$isei[cl09$ISCO88_4==9140]=23 
  cl09$isei[cl09$ISCO88_4==9141]=23 
  cl09$isei[cl09$ISCO88_4==9142]=23 
  cl09$isei[cl09$ISCO88_4==9150]=27 
  cl09$isei[cl09$ISCO88_4==9151]=25 
  cl09$isei[cl09$ISCO88_4==9152]=27 
  cl09$isei[cl09$ISCO88_4==9153]=27 
  cl09$isei[cl09$ISCO88_4==9160]=23 
  cl09$isei[cl09$ISCO88_4==9161]=23 
  cl09$isei[cl09$ISCO88_4==9162]=23 
  cl09$isei[cl09$ISCO88_4==9200]=16 
  cl09$isei[cl09$ISCO88_4==9210]=16 
  cl09$isei[cl09$ISCO88_4==9211]=16 
  cl09$isei[cl09$ISCO88_4==9212]=16 
  cl09$isei[cl09$ISCO88_4==9213]=16 
  cl09$isei[cl09$ISCO88_4==9300]=23 
  cl09$isei[cl09$ISCO88_4==9310]=21 
  cl09$isei[cl09$ISCO88_4==9311]=21 
  cl09$isei[cl09$ISCO88_4==9312]=21 
  cl09$isei[cl09$ISCO88_4==9313]=21 
  cl09$isei[cl09$ISCO88_4==9320]=20 
  cl09$isei[cl09$ISCO88_4==9321]=20 
  cl09$isei[cl09$ISCO88_4==9322]=24 
  cl09$isei[cl09$ISCO88_4==9330]=29 
  cl09$isei[cl09$ISCO88_4==9331]=22 
  cl09$isei[cl09$ISCO88_4==9332]=22 
  cl09$isei[cl09$ISCO88_4==9333]=30
  cl09$isei[cl09$ISCO88_4==110 ]=70}
cl09$isei <- as.numeric(cl09$isei)
summary(cl09$isei)
table(cl09$isei)

# ISEI individual + pareja
cl09$SPISCO88[cl09$SPISCO88 %in% c(0,9996,9997,9998,9999) ] <- NA
cl09$isei_sp <- ifelse(test = (is.na(cl09$isei)),yes = cl09$SPISCO88,no = cl09$isei)

# View(cl09[,c("ISCO88","SPISCO88","isei","isei_sp")]) 

# year 2019

# No hay codigo ISCO88 en CEP 2019


# Estatus laboral ---------------------------------------------------------

# Year 1999

cl99$estlab<- cl99$WRKST
cl99$estlab[cl99$estlab %in% c(0,99)] <- NA
table(cl99$estlab) 
sjlabelled::get_labels(cl99$estlab)

# Year 2009

cl09$estlab<- cl09$WRKST
cl09$estlab[cl09$estlab %in% c(97,98,99)] <- NA
table(cl09$estlab)
sjlabelled::get_labels(cl09$estlab)


# Year 2019

cl19$estlab<- cl19$DS_P13
cl19$estlab[cl19$estlab %in% c(11,88,99)] <- NA
table(cl19$estlab)
sjlabelled::get_labels(cl19$estlab)

# Sexo --------------------------------------------------------------------

# year 1999

table(cl99$SEX) 


# 1 Male 
# 2 Female
# 9 No answer/ Refused

# year 2009

table(cl09$SEX)
# 1 Male
# 2 Female
# 9 No answer, refused

table(cl19$DS_P1)

# 1.	Hombre	48,3%
# 2.	Mujer	51,7%

cl99 <- rename(cl99,sexo=SEX)
cl09 <- rename(cl09,sexo=SEX)
cl19 <- rename(cl19,sexo=DS_P1)


# Edad --------------------------------------------------------------------
# year 1999
table(cl99$AGE)
cl99$AGE[cl99$AGE %in% c(98,99) ] <- NA
summary(cl99$AGE)
cl99 <- rename(cl99,edad=AGE)
cl99$edad2 <- cl99$edad^2

# Year 2009
table(cl09$AGE)
cl09$AGE[cl09$AGE==99] <- NA
summary(cl09$AGE)
cl09 <- rename(cl09,edad=AGE)
cl09$edad2 <- cl09$edad^2


# Year 2019
table(cl19$DS_P2_EXACTA)
summary(cl19$DS_P2_EXACTA)
cl19 <- rename(cl19,edad=DS_P2_EXACTA)
cl19$edad2 <- cl19$edad^2

## Variables subjetivas----------------------------------------------------


# {r salario percibido y justo 1999}---------------------------------------------------------------
# M2_P2_PRESIDENTE cuanto gana GERENTE
# M2_P2_OBRERO cuanto gana OBRERO
# M2_P3_PRESIDENTE cuanto deberia ganar GERENTE
# M2_P3_OBRERO cuanto deberia ganar OBRERO
# table(salarios99$V21) #  99999999999
# table(salarios99$V16) #  99999999998 99999999999
# table(salarios99$V31) # 99999999998 99999999999 
# table(salarios99$V26) # 99999999998 99999999999 
cl99$V21[cl99$V21 %in% c(99999999998,99999999999)] <- NA
cl99$V16[cl99$V16 %in% c(99999999998,99999999999)] <- NA
cl99$V31[cl99$V31 %in% c(99999999998,99999999999)] <- NA
cl99$V26[cl99$V26 %in% c(99999999998,99999999999)] <- NA

#--- Nombres sustantivos para analisis
cl99 <- rename(cl99,pais=V3,salperger=V16,salperobr=V21,saljusger=V26,saljusobr=V31)
cl99$year <- 1999 


# {r remover outliers 1999, include=FALSE}
table(cl99$salperobr) # <22000000 120000000 
table(cl99$salperger) # <100000000 150000000 200000000 300000000 500000000 543000000


table(cl99$saljusobr) # <30000000
table(cl99$saljusger)

#Quitar outliers *************************
# cl99 <- cl99 %>% filter(salperobr<22000000 & salperobr>=50000 & salperger<100000000 & salperger>=400000)

# Filtrato por outliers menor a $22.000.000 en salario percibido obrero y mayor o igual a $50.000
# Filtrato por outliers menor a $100.000.000 en salario percibido gerente y mayor o igual a $400.000
cl99$gap_perc  <-  as.numeric(cl99$salperger/cl99$salperobr) # brecha total salario percibido 
cl99$gap_just  <-  as.numeric(cl99$saljusger/cl99$saljusobr) # brecha total salario justo


# {r gap salario 1999}
#---Brecha salarial percibida
cl99$gap_perc   <-  as.numeric(cl99$salperger/cl99$salperobr)       # diferencia total
cl99$lngap_perc <-  as.numeric(log(cl99$gap_perc))                  # diferencia log

#---Brecha salarial justa
cl99$gap_just   <-  as.numeric(cl99$saljusger/cl99$saljusobr) # diferencia total
cl99$lngap_just <-  as.numeric(log(cl99$gap_just))                 # diferencia log


# {r salario percibido y justo 2009}---------------------------------------------------------------

# table(cl09$V23) # cuanto gana GERENTE
# table(cl09$V25) # cuanto gana OBRERO
# table(cl09$V28) # cuando deberia ganar GERENTE
# table(cl09$V30) # cuanto deberia ganar OBRERO
cl09$V23[cl09$V23 %in%c(-99,-98,-97,999999999996)] <- NA
cl09$V25[cl09$V25 %in%c(-99,-98,-97)]              <- NA
cl09$V28[cl09$V28 %in%c(-99,-98,-97)]              <- NA
cl09$V30[cl09$V30 %in%c(-99,-98,-97)]              <- NA

#--- Nombres sustantivos para analisis
cl09 <- rename(cl09,pais=V5,salperger=V23,salperobr=V25,saljusger=V28,saljusobr=V30)
cl09$year <- 2009


# {r remover outliers 2009, include=FALSE}
table(cl09$salperobr) #   880     150     158     160     180     190     200     300   12000   16000   50000
table(cl09$salperger) #   5000     10000     20000

table(cl09$saljusobr) #   190      230      250      300      350      500
table(cl09$saljusger) # 50000
#Quitar outliers *************************
# cl09 <- cl09 %>% filter(salperobr<22000000 & salperobr>=50000 & salperger<100000000 & salperger>=400000)
# Filtrato por outliers menor a $22.000.000 en salario percibido obrero y mayor a $50.000
# Filtrato por outliers menor a $100.000.000 en salario percibido gerente y mayor a $400.000
cl09$gap_perc  <-  as.numeric(cl09$salperger/cl09$salperobr) # brecha total salario percibido 
cl09$gap_just  <-  as.numeric(cl09$saljusger/cl09$saljusobr) # brecha total salario justo


# {r gap salario 2009}
#---Brecha salarial percibida
cl09$gap_perc   <-  as.numeric(cl09$salperger/cl09$salperobr) # diferencia total
cl09$lngap_perc <-  as.numeric(log(cl09$gap_perc))                # diferencia log

#---Brecha salarial justa
cl09$gap_just   <-  as.numeric(cl09$saljusger/cl09$saljusobr) # diferencia total
cl09$lngap_just <-  as.numeric(log(cl09$gap_just))                # diferencia log


# {r salario percibido y justo 2019}---------------------------------------------------------------

# table(cl19$M2_P2_PRESIDENTE) # cuanto gana GERENTE
# table(cl19$M2_P2_OBRERO)     # cuanto gana OBRERO
# table(cl19$M2_P3_PRESIDENTE) # cuando deberia ganar GERENTE
# table(cl19$M2_P3_OBRERO )    # cuanto deberia ganar OBRERO
# cl09$V23[cl09$V23 %in%c(-99,-98,-97,999999999996)] <- NA
# cl09$V25[cl09$V25 %in%c(-99,-98,-97)]              <- NA
# cl09$V28[cl09$V28 %in%c(-99,-98,-97)]              <- NA
# cl09$V30[cl09$V30 %in%c(-99,-98,-97)]              <- NA

#--- Nombres sustantivos para analisis
cl19 <- rename(cl19,salperger=M2_P2_PRESIDENTE,salperobr=M2_P2_OBRERO,
                     saljusger=M2_P3_PRESIDENTE,saljusobr=M2_P3_OBRERO)
cl19$year <- 2019


# {r remover outliers 2019, include=FALSE}
table(cl19$salperobr) # 500
table(cl19$saljusobr) # 300     500     600     800
table(cl19$salperger) # ok 
table(cl19$saljusger) # ok
#Quitar outliers *************************
# cl19 <- cl19 %>% filter(salperobr<22000000 & salperobr>=50000 & salperger<100000000 & salperger>=400000)
# Filtrato por outliers menor a $22.000.000 en salario percibido obrero y mayor a $50.000
# Filtrato por outliers menor a $100.000.000 en salario percibido gerente y mayor a $400.000
cl19$gap_perc  <-  as.numeric(cl19$salperger/cl19$salperobr) # brecha total salario percibido 
cl19$gap_just  <-  as.numeric(cl19$saljusger/cl19$saljusobr) # brecha total salario justo


# cl19 <- cl19 %>% filter(salperobr<22000000 & salperobr>=50000 & salperger<100000000 & salperger>=400000)
#---Brecha salarial percibida
cl19$gap_perc   <-  as.numeric(cl19$salperger/cl19$salperobr) # diferencia total
cl19$lngap_perc <-  as.numeric(log(cl19$gap_perc))                  # diferencia log

#---Brecha salarial justa
cl19$gap_just   <-  as.numeric(cl19$saljusger/cl19$saljusobr) # diferencia total
cl19$lngap_just <-  as.numeric(log(cl19$gap_just))            # diferencia log



# Percepcion de desigualdad: Las diferencias de ingreso --------

cl99$V34    <- sjmisc::rec(cl99$V34 ,rec="rev")
cl09$V32    <- sjmisc::rec(cl09$V32 ,rec="rev")
cl19$M2_P4_1<- sjmisc::rec(cl19$M2_P4_1 ,rec="rev")


cl99$V34    [cl99$V34     %in% c(8,9)] <- NA
cl09$V32    [cl09$V32     %in% c(8,9)] <- NA
cl19$M2_P4_1[cl19$M2_P4_1 %in% c(8,9)] <- NA

table(cl99$V34)
table(cl09$V32)
table(cl19$M2_P4_1) 

# Seleccionar variables  --------------------------------------------------

db01 <- cl99 %>% dplyr::select(year,sexo,edad,edad2,ess,educ,educat,edter,D10,D10F,zinc,zincf,loginc,logincf,gap_perc,gap_just,lngap_perc,lngap_just)
db02 <- cl09 %>% dplyr::select(year,sexo,edad,edad2,ess,educ,educat,edter,D10,D10F,zinc,zincf,loginc,logincf,gap_perc,gap_just,lngap_perc,lngap_just)
db03 <- cl19 %>% dplyr::select(year,sexo,edad,edad2,ess,educ,educat,edter,D10,D10F,zinc,zincf,loginc,logincf,gap_perc,gap_just,lngap_perc,lngap_just)

names(db01)
names(db02)
names(db03)


# Data imputation para ingresos -------------------------------------------
# método con OLS ---------------------------------------------------------#
# year 1999--------------------------------------------------------------------------#
lm01<- lm(as.numeric(D10)~educ+sexo+edad+edad2,data=db01,na.action = na.exclude)
summary(lm01)
db01$fit <- predict.lm(object = lm01,newdata = db01[,c("educ","sexo","edad","edad2")])

db01$D10i <- ifelse(test = (db01$fit>0 & db01$fit <=1),yes = 1, no = un01$fit)
db01$D10i <- ifelse(test = (db01$fit>1 & db01$fit <=2),yes = 2, no = db01$D10i)
db01$D10i <- ifelse(test = (db01$fit>2 & db01$fit <=3),yes = 3, no = db01$D10i)
db01$D10i <- ifelse(test = (db01$fit>3 & db01$fit <=4),yes = 4, no = db01$D10i)
db01$D10i <- ifelse(test = (db01$fit>4 & db01$fit <=5),yes = 5, no = db01$D10i)
db01$D10i <- ifelse(test = (db01$fit>5 & db01$fit <=6),yes = 6, no = db01$D10i)
db01$D10i <- ifelse(test = (db01$fit>6 & db01$fit <=7),yes = 7, no = db01$D10i)
db01$D10i <- ifelse(test = (db01$fit>7 & db01$fit <=8),yes = 8, no = db01$D10i)
db01$D10i <- ifelse(test = (db01$fit>8 & db01$fit <=9),yes = 9, no = db01$D10i)
db01$D10i <- ifelse(test = (db01$fit>9 | db01$fit >10),yes = 10,no = db01$D10i)
table(db01$D10i)

# Remplazamos los NA de D10 por los valores predichos por el modelo
db01$D10imp <- ifelse(test = is.na(db01$D10),yes = db01$D10i,no = db01$D10) 
summary(db01$D10imp)
summary(db01$D10)
t.test(x = summary(db01$D10imp),y = summary(db01$D10)) # ttest medias de D10 

# Year 2009 --------------------------------------------------------------------------#
lm02<- lm(as.numeric(D10)~educ+sexo+edad+edad2,data=db02)
summary(lm02)
db02$fit <- predict.lm(object = lm02,newdata = db02[,c("educ","sexo","edad","edad2")])

db02$D10i <- ifelse(test = (db02$fit>0 & db02$fit <=1),yes = 1, no = un01$fit)
db02$D10i <- ifelse(test = (db02$fit>1 & db02$fit <=2),yes = 2, no = db02$D10i)
db02$D10i <- ifelse(test = (db02$fit>2 & db02$fit <=3),yes = 3, no = db02$D10i)
db02$D10i <- ifelse(test = (db02$fit>3 & db02$fit <=4),yes = 4, no = db02$D10i)
db02$D10i <- ifelse(test = (db02$fit>4 & db02$fit <=5),yes = 5, no = db02$D10i)
db02$D10i <- ifelse(test = (db02$fit>5 & db02$fit <=6),yes = 6, no = db02$D10i)
db02$D10i <- ifelse(test = (db02$fit>6 & db02$fit <=7),yes = 7, no = db02$D10i)
db02$D10i <- ifelse(test = (db02$fit>7 & db02$fit <=8),yes = 8, no = db02$D10i)
db02$D10i <- ifelse(test = (db02$fit>8 & db02$fit <=9),yes = 9, no = db02$D10i)
db02$D10i <- ifelse(test = (db02$fit>9 | db02$fit >10),yes = 10,no = db02$D10i)
table(db02$D10i)

# Remplazamos los NA de D10 por los valores predichos por el modelo
db02$D10imp <- ifelse(test = is.na(db02$D10),yes = db02$D10i,no = db02$D10) 
summary(db02$D10imp)
summary(db02$D10)
t.test(x = summary(db02$D10imp),y = summary(db02$D10)) # ttest medias de D10 vs D10imp
 
#Year 2019 --------------------------------------------------------------------------#
lm03<- lm(as.numeric(D10)~educ+sexo+edad+edad2,data=db03)
summary(lm03)
db03$fit <- predict.lm(object = lm03,newdata = db03[,c("educ","sexo","edad","edad2")])

db03$D10i <- ifelse(test = (db03$fit>0 & db03$fit <=1),yes = 1, no = un01$fit)
db03$D10i <- ifelse(test = (db03$fit>1 & db03$fit <=2),yes = 2, no = db03$D10i)
db03$D10i <- ifelse(test = (db03$fit>2 & db03$fit <=3),yes = 3, no = db03$D10i)
db03$D10i <- ifelse(test = (db03$fit>3 & db03$fit <=4),yes = 4, no = db03$D10i)
db03$D10i <- ifelse(test = (db03$fit>4 & db03$fit <=5),yes = 5, no = db03$D10i)
db03$D10i <- ifelse(test = (db03$fit>5 & db03$fit <=6),yes = 6, no = db03$D10i)
db03$D10i <- ifelse(test = (db03$fit>6 & db03$fit <=7),yes = 7, no = db03$D10i)
db03$D10i <- ifelse(test = (db03$fit>7 & db03$fit <=8),yes = 8, no = db03$D10i)
db03$D10i <- ifelse(test = (db03$fit>8 & db03$fit <=9),yes = 9, no = db03$D10i)
db03$D10i <- ifelse(test = (db03$fit>9 | db03$fit >10),yes = 10,no = db03$D10i)
table(db03$D10i)

# Remplazamos los NA de D10 por los valores predichos por el modelo
db03$D10imp <- ifelse(test = is.na(db03$D10),yes = db03$D10i,no = db03$D10) 
summary(db03$D10imp)
summary(db03$D10)
t.test(x = summary(db03$D10imp),y = summary(db03$D10)) # ttest medias de D10 vs D10imp


# Pegar base de datos -----------------------------------------------------
data_chile <- bind_rows(db01,db02,db03)
skimr::skim(data_chile)
table(data_chile$year)

save(data_chile,file = here("data","data_chile.RData"))


# Base sin outliers en variables brechas ----------------------------------

db01cln <- cl99 %>% dplyr::filter(salperobr<22000000 & salperobr>=50000 & salperger<100000000 & salperger>=400000)
db02cln <- cl09 %>% dplyr::filter(salperobr<22000000 & salperobr>=50000 & salperger<100000000 & salperger>=400000)
db03cln <- cl19 %>% dplyr::filter(salperobr<22000000 & salperobr>=50000 & salperger<100000000 & salperger>=400000)

db01cln <- db01cln %>% dplyr::select(year,sexo,ess,educ,educat,edter,D10,D10F,loginc,loginc,zinc,zincf,gap_perc,gap_just,lngap_perc,lngap_just)
db02cln <- db02cln %>% dplyr::select(year,sexo,ess,educ,educat,edter,D10,D10F,loginc,loginc,zinc,zincf,gap_perc,gap_just,lngap_perc,lngap_just)
db03cln <- db03cln %>% dplyr::select(year,sexo,ess,educ,educat,edter,D10,D10F,loginc,loginc,zinc,zincf,gap_perc,gap_just,lngap_perc,lngap_just)


data_chile_clean <- bind_rows(db01cln,db02cln,db03cln)
save(data_chile_clean,file = here("data","data_chile_clean.RData"))





