
# Preparacion de bbdd -----------------------------------------------------

library(sjlabelled)
library("here")
library(dplyr)


rm(list=ls())

options(scipen=999) 
# cl09 <- read_stata(path = here("data","issp2009.dta"))
# save(cl09,file = here("data","issp2009.RData"))

load(file = here("data","issp2009.RData"))
names(cl09)

# sjPlot::view_df(x = cl09)

# Estatus social subjetivo------------------------------------

cl09$V44[cl09$V44 %in% c(97,98,99)]         <- NA

## nombre sustantivo: Estatus subjetivo

cl09 <- rename(cl09,ess=V44)
cl09$ess <- as.numeric(cl09$ess)
summary(cl09$ess)
class(cl09$ess)

# Educacion ---------------------------------------------------------------


table(cl09$DEGREE)

cl09$EDUCYRS[cl09$EDUCYRS %in% c(95,96,97,98,99)] <- NA # year of education
cl09$DEGREE[cl09$DEGREE   %in% c(8,9)]            <- NA # formal qualification level (comparative)

summary(cl09$EDUCYRS)
class(cl09$EDUCYRS)
cl09$EDUCYRS <- as.numeric(cl09$EDUCYRS)
table(cl09$DEGREE)

cl09$DEGREE <- as_factor(cl09$DEGREE)



# nombre sustantivo educacion ----------------------------------------------#

cl09 <- rename(cl09,educ=EDUCYRS,educat=DEGREE)

table(cl09$educat) # OK, (CORRER TODO PARA QUE QUEDE BIEN!)

# Educación: Educacion terciaria 

cl09$edter <- ifelse(test = (cl09$educat ==5),yes = 1,no = 0) # Complete & Incomplete Terciary =1; Less than Terciary =0
table(cl09$edter)


# Ingresos ----------------------------------------------------------------

# - Ingresos en Deciles
# - Ingresos en log
# - Si voy a usar la variable ingreso como control, ¿debería usarla en zscore para mantener validez inter-temporal?

# Year 2009---------------------------------------------------------------------------------#


income <- c("AR_INC","AT_INC","AU_INC","BE_INC","BG_INC","CH_INC","CL_INC","CN_INC","CY_INC","CZ_INC","DE_INC","DK_INC",
              "EE_INC","ES_INC","FI_INC","FR_INC","GB_INC","HR_INC","HU_INC","IL_INC","IS_INC","IT_INC","JP_INC","KR_INC","LT_INC",
              "LV_INC","NO_INC","NZ_INC","PH_INC","PL_INC","PT_INC","RU_INC","SE_INC","SI_INC","SK_INC","TR_INC","TW_INC","UA_INC",
              "US_INC","VE_INC","ZA_INC")

lapply(cl09[,income], table)

for (i in income) {
  cl09[[i]][cl09[[i]] %in% c(999990,999997,999998,999999,9999990,9999998,9999999,9999997,99999990,99999999,99999990,99999999,99999998)] <- NA
  cl09[[i]][cl09[[i]] %in% c(999990,999997,999998,999999,9999990,9999998,9999999,9999997,99999990,99999999,99999990,99999999,99999998)] <- NA
  }

lapply(cl09[,income], table)  # Ok el recode de ingresos familiares 

# Crear variable ingresos unica 

psum2 <- function(...,na.rm=FALSE) { 
  dat <- do.call(cbind,list(...))
  res <- rowSums(dat, na.rm=na.rm) 
  idx_na <- !rowSums(!is.na(dat))
  res[idx_na] <- NA
  res 
}

cl09$INCOME <- psum2(x = cl09[,income],na.rm = TRUE)
class(cl09$INCOME)

table(cl09$HOMPOP)
cl09$HOMPOP[cl09$HOMPOP %in%c(0,99) ] <- NA

cl09$incomepcap <- as.numeric(cl09$INCOME/cl09$HOMPOP) # Ingreso per capita del hogar
  
cl09 <- rename(cl09,country=V4)

cl09 <- cl09 %>%group_by(country)  %>% mutate(D10=ntile(incomepcap,10),
                                              Q05=ntile(incomepcap,5),
                                              zinc=scale(incomepcap,center = TRUE),
                                              loginc=log(incomepcap)) %>% ungroup()
deciles <- cl09[,c("country","INCOME","HOMPOP","incomepcap","zinc","D10")]

# Ocupacion ---------------------------------------------------------------
#     - 9996 Not classif; inadeq described
#     - 9997 Refused
#     - 9998 Dont know
#     - 9999 Na
#     - 0000 NAP,NAV 
table(cl09$ISCO88)

500+170+600+1451+6820 #check de missing. Debería tener 9541 missing en ISEI
# Crear ISEI con recode de Ganzeboom & Treiman-----------------------------#
cl09$isei<- cl09$ISCO88
cl09$isei[cl09$isei %in% c(0,9996,9997,9998,9999) ] <- NA


{
cl09$isei[cl09$ISCO88==1000]=55 
cl09$isei[cl09$ISCO88==1100]=70 
cl09$isei[cl09$ISCO88==1110]=77 
cl09$isei[cl09$ISCO88==1120]=77 
cl09$isei[cl09$ISCO88==1130]=66 
cl09$isei[cl09$ISCO88==1140]=58 
cl09$isei[cl09$ISCO88==1141]=58 
cl09$isei[cl09$ISCO88==1142]=58 
cl09$isei[cl09$ISCO88==1143]=58 
cl09$isei[cl09$ISCO88==1200]=68 
cl09$isei[cl09$ISCO88==1210]=70 
cl09$isei[cl09$ISCO88==1220]=67 
cl09$isei[cl09$ISCO88==1221]=67 
cl09$isei[cl09$ISCO88==1222]=67 
cl09$isei[cl09$ISCO88==1223]=67 
cl09$isei[cl09$ISCO88==1224]=59 
cl09$isei[cl09$ISCO88==1225]=59 
cl09$isei[cl09$ISCO88==1226]=59 
cl09$isei[cl09$ISCO88==1227]=87 
cl09$isei[cl09$ISCO88==1228]=59 
cl09$isei[cl09$ISCO88==1229]=67 
cl09$isei[cl09$ISCO88==1230]=61 
cl09$isei[cl09$ISCO88==1231]=69 
cl09$isei[cl09$ISCO88==1232]=69 
cl09$isei[cl09$ISCO88==1233]=56 
cl09$isei[cl09$ISCO88==1234]=69 
cl09$isei[cl09$ISCO88==1235]=69 
cl09$isei[cl09$ISCO88==1236]=69 
cl09$isei[cl09$ISCO88==1237]=69 
cl09$isei[cl09$ISCO88==1239]=69 
cl09$isei[cl09$ISCO88==1240]=58 
cl09$isei[cl09$ISCO88==1250]=64 
cl09$isei[cl09$ISCO88==1251]=70 
cl09$isei[cl09$ISCO88==1252]=60 
cl09$isei[cl09$ISCO88==1300]=51 
cl09$isei[cl09$ISCO88==1310]=51 
cl09$isei[cl09$ISCO88==1311]=43 
cl09$isei[cl09$ISCO88==1312]=56 
cl09$isei[cl09$ISCO88==1313]=51 
cl09$isei[cl09$ISCO88==1314]=49 
cl09$isei[cl09$ISCO88==1315]=44 
cl09$isei[cl09$ISCO88==1316]=51 
cl09$isei[cl09$ISCO88==1317]=51 
cl09$isei[cl09$ISCO88==1318]=51 
cl09$isei[cl09$ISCO88==1319]=51 
cl09$isei[cl09$ISCO88==2000]=70 
cl09$isei[cl09$ISCO88==2100]=69 
cl09$isei[cl09$ISCO88==2110]=74 
cl09$isei[cl09$ISCO88==2111]=74 
cl09$isei[cl09$ISCO88==2112]=74 
cl09$isei[cl09$ISCO88==2113]=74 
cl09$isei[cl09$ISCO88==2114]=74 
cl09$isei[cl09$ISCO88==2120]=71 
cl09$isei[cl09$ISCO88==2121]=71 
cl09$isei[cl09$ISCO88==2122]=71 
cl09$isei[cl09$ISCO88==2130]=71 
cl09$isei[cl09$ISCO88==2131]=71 
cl09$isei[cl09$ISCO88==2132]=71 
cl09$isei[cl09$ISCO88==2139]=71 
cl09$isei[cl09$ISCO88==2140]=73 
cl09$isei[cl09$ISCO88==2141]=69 
cl09$isei[cl09$ISCO88==2142]=69 
cl09$isei[cl09$ISCO88==2143]=68 
cl09$isei[cl09$ISCO88==2144]=68 
cl09$isei[cl09$ISCO88==2145]=67 
cl09$isei[cl09$ISCO88==2146]=71 
cl09$isei[cl09$ISCO88==2147]=67 
cl09$isei[cl09$ISCO88==2148]=56 
cl09$isei[cl09$ISCO88==2149]=69 
cl09$isei[cl09$ISCO88==2200]=80 
cl09$isei[cl09$ISCO88==2210]=78 
cl09$isei[cl09$ISCO88==2211]=77 
cl09$isei[cl09$ISCO88==2212]=77 
cl09$isei[cl09$ISCO88==2213]=79 
cl09$isei[cl09$ISCO88==2220]=85 
cl09$isei[cl09$ISCO88==2221]=88 
cl09$isei[cl09$ISCO88==2222]=85 
cl09$isei[cl09$ISCO88==2223]=83 
cl09$isei[cl09$ISCO88==2224]=74 
cl09$isei[cl09$ISCO88==2229]=85 
cl09$isei[cl09$ISCO88==2230]=43 
cl09$isei[cl09$ISCO88==2300]=69 
cl09$isei[cl09$ISCO88==2310]=77 
cl09$isei[cl09$ISCO88==2320]=69 
cl09$isei[cl09$ISCO88==2321]=70 
cl09$isei[cl09$ISCO88==2322]=66 
cl09$isei[cl09$ISCO88==2330]=66 
cl09$isei[cl09$ISCO88==2331]=66 
cl09$isei[cl09$ISCO88==2332]=43 
cl09$isei[cl09$ISCO88==2340]=66 
cl09$isei[cl09$ISCO88==2350]=66 
cl09$isei[cl09$ISCO88==2351]=70 
cl09$isei[cl09$ISCO88==2352]=70 
cl09$isei[cl09$ISCO88==2359]=65 
cl09$isei[cl09$ISCO88==2400]=68 
cl09$isei[cl09$ISCO88==2410]=69 
cl09$isei[cl09$ISCO88==2411]=69 
cl09$isei[cl09$ISCO88==2412]=69 
cl09$isei[cl09$ISCO88==2419]=69 
cl09$isei[cl09$ISCO88==2420]=85 
cl09$isei[cl09$ISCO88==2421]=85 
cl09$isei[cl09$ISCO88==2422]=90 
cl09$isei[cl09$ISCO88==2429]=82 
cl09$isei[cl09$ISCO88==2430]=65 
cl09$isei[cl09$ISCO88==2431]=65 
cl09$isei[cl09$ISCO88==2432]=65 
cl09$isei[cl09$ISCO88==2440]=65 
cl09$isei[cl09$ISCO88==2441]=78 
cl09$isei[cl09$ISCO88==2442]=71 
cl09$isei[cl09$ISCO88==2443]=71 
cl09$isei[cl09$ISCO88==2444]=65 
cl09$isei[cl09$ISCO88==2445]=71 
cl09$isei[cl09$ISCO88==2446]=51 
cl09$isei[cl09$ISCO88==2450]=61 
cl09$isei[cl09$ISCO88==2451]=65 
cl09$isei[cl09$ISCO88==2452]=54 
cl09$isei[cl09$ISCO88==2453]=64 
cl09$isei[cl09$ISCO88==2454]=64 
cl09$isei[cl09$ISCO88==2455]=64 
cl09$isei[cl09$ISCO88==2460]=53 
cl09$isei[cl09$ISCO88==3000]=54 
cl09$isei[cl09$ISCO88==3100]=50 
cl09$isei[cl09$ISCO88==3110]=49 
cl09$isei[cl09$ISCO88==3111]=45 
cl09$isei[cl09$ISCO88==3112]=45 
cl09$isei[cl09$ISCO88==3113]=46 
cl09$isei[cl09$ISCO88==3114]=46 
cl09$isei[cl09$ISCO88==3115]=54 
cl09$isei[cl09$ISCO88==3116]=54 
cl09$isei[cl09$ISCO88==3117]=54 
cl09$isei[cl09$ISCO88==3118]=51 
cl09$isei[cl09$ISCO88==3119]=53 
cl09$isei[cl09$ISCO88==3120]=52 
cl09$isei[cl09$ISCO88==3121]=52 
cl09$isei[cl09$ISCO88==3122]=52 
cl09$isei[cl09$ISCO88==3123]=52 
cl09$isei[cl09$ISCO88==3130]=52 
cl09$isei[cl09$ISCO88==3131]=48 
cl09$isei[cl09$ISCO88==3132]=57 
cl09$isei[cl09$ISCO88==3133]=57 
cl09$isei[cl09$ISCO88==3139]=52 
cl09$isei[cl09$ISCO88==3140]=57 
cl09$isei[cl09$ISCO88==3141]=52 
cl09$isei[cl09$ISCO88==3142]=52 
cl09$isei[cl09$ISCO88==3143]=69 
cl09$isei[cl09$ISCO88==3144]=69 
cl09$isei[cl09$ISCO88==3145]=50 
cl09$isei[cl09$ISCO88==3150]=50 
cl09$isei[cl09$ISCO88==3151]=50 
cl09$isei[cl09$ISCO88==3152]=50 
cl09$isei[cl09$ISCO88==3200]=48 
cl09$isei[cl09$ISCO88==3210]=50 
cl09$isei[cl09$ISCO88==3211]=50 
cl09$isei[cl09$ISCO88==3212]=50 
cl09$isei[cl09$ISCO88==3213]=50 
cl09$isei[cl09$ISCO88==3220]=55 
cl09$isei[cl09$ISCO88==3221]=51 
cl09$isei[cl09$ISCO88==3222]=51 
cl09$isei[cl09$ISCO88==3223]=51 
cl09$isei[cl09$ISCO88==3224]=60 
cl09$isei[cl09$ISCO88==3225]=51 
cl09$isei[cl09$ISCO88==3226]=60 
cl09$isei[cl09$ISCO88==3227]=51 
cl09$isei[cl09$ISCO88==3228]=51 
cl09$isei[cl09$ISCO88==3229]=51 
cl09$isei[cl09$ISCO88==3230]=38 
cl09$isei[cl09$ISCO88==3231]=38 
cl09$isei[cl09$ISCO88==3232]=38 
cl09$isei[cl09$ISCO88==3240]=49 
cl09$isei[cl09$ISCO88==3241]=51 
cl09$isei[cl09$ISCO88==3242]=38 
cl09$isei[cl09$ISCO88==3300]=38 
cl09$isei[cl09$ISCO88==3310]=38 
cl09$isei[cl09$ISCO88==3320]=38 
cl09$isei[cl09$ISCO88==3330]=38 
cl09$isei[cl09$ISCO88==3340]=38 
cl09$isei[cl09$ISCO88==3400]=55 
cl09$isei[cl09$ISCO88==3410]=55 
cl09$isei[cl09$ISCO88==3411]=61 
cl09$isei[cl09$ISCO88==3412]=54 
cl09$isei[cl09$ISCO88==3413]=59 
cl09$isei[cl09$ISCO88==3414]=56 
cl09$isei[cl09$ISCO88==3415]=56 
cl09$isei[cl09$ISCO88==3416]=50 
cl09$isei[cl09$ISCO88==3417]=56 
cl09$isei[cl09$ISCO88==3419]=55 
cl09$isei[cl09$ISCO88==3420]=55 
cl09$isei[cl09$ISCO88==3421]=55 
cl09$isei[cl09$ISCO88==3422]=55 
cl09$isei[cl09$ISCO88==3423]=55 
cl09$isei[cl09$ISCO88==3429]=55 
cl09$isei[cl09$ISCO88==3430]=54 
cl09$isei[cl09$ISCO88==3431]=54 
cl09$isei[cl09$ISCO88==3432]=59 
cl09$isei[cl09$ISCO88==3433]=51 
cl09$isei[cl09$ISCO88==3434]=61 
cl09$isei[cl09$ISCO88==3439]=54 
cl09$isei[cl09$ISCO88==3440]=56 
cl09$isei[cl09$ISCO88==3441]=56 
cl09$isei[cl09$ISCO88==3442]=57 
cl09$isei[cl09$ISCO88==3443]=56 
cl09$isei[cl09$ISCO88==3444]=46 
cl09$isei[cl09$ISCO88==3449]=56 
cl09$isei[cl09$ISCO88==3450]=56 
cl09$isei[cl09$ISCO88==3451]=55 
cl09$isei[cl09$ISCO88==3452]=56 
cl09$isei[cl09$ISCO88==3460]=43 
cl09$isei[cl09$ISCO88==3470]=52 
cl09$isei[cl09$ISCO88==3471]=53 
cl09$isei[cl09$ISCO88==3472]=64 
cl09$isei[cl09$ISCO88==3473]=50 
cl09$isei[cl09$ISCO88==3474]=50 
cl09$isei[cl09$ISCO88==3475]=54 
cl09$isei[cl09$ISCO88==3480]=38 
cl09$isei[cl09$ISCO88==4000]=45 
cl09$isei[cl09$ISCO88==4100]=45 
cl09$isei[cl09$ISCO88==4110]=51 
cl09$isei[cl09$ISCO88==4111]=51 
cl09$isei[cl09$ISCO88==4112]=50 
cl09$isei[cl09$ISCO88==4113]=50 
cl09$isei[cl09$ISCO88==4114]=51 
cl09$isei[cl09$ISCO88==4115]=53 
cl09$isei[cl09$ISCO88==4120]=51 
cl09$isei[cl09$ISCO88==4121]=51 
cl09$isei[cl09$ISCO88==4122]=51 
cl09$isei[cl09$ISCO88==4130]=36 
cl09$isei[cl09$ISCO88==4131]=32 
cl09$isei[cl09$ISCO88==4132]=43 
cl09$isei[cl09$ISCO88==4133]=45 
cl09$isei[cl09$ISCO88==4140]=39 
cl09$isei[cl09$ISCO88==4141]=39 
cl09$isei[cl09$ISCO88==4142]=39 
cl09$isei[cl09$ISCO88==4143]=39 
cl09$isei[cl09$ISCO88==4144]=39 
cl09$isei[cl09$ISCO88==4190]=39 
cl09$isei[cl09$ISCO88==4200]=49 
cl09$isei[cl09$ISCO88==4210]=48 
cl09$isei[cl09$ISCO88==4211]=53 
cl09$isei[cl09$ISCO88==4212]=46 
cl09$isei[cl09$ISCO88==4213]=40 
cl09$isei[cl09$ISCO88==4214]=40 
cl09$isei[cl09$ISCO88==4215]=40 
cl09$isei[cl09$ISCO88==4220]=52 
cl09$isei[cl09$ISCO88==4221]=52 
cl09$isei[cl09$ISCO88==4222]=52 
cl09$isei[cl09$ISCO88==4223]=52 
cl09$isei[cl09$ISCO88==5000]=40 
cl09$isei[cl09$ISCO88==5100]=38 
cl09$isei[cl09$ISCO88==5110]=34 
cl09$isei[cl09$ISCO88==5111]=34 
cl09$isei[cl09$ISCO88==5112]=34 
cl09$isei[cl09$ISCO88==5113]=34 
cl09$isei[cl09$ISCO88==5120]=32 
cl09$isei[cl09$ISCO88==5121]=30 
cl09$isei[cl09$ISCO88==5122]=30 
cl09$isei[cl09$ISCO88==5123]=34 
cl09$isei[cl09$ISCO88==5130]=25 
cl09$isei[cl09$ISCO88==5131]=25 
cl09$isei[cl09$ISCO88==5132]=25 
cl09$isei[cl09$ISCO88==5133]=25 
cl09$isei[cl09$ISCO88==5139]=25 
cl09$isei[cl09$ISCO88==5140]=30 
cl09$isei[cl09$ISCO88==5141]=29 
cl09$isei[cl09$ISCO88==5142]=19 
cl09$isei[cl09$ISCO88==5143]=54 
cl09$isei[cl09$ISCO88==5149]=19 
cl09$isei[cl09$ISCO88==5150]=43 
cl09$isei[cl09$ISCO88==5151]=43 
cl09$isei[cl09$ISCO88==5152]=43 
cl09$isei[cl09$ISCO88==5160]=47 
cl09$isei[cl09$ISCO88==5161]=42 
cl09$isei[cl09$ISCO88==5162]=50 
cl09$isei[cl09$ISCO88==5163]=40 
cl09$isei[cl09$ISCO88==5164]=40 
cl09$isei[cl09$ISCO88==5169]=40 
cl09$isei[cl09$ISCO88==5200]=43 
cl09$isei[cl09$ISCO88==5210]=43 
cl09$isei[cl09$ISCO88==5220]=43 
cl09$isei[cl09$ISCO88==5230]=37 
cl09$isei[cl09$ISCO88==6000]=23 
cl09$isei[cl09$ISCO88==6100]=23 
cl09$isei[cl09$ISCO88==6110]=23 
cl09$isei[cl09$ISCO88==6111]=23 
cl09$isei[cl09$ISCO88==6112]=23 
cl09$isei[cl09$ISCO88==6113]=23 
cl09$isei[cl09$ISCO88==6114]=23 
cl09$isei[cl09$ISCO88==6120]=23 
cl09$isei[cl09$ISCO88==6121]=23 
cl09$isei[cl09$ISCO88==6122]=23 
cl09$isei[cl09$ISCO88==6123]=23 
cl09$isei[cl09$ISCO88==6124]=23 
cl09$isei[cl09$ISCO88==6129]=23 
cl09$isei[cl09$ISCO88==6130]=23 
cl09$isei[cl09$ISCO88==6131]=23 
cl09$isei[cl09$ISCO88==6132]=27 
cl09$isei[cl09$ISCO88==6133]=28 
cl09$isei[cl09$ISCO88==6134]=28 
cl09$isei[cl09$ISCO88==6140]=22 
cl09$isei[cl09$ISCO88==6141]=22 
cl09$isei[cl09$ISCO88==6142]=22 
cl09$isei[cl09$ISCO88==6150]=28 
cl09$isei[cl09$ISCO88==6151]=28 
cl09$isei[cl09$ISCO88==6152]=28 
cl09$isei[cl09$ISCO88==6153]=28 
cl09$isei[cl09$ISCO88==6154]=28 
cl09$isei[cl09$ISCO88==6200]=16 
cl09$isei[cl09$ISCO88==6210]=16 
cl09$isei[cl09$ISCO88==7000]=34 
cl09$isei[cl09$ISCO88==7100]=31 
cl09$isei[cl09$ISCO88==7110]=30 
cl09$isei[cl09$ISCO88==7111]=30 
cl09$isei[cl09$ISCO88==7112]=30 
cl09$isei[cl09$ISCO88==7113]=27 
cl09$isei[cl09$ISCO88==7120]=30 
cl09$isei[cl09$ISCO88==7121]=29 
cl09$isei[cl09$ISCO88==7122]=29 
cl09$isei[cl09$ISCO88==7123]=26 
cl09$isei[cl09$ISCO88==7124]=29 
cl09$isei[cl09$ISCO88==7129]=30 
cl09$isei[cl09$ISCO88==7130]=34 
cl09$isei[cl09$ISCO88==7131]=19 
cl09$isei[cl09$ISCO88==7132]=30 
cl09$isei[cl09$ISCO88==7133]=31 
cl09$isei[cl09$ISCO88==7134]=34 
cl09$isei[cl09$ISCO88==7135]=26 
cl09$isei[cl09$ISCO88==7136]=33 
cl09$isei[cl09$ISCO88==7137]=37 
cl09$isei[cl09$ISCO88==7140]=29 
cl09$isei[cl09$ISCO88==7141]=29 
cl09$isei[cl09$ISCO88==7142]=32 
cl09$isei[cl09$ISCO88==7143]=29 
cl09$isei[cl09$ISCO88==7200]=34 
cl09$isei[cl09$ISCO88==7210]=31 
cl09$isei[cl09$ISCO88==7211]=29 
cl09$isei[cl09$ISCO88==7212]=30 
cl09$isei[cl09$ISCO88==7213]=33 
cl09$isei[cl09$ISCO88==7214]=30 
cl09$isei[cl09$ISCO88==7215]=30 
cl09$isei[cl09$ISCO88==7216]=30 
cl09$isei[cl09$ISCO88==7220]=35 
cl09$isei[cl09$ISCO88==7221]=33 
cl09$isei[cl09$ISCO88==7222]=40 
cl09$isei[cl09$ISCO88==7223]=34 
cl09$isei[cl09$ISCO88==7224]=24 
cl09$isei[cl09$ISCO88==7230]=34 
cl09$isei[cl09$ISCO88==7231]=34 
cl09$isei[cl09$ISCO88==7232]=42 
cl09$isei[cl09$ISCO88==7233]=33 
cl09$isei[cl09$ISCO88==7234]=23 
cl09$isei[cl09$ISCO88==7240]=40 
cl09$isei[cl09$ISCO88==7241]=40 
cl09$isei[cl09$ISCO88==7242]=39 
cl09$isei[cl09$ISCO88==7243]=41 
cl09$isei[cl09$ISCO88==7244]=40 
cl09$isei[cl09$ISCO88==7245]=38 
cl09$isei[cl09$ISCO88==7300]=34 
cl09$isei[cl09$ISCO88==7310]=38 
cl09$isei[cl09$ISCO88==7311]=38 
cl09$isei[cl09$ISCO88==7312]=38 
cl09$isei[cl09$ISCO88==7313]=38 
cl09$isei[cl09$ISCO88==7320]=28 
cl09$isei[cl09$ISCO88==7321]=27 
cl09$isei[cl09$ISCO88==7322]=29 
cl09$isei[cl09$ISCO88==7323]=29 
cl09$isei[cl09$ISCO88==7324]=29 
cl09$isei[cl09$ISCO88==7330]=29 
cl09$isei[cl09$ISCO88==7331]=29 
cl09$isei[cl09$ISCO88==7332]=29 
cl09$isei[cl09$ISCO88==7340]=40 
cl09$isei[cl09$ISCO88==7341]=40 
cl09$isei[cl09$ISCO88==7342]=40 
cl09$isei[cl09$ISCO88==7343]=42 
cl09$isei[cl09$ISCO88==7344]=40 
cl09$isei[cl09$ISCO88==7345]=37 
cl09$isei[cl09$ISCO88==7346]=38 
cl09$isei[cl09$ISCO88==7400]=33 
cl09$isei[cl09$ISCO88==7410]=30 
cl09$isei[cl09$ISCO88==7411]=30 
cl09$isei[cl09$ISCO88==7412]=31 
cl09$isei[cl09$ISCO88==7413]=30 
cl09$isei[cl09$ISCO88==7414]=30 
cl09$isei[cl09$ISCO88==7415]=30 
cl09$isei[cl09$ISCO88==7416]=30 
cl09$isei[cl09$ISCO88==7420]=33 
cl09$isei[cl09$ISCO88==7421]=33 
cl09$isei[cl09$ISCO88==7422]=33 
cl09$isei[cl09$ISCO88==7423]=33 
cl09$isei[cl09$ISCO88==7424]=33 
cl09$isei[cl09$ISCO88==7430]=36 
cl09$isei[cl09$ISCO88==7431]=29 
cl09$isei[cl09$ISCO88==7432]=29 
cl09$isei[cl09$ISCO88==7433]=45 
cl09$isei[cl09$ISCO88==7434]=36 
cl09$isei[cl09$ISCO88==7435]=36 
cl09$isei[cl09$ISCO88==7436]=33 
cl09$isei[cl09$ISCO88==7437]=28 
cl09$isei[cl09$ISCO88==7440]=31 
cl09$isei[cl09$ISCO88==7441]=31 
cl09$isei[cl09$ISCO88==7442]=31 
cl09$isei[cl09$ISCO88==7500]=42 
cl09$isei[cl09$ISCO88==7510]=42 
cl09$isei[cl09$ISCO88==7520]=39 
cl09$isei[cl09$ISCO88==7530]=26 
cl09$isei[cl09$ISCO88==8000]=31 
cl09$isei[cl09$ISCO88==8100]=30 
cl09$isei[cl09$ISCO88==8110]=35 
cl09$isei[cl09$ISCO88==8111]=35 
cl09$isei[cl09$ISCO88==8112]=35 
cl09$isei[cl09$ISCO88==8113]=35 
cl09$isei[cl09$ISCO88==8120]=30 
cl09$isei[cl09$ISCO88==8121]=31 
cl09$isei[cl09$ISCO88==8122]=30 
cl09$isei[cl09$ISCO88==8123]=28 
cl09$isei[cl09$ISCO88==8124]=30 
cl09$isei[cl09$ISCO88==8130]=22 
cl09$isei[cl09$ISCO88==8131]=22 
cl09$isei[cl09$ISCO88==8139]=22 
cl09$isei[cl09$ISCO88==8140]=27 
cl09$isei[cl09$ISCO88==8141]=27 
cl09$isei[cl09$ISCO88==8142]=27 
cl09$isei[cl09$ISCO88==8143]=27 
cl09$isei[cl09$ISCO88==8150]=35 
cl09$isei[cl09$ISCO88==8151]=35 
cl09$isei[cl09$ISCO88==8152]=35 
cl09$isei[cl09$ISCO88==8153]=35 
cl09$isei[cl09$ISCO88==8154]=35 
cl09$isei[cl09$ISCO88==8155]=35 
cl09$isei[cl09$ISCO88==8159]=35 
cl09$isei[cl09$ISCO88==8160]=32 
cl09$isei[cl09$ISCO88==8161]=33 
cl09$isei[cl09$ISCO88==8162]=27 
cl09$isei[cl09$ISCO88==8163]=33 
cl09$isei[cl09$ISCO88==8170]=26 
cl09$isei[cl09$ISCO88==8171]=26 
cl09$isei[cl09$ISCO88==8172]=26 
cl09$isei[cl09$ISCO88==8200]=32 
cl09$isei[cl09$ISCO88==8210]=36 
cl09$isei[cl09$ISCO88==8211]=36 
cl09$isei[cl09$ISCO88==8212]=30 
cl09$isei[cl09$ISCO88==8220]=30 
cl09$isei[cl09$ISCO88==8221]=30 
cl09$isei[cl09$ISCO88==8222]=30 
cl09$isei[cl09$ISCO88==8223]=30 
cl09$isei[cl09$ISCO88==8224]=30 
cl09$isei[cl09$ISCO88==8229]=30 
cl09$isei[cl09$ISCO88==8230]=30 
cl09$isei[cl09$ISCO88==8231]=30 
cl09$isei[cl09$ISCO88==8232]=30 
cl09$isei[cl09$ISCO88==8240]=29 
cl09$isei[cl09$ISCO88==8250]=38 
cl09$isei[cl09$ISCO88==8251]=38 
cl09$isei[cl09$ISCO88==8252]=38 
cl09$isei[cl09$ISCO88==8253]=38 
cl09$isei[cl09$ISCO88==8260]=30 
cl09$isei[cl09$ISCO88==8261]=29 
cl09$isei[cl09$ISCO88==8262]=29 
cl09$isei[cl09$ISCO88==8263]=32 
cl09$isei[cl09$ISCO88==8264]=24 
cl09$isei[cl09$ISCO88==8265]=32 
cl09$isei[cl09$ISCO88==8266]=32 
cl09$isei[cl09$ISCO88==8269]=32 
cl09$isei[cl09$ISCO88==8270]=29 
cl09$isei[cl09$ISCO88==8271]=29 
cl09$isei[cl09$ISCO88==8272]=29 
cl09$isei[cl09$ISCO88==8273]=29 
cl09$isei[cl09$ISCO88==8274]=29 
cl09$isei[cl09$ISCO88==8275]=29 
cl09$isei[cl09$ISCO88==8276]=29 
cl09$isei[cl09$ISCO88==8277]=29 
cl09$isei[cl09$ISCO88==8278]=29 
cl09$isei[cl09$ISCO88==8279]=29 
cl09$isei[cl09$ISCO88==8280]=31 
cl09$isei[cl09$ISCO88==8281]=30 
cl09$isei[cl09$ISCO88==8282]=34 
cl09$isei[cl09$ISCO88==8283]=34 
cl09$isei[cl09$ISCO88==8284]=30 
cl09$isei[cl09$ISCO88==8285]=30 
cl09$isei[cl09$ISCO88==8286]=30 
cl09$isei[cl09$ISCO88==8290]=26 
cl09$isei[cl09$ISCO88==8300]=32 
cl09$isei[cl09$ISCO88==8310]=36 
cl09$isei[cl09$ISCO88==8311]=41 
cl09$isei[cl09$ISCO88==8312]=32 
cl09$isei[cl09$ISCO88==8320]=34 
cl09$isei[cl09$ISCO88==8321]=30 
cl09$isei[cl09$ISCO88==8322]=30 
cl09$isei[cl09$ISCO88==8323]=30 
cl09$isei[cl09$ISCO88==8324]=34 
cl09$isei[cl09$ISCO88==8330]=26 
cl09$isei[cl09$ISCO88==8331]=26 
cl09$isei[cl09$ISCO88==8332]=26 
cl09$isei[cl09$ISCO88==8333]=28 
cl09$isei[cl09$ISCO88==8334]=28 
cl09$isei[cl09$ISCO88==8340]=32 
cl09$isei[cl09$ISCO88==8400]=24 
cl09$isei[cl09$ISCO88==9000]=20 
cl09$isei[cl09$ISCO88==9100]=25 
cl09$isei[cl09$ISCO88==9110]=29 
cl09$isei[cl09$ISCO88==9111]=29 
cl09$isei[cl09$ISCO88==9112]=28 
cl09$isei[cl09$ISCO88==9113]=29 
cl09$isei[cl09$ISCO88==9120]=28 
cl09$isei[cl09$ISCO88==9130]=16 
cl09$isei[cl09$ISCO88==9131]=16 
cl09$isei[cl09$ISCO88==9132]=16 
cl09$isei[cl09$ISCO88==9133]=16 
cl09$isei[cl09$ISCO88==9140]=23 
cl09$isei[cl09$ISCO88==9141]=23 
cl09$isei[cl09$ISCO88==9142]=23 
cl09$isei[cl09$ISCO88==9150]=27 
cl09$isei[cl09$ISCO88==9151]=25 
cl09$isei[cl09$ISCO88==9152]=27 
cl09$isei[cl09$ISCO88==9153]=27 
cl09$isei[cl09$ISCO88==9160]=23 
cl09$isei[cl09$ISCO88==9161]=23 
cl09$isei[cl09$ISCO88==9162]=23 
cl09$isei[cl09$ISCO88==9200]=16 
cl09$isei[cl09$ISCO88==9210]=16 
cl09$isei[cl09$ISCO88==9211]=16 
cl09$isei[cl09$ISCO88==9212]=16 
cl09$isei[cl09$ISCO88==9213]=16 
cl09$isei[cl09$ISCO88==9300]=23 
cl09$isei[cl09$ISCO88==9310]=21 
cl09$isei[cl09$ISCO88==9311]=21 
cl09$isei[cl09$ISCO88==9312]=21 
cl09$isei[cl09$ISCO88==9313]=21 
cl09$isei[cl09$ISCO88==9320]=20 
cl09$isei[cl09$ISCO88==9321]=20 
cl09$isei[cl09$ISCO88==9322]=24 
cl09$isei[cl09$ISCO88==9330]=29 
cl09$isei[cl09$ISCO88==9331]=22 
cl09$isei[cl09$ISCO88==9332]=22 
cl09$isei[cl09$ISCO88==9333]=30
cl09$isei[cl09$ISCO88==110 ]=70}
cl09$isei <- as.numeric(cl09$isei)
summary(cl09$isei)
table(cl09$isei)

View(cl09[,c("ISCO88","isei")]) 

# estatus laboral ---------------------------------------------------------

# 0          VE: NAP, never in paid work (NA)
# 1         Employed-full time, main job (empleado) (1)
# 2         Employed-part time, main job (empleado) (1)
# 3        Employed, less than part-time (empleado) (1)
# 4                Helping family member (trabajo domestico/cuidados)
# 5                           Unemployed (desempleado) (2)
# 6 Student, school, vocational training (estudia)
# 7                              Retired (retirado)
# 8         Housewife, -man, home duties (trabajo domestico/cuidados)
# 9                 Permanently disabled  (NA)
# 10            Other,not in labour force (NA)
# 97                              Refused (NA)
# 98                            Dont know (NA)
# 99                                   NA 

table(cl09$WRKST)
prop.table(table(cl09$WRKST))*100

cl09$WRKST[cl09$WRKST %in% c(0,9,10,10,97,98,99)] <- NA

cl09$labsta <- cl09$WRKST
cl09$labsta <- car::recode(cl09$labsta,recodes = "c(1,2,3)=1;5=2;7=3;6=4;c(4,8)=5",as.factor = TRUE)

cl09$labsta <- factor(cl09$labsta,levels = c(1:5), labels = c("empleado","desempleado","retirado","estudia","trabajo domestico_cuidados"))

table(cl09$labsta)
margin.table(table(cl09$labsta)) 

# Sexo --------------------------------------------------------------------


# year 2009
table(cl09$SEX)
cl09$SEX[cl09$SEX==9] <- NA
# 1 Male
# 2 Female
# 9 No answer, refused
cl09$SEX[cl09$SEX==2] <- 0 # Mujer a categoría de referencia

cl09 <- rename(cl09,hombre=SEX)
cl09$hombre <- as_factor(cl09$hombre)
table(cl09$hombre)


# Edad --------------------------------------------------------------------

cl09$edad <- cl09$AGE 

cl09$edad[cl09$edad==99] <- NA
table(cl09$edad)

## Variables subjetivas----------------------------------------------------

# {r salario percibido y justo 2009}---------------------------------------------------------------

table(cl09$V23) # cuanto gana GERENTE
table(cl09$V25) # cuanto gana OBRERO
table(cl09$V28) # cuando deberia ganar GERENTE
table(cl09$V30) # cuanto deberia ganar OBRERO
cl09$V23[cl09$V23 %in%c(-99,-98,-97,999999999996)] <- NA
cl09$V25[cl09$V25 %in%c(-99,-98,-97)]              <- NA
cl09$V28[cl09$V28 %in%c(-99,-98,-97)]              <- NA
cl09$V30[cl09$V30 %in%c(-99,-98,-97)]              <- NA

#--- Nombres sustantivos para analisis
cl09 <- rename(cl09,pais=V5,salperger=V23,salperobr=V25,saljusger=V28,saljusobr=V30)

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

cl09$lngap_perc <-  as.numeric(log(cl09$gap_perc))  # diferencia log
cl09$lngap_just <-  as.numeric(log(cl09$gap_just))  # diferencia log


tabper01 <- questionr::freq(cl09$gap_perc,cum = TRUE)


summary(cl09$gap_perc)
summary(cl09$gap_just)

quantile(x = cl09$gap_perc,probs = c(0.025,0.97),na.rm = TRUE)

sp01 <- cl09 %>% filter(gap_perc<=269.580420 & gap_perc>=1.666667)
sp01$country<- as_character(sp01$country)
summary(sp01$gap_perc)
summary(sp01$gap_just)

psych::describe.by(sp01$gap_perc,sp01$country,mat = TRUE,fast=FALSE)
psych::describe.by(sp01$gap_just,sp01$country,mat = TRUE,fast=FALSE)

cl09 <- cl09 %>% filter(gap_perc<=269.580420 & gap_perc>=1.666667)

# Seleccionar variables  --------------------------------------------------

cl09$country
cl09$pais
cl09$country<- as_character(cl09$country)



db09 <- cl09 %>% dplyr::select(country,pais,hombre,edad,ess,educ,educat,edter,isei,Q05,D10,labsta,INCOME,zinc,loginc,gap_perc,gap_just,lngap_perc,lngap_just)

cor(x = cl09$educ,cl09$ess, use = "complete.obs")
cor(x = cl09$D10,cl09$ess,  use = "complete.obs")
cor(x = cl09$zinc,cl09$ess, use = "complete.obs")
cor(x = cl09$isei,cl09$ess, use = "complete.obs")
cor(x = cl09$lngap_perc,cl09$ess, use = "complete.obs")

names(db09)
str(db09)
skimr::skim(db09)

save(db09,file = here("data","data_issp09.RData"))


# # Base sin outliers en variables brechas ----------------------------------
# 
# db01cln <- cl99 %>% dplyr::filter(salperobr<22000000 & salperobr>=50000 & salperger<100000000 & salperger>=400000)
# db02cln <- cl09 %>% dplyr::filter(salperobr<22000000 & salperobr>=50000 & salperger<100000000 & salperger>=400000)
# db03cln <- cl19 %>% dplyr::filter(salperobr<22000000 & salperobr>=50000 & salperger<100000000 & salperger>=400000)
# 
# db01cln <- db01cln %>% dplyr::select(year,sexo,ess,educ,educat,edter,D10,D10F,loginc,loginc,zinc,zincf,gap_perc,gap_just,lngap_perc,lngap_just)
# db02cln <- db02cln %>% dplyr::select(year,sexo,ess,educ,educat,edter,D10,D10F,loginc,loginc,zinc,zincf,gap_perc,gap_just,lngap_perc,lngap_just)
# db03cln <- db03cln %>% dplyr::select(year,sexo,ess,educ,educat,edter,D10,D10F,loginc,loginc,zinc,zincf,gap_perc,gap_just,lngap_perc,lngap_just)
# 
# 
# data_chile_clean <- bind_rows(db01cln,db02cln,db03cln)
# save(data_chile_clean,file = here("data","data_chile_clean.RData"))





