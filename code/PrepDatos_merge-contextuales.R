
# Merge datos contextuales ------------------------------------------------

library(sjlabelled)
library(dplyr)
library(countrycode)


rm(list=ls())

solt<- read.csv(file = here("data","swiid8_1","swiid8_1_summary.csv"))
names(solt)
solt09       <- solt %>% filter(year==2009) %>% dplyr::select(year,country,"gini_disp","gini_mkt") 
solt09$ccode <- countrycode(solt09$country,'country.name','iso3n') # Crear un ccode para solt




load(file = here("data","data_issp09.RData"))


qog012017<- read.csv(file = here("data","qog19.csv"))


dbqog <- qog012017 %>% filter(year==2009) %>% dplyr::select(ccode,cname,iiag_hd,wdi_gdpcappppcur,wdi_gini,lis_gini,wiid_gini)

# iiag_hd Human Development
# wdi_gdpcapcur GDP per capita (current US dollar)
# wdi_gdpcappppcur GDP per capita, PPP (current international dollar)

# wdi_gdpcapcon2010 GDP per capita (constant 2010 US dollar)
# wdi_gdppppcon2011 GDP, PPP (constant 2011 international dollar)
# wdi_gini GINI index (World Bank estimate)

db09 <- rename(db09,ccode=pais) #cambiar pais a ccode

table01 <- db09 %>% group_by(country,ccode=as.numeric(ccode)) %>% summarise(ene=n())

db02 <- left_join(x = table01, y = solt09,"ccode")
db03 <- left_join(x = db09,    y = db02,"ccode")
db03$ccode <- as.numeric(db03$ccode)
db04 <- left_join(x = db03,    y = dbqog,"ccode")

db04$cname <- db04$country.y
names(db04)
table(db04$country.y)

db04$country.y <- NULL
db04$country.x <- NULL


db04 %>% group_by(country) %>% summarise(mean(wdi_gini),mean(lis_gini),mean(wiid_gini),mean(gini_disp)) %>% View()

db04 <- db04 %>% mutate(gini_comp=if_else(condition = (is.na(wdi_gini)),true =gini_disp,false =  wdi_gini ))

db04 %>% group_by(country) %>% summarise(mean(wdi_gini),mean(lis_gini),mean(wiid_gini),mean(gini_disp),mean(gini_comp)) %>% View()

save(db04,file =  here("data","cl09gini.RData"))
     