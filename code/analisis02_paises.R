

# Análisis 41 paises -------------------------------------------------------

library(here)
library(dplyr)
library(texreg)
library(sjPlot)
library(car)
library(lmtest)
library(estimatr)
library(mediation)
library(margins)
library(ggplot2)
library(interplot)
library(lme4)

rm(list=ls())
load(file = here("data","data_issp09.RData"))

pacman::p_load(influence.ME,
               lattice, # dotplot
               dplyr,
               texreg)


dat01 <- db09 %>% dplyr::select(country,hombre,edad,ess,educ,educat,D10,zinc,isei,labsta,lngap_perc)

# Modelos con Efectos fijos por pais --------------------------------------

m01a <- lm(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc,data = dat01)
m01  <- lm_robust(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc,data = dat01)
m02  <- lm_robust(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc,data = dat01,fixed_effects = ~country)
m03  <- lm_robust(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc+educat*lngap_perc,data = dat01,fixed_effects = ~country)
m04  <- lm(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc+educ*lngap_perc+factor(country),data = dat01)

screenreg(l = list(m01a,m01,m02))
screenreg(l = list(m02,m03))


# Modelos con lmer --------------------------------------------------------

ml00 <- lmer(ess~ 1 + (1|country),data = dat01)
screenreg(ml00)
plot_model(ml00, type = "re", sort.est = "(Intercept)")


varcomp=as.data.frame(VarCorr(ml00))# dataframe random effects
varcomp
tau00=varcomp[1,4]
sigma2=varcomp[2,4]
icc=tau00/(tau00+sigma2)
icc

ml01 <- lmer(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc + (1|country),data = dat01)
ml02 <- lmer(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc + lngap_perc:educat + (1|country),data = dat01)
ml03 <- lmer(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc + lngap_perc:D10 + (1|country),data = dat01)
ml04 <- lmer(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc + lngap_perc:isei + (1|country),data = dat01)
screenreg(l = list(ml01,ml02,ml03,ml04))
plot_model(ml04, type = "re", sort.est = "(Intercept)")


# Modelos  ------------------------------------------------

ml05 <- lmer(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc + educat:lngap_perc + (1 |country),data = dat01,REML = FALSE)
ml06 <- lmer(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc + D10:lngap_perc  + (1 |country),data = dat01,REML = FALSE)
ml07 <- lmer(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc + isei:lngap_perc + (1 |country),data = dat01,REML = FALSE)

screenreg(l = list(ml01,ml05,ml06,ml07))

p01 <- plot_model(ml03,type = "pred", terms = c("educ", "lngap_perc [-10.404,13.346] "))
p01 + theme_classic()

plot_model(ml03,type = "pred", terms = c("D10", "lngap_perc [-10.404,2.5186,13.346] "))

# Revisar casos influyentes -----------------------------------------------
  
estex.m23 <- influence(ml06, "country") 

plot(estex.m23, which="dfbetas")

plot(estex.m23, 
     which="cook",
     cutoff=.17, 
     sort=TRUE,
     xlab="Cooks Distance",
     ylab="Country")

sigtest(estex.m23, test=-1.96)$D10
sigtest(estex.m23, test=-1.96)$educ

# Casos influyentes: Cyprus, Venezuela, Sud Africa y China
table(dat01$country)

data02 <- dat01 %>% filter(country !="AU-Australia",country !="VE-Venezuela",country !="ZA-South Africa",country !="CN-China",country !="FR-France")
table(data02$country)

# Modelo sin casos influyentes de N2 --------------------------------------

ml05b <- lmer(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc + educat:lngap_perc + (1 |country),data = data02,REML = TRUE)
ml06b <- lmer(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc + D10:lngap_perc  + (1 |country),data = data02,REML = TRUE)
ml07b <- lmer(ess~hombre+edad+educat+D10+isei+labsta+lngap_perc + isei:lngap_perc + (1 |country),data = data02,REML = TRUE)

screenreg(l = list(ml01,ml05,ml05b))
screenreg(l = list(ml05b,ml06b,ml07b))

# Base con gini  -----------------------------------------------------------

rm(list=ls())

load(file =  here("data","cl09gini.RData"))
names(db04)

# Hay dos versiones del gini:
  # - Gini household
  # - Gini Market

 # gini_disp: Estimate of Gini index of inequality in equivalized (square root scale) household
# disposable (post-tax, post-transfer) income, using Luxembourg Income Study data as the
# standard.

# gini_mkt: Estimate of Gini index of inequality in equivalized (square root scale) household
# market (pre-tax, pre-transfer) income, using Luxembourg Income Study data as the standard.

summary(db04$gini_disp)
summary(db04$gini_mkt)
summary(db04$gini_comp)
names(db04)

dat02 <- db04 %>% dplyr::select(country,hombre,edad,ess,educ,educat,Q05,D10,zinc,isei,labsta,lngap_perc,gini_disp,gini_mkt,wdi_gini,wdi_gdpcappppcur,gini_comp)
dat02$wdi_gdpcappppcur <- dat02$wdi_gdpcappppcur/1000
dat02$Q05 <- factor(dat02$Q05,levels = c(1,2,3,4,5),labels = c("Q01","Q02","Q03","Q04","Q05"))



# Analisis descriptivos -----------------------------------------------------------------------


des01<- db04 %>% group_by(cname) %>% summarise(gini=mean(gini_comp),
                                               ess=mean(ess,na.rm=T),
                                               gap_perc=mean(lngap_perc),
                                               gdp=mean(wdi_gdpcappppcur))
set_theme(base = theme_classic(),
          theme.font = "serif", 
          axis.textsize.x = 1.0,
          axis.textsize.y = 1.0,
          geom.label.size = 4, 
          legend.just = 0.5,legend.inside = T, 
          legend.size = 1.0,
          legend.backgroundcol = "white")


db04 %>%
  group_by(ess, cname) %>%
  summarise(n = n()) -> ess.n

db04 %>% group_by(cname) %>% summarise(n.country=n())->n.country
ess.n<- ess.n %>% left_join(n.country) %>% mutate(perc = n / n.country)

ggplot(ess.n,)+
  geom_bar(aes(y = perc,x = ess),stat = "identity")+
  scale_x_continuous("Subjective Social Status",breaks = 1:10,labels = c(1,2:9,10))+
  scale_y_continuous(" ",labels = scales::percent_format(accuracy = 5L))+
  facet_wrap(~ cname) -> ess_macro;ess_macro

ggsave(ess_macro,filename = "output/images/ess-country.png",device = "png",width = 15,height = 10)

macro01<- plot_scatter(des01,x = gini,y = ess,dot.labels = des01$cname,fit.line = "lm",show.ci = T) +
  ylab("Subjective Social Status")+xlab("Gini Index")
macro02<- plot_scatter(des01,x = gap_perc,y = ess,dot.labels = des01$cname,fit.line = "lm",show.ci = T) +
  ylab("Subjective Social Status")+xlab("Perceived Salary Gap")
macro03<- plot_scatter(des01,x = gdp,y = ess,dot.labels = des01$cname,fit.line = "lm",show.ci = T) +
  ylab("Subjective Social Status")+xlab("Gross Domestic Product (US dollars)")


library(ggpubr)
macro_full<- ggarrange(ggarrange(macro02, macro01, ncol = 2, labels = c("A", "B")), # first row with box and dot plots
          ggarrange(macro03, ncol = 1, labels = c("C")),nrow = 2       # Second row with box and dot plots
 ) 

ggsave(macro_full,filename = "output/images/macro-country.png",device = "png",width = 15,height = 9)

# Analisis 01: base completa 41 paises -----------------------------------------

# Modelo Gini household ---------------------------------------------------

{
  ml05c <- lmer(ess~hombre+edad+educat+D10+labsta+lngap_perc+ wdi_gdpcappppcur+(1|country),data = dat02,REML = TRUE)
  ml06c <- lmer(ess~hombre+edad+educat+D10+labsta+gini_comp + wdi_gdpcappppcur+(1|country),data = dat02,REML = TRUE)
  ml07c <- lmer(ess~hombre+edad+educat+D10+labsta+lngap_perc+ gini_comp + wdi_gdpcappppcur+(1|country),data = dat02,REML = TRUE)
screenreg(l = list(ml05c,ml06c,ml07c))
}

estex.m24 <- influence(ml07c, "country") 

plot(estex.m24, which="cook",
     cutoff=.17, sort=TRUE,
     xlab="Cooks Distance",
     ylab="Country")

# Analisis 02: sin casos leverage, 38 paises -----------------------------------------

# Quitemos los casos que hacen leverage:
dat02b <- dat02 %>% filter(country !="ZA-South Africa",country !="FR-France")

# Modelo nulo con 37 países -------------------------------------------------------------

ml00 <- lmer(ess~1 +(1|country) ,data = dat02b,REML = TRUE)

varcomp=as.data.frame(VarCorr(ml00)) # dataframe random effects
varcomp
tau00=varcomp[1,4]
sigma2=varcomp[2,4]
icc=tau00/(tau00+sigma2)
icc # ICC = 0.1595456

# Modelo Gini household + random intercept---------------------------------------------------

{
  ml05c <- lmer(ess~hombre+edad+educ+D10+labsta+ lngap_perc + wdi_gdpcappppcur+(1|country),data = dat02b,REML = TRUE)
  ml06c <- lmer(ess~hombre+edad+educ+D10+labsta+ gini_comp + wdi_gdpcappppcur+(1|country),data = dat02b,REML = TRUE)
  ml07c <- lmer(ess~hombre+edad+educ+D10+labsta+lngap_perc + gini_comp + wdi_gdpcappppcur+(1|country),data = dat02b,REML = TRUE)
  screenreg(l = list(ml05c,ml06c,ml07c))
}

plot_model(ml07c,show.values = T,show.p = F) # Modelo Completo

texreg::plotreg(list(ml07c),omit.coef ="(Intercept)")

# Modelo gini household | sin leverage ------------------------------------------------------

# Random intercept + fixed slope + interacción de estatus objetivo y percepción de desigualdad
{
  ml04gm <- lmer(ess~hombre+edad+educ+D10+labsta+lngap_perc + gini_comp +wdi_gdpcappppcur + D10*lngap_perc+  (1|country), data = dat02,REML = TRUE)
  ml05gm <- lmer(ess~hombre+edad+educ+D10+labsta+lngap_perc + gini_comp +wdi_gdpcappppcur + D10*lngap_perc+  (1|country), data = dat02b,REML = TRUE)
  screenreg(l = list(ml04gm,ml05gm))
}
plot_model(ml04gm,type = "int") + theme_classic()

plot_model(ml05gm,type = "int",mdrt.values = "minmax") +
  labs(title = NULL,color="ln(Perceived)")+
  scale_x_continuous(breaks = 1:10)+
  xlab("Income Decile")+
  ylab("Subjective Social Status")+
  theme(legend.position = c(0.26, 0.95),
        legend.direction = "horizontal",
        legend.box.just = "top") ->int01;int01


db04 %>% dplyr::select(lngap_perc) %>% mutate(z=scale(lngap_perc)) %>% View()

ggsave(int01,filename = "output/images/int01.png",device = "png",width = 10,height = 5)


# Analisis 03: interacciones entre niveles ------------------------------------------------------------

#Interacciones entre niveles: Base completa 41 paises -------------------------------------------------

# Modelos con interacción entre niveles 41 paises | fixed slope ------------------
# Gini Household ----------------------------------------------------------#
{
  ml05d <- lmer(ess~hombre+edad+educat+D10+labsta+lngap_perc + D10*gini_comp + wdi_gdpcappppcur +(1|country),data = dat02,REML = TRUE)
screenreg(l = list(ml05d))
}
plot_model(ml05d,type = "int")


# Modelos con interacción entre niveles 37 paises | random slope ------------------ #

{
ml05rs <- lmer(ess~hombre+edad+educat+D10+labsta+lngap_perc + D10*gini_comp +wdi_gdpcappppcur+(1+D10|country),data = dat02,REML = TRUE)
screenreg(l = list(ml05rs))
}

plot_model(ml05rs,type = "int") + theme_blank()


{
  ml05rs <- lmer(ess~hombre+edad+educ+D10+labsta+lngap_perc + D10*gini_comp +wdi_gdpcappppcur+(1+D10|country),data = dat02b,REML = TRUE)
  screenreg(l = list(ml05rs))
}

plot_model(ml05rs,type = "int",mdrt.values = "minmax") +
  labs(title = NULL,color="Gini Index")+
  scale_x_continuous(breaks = 1:10)+
  xlab("Income Decile")+
  ylab("Subjective Social Status")+
  theme(legend.position = c(0.25, 0.95),
        legend.direction = "horizontal",
        legend.box.just = "top") ->int02;int02

moderation01<- ggarrange(ggarrange(int01, int02, ncol = 2, labels = c("A", "B")))
ggsave(moderation01,filename = "output/images/moderation01.png",device = "png",width = 10,height = 5)



save(ml07c,ml05gm,ml05rs,file = "output/tables/modelos.RData")

texreg::knitreg(list(ml07c,ml05gm,ml05rs),
                omit.coef ="(Intercept)|hombre1|edad|labstadesempleado|labstaretirado|labstaestudia|labstatrabajo domestico_cuidados",
                custom.coef.names = c("Year of education","Income Decile (D10)","Perceived Salary Gap (log)","Gini index","GDP","D10 x Perc Gap","D10 x Gini"))





