

# Primeros análisis -------------------------------------------------------

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

rm(list=ls())
load(file = here("data","data_chile.RData"))
load(file = here("data","data_chile_clean.RData"))

skimr::skim(data_chile)
skimr::skim(data_chile_clean)

data01 <- data_chile %>% dplyr::select(year,sexo,ess,educ,lngap_perc,D10) %>% na.omit()
data02 <- data_chile_clean %>% dplyr::select(year,sexo,ess,educ,lngap_perc,D10) %>% na.omit()


plot_scatter(data = data_chile,x = lngap_perc,y =ess , fit.line = "loess")
plot_scatter(data = data_chile,x = educ ,y = ess,fit.line = "loess")

plot_scatter(data = data_chile_clean,x = lngap_perc,y =ess , fit.line = "loess")
plot_scatter(data = data_chile_clean,x = educ ,y = ess,fit.line = "loess")

# Mediación ? -------------------------------------------------------------

mc <- lm(ess~educ+lngap_perc,data = data02) # y ~ x + m
mb <- lm(lngap_perc~educ+ess,data = data02) # m ~ x + y
scatter.smooth(x = data02$educ,y = fitted(m06b))
texreg::screenreg(l = list(mc,mb))

# Estimation via quasi-Bayesian approximation
contcont <- mediate(mb, mc, sims=100, treat="educ", mediator="lngap_perc")
summary(contcont)
plot(contcont)

# - Análisis de mediación no es viable.

# Modelos con base con outliers -------------------------------------------
m01 <- lm(ess~lngap_perc,data = data01)
m02 <- lm(ess~lngap_perc+educ,data = data01)
m03 <- lm(ess~lngap_perc+educ+D10,data = data01)
m04 <- lm(ess~lngap_perc+educ+D10+sexo,data = data01)
m05 <- lm(ess~lngap_perc+educ+D10+sexo+lngap_perc:educ,data = data01)
m06 <- lm(ess~lngap_perc+educ+D10+sexo+lngap_perc:educ+year,data = data01)

texreg::screenreg(l = list(m01,m02,m03,m04,m05,m06))

data02$year  <- as.factor(data02$year)
data02$y1999 <- ifelse((data02$year==1999),1,0)
data02$y2009 <- ifelse((data02$year==2009),1,0)
data02$y2019 <- ifelse((data02$year==2019),1,0)

# Modelos con base sin outliers -------------------------------------------

m01b <- lm(ess~lngap_perc,data = data02)
m02b <- lm(ess~lngap_perc+educ,data = data02)
m03b <- lm(ess~lngap_perc+educ+D10,data = data02)
m04b <- lm(ess~lngap_perc+educ+D10+sexo,data = data02)
m010b <-lm(ess~lngap_perc+educ+D10+sexo+educ+year,data = data02)
m05b <- lm(ess~lngap_perc+educ+D10+sexo+educ*lngap_perc,data = data02)     # Interacción educ:lngap_perc
m06b <- lm(ess~lngap_perc+educ+D10+sexo+educ*lngap_perc+year,data = data02)# Interacción educ:lngap_perc + years
m09b <- lm(ess~lngap_perc+educ+sexo+educ*lngap_perc+year,data = data02)    # Interacción educ*lngap_perc - D10

screenreg(l = list(m06b,m09b)) 

# Interacciones de educ con year de ola
m07b <- lm(ess~lngap_perc+educ+D10+sexo+educ*year,data = data02)          # Interacción year:educ
summary(m07b)

# -  La interacción de educ*year es significativa para el 2019 (ref: 1999)

pred01 <- plot_model(model = m06b,type = "int") # Predicted values by min(lngap_perc) & max(lngap_perc) 
pred01 <- plot_model(model = m06b,type = "pred",terms = c("educ","lngap_perc [-1.163,3.366,6.908 ]")) # Predicted values by min(lngap_perc) & max(lngap_perc) 
pred01

marg01 <- interplot(m = m06b,var1 = "lngap_perc",var2 = "educ",point = TRUE, adjCI = FALSE) +
  geom_hline(yintercept=0, color="red") + 
  ggtitle("Efecto marginal de Educacion (condicionado por ln perc)")

library(gridExtra)
grid.arrange(marg01, pred01, ncol = 2)

# Análisis de casos influyentes -------------------------------------------

vif(mod = m06b)
dwtest(m06b)

data02[-c(1349,2159,1167,1624,1803),] # base sin outliers
data02b <- data02[-c(1349,2159,1167,1624,1803,1801,1623),]

m08b <- lm(ess~lngap_perc+educ+D10+sexo+educ*lngap_perc+year,data = data02b) # Sin leverage
summary(m08b)


# - educ in years  + D10 | la mejor spec
# - educ in years  + zinc| no hay efecto principal de percepcion y tampoco de la inetraccion
# - educ category (continua)  + D10 | educ no sig, percep no sig, la interaccion es sig (0.04 coef)
# - educ category (factor)  + D10 | educ no sig, percep no sig, la interaccion no sig


# Comparar modelos con y sin "casos influyentes" ----------------------------

screenreg(l = list(m06b,m08b))

# Analisis con edter  = educacion terciaria------------------------------------------------------

data03 <- data_chile_clean %>% dplyr::select(year,sexo,ess,edter,lngap_perc,D10) %>% na.omit()

m01edt <- lm(ess~lngap_perc,data = data03)
m02edt <- lm(ess~lngap_perc+edter,data = data03)
m03edt <- lm(ess~lngap_perc+edter+D10,data = data03)
m04edt <- lm(ess~lngap_perc+edter+D10+sexo,data = data03)
m05edt <- lm(ess~lngap_perc+edter+D10+sexo+lngap_perc:edter,data = data03)
m06edt <- lm(ess~lngap_perc+edter+D10+sexo+lngap_perc:edter+factor(year),data = data03)

texreg::screenreg(l = list(m01edt,m02edt,m03edt,m04edt,m05edt,m06edt),digits = 2)

# - Usar variable de Educación binaria Terciaria=1, no funciona. La interacción no es sig.

# Robust con modelos sin outliers en percepción de desigualdad ------------------------------------------------------------------

m01ar <- lm_robust(ess~lngap_perc,data = data02,fixed_effects = ~ year)
m02ar <- lm_robust(ess~lngap_perc+educ,data = data02,fixed_effects = ~ year)
m03ar <- lm_robust(ess~lngap_perc+educ+D10,data = data02,fixed_effects = ~ year)
m04ar <- lm_robust(ess~lngap_perc+educ+D10+sexo,data = data02,fixed_effects = ~ year)
m05ar <- lm_robust(ess~lngap_perc+educ+D10+sexo+educ*lngap_perc,data = data02,fixed_effects  = ~ year,se_type = "stata")
m06ar <- lm_robust(ess~lngap_perc+educ+D10+sexo+educ*lngap_perc,data = data02,fixed_effects  = ~ year,se_type = "HC2")

summary(m05ar)
summary(m06ar)

texreg::htmlreg(l = list(m01ar,m02ar,m03ar,m04ar,m05ar), digits = 3,file = "reg01.xls")







