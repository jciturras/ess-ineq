
# Clases latentes de Nivel Socioeconómico  --------------------------------

library(mclust)
library(tidyverse)
library(here)

rm(list=ls())
load(file = here("data","issp2009CL.RData"))

cl09
ses  <- cl09 %>% dplyr::select(INCOME,educ) %>% mutate(loginc=log(INCOME)) %>% filter(pais==152) %>% na.omit() %>% select(-pais,-INCOME)

names(ses)

BIC  <- mclustBIC(ses)
plot(BIC)

summary(BIC)

mod0 <- Mclust(ses)
summary(mod0)

mod0$data
mod0$classification

db01 <- cbind.data.frame(mod0$data,mod0$classification)

psych::describe.by(x = db01$educ,group = db01$`mod0$classification`,mat = TRUE)
psych::describe.by(x = db01$isei,group = db01$`mod0$classification`,mat = TRUE)



mod1 <- Mclust(ses, modelNames = "VII", G = 3, x = BIC)
mod2 <- Mclust(ses, modelNames = "VII", G = 4, x = BIC)
mod3 <- Mclust(ses, modelNames = "VII", G = 5, x = BIC)
mod4 <- Mclust(ses, modelNames = "VII", G = 6, x = BIC)
mod5 <- Mclust(ses, modelNames = "VII", G = 7, x = BIC)
mod6 <- Mclust(ses, modelNames = "VII", G = 8, x = BIC)
mod7 <- Mclust(ses, modelNames = "VII", G = 9, x = BIC)


summary(mod1) # 3 class
summary(mod2) # 4 class
summary(mod3) # 5 class
summary(mod4) # 6 class
summary(mod5) # 7 class
summary(mod6) # 8 class
summary(mod7) # 9 class

mod1$BIC[1] # 3 class
mod2$BIC[1] # 4 class
mod3$BIC[1] # 5 class
mod4$BIC[1] # 6 class
mod5$BIC[1] # 7 class
mod6$BIC[1] # 8 class
mod7$BIC[1] # 9 class


lrt01<- mclustBootstrapLRT(data = ses,modelName = "VII")
lrt01
# save(lrt01,file = here("data","lca02.RData"))


plot(mod7)
