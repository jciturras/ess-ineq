# Plots descriptivos


library(here)
library(ggplot2)
library(ggrepel)
library(dplyr)
# remotes::install_git("https://git.rud.is/hrbrmstr/hrbrthemes.git")

library(hrbrthemes)
# hrbrthemes::import_roboto_condensed()
rm(list=ls())

load(file =  here("data","cl09gini.RData"))
names(db04)


data01 <- db04 %>% 
  dplyr::select(ess,gini_disp,gini_mkt,cname,gap_perc,wdi_gdpcappppcur,iiag_hd,wdi_gini) %>% 
  group_by(cname)%>% 
  summarise(ess=mean(ess,na.rm = TRUE),
            essm=median(ess,na.rm = TRUE),
            gap_per=median(gap_perc),
            gini_disp=mean(gini_disp),
            gini_mkt=mean(gini_mkt),
            gdp=mean(wdi_gdpcappppcur),
            gini=mean(wdi_gini))


hrbrthemes::import_roboto_condensed()

ggplot(data = data01,aes(x = gini_disp,y = ess,label=cname))+
  geom_point() +
  geom_smooth(method = "lm") +
  geom_label_repel() + 
  labs(x="Gini Index ", y="Subjective Social Status",
       titllk66e="Distribution of Subjective Social Status ",
       subtitle=NULL,
       caption="Source: ISSP - Social Inequality 2009") 

ggplot(data = data01,aes(x = gini_disp,y = gap_per,label=cname))+
  geom_point() +
  geom_smooth(method = "lm") +
  geom_label_repel()

ggplot(data = data01,aes(x = gap_per,y = ess,label=cname))+
  geom_point() +
  geom_smooth(method = "lm") +
  geom_label_repel()
