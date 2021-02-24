library("here")
library(ggplot2)
library(sjPlot)
library(ggthemes)
library(gridExtra)
library(grid)
library(dplyr)

rm(list=ls())
load(file = here("data","wvs1990CL.RData"))
load(file = here("data","wvs1996CL.RData"))
load(file = here("data","issp1999CL.RData"))
load(file = here("data","issp2006CL.RData"))
load(file = here("data","issp2009CL.RData"))
load(file = here("data","enacoes.RData"))
load(file = here("data","issp2016_A.RData"))
load(file = here("data","issp2016_B.RData"))
load(file = here("data","elsoc2017.RData"))
load(file = here("data","elsoc2018.RData"))
load(file = here("data","issp2019CL.RData"))


# Filtro Zona == URBANO ---------------------------------------------------
cl99  <- cl99 %>% filter(URBRURAL==1)
cl06  <- cl06 %>% filter(ZONA==1)
cl09  <- cl09 %>% filter(URBRURAL %in% c(1,2,3))
cl16a <- cl16a %>% filter(ZONA==1) 
cl16b <- cl16b %>% filter(ZONA_U_R==1) 
cl19 <-  cl19 %>% filter(ZONA==1) 

par(mfrow=c(2,4))



table(cl90$V363)
cl90$V363[cl90$V363==c(-2)] <- NA
summary(cl90$V363)
hist.default(cl90$V363,freq = FALSE,breaks = seq(0,10,by=1),ylim=c(0, 0.5),labels = TRUE,
             xlab = "Estatus Social Subjetivo",ylab = "Porcentaje",
             main = "WVS 1990") # WVS 1990


table(cl96$V227)
cl96$V227[cl96$V227==-2] <- NA
summary(cl96$V227)
hist.default(cl96$V227,freq = FALSE,breaks = seq(0,10,by=1),ylim=c(0, 0.5),labels = TRUE,
             xlab = "Estatus Social Subjetivo",ylab = "Porcentaje",
             main = "WVS 1996") # WVS 1996

cl99$V46[cl99$V46 == 98]<-NA
cl99$V46[cl99$V46 == 99]<-NA
cl99$V46[cl99$V46 == 97]<-NA
cl99$V46<-as.numeric(cl99$V46)
cl99$V46<- sjmisc::rec(cl99$V46 ,rec="rev")
table(cl99$V46)
summary(cl99$V46)
hist.default(cl99$V46,freq = FALSE,breaks = seq(0,10,by=1),ylim=c(0, 0.5),labels = TRUE,
             xlab = "Estatus Social Subjetivo",ylab = "Porcentaje",
             main = "ISSP 1999") # ISSSP 1999



cl06$dd19[cl06$dd19 %in% c(88,99)] <- NA
hist.default(cl06$dd19,freq = FALSE,breaks = seq(0,10,by=1),ylim=c(0, 0.5),labels = TRUE,
             xlab = "Estatus Social Subjetivo",ylab = "Porcentaje",
             main = "ISSP 2006") # ISSP 2006


cl09$V44[cl09$V44 == 98] <- NA
cl09$V44[cl09$V44 == 99] <- NA
table(cl09$V44)
summary(cl09$V44)
hist.default(cl09$V44,freq = FALSE,breaks = seq(0,10,by=1),ylim=c(0, 0.4),labels = TRUE,
             xlab = "Estatus Social Subjetivo",ylab = "Porcentaje",
             main = "ISSP 2009") # ISSP 2009


enacoes$B4_A[enacoes$B4_A %in% c(88,99)] <- NA
table(enacoes$B4_A)
hist.default(cl09$V44,freq = FALSE,breaks = seq(0,10,by=1),ylim=c(0, 0.4),labels = TRUE,
             xlab = "Estatus Social Subjetivo",ylab = "Porcentaje",
             main = "ENACOES 2014") # ENACOES 2014



cl16a$DS_P27[cl16a$DS_P27 %in% c(88,99)] <- NA
cl16b$DS_P13A[cl16b$DS_P13A %in% c(88,99)] <- NA
prop.table(table(cl16b$DS_P13A))*100
summary(cl16b$DS_P13A)
hist.default(cl16a$DS_P27,freq = FALSE,breaks = seq(0,10,by=1),ylim=c(0, 0.4),labels = TRUE,
             xlab = "Estatus Social Subjetivo",ylab = "Porcentaje",
             main = "ISSP 2016") # ISSP 2016



cl17$d01_01[cl17$d01_01 %in% c(-888,-999)] <- NA
cl17$d01_01[cl17$d01_01 == 0] <- NA # Eliminar los que se posicionan en 0 para homologar escala
prop.table(table(cl17$d01_01))*100
summary(cl17$d01_01)
hist.default(cl17$d01_01,freq = FALSE,breaks = seq(0,10,by=1),ylim=c(0, 0.4),labels = TRUE,
             xlab = "Estatus Social Subjetivo",ylab = "Porcentaje",
             main = "ELSOC 2017") # ELSOC 2017

cl18$d01_01[cl18$d01_01 %in% c(-888,-999)] <- NA
cl18$d01_01[cl18$d01_01 == 0] <- NA # Eliminar los que se posicionan en 0 para homologar escala
prop.table(table(cl18$d01_01))*100
summary(cl18$d01_01)
hist.default(cl18$d01_01,freq = FALSE,breaks = seq(0,10,by=1),ylim=c(0, 0.4),labels = TRUE,
             xlab = "Estatus Social Subjetivo",ylab = "Porcentaje",
             main = "ELSOC 2018") # ELSOC 2018

cl19$M2_P13A[cl19$M2_P13A == 88] <- NA
cl19$M2_P13A[cl19$M2_P13A == 99] <- NA
table(cl19$M2_P13A)
summary(cl19$M2_P13A)
hist.default(cl19$M2_P13A,freq = FALSE,breaks = seq(0,10,by=1),ylim=c(0, 0.4),labels = TRUE,
             xlab = "Estatus Social Subjetivo",ylab = "Porcentaje",
             main = "ISSP 2019") # ISSP 2019


# Plots en ggplot2 --------------------------------------------------------------

labels01 <- c("Más\nbajo",2,3,4,5,6,7,8,9," Más\nalto")

cl90$POPWEIGHT <- cl90$S018/1000*16929000


plot90<- ggplot(data = cl90,mapping = aes(x = V363,weight = POPWEIGHT)) + 
         geom_bar(aes(y=..prop.., group = 1), color="black", fill="darkgrey") +
         scale_x_continuous(breaks = seq(1,10,by=1),labels = labels01,name = NULL)+
         scale_y_continuous(labels=scales::percent_format(accuracy = 1),limits = c(0,0.50),name = NULL) +
         ggtitle(label = "1990")+
         geom_text(aes(label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5,size=2.5) +
         theme_classic() +
         theme(plot.title = element_text(size=14,hjust = 0.5))

cl96$POPWEIGHT <- cl96$S018/1000*16929000
plot96<- ggplot(data = cl96,mapping = aes(x = V227,weight = POPWEIGHT)) + 
  geom_bar(aes(y=..prop.., group = 1), color="black", fill="darkgrey") +
  scale_x_continuous(breaks = seq(1,10,by=1),labels = labels01,name = NULL)+
  scale_y_continuous(labels=scales::percent_format(accuracy = 1),limits = c(0,0.50),name = NULL) +
  ggtitle(label = "1996")+
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5,size=2.5) +
  theme_classic() +
  theme(plot.title = element_text(size=14,hjust = 0.5))

plot99<- ggplot(data = cl99,mapping = aes(x = V46,weight = WEIGHT)) + 
         geom_bar(aes(y=..prop.., group = 1), color="black", fill="darkgrey") +
         scale_x_continuous(breaks = seq(1,10,by=1),labels = labels01,name = NULL)+
         scale_y_continuous(labels=scales::percent_format(accuracy = 1),limits =c(0,0.50),name = NULL) +
         ggtitle(label = "1999")+
         geom_text(aes(label = scales::percent(..prop..),
                        y= ..prop.. ), stat= "count", vjust = -.5,size=2.5) +
         theme_classic() +
         theme(plot.title = element_text(size=14,hjust = 0.5))

plot06<- ggplot(data = cl06,mapping = aes(x = dd19, weight= f_correc)) + 
         geom_bar(aes(y=..prop.., group = 1), color="black", fill="darkgrey") +
         scale_x_continuous(breaks = seq(1,10,by=1),labels = labels01,name = NULL)+
         scale_y_continuous(labels=scales::percent_format(accuracy = 1),limits = c(0,0.50),name = NULL) +
         ggtitle(label = "2006")+
         geom_text(aes(label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5,size=2.5) +
         theme_classic() +
         theme(plot.title = element_text(size=14,hjust = 0.5))


plot09<- ggplot(data = cl09,mapping = aes(x = V44, weight=WEIGHT)) + 
         geom_bar(aes(y=..prop.., group = 1), color="black", fill="darkgrey") +
         scale_x_continuous(breaks = seq(1,10,by=1),labels = labels01,name = NULL)+
         ggtitle(label = "2009")+
         scale_y_continuous(labels = NULL,name = NULL,limits = c(0,0.50)) +
         geom_text(aes(label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5,size=2.5) +
         theme_classic() + 
         theme(plot.title = element_text(size=14,hjust = 0.5))


plot14<- ggplot(data = enacoes,mapping = aes(x = B4_A, weight=POND_MUESTRAL)) + 
         geom_bar(aes(y=..prop.., group = 1), color="black", fill="darkgrey") +
         scale_x_continuous(breaks = seq(1,10,by=1),labels = labels01,name = NULL)+
         ggtitle(label = "2014")+
         scale_y_continuous(labels = NULL,name = NULL,limits = c(0,0.50)) +
         geom_text(aes(label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5,size=2.5) +
         theme_classic() + 
         theme(plot.title = element_text(size=14,hjust = 0.5))


plot16a<- ggplot(data = cl16a,mapping = aes(x = DS_P27,weight=pond)) + 
         geom_bar(aes(y=..prop.., group = 1), color="black", fill="darkgrey") +
         scale_x_continuous(breaks = seq(1,10,by=1),labels = labels01,name = NULL)+
         scale_y_continuous(labels = NULL,name = NULL,limits = c(0,0.50)) +
         ggtitle(label = "2016 (a)")+
         geom_text(aes(label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5,size=2.5) +
         theme_classic() + theme(plot.title = element_text(size=14,hjust = 0.5))

plot16b<- ggplot(data = cl16b,mapping = aes(x = DS_P13A,weight=Ponderador)) + 
          geom_bar(aes(y=..prop.., group = 1), color="black", fill="darkgrey") +
          scale_x_continuous(breaks = seq(1,10,by=1),labels = labels01,name = NULL)+
          scale_y_continuous(labels = NULL,name = NULL,limits = c(0,0.50)) +
          ggtitle(label = "2016 (b)")+
          geom_text(aes(label = scales::percent(..prop..),
                        y= ..prop.. ), stat= "count", vjust = -.5,size=2.5) +
          theme_classic() + theme(plot.title = element_text(size=14,hjust = 0.5))

plot17<- ggplot(data = cl17,mapping = aes(x = d01_01,weight=ponderador02)) + 
         geom_bar(aes(y=..prop.., group = 1), color="black", fill="darkgrey") +
         scale_x_continuous(breaks = seq(1,10,by=1),labels = labels01,name = NULL)+
         scale_y_continuous(labels = NULL,name = NULL,limits = c(0,0.50)) +
         ggtitle(label = "2017")+
         geom_text(aes(label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5,size=2.5) +
         theme_classic() + theme(plot.title = element_text(size=14,hjust = 0.5))

plot18<- ggplot(data = cl18,mapping = aes(x = d01_01,weight=ponderador02)) + 
         geom_bar(aes(y=..prop.., group = 1), color="black", fill="darkgrey") +
         scale_x_continuous(breaks = seq(1,10,by=1),labels = labels01,name = NULL)+
         scale_y_continuous(labels = NULL,name = NULL,limits = c(0,0.50)) +
         ggtitle(label = "2018 ")+
         geom_text(aes(label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5,size=2.5) +
         theme_classic() + theme(plot.title = element_text(size=14,hjust = 0.5))

plot19<- ggplot(data = cl19,mapping = aes(x = M2_P13A, weight=FACTOR)) + 
         geom_bar(aes(y=..prop.., group = 1), color="black", fill="darkgrey") +
         scale_x_continuous(breaks = seq(1,10,by=1),labels = labels01,name = NULL)+
         scale_y_continuous(labels = NULL,name = NULL,limits = c(0,0.50)) +
         ggtitle(label = "2019")+
         geom_text(aes(label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5,size=2.5) +
         theme_classic() + theme(plot.title = element_text(size=14,hjust = 0.5))

grid.arrange(plot90, plot96,plot99,plot06,plot09,plot14,plot16a,plot17,plot18,plot19, 
             nrow = 1,
             top = "¿Dónde se ubicaría usted en la sociedad chilena?",
             bottom = textGrob("Fuente: Elaboración propia en base a WVS*, ISSP**, COES***, 1990*, 1996*, 1999**, 2006**, 2009**, 2014***,2016**,2017***,2018*** y 2019** \n Iturra J. (2020)",
                               
                               gp = gpar(fontface = 1, fontsize = 9),
                               hjust = 1.1,
                               x = 1))

dat90 <- cl90 %>% rename(ess=V363) %>% mutate(year=1990)   %>% select(ess,year,pond=POPWEIGHT)
dat96 <- cl96 %>% rename(ess=V227) %>% mutate(year=1996)   %>% select(ess,year,pond=POPWEIGHT)
dat99 <- cl99 %>% rename(ess=V46) %>% mutate(year=1999)    %>% select(ess,year,pond=WEIGHT)
dat06 <- cl06 %>% rename(ess=dd19) %>% mutate(year=2006)   %>% select(ess,year,pond=f_correc)
dat09 <- cl09 %>% rename(ess=V44) %>% mutate(year=2009)    %>% select(ess,year,pond=WEIGHT)
dat14 <- enacoes %>% rename(ess=B4_A) %>% mutate(year=2014)%>% select(ess,year,pond=POND_MUESTRAL)
dat16 <- cl16b %>% rename(ess=DS_P13A) %>% mutate(year=2016)%>% select(ess,year,pond=Ponderador)
dat17 <- cl17 %>% rename(ess=d01_01) %>% mutate(year=2017)%>% select(ess,year,pond=ponderador02)
dat18 <- cl18 %>% rename(ess=d01_01) %>% mutate(year=2018)%>% select(ess,year,pond=ponderador02)
dat19 <- cl19 %>% rename(ess=M2_P13A) %>% mutate(year=2019)%>% select(ess,year,pond=FACTOR)


data_ess <- bind_rows(dat90,
          dat96,
          dat99,
          dat06,
          dat09,
          dat14,
          dat16,
          dat17,
          dat18,
          dat19)

table01<- data_ess %>% group_by(year) %>% mutate(mean=weighted.mean(x = ess,na.rm = TRUE,w = pond))
table01
cor(table01$ess,table01$mean,use = "complete.obs")
table02 <- data_ess %>% group_by(year) %>% summarise(mean=weighted.mean(x = ess,na.rm = TRUE,w = pond)) 

# stacked bars ------------------------------------------------------------

tab90<- prop.table(questionr::wtd.table(cl90$V363,weights = cl90$POPWEIGHT))
tab90<- prop.table(questionr::wtd.table(cl96$V227,weights = cl96$POPWEIGHT))
tab99<- prop.table(questionr::wtd.table(cl99$V46 ,weights = cl99$WEIGHT))
tab06<- prop.table(questionr::wtd.table(cl06$dd19,weights = cl06$f_correc))
tab09<- prop.table(questionr::wtd.table(cl09$V44, weights = cl09$WEIGHT))
tab14<- prop.table(questionr::wtd.table(enacoes$B4_A, weights = enacoes$POND_MUESTRAL))


tab16<- prop.table(questionr::wtd.table(cl16b$DS_P13A,weights = cl16b$Ponderador))

tab17<- prop.table(questionr::wtd.table(cl17$d01_01,weights = cl17$ponderador02))
tab18<- prop.table(questionr::wtd.table(cl18$d01_01,weights = cl18$ponderador02))
tab19<- prop.table(questionr::wtd.table(cl19$M2_P13A, weights = cl19$FACTOR))

data01<- bind_rows(
data.frame("ess"=prop.table(questionr::wtd.table(cl90$V363,weights = cl90$POPWEIGHT)),year=1990),
data.frame("ess"=prop.table(questionr::wtd.table(cl96$V227,weights = cl96$POPWEIGHT)),year=1996),
data.frame("ess"=prop.table(questionr::wtd.table(cl99$V46 ,weights = cl99$WEIGHT)),year=1999),
data.frame("ess"=prop.table(questionr::wtd.table(cl06$dd19,weights = cl06$f_correc)),year=2006),
data.frame("ess"=prop.table(questionr::wtd.table(cl09$V44, weights = cl09$WEIGHT)),year=2009),
data.frame("ess"=prop.table(questionr::wtd.table(enacoes$B4_A, weights = enacoes$POND_MUESTRAL)),year=2014),
data.frame("ess"=prop.table(questionr::wtd.table(cl16b$DS_P13A,weights = cl16b$Ponderador)),year=2016),
data.frame("ess"=prop.table(questionr::wtd.table(cl17$d01_01,weights = cl17$ponderador02)),year=2017),
data.frame("ess"=prop.table(questionr::wtd.table(cl18$d01_01,weights = cl18$ponderador02)),year=2018),
data.frame("ess"=prop.table(questionr::wtd.table(cl19$M2_P13A, weights = cl19$FACTOR)),year=2019))

data02<- left_join(data01,table02)
cor(data02$year,data02$mean,use = "complete.obs") # cor ess~year
names(data02)
data02$labs <- ifelse(test = (data02$ess.Var1 %in% c(3,4,5,6,7)),yes = data02$ess.Freq,no = NA) 

plot01ess<- ggplot(data02,aes(y = ess.Freq, x=factor(year),label=round(labs*100,1),fill=reorder(ess.Var1,desc(ess.Var1)))) + 
  geom_bar(stat = "identity", position = "stack",width = 0.95)+
  scale_fill_brewer(palette = "RdGy",name="")+ 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_text(position = position_stack(vjust = 0.5), size=3) +
  # geom_text(,aes(x=factor(year),label=round(mean,2)),position = position_stack(vjust = 0.5))+ 
  guides()+
  labs(title = "Estatus Social Subjetivo")+
  ylab(label = NULL)+
  xlab(label = NULL)+
  theme_classic()
plot01ess

ggsave(plot01ess,filename = "paper/images/Figura1.png",device = "png",width = 20,height = 15,dpi = "retina",units = "cm")

data_ess2 <- bind_cols(dat90,
                      dat96,
                      dat99,
                      dat06,
                      dat09,
                      dat14,
                      dat16,
                      dat17,
                      dat18,
                      dat19)


test<- qpcR:::cbind.na(dat90,dat96,dat99,dat06,dat09,dat14,dat16,dat17,dat18,dat19)
library(sjPlot)


# gganimate de estatus subjetivo ------------------------------------------

dat01 <- data.frame(ess=cl99$V46,year=1999)
dat02 <- data.frame(ess=cl09$V44,year=2009)
dat03 <- data.frame(ess=cl19$M2_P13A, year=2019)

names(dat01)
names(dat02)
names(dat03)

data <- dplyr::bind_rows(dat01, dat02)
data <- dplyr::bind_rows(data, dat03)
summary(data)

summary(table01)


library(gganimate)

library(gganimate)

p <- ggplot(data = table01,mapping = aes(x = ess)) + 
  geom_bar(aes(y=..prop.., group = 1),fill = "#3E7622") +
  scale_x_continuous(breaks = seq(1,10,by=1),labels = labels01,name = NULL)+
  scale_y_continuous(labels = NULL,name = NULL,limits = c(0,0.45)) +
  geom_text(aes(label = scales::percent(..prop..,accuracy = 0.1),
                y= ..prop..), stat= "count", vjust = -.5,size=6.0) +
  theme_classic() + 
  theme(plot.title = element_text(size=18,hjust = 0.5),
        plot.subtitle = element_text(size=12,hjust = 0.5),
        plot.caption = element_text(size=12),
        axis.text.x = element_text(size=12)) +
  labs(subtitle = "Autor: Julio Iturra, @jciturras",title=  "Hay grupos que tienden a ubicarse en los niveles más altos y otros en los niveles más bajos. \n ¿Dónde se ubicaría usted? \n Año {closest_state} ",
       caption = "Fuente: WVS (1990,1996), ISSP (1999,2006,2009,2019), COES (2014,2017,2018)" )
p

p1 <- p + 
  transition_states(year) 

anim_save(animation = p1,filename = "estatus.gif",width = 800)






