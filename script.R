setwd("")
library(foreign)
library(nnet)
library(ggplot2)
library(scales)
library(texreg)
library(ineq)
library(data.table)
library(dplyr)
library(MCMCpack)
library(ggmcmc)

m<-read.csv("master_data_identity.csv",header=T,sep=",")

names(m)


## construct the dependent variable

#ethnicity
summary(factor(m$v4018)) #friends among other ethnic groups
summary(factor(m$v3040)) #ethnic quotas
summary(factor(m$v4022)) #newcomers should adopt traditions
summary(factor(m$v301)) #satisfied with the situation in the republic
summary(factor(m$v307)) #authority division
summary(factor(m$v3025)) #election of republican officials



m$nofriend[m$v4018==1]<-1
m$nofriend[m$v4018==2]<-2
summary(m$nofriend)

summary(factor(m$ethnic_representation))
m$ethnic_representation2[m$ethnic_representation==0]<-1
m$ethnic_representation2[m$ethnic_representation==1]<-2
m$ethnic_representation3[m$v3040==1]<-1
m$ethnic_representation3[m$v3040==2]<-3
m$ethnic_representation3[m$v3040==3]<-2




summary(factor(m$v4022))
m$newcomers[m$v4022==1]<-5
m$newcomers[m$v4022==2]<-4
m$newcomers[m$v4022==3]<-2
m$newcomers[m$v4022==4]<-1
m$newcomers[m$v4022==5]<-3
summary(factor(m$newcomers))

m$satisfied[m$v301==1]<-1
m$satisfied[m$v301==2]<-2
m$satisfied[m$v301==3]<-3
m$satisfied[m$v301==4]<-4
summary(factor(m$satisfied))

m$satisfied2<-ifelse(m$dominant_ethnicity==1,m$satisfied,NA) 

m$repower[m$v307==1]<-1
m$repower[m$v307==2]<-4
m$repower[m$v307==3]<-3
m$repower[m$v307==4]<-2

m$repower2<-ifelse(m$dominant_ethnicity==1,m$repower,NA)
summary(factor(m$repower2))

m$fepower[m$v307==1]<-4
m$fepower[m$v307==2]<-1
m$fepower[m$v307==3]<-2
m$fepower[m$v307==4]<-3  

m$fepower2<-ifelse(m$rus==1,m$fepower,NA)
summary(factor(m$fepower2))

m$power<-ifelse(is.na(m$repower2),m$fepower2,m$repower2)
summary(factor(m$power))

m$felect<-recode(m$v3025, `1`=4, `2`=3, `3`=2, `4`=1)
m$relect<-recode(m$v3025, `1`=1, `2`=2, `3`=3, `4`=4)

#religion
summary(factor(m$v408)) #follow religious rituals
summary(factor(m$v409)) #read religious texts
summary(factor(m$v4010)) #clerical organizations take part in solving the state affairs
summary(factor(m$v4012)) #religious education in schools
summary(factor(m$v4016)) #live in secular or sharia state

m$nofriend01<-recode(m$nofriend, `1`=0, `2`=1)

summary(factor(m$newcomers)) ##contingent on religiosity - actually, not necessarily
m$newcomers01<-recode(m$newcomers, `1`=0, `2`=0, `3`=0.5, `4`=1, `5`=1)

summary(factor(m$satisfied2))
m$satisfied01<-recode(m$satisfied2, `1`=0, `2`=0, `3`=1, `4`=1)
summary(factor(m$satisfied01))

summary(factor(m$power))
m$power01[m$power==1]<-0
m$power01[m$power==2]<-0
m$power01[m$power==3]<-1
m$power01[m$power==2 & m$repower2==2]<-0.5
m$power01[m$power==3 & m$fepower2==3]<-0.5
m$power01[m$power==4]<-1
summary(factor(m$power01))
summary(factor(m$fepower2))


m$felect2<-ifelse(m$rus==1,m$felect,NA)
m$relect2<-ifelse(m$dominant_ethnicity==1,m$relect,NA)
m$elect<-ifelse(is.na(m$relect2),m$felect2,m$relect2)
summary(factor(m$elect))

m$elect01<-recode(m$elect,`1`=0, `2`=0, `3`=1, `4`=1)

summary(factor(m$v408))
m$rituals01<-recode(m$v408, `1`=1, `2`=1, `3`=0, `4`=0)
summary(factor(m$rituals01))

summary(factor(m$v409))
m$readholytexts01<-recode(m$v409, `1`=0, `2`=0.5, `3`=1, `4`=1, `5`=1)
summary(factor(m$readholytexts01))

summary(factor(m$v4010))
m$rel_state_affairs<-recode(m$v4010, `1`=1, `2`=1, `3`=0, `4`=0)
summary(factor(m$rel_state_affairs))

summary(factor(m$v4012))
m$rel_edu<-ifelse(m$v4012<5, 1,0)
summary(factor(m$rel_edu))

summary(factor(m$v4016))
m$sharia <-recode(m$v4016, `1`=0, `2`=0, `3`=1, `4`=1)
summary(factor(m$sharia))

names(m)
m$ethnic_score<-rowMeans(cbind(m$nofriend01,m$satisfied01,m$power01,m$elect01,
                               m$newcomers01,m$ethnic_representation),na.rm=T)
summary(factor(m$ethnic_score))

m$religious_score<-rowMeans(cbind(m$rituals01,m$readholytexts01,m$rel_state_affairs,m$rel_edu,
                                  m$sharia),na.rm=T)
summary(factor(m$religious_score))

m$id12[m$ethnic_score>=0.5 & m$religious_score>=0.5]<-3
m$id12[m$ethnic_score>=0.5 & m$religious_score<0.5]<-1
m$id12[m$ethnic_score<0.5 & m$religious_score>=0.5]<-2

summary(factor(m$id12))

## independent variables

summary(factor(m$v401))
m$distrust_strangers<-ifelse(m$v401<5,m$v401,NA)
summary(factor(m$distrust_strangers))


###create dominant ethnicity dummy
summary(m$v5024)
m$dominant_ethnicity[m$reg==4 & m$v5024==3]<-1
m$dominant_ethnicity[m$reg==6 & m$v5024==1]<-1
m$dominant_ethnicity[m$reg==2 & m$v5024==3]<-1
m$dominant_ethnicity[m$reg==3 & m$v5024==1]<-1
m$dominant_ethnicity[m$reg==5 & m$v5024==1]<-1
m$dominant_ethnicity[m$reg==1 & m$v5024==1]<-1
m$dominant_ethnicity[m$reg==7 & m$v5024==1]<-1
summary(m$dominant_ethnicity)
m$dominant_ethnicity<-ifelse((is.na(m$dominant_ethnicity) & !is.na(m$v5024)), 0, m$dominant_ethnicity)


# descriptive graphs
# gini coefficient plots
library(ineq)
eth<-subset(m,id12==1) #ethnicity
rel<-subset(m,id12==2) #religion
mix<-subset(m,id12==3) #mixed

eth_gini<-ineq(eth$income,type="Gini",na.rm=T)
rel_gini<-ineq(rel$income,type="Gini",na.rm=T)
mix_gini<-ineq(mix$income,type="Gini",na.rm=T)

gini<-as.data.frame(c(eth_gini,rel_gini,mix_gini))
gini$name<-c("Ethnicity","Religion","Mixed")
names(gini)
setnames(gini, "c(eth_gini, rel_gini, mix_gini)", "value")
ggplot(gini, aes(x=name, y=value)) +
  geom_point(aes(shape=name,size=3))+
  scale_x_discrete(limits=c("Ethnicity", "Religion", 
                            "Mixed"), labels=c("Ethnicity", "Religion", 
                                               "Mixed"))+
  xlab("") +
  ylab("Gini coefficient") +
  ggtitle("") +guides(shape=FALSE,size=FALSE)+
  theme(axis.text.x = element_text(size=12,face="bold",angle=0),
        axis.title.x=element_text(size=14,face="bold"),
        axis.title.y=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size = 14),
        legend.title=element_text(size=14),
        legend.position="none")
ggsave("the_gini.pdf",width=7,height=5)
dev.off()


###randomly sample 50 observations from each group category
set.seed(12)
samp_eth <- sample(nrow(eth),50)
samp_rel <- sample(nrow(rel),50)
samp_mix <- sample(nrow(mix),50)

eth_s <- eth[samp_eth,]
rel_s <- rel[samp_rel,]
mix_s <- mix[samp_mix,]

eth_gini_s<-ineq(eth_s$income,type="Gini",na.rm=T)
rel_gini_s<-ineq(rel_s$income,type="Gini",na.rm=T)
mix_gini_s<-ineq(mix_s$income,type="Gini",na.rm=T)

gini_s<-as.data.frame(c(eth_gini_s,rel_gini_s,mix_gini_s))
gini_s$name<-c("Ethnicity","Religion","Mixed")
names(gini_s)
setnames(gini_s, "c(eth_gini_s, rel_gini_s, mix_gini_s)", "value")
ggplot(gini_s, aes(x=name, y=value)) +
  geom_point(aes(shape=name,size=3))+
  scale_x_discrete(limits=c("Ethnicity", "Religion", 
                            "Mixed"), labels=c("Ethnicity", "Religion", 
                                               "Mixed"))+
  xlab("") +
  ylab("Gini coefficient") +
  ggtitle("") +guides(shape=FALSE,size=FALSE)+
  theme(axis.text.x = element_text(size=12,face="bold",angle=0),
        axis.title.x=element_text(size=14,face="bold"),
        axis.title.y=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size = 14),
        legend.title=element_text(size=14),
        legend.position="none")
ggsave("the_gini_sample.pdf",width=7,height=5)
dev.off()



vars<-c("region","income","id12")
m2<-na.omit
income<-m[vars]
m2<-na.omit(income)
m2$reg[m2$region==1]<-"North Ossetia"
m2$reg[m2$region==2]<-"Karachay-Cherkessia"
m2$reg[m2$region==3]<-"Dagestan"
m2$reg[m2$region==4]<-"Adygea"
m2$reg[m2$region==5]<-"Ingushetia"
m2$reg[m2$region==6]<-"Kabardin-Balkaria"
m2$reg[m2$region==7]<-"Chechnya"

in_oss<-subset(m2,region==1)
in_kcr<-subset(m2,region==2)
in_dag<-subset(m2,region==3)
in_ady<-subset(m2,region==4)
in_ing<-subset(m2,region==5)
in_kbr<-subset(m2,region==6)
in_che<-subset(m2,region==7)


hist_income <- ggplot(m2, aes(x=income, fill=factor(income)))
hist_income +   geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent,limits = c(0,.5)) +
  labs(title = "", y = "Percent", x = "")+
  theme(axis.text.x = element_text(face="bold", vjust=0.5,hjust=.95,
                                   size=12, angle=90))+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank())+
  scale_x_discrete(limit=c("1","2","3","4","5"),
                   labels=c("No food or clothes", "Food but not clothes", 
                            "Food and clothes","Expensive items", "Everything"))+
  guides(fill=FALSE)+
  scale_fill_grey(start=0.15, end=0.85)


ggsave("descr_income.pdf",width=7,height=7)
dev.off()



hist <- ggplot(m, aes(x=id12, fill=factor(id12)))
hist +   geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent,limits = c(0,.5)) +
  labs(title = "", y = "Percent", x = "")+
  theme(axis.text.x = element_text(face="bold", vjust=0.5,
                                   size=12, angle=45))+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank())+
  scale_x_discrete(limit=c("1","2","3"),
                   labels=c("Ethnicity", "Religion", "Mixed"))+
  guides(fill=FALSE)+
  scale_fill_grey(start=0.15, end=0.85)

ggsave("the_dv.pdf",width=5,height=7)
dev.off()




hist <- ggplot(m, aes(x=id12, fill=factor(id12)))
hist + geom_histogram(binwidth = 0.5,stat="count")+
  scale_x_discrete(limits=c("1","2","3"),
                   labels=c("Ethnicity", "Religion", "Mixed"))+
  theme(axis.text.x = element_text(face="bold", vjust=0.5,
                                   size=12, angle=45))+
  ylab("Number of Respondents") +
  xlab("")+guides(fill=FALSE)+
  scale_fill_grey(start=0.15, end=0.85)+
  scale_y_continuous(breaks = seq(0, 600, by=100), limits=c(0,600))
ggsave("the_dv_number.pdf",width=5,height=7)
dev.off()

vars<-c("region","v5024","id12","income")
e_group<-m[vars]
#Ossetia
e_group$egroup[e_group$region==1 & e_group$v5024==1]<-"Ossetian"
e_group$egroup[e_group$region==1 & e_group$v5024==2]<-"Russian"
e_group$egroup[e_group$region==1 & e_group$v5024==3]<-"Armenian"
e_group$egroup[e_group$region==1 & e_group$v5024==4]<-"Ingush"
e_group$egroup[e_group$region==1 & e_group$v5024==5]<-"Lezgin"
e_group$egroup[e_group$region==1 & e_group$v5024==6]<-"Georgian"
#KCR
e_group$egroup[e_group$region==2 & e_group$v5024==1]<-"Russian"
e_group$egroup[e_group$region==2 & e_group$v5024==2]<-"Abazin"
e_group$egroup[e_group$region==2 & e_group$v5024==3]<-"Cherkess and Karachay"
e_group$egroup[e_group$region==2 & e_group$v5024==4]<-"Nogai"
e_group$egroup[e_group$region==2 & e_group$v5024==5]<-"Ossetian"
e_group$egroup[e_group$region==2 & e_group$v5024==6]<-"Ukrainian"
e_group$egroup[e_group$region==2 & e_group$v5024==7]<-"Mixed race"
e_group$egroup[e_group$region==2 & e_group$v5024==8]<-"Jewish"
e_group$egroup[e_group$region==2 & e_group$v5024==9]<-"Chechen"
e_group$egroup[e_group$region==2 & e_group$v5024==10]<-"Ingush"
#Dagestan
e_group$egroup[e_group$region==3 & e_group$v5024==1]<-"Avar"
e_group$egroup[e_group$region==3 & e_group$v5024==2]<-"Kumyk"
e_group$egroup[e_group$region==3 & e_group$v5024==3]<-"Lak"
e_group$egroup[e_group$region==3 & e_group$v5024==4]<-"Dargin"
e_group$egroup[e_group$region==3 & e_group$v5024==5]<-"Tabasaran"
e_group$egroup[e_group$region==3 & e_group$v5024==6]<-"Tatar"
e_group$egroup[e_group$region==3 & e_group$v5024==7]<-"Russian"
e_group$egroup[e_group$region==3 & e_group$v5024==8]<-"Nogai"
e_group$egroup[e_group$region==3 & e_group$v5024==9]<-"Lezgin"
e_group$egroup[e_group$region==3 & e_group$v5024==10]<-"Karachay"
e_group$egroup[e_group$region==3 & e_group$v5024==11]<-"Agul"
#Adygea
e_group$egroup[e_group$region==4 & e_group$v5024==1]<-"Russian"
e_group$egroup[e_group$region==4 & e_group$v5024==2]<-"Ukrainian"
e_group$egroup[e_group$region==4 & e_group$v5024==3]<-"Adygean"
e_group$egroup[e_group$region==4 & e_group$v5024==4]<-"German"
e_group$egroup[e_group$region==4 & e_group$v5024==5]<-"Armenian"
e_group$egroup[e_group$region==4 & e_group$v5024==6]<-"Azeri"
e_group$egroup[e_group$region==4 & e_group$v5024==7]<-"Tatar"
e_group$egroup[e_group$region==4 & e_group$v5024==8]<-"Byelorussian"
e_group$egroup[e_group$region==4 & e_group$v5024==9]<-"Latvian"
e_group$egroup[e_group$region==4 & e_group$v5024==10]<-"Uzbek"
e_group$egroup[e_group$region==4 & e_group$v5024==11]<-"Moldavian"
e_group$egroup[e_group$region==4 & e_group$v5024==12]<-"Cossack"
#Ingushetia
e_group$egroup[e_group$region==5 & e_group$v5024==1]<-"Ingush"
e_group$egroup[e_group$region==5 & e_group$v5024==2]<-"Chechen"
e_group$egroup[e_group$region==5 & e_group$v5024==3]<-"Russian"
#KBR
e_group$egroup[e_group$region==6 & e_group$v5024==1]<-"Kabardin"
e_group$egroup[e_group$region==6 & e_group$v5024==2]<-"Russian"
e_group$egroup[e_group$region==6 & e_group$v5024==3]<-"Balkar"
e_group$egroup[e_group$region==6 & e_group$v5024==4]<-"Mixed race"
e_group$egroup[e_group$region==6 & e_group$v5024==5]<-"Ingush"
e_group$egroup[e_group$region==6 & e_group$v5024==6]<-"Cherkes"
e_group$egroup[e_group$region==6 & e_group$v5024==7]<-"Adygean"
e_group$egroup[e_group$region==6 & e_group$v5024==8]<-"Armenian"
e_group$egroup[e_group$region==6 & e_group$v5024==9]<-"Jewish"
e_group$egroup[e_group$region==6 & e_group$v5024==10]<-"Korean"
e_group$egroup[e_group$region==6 & e_group$v5024==11]<-"Georgian"
e_group$egroup[e_group$region==6 & e_group$v5024==12]<-"German"
e_group$egroup[e_group$region==6 & e_group$v5024==13]<-"Ingush"
#Chechnya
e_group$egroup[e_group$region==7 & e_group$v5024==1]<-"Chechen"
e_group$egroup[e_group$region==7 & e_group$v5024==2]<-"Russian"
e_group$egroup[e_group$region==7 & e_group$v5024==3]<-"Kumyk"
e_group$egroup[e_group$region==7 & e_group$v5024==4]<-"Ingush"
e_group$egroup[e_group$region==7 & e_group$v5024==5]<-"Azeri"
e_group$egroup[e_group$region==7 & e_group$v5024==6]<-"Tatar"
e_group$egroup[e_group$region==7 & e_group$v5024==7]<-"Armenian"

e_group$count<-1
e_group$reg[e_group$region==1]<-"N Ossetia"
e_group$reg[e_group$region==2]<-"KChR"
e_group$reg[e_group$region==3]<-"Dagestan"
e_group$reg[e_group$region==4]<-"Adygea"
e_group$reg[e_group$region==5]<-"Ingushetia"
e_group$reg[e_group$region==6]<-"KBR"
e_group$reg[e_group$region==7]<-"Chechnya"

e_group2<-subset(e_group, !is.na(id12))
e_group2<-subset(e_group2, !is.na(income))
e_no<-subset(e_group2, region==1)
e_kcr<-subset(e_group2, region==2)
e_dag<-subset(e_group2, region==3)
e_ady<-subset(e_group2, region==4)
e_ing<-subset(e_group2, region==5)
e_kbr<-subset(e_group2, region==6)
e_che<-subset(e_group2, region==7)

e_no$egroup <- factor(e_no$egroup, 
                      levels=c("Ossetian","Russian","Armenian","Georgian","Ingush","Lezgin","NA"))

h_no <- ggplot(e_no, aes(x=egroup, fill=egroup))
h_no +   geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent,limits = c(0,1)) +
  labs(title = "", y = "Percent", x = "")+
  theme(axis.text.x = element_text(face="bold", vjust=0.5,hjust=.95,
                                   size=12, angle=90))+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank())+
  guides(fill=FALSE)+
  scale_fill_grey(start=0.15, end=0.85)+
  annotate("text", label = "North Ossetia, N=253", x = 1.7, y = 1)


ggsave("e_no.pdf",width=7,height=5)
dev.off()


e_kcr$egroup <- factor(e_kcr$egroup, 
                       levels=c("Cherkess and Karachay","Russian","Nogai","Abazin","Ossetian",
                                "Chechen","Ingush","Jewish","Mixed race","Ukrainian","NA"))
h_kcr <- ggplot(e_kcr, aes(x=egroup, fill=egroup))
h_kcr +   geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent,limits = c(0,1)) +
  labs(title = "", y = "Percent", x = "")+
  theme(axis.text.x = element_text(face="bold", vjust=0.5,hjust=.95,
                                   size=12, angle=90))+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank())+
  guides(fill=FALSE)+
  scale_fill_grey(start=0.15, end=0.85)+
  annotate("text", label = "Karachay-Cherkessia, N=102", x = 3, y = 1)

ggsave("e_kcr.pdf",width=7,height=5)
dev.off()


e_dag$egroup <- factor(e_dag$egroup, 
                       levels=c("Avar","Nogai","Kumyk","Dargin","Karachay","Lak",
                                "Lezgin", "Russian","Tabasaran","Tatar","Agul","NA"))

h_dag <- ggplot(e_dag, aes(x=egroup, fill=egroup))
h_dag +   geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent,limits = c(0,1)) +
  labs(title = "", y = "Percent", x = "")+
  theme(axis.text.x = element_text(face="bold", vjust=0.5,hjust=.95,
                                   size=12, angle=90))+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank())+
  guides(fill=FALSE)+
  scale_fill_grey(start=0.15, end=0.85)+
  annotate("text", label = "Dagestan, N=191", x = 2.2, y = 1)

ggsave("e_dag.pdf",width=7,height=5)
dev.off()


e_ady$egroup <- factor(e_ady$egroup, 
                       levels=c("Russian","Adygean","Ukrainian","Armenian","Byelorussian",
                                "Azeri","German","Latvian",
                                "Moldavian","Tatar","NA"))

h_ady <- ggplot(e_ady, aes(x=egroup, fill=egroup))
h_ady +   geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent,limits = c(0,1)) +
  labs(title = "", y = "Percent", x = "")+
  theme(axis.text.x = element_text(face="bold", vjust=0.5,hjust=.95,
                                   size=12, angle=90))+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank())+
  guides(fill=FALSE)+
  scale_fill_grey(start=0.15, end=0.85)+
  annotate("text", label = "Adygea, N=210", x = 2, y = 1)

ggsave("e_ady.pdf",width=7,height=5)
dev.off()


e_ing$egroup <- factor(e_ing$egroup, 
                       levels=c("Ingush","Chechen","Russian","NA"))

h_ing <- ggplot(e_ing, aes(x=egroup, fill=egroup))
h_ing +   geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent,limits = c(0,1)) +
  labs(title = "", y = "Percent", x = "")+
  theme(axis.text.x = element_text(face="bold", vjust=0.5,hjust=.95,
                                   size=12, angle=90))+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank())+
  guides(fill=FALSE)+
  scale_fill_grey(start=0.15, end=0.85)+
  annotate("text", label = "Ingushetia, N=116", x = 1, y = 1)

ggsave("e_ing.pdf",width=7,height=5)
dev.off()


e_kbr$egroup <- factor(e_kbr$egroup, 
                       levels=c("Kabardin","Russian","Balkar", "Jewish", "Ingush",
                                "Mixed race", "Georgian", "Korean",  "NA"))

h_kbr <- ggplot(e_kbr, aes(x=egroup, fill=egroup))
h_kbr +   geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent,limits = c(0,1)) +
  labs(title = "", y = "Percent", x = "")+
  theme(axis.text.x = element_text(face="bold", vjust=0.5,hjust=.95,
                                   size=12, angle=90))+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank())+
  guides(fill=FALSE)+
  scale_fill_grey(start=0.15, end=0.85)+
  annotate("text", label = "Kabardino-Balkaria, N=204", x = 2.3, y = 1)


ggsave("e_kbr.pdf",width=7,height=5)
dev.off()

e_che$egroup <- factor(e_che$egroup, 
                       levels=c("Chechen","Russian","Kumyk", "Ingush", "Armenian",
                                "Azeri", "Tatar",  "NA"))

h_che <- ggplot(e_che, aes(x=egroup, fill=egroup))
h_che +   geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent,limits = c(0,1)) +
  labs(title = "", y = "Percent", x = "")+
  theme(axis.text.x = element_text(face="bold", vjust=0.5,hjust=.95,
                                   size=12, angle=90))+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank())+
  guides(fill=FALSE)+
  scale_fill_grey(start=0.15, end=0.85)+
  annotate("text", label = "Chechnya, N=231", x = 1.7, y = 1)

ggsave("e_che.pdf",width=7,height=5)
dev.off()

## multinomial MLE models with base religion

#relevel the dependent variable to base in religion
m$id12f <- relevel(m$id12f, ref = "2")

## hypothesis variables only
m1<- multinom(id12f ~ income+income2, data = m)

## full
m3<- multinom(id12f ~ income+income2+male+age+urban+rus+distrust_strangers+
                dominant_ethnicity+violence+av.salary0000, data = m)

#relevel the dependent variable to base in ethnicity

m$id12f <- relevel(m$id12f, ref = "1")
## models with base ethnicity

## hypothesis variables only
m2<- multinom(id12f ~ income+income2, data = m)

## full
m4<- multinom(id12f ~ income+income2+male+age+urban+rus+distrust_strangers+
                dominant_ethnicity+violence+av.salary0000, data = m)

### create tables

texreg(list(m1,m2),booktabs = TRUE, dcolumn = TRUE)

texreg(list(m3,m4),booktabs = TRUE, dcolumn = TRUE)


# margina effects, religion with ethnicity as base

# convert the dependent variable into factor
m$id12f<-as.factor(m$id12)

m$id12f <- relevel(m$id12f, ref = "1")#set ethnicity as base category of the dv

# run the multinomial full regression
m4<- multinom(id12f ~ income+income2+male+age+urban+rus+distrust_strangers+
                dominant_ethnicity+violence+av.salary0000, data = m)

#create dataframe, predict and plot the marginal effects

dat <-with(m, data.frame( income=rep(seq(from = min(m$income, na.rm=T), to=max(m$income,na.rm=T),
                                         length.out=1000)),
                          income2=mean(m$income2,na.rm=T),
                          male=mean(m$male,na.rm=T),
                          age=mean(m$age,na.rm=T),
                          urban=mean(m$urban,na.rm=T),
                          rus=mean(m$rus,na.rm=T),
                          dominant_ethnicity=mean(m$dominant_ethnicity,na.rm=T),
                          distrust_strangers=mean(m$distrust_strangers,na.rm=T),
                          av.salary0000=mean(m$av.salary0000,na.rm=T),
                          violence=mean(violence,na.rm = T)))

ndat <- cbind(dat, predict(m4, newdata=dat, type = "probs", se.fit=T)) 
newdat <- melt(ndat, id.vars = c("income", "2"))
newdat2 <- newdat[1:1000,]
setnames(newdat2, "2", "fit")

datsq <-with(m, data.frame( 
  income2=rep(seq(from = min(m$income2, na.rm=T), to=max(m$income2,na.rm=T),
                  length.out=1000)),
  income=mean(m$income,na.rm=T),
  male=mean(m$male,na.rm=T),
  age=mean(m$age,na.rm=T),
  urban=mean(m$urban,na.rm=T),
  distrust_strangers=mean(m$distrust_strangers,na.rm=T),
  rus=mean(m$rus,na.rm=T),
  dominant_ethnicity=mean(m$dominant_ethnicity,na.rm=T),
  av.salary0000=mean(m$av.salary0000,na.rm=T),
  violence=mean(violence,na.rm = T)))

ndatsq <- cbind(datsq, predict(m4, newdata=datsq, type = "probs", se.fit=T)) 
ndatsq2 <- melt(ndatsq, id.vars = c("income2", "2"))
ndatsq3 <- ndatsq2[1:1000,]

setnames(ndatsq3, "2", "fit")
setnames(ndatsq3, "income2", "income")

newdat2$bins<-rep(c("one","two","three","four","five"),each=200)
ndatsq3$bins<-rep(c("one","two","three","four","five"),each=200)

rel<-rbind(newdat2,ndatsq3)

rel$inc<-rep(c("Income","Income squared"), each=1000)

rel2 <- rel %>%
  group_by(bins, inc)

rel3<-rel2 %>%
  summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), fit)

d3<-as.data.frame(rel3)
d3$mean.est<-d3$mean
d3$LL<-d3$mean.est - (1.96 * d3$se)
d3$UL<-d3$mean.est + (1.96 * d3$se)

d3$bins<-factor(d3$bins, levels = c("one","two","three","four","five"))


p <- ggplot(d3, aes(y=mean.est, x=factor(bins), group=inc, color = inc))
p + geom_pointrange(aes(ymin = LL, ymax = UL),size=0.5,stat = "identity")+
  geom_line()+
  scale_colour_grey(start = 0.3, end = 0.7)+
  xlab("Income: Low to High")+#scale_fill_grey()+
  ylab("Probability of identifying with religious networks")+
  ggtitle("")+#coord_flip()+
  scale_x_discrete(limit=c("one","two","three","four","five"),
                   labels=c("1","","","","5"))+
  guides(color=F)+
  facet_grid(~inc)+
  theme(axis.text.x = element_text(size=12,face="bold",angle=0),
        axis.title.x=element_text(size=14,face="bold"),
        axis.title.y=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size = 14),
        legend.title=element_text(size=14),
        legend.position="none")
ggsave(file="me_religion.pdf",width = 7, height = 7)
dev.off()

## marginal effects, mixed affiliation with base ethnicity

dat <-with(m, data.frame( income=rep(seq(from = min(m$income, na.rm=T), to=max(m$income,na.rm=T),
                                         length.out=1000)),
                          income2=mean(m$income2,na.rm=T),
                          male=mean(m$male,na.rm=T),
                          age=mean(m$age,na.rm=T),
                          urban=mean(m$urban,na.rm=T),
                          rus=mean(m$rus,na.rm=T),
                          dominant_ethnicity=mean(m$dominant_ethnicity,na.rm=T),
                          distrust_strangers=mean(m$distrust_strangers,na.rm=T),
                          av.salary0000=mean(m$av.salary0000,na.rm=T),
                          violence=mean(violence,na.rm = T)))

ndat <- cbind(dat, predict(m4, newdata=dat, type = "probs", se.fit=T)) 
newdat <- melt(ndat, id.vars = c("income", "3"))
newdat2 <- newdat[1:1000,]
setnames(newdat2, "3", "fit")

datsq <-with(m, data.frame( 
  income2=rep(seq(from = min(m$income2, na.rm=T), to=max(m$income2,na.rm=T),
                  length.out=1000)),
  income=mean(m$income,na.rm=T),
  male=mean(m$male,na.rm=T),
  age=mean(m$age,na.rm=T),
  urban=mean(m$urban,na.rm=T),
  distrust_strangers=mean(m$distrust_strangers,na.rm=T),
  rus=mean(m$rus,na.rm=T),
  dominant_ethnicity=mean(m$dominant_ethnicity,na.rm=T),
  av.salary0000=mean(m$av.salary0000,na.rm=T),
  violence=mean(violence,na.rm = T)))

ndatsq <- cbind(datsq, predict(m4, newdata=datsq, type = "probs", se.fit=T)) 
ndatsq2 <- melt(ndatsq, id.vars = c("income2", "3"))
ndatsq3 <- ndatsq2[1:1000,]

setnames(ndatsq3, "3", "fit")
setnames(ndatsq3, "income2", "income")

newdat2$bins<-rep(c("one","two","three","four","five"),each=200)
ndatsq3$bins<-rep(c("one","two","three","four","five"),each=200)

mixed<-rbind(newdat2,ndatsq3)

mixed$inc<-rep(c("Income","Income squared"), each=1000)

mixed2 <- mixed %>%
  group_by(bins, inc)

mixed3<-mixed2 %>%
  summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), fit)

d3<-as.data.frame(mixed3)
d3$mean.est<-d3$mean
d3$LL<-d3$mean.est - (1.96 * d3$se)
d3$UL<-d3$mean.est + (1.96 * d3$se)

d3$bins<-factor(d3$bins, levels = c("one","two","three","four","five"))


p <- ggplot(d3, aes(y=mean.est, x=factor(bins), group=inc, color = inc))
p + geom_pointrange(aes(ymin = LL, ymax = UL),size=0.5,stat = "identity")+
  geom_line()+
  scale_colour_grey(start = 0.3, end = 0.7)+
  xlab("Income: Low to High")+#scale_fill_grey()+
  ylab("Probability of identifying with mixed networks")+
  ggtitle("")+#coord_flip()+
  scale_x_discrete(limit=c("one","two","three","four","five"),
                   labels=c("1","","","","5"))+
  guides(color=F)+
  facet_grid(~inc)+
  theme(axis.text.x = element_text(size=12,face="bold",angle=0),
        axis.title.x=element_text(size=14,face="bold"),
        axis.title.y=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size = 14),
        legend.title=element_text(size=14),
        legend.position="none")
ggsave(file="me_mixed.pdf",width = 7, height = 7)
dev.off()


## marginal effect, ethnicity with religion as base

m$id12f<-as.factor(m$id12)
m$id12f <- relevel(m$id12f, ref = "2")#set religion as base category of the dv

m3<- multinom(id12f ~ income+income2+male+age+urban+rus+distrust_strangers+
                dominant_ethnicity+violence+av.salary0000, data = m)


dat <-with(m, data.frame( income=rep(seq(from = min(m$income, na.rm=T), to=max(m$income,na.rm=T),
                                         length.out=1000)),
                          income2=mean(m$income2,na.rm=T),
                          male=mean(m$male,na.rm=T),
                          age=mean(m$age,na.rm=T),
                          urban=mean(m$urban,na.rm=T),
                          rus=mean(m$rus,na.rm=T),
                          dominant_ethnicity=mean(m$dominant_ethnicity,na.rm=T),
                          distrust_strangers=mean(m$distrust_strangers,na.rm=T),
                          av.salary0000=mean(m$av.salary0000,na.rm=T),
                          violence=mean(violence,na.rm = T)))

ndat <- cbind(dat, predict(m3, newdata=dat, type = "probs", se.fit=T)) 
newdat <- melt(ndat, id.vars = c("income", "1"))
newdat2 <- newdat[1:1000,]
setnames(newdat2, "1", "fit")

datsq <-with(m, data.frame( 
  income2=rep(seq(from = min(m$income2, na.rm=T), to=max(m$income2,na.rm=T),
                  length.out=1000)),
  income=mean(m$income,na.rm=T),
  male=mean(m$male,na.rm=T),
  age=mean(m$age,na.rm=T),
  urban=mean(m$urban,na.rm=T),
  distrust_strangers=mean(m$distrust_strangers,na.rm=T),
  rus=mean(m$rus,na.rm=T),
  dominant_ethnicity=mean(m$dominant_ethnicity,na.rm=T),
  av.salary0000=mean(m$av.salary0000,na.rm=T),
  violence=mean(violence,na.rm = T)))

ndatsq <- cbind(datsq, predict(m3, newdata=datsq, type = "probs", se.fit=T)) 
ndatsq2 <- melt(ndatsq, id.vars = c("income2", "1"))
ndatsq3 <- ndatsq2[1:1000,]

setnames(ndatsq3, "1", "fit")
setnames(ndatsq3, "income2", "income")

newdat2$bins<-rep(c("one","two","three","four","five"),each=200)
ndatsq3$bins<-rep(c("one","two","three","four","five"),each=200)

eth<-rbind(newdat2,ndatsq3)

eth$inc<-rep(c("Income","Income squared"), each=1000)

eth2 <- eth %>%
  group_by(bins, inc)

eth3<-eth2 %>%
  summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), fit)

d3<-as.data.frame(eth3)
d3$mean.est<-d3$mean
d3$LL<-d3$mean.est - (1.96 * d3$se)
d3$UL<-d3$mean.est + (1.96 * d3$se)

d3$bins<-factor(d3$bins, levels = c("one","two","three","four","five"))


p <- ggplot(d3, aes(y=mean.est, x=factor(bins), group=inc, color = inc))
p + geom_pointrange(aes(ymin = LL, ymax = UL),size=0.5,stat = "identity")+
  geom_line()+
  scale_colour_grey(start = 0.3, end = 0.7)+
  xlab("Income: Low to High")+#scale_fill_grey()+
  ylab("Probability of identifying with ethnic networks")+
  ggtitle("")+#coord_flip()+
  scale_x_discrete(limit=c("one","two","three","four","five"),
                   labels=c("1","","","","5"))+
  guides(color=F)+
  facet_grid(~inc)+
  theme(axis.text.x = element_text(size=12,face="bold",angle=0),
        axis.title.x=element_text(size=14,face="bold"),
        axis.title.y=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size = 14),
        legend.title=element_text(size=14),
        legend.position="none")
ggsave(file="me_ethnic.pdf",width = 7, height = 7)
dev.off()


# Bayesian analysis of multiethnic and monoethnic republics


##normalize variables by region (except for Russian)

vars_m<-c("id12","region","income","age","rus","urban","male",
          "dominant_ethnicity","distrust_strangers","violence","av.salary0000")
m2<-m[vars_m]
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
m5<- m2 %>% mutate_at("rus", scale2, na.rm = TRUE) %>%
  group_by(`region`) %>% mutate_at(c("income","age","urban","male",
                                     "dominant_ethnicity","distrust_strangers"), scale2, na.rm = TRUE)%>% 
  na.omit(m2) %>% 
  mutate(id=1:n())

m5$income2<-m5$income^2


m_multi<-subset(m5, region==2 | region==3 | region==4 | region==6 | region==1) # multiethnic republics
m_mono<-subset(m5, region==5 | region==7) #mono ethnic republics

## Multiethnic republics analysis, base religion
post1<- MCMCmnl(factor(id12) ~ income+income2+male+age+urban+rus+
                  dominant_ethnicity+distrust_strangers, baseline="2", mcmc.method="IndMH",chain=10,
                verbose=500, mcmc=100000, thin=10, tune=0.1,
                b0 = 0, B0 = 0,
                data=m_multi)
summary(post1)

## plot results

s1<-ggs(post1)

## ethnicity
s1eth<-subset(s1, Parameter=="dominant_ethnicity.1" | Parameter=="urban.1" |
                Parameter=="income2.1" | Parameter=="rus.1" |
                Parameter=="age.1" | Parameter=="distrust_strangers.1" |
                Parameter=="male.1" | Parameter=="income.1")


g1eth<-ggs_caterpillar(s1eth,thick_ci = c(0.25, 0.75),
                       thin_ci = c(0.025, 0.975))

g1eth+labs(y="",x="")+geom_vline(xintercept= 0,linetype="dotdash",size=.5)+
  scale_y_discrete(limits=c("urban.1","age.1","male.1",
                            "distrust_strangers.1","dominant_ethnicity.1",
                            "rus.1","income2.1","income.1"),
                   labels=c("Urban","Age", "Male",
                            "Distrust strangers","Dominant ethnicity", 
                            "Russian","Income sq", 
                            "Income"))+
  theme(axis.text.y = element_text(colour="grey20",size=14))
ggsave(file="bayes_ethnicity_multi.pdf",width=7,height=7)
dev.off()

## mixed

s1mix<-subset(s1, Parameter=="dominant_ethnicity.3" | Parameter=="urban.3" |
                Parameter=="income2.3" | Parameter=="rus.3" |
                Parameter=="age.3" | Parameter=="distrust_strangers.3" |
                Parameter=="male.3" | Parameter=="income.3")

g1mix<-ggs_caterpillar(s1mix,thick_ci = c(0.25, 0.75),
                       thin_ci = c(0.025, 0.975))

g1mix+labs(y="",x="")+geom_vline(xintercept= 0,linetype="dotdash",size=.5)+
  scale_y_discrete(limits=c("urban.3","age.3","male.3",
                            "distrust_strangers.3","dominant_ethnicity.3",
                            "rus.3","income2.3","income.3"),
                   labels=c("Urban","Age", "Male",
                            "Distrust strangers","Dominant ethnicity", 
                            "Russian","Income sq", 
                            "Income"))+
  theme(axis.text.y = element_text(colour="grey20",size=14))
ggsave(file="bayes_mix_base_rel_multi.pdf",width=7,height=7)
dev.off()


## Multiethnic republics analysis, base ethnicity
post2<- MCMCmnl(factor(id12) ~ income+income2+male+age+urban+rus+
                  dominant_ethnicity+distrust_strangers, baseline="1", mcmc.method="IndMH",chain=10,
                verbose=500, mcmc=100000, thin=10, tune=0.1,
                b0 = 0, B0 = 0,
                data=m_multi)
summary(post2)

s2<-ggs(post2)

## religion
s2rel<-subset(s2, Parameter=="dominant_ethnicity.2" | Parameter=="urban.2" |
                Parameter=="income2.2" | Parameter=="rus.2" |
                Parameter=="age.2" | Parameter=="distrust_strangers.2" |
                Parameter=="male.2" | Parameter=="income.2")


g2rel<-ggs_caterpillar(s2rel,thick_ci = c(0.25, 0.75),
                       thin_ci = c(0.025, 0.975))

g2rel+labs(y="",x="")+geom_vline(xintercept= 0,linetype="dotdash",size=.5)+
  scale_y_discrete(limits=c("urban.2","age.2","male.2",
                            "distrust_strangers.2","dominant_ethnicity.2",
                            "rus.2","income2.2","income.2"),
                   labels=c("Urban","Age", "Male",
                            "Distrust strangers","Dominant ethnicity", 
                            "Russian","Income sq", 
                            "Income"))+
  theme(axis.text.y = element_text(colour="grey20",size=14))
ggsave(file="bayes_religion_multi.pdf",width=7,height=7)
dev.off()

## mixed

s2mix<-subset(s2, Parameter=="dominant_ethnicity.3" | Parameter=="urban.3" |
                Parameter=="income2.3" | Parameter=="rus.3" |
                Parameter=="age.3" | Parameter=="distrust_strangers.3" |
                Parameter=="male.3" | Parameter=="income.3")

g2mix<-ggs_caterpillar(s2mix,thick_ci = c(0.25, 0.75),
                       thin_ci = c(0.025, 0.975))

g2mix+labs(y="",x="")+geom_vline(xintercept= 0,linetype="dotdash",size=.5)+
  scale_y_discrete(limits=c("urban.3","age.3","male.3",
                            "distrust_strangers.3","dominant_ethnicity.3",
                            "rus.3","income2.3","income.3"),
                   labels=c("Urban","Age", "Male",
                            "Distrust strangers","Dominant ethnicity", 
                            "Russian","Income sq", 
                            "Income"))+
  theme(axis.text.y = element_text(colour="grey20",size=14))
ggsave(file="bayes_mix_base_eth_multi.pdf",width=7,height=7)
dev.off()



## Monoethnic republics analysis, base religion
post3<- MCMCmnl(factor(id12) ~ income+income2+male+age+urban+rus+
                  dominant_ethnicity+distrust_strangers, baseline="2", mcmc.method="IndMH",chain=10,
                verbose=500, mcmc=100000, thin=10, tune=0.1,
                b0 = 0, B0 = 0,
                data=m_mono)
summary(post3)


## plot results

s3<-ggs(post3)

## ethnicity
s3eth<-subset(s3, Parameter=="dominant_ethnicity.1" | Parameter=="urban.1" |
                Parameter=="income2.1" | Parameter=="rus.1" |
                Parameter=="age.1" | Parameter=="distrust_strangers.1" |
                Parameter=="male.1" | Parameter=="income.1")


g3eth<-ggs_caterpillar(s3eth,thick_ci = c(0.25, 0.75),
                       thin_ci = c(0.025, 0.975))

g3eth+labs(y="",x="")+geom_vline(xintercept= 0,linetype="dotdash",size=.5)+
  scale_y_discrete(limits=c("urban.1","age.1","male.1",
                            "distrust_strangers.1","dominant_ethnicity.1",
                            "rus.1","income2.1","income.1"),
                   labels=c("Urban","Age", "Male",
                            "Distrust strangers","Dominant ethnicity", 
                            "Russian","Income sq", 
                            "Income"))+
  theme(axis.text.y = element_text(colour="grey20",size=14))
ggsave(file="bayes_ethnicity_mono.pdf",width=7,height=7)
dev.off()

## mixed

s3mix<-subset(s3, Parameter=="dominant_ethnicity.3" | Parameter=="urban.3" |
                Parameter=="income2.3" | Parameter=="rus.3" |
                Parameter=="age.3" | Parameter=="distrust_strangers.3" |
                Parameter=="male.3" | Parameter=="income.3")

g3mix<-ggs_caterpillar(s3mix,thick_ci = c(0.25, 0.75),
                       thin_ci = c(0.025, 0.975))

g3mix+labs(y="",x="")+geom_vline(xintercept= 0,linetype="dotdash",size=.5)+
  scale_y_discrete(limits=c("urban.3","age.3","male.3",
                            "distrust_strangers.3","dominant_ethnicity.3",
                            "rus.3","income2.3","income.3"),
                   labels=c("Urban","Age", "Male",
                            "Distrust strangers","Dominant ethnicity", 
                            "Russian","Income sq", 
                            "Income"))+
  theme(axis.text.y = element_text(colour="grey20",size=14))
ggsave(file="bayes_mix_base_rel_mono.pdf",width=7,height=7)
dev.off()


## Monoethnic republics analysis, base ethnicity
post4<- MCMCmnl(factor(id12) ~ income+income2+male+age+urban+rus+
                  dominant_ethnicity+distrust_strangers, baseline="1", mcmc.method="IndMH",chain=10,
                verbose=500, mcmc=100000, thin=10, tune=0.1,
                b0 = 0, B0 = 0,
                data=m_mono)
summary(post4)

## plot results

s4<-ggs(post4)

## religion

s4rel<-subset(s4, Parameter=="dominant_ethnicity.2" | Parameter=="urban.2" |
                Parameter=="income2.2" | Parameter=="rus.2" |
                Parameter=="age.2" | Parameter=="distrust_strangers.2" |
                Parameter=="male.2" | Parameter=="income.2")


g4rel<-ggs_caterpillar(s4rel,thick_ci = c(0.25, 0.75),
                       thin_ci = c(0.025, 0.975))

g4rel+labs(y="",x="")+geom_vline(xintercept= 0,linetype="dotdash",size=.5)+
  scale_y_discrete(limits=c("urban.2","age.2","male.2",
                            "distrust_strangers.2","dominant_ethnicity.2",
                            "rus.2","income2.2","income.2"),
                   labels=c("Urban","Age", "Male",
                            "Distrust strangers","Dominant ethnicity", 
                            "Russian","Income sq", 
                            "Income"))+
  theme(axis.text.y = element_text(colour="grey20",size=14))
ggsave(file="bayes_religion_mono.pdf",width=7,height=7)
dev.off()

## mixed

s4mix<-subset(s4, Parameter=="dominant_ethnicity.3" | Parameter=="urban.3" |
                Parameter=="income2.3" | Parameter=="rus.3" |
                Parameter=="age.3" | Parameter=="distrust_strangers.3" |
                Parameter=="male.3" | Parameter=="income.3")

g4mix<-ggs_caterpillar(s4mix,thick_ci = c(0.25, 0.75),
                       thin_ci = c(0.025, 0.975))

g4mix+labs(y="",x="")+geom_vline(xintercept= 0,linetype="dotdash",size=.5)+
  scale_y_discrete(limits=c("urban.3","age.3","male.3",
                            "distrust_strangers.3","dominant_ethnicity.3",
                            "rus.3","income2.3","income.3"),
                   labels=c("Urban","Age", "Male",
                            "Distrust strangers","Dominant ethnicity", 
                            "Russian","Income sq", 
                            "Income"))+
  theme(axis.text.y = element_text(colour="grey20",size=14))
ggsave(file="bayes_mix_base_eth_mono.pdf",width=7,height=7)
dev.off()
