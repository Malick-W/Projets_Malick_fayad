library(readr)
Synthese_donnees_des_semis <- read_delim("Synthese_donnees_des_semis.csv", 
                                         "\t", escape_double = FALSE, col_types = cols(`15_j` = col_number(), 
                                                                                       `16_j` = col_number(), `17_j` = col_number(), 
                                                                                       `18_j` = col_number(), `19_j` = col_number(), 
                                                                                       `20_j` = col_number(), `21_j` = col_number(), 
                                                                                       `5_degres_C_TMG_h` = col_number(), 
                                                                                       `5_degres_C_TMG_j` = col_number(), 
                                                                                       Aire_sous_la_courbe = col_number(), 
                                                                                       Bancs = col_character(), camera = col_character(), 
                                                                                       zone = col_character()), trim_ws = TRUE)

library(factoextra)
library(FactoMineR)
library(DataCombine)
synth <- DropNA(Synthese_donnees_des_semis)
attach(synth)

synth2=synth[c(8:14)]

acp = PCA(synth2,scale.unit = FALSE)
acp$eig
barplot(acp$eig[,1])
fviz_eig(acp, addlabels = TRUE, ylim = c(0, 60))

#a=synth$`17_j`

a=acp$ind$coord
A=a[,1] 

kk=cbind(synth2, A)


h1=synth[which(Pop=='830'),]
h3=synth[which(Pop=='803'),]
h6=synth[which(Pop=='815'),]

g1=kk[which(Pop=='830'),][,8]
g3=kk[which(Pop=='803'),][,8]
g6=kk[which(Pop=='815'),][,8]

#############################################3

anova(lm(g1~h1$Bancs))
anova(lm(g1~h1$camera))
anova(lm(g1~h1$zone))

t1=aov(g1~h1$Bancs)
syn.HSD=TukeyHSD(t1,'h1$Bancs');syn.HSD
plot(syn.HSD)

t2=aov(g1~h1$camera) #oui 
syn.HSD=TukeyHSD(t2,'h1$camera');syn.HSD
plot(syn.HSD)


t3=aov(g1~h1$zone)
syn.HSD=TukeyHSD(t3,'h1$zone');syn.HSD
plot(syn.HSD)

#####################################################

anova(lm(g3~h3$Bancs))
anova(lm(g3~h3$camera))
anova(lm(g3~h3$zone))

t1=aov(g3~h3$Bancs)
syn.HSD=TukeyHSD(t1,'h3$Bancs');syn.HSD
plot(syn.HSD)

t2=aov(g3~h3$camera)
syn.HSD=TukeyHSD(t2,'h3$camera');syn.HSD
plot(syn.HSD)

t3=aov(g3~h3$zone)
syn.HSD=TukeyHSD(t3,'h3$zone');syn.HSD
plot(syn.HSD)

#####################################################


anova(lm(g6~h6$Bancs))
anova(lm(g6~h6$camera))
anova(lm(g6~h6$zone))

t1=aov(g6~h6$Bancs) #oui
syn.HSD=TukeyHSD(t1,'h6$Bancs');syn.HSD
plot(syn.HSD)

t2=aov(g6~h6$camera) #oui
syn.HSD=TukeyHSD(t2,'h6$camera');syn.HSD
plot(syn.HSD)

t3=aov(g6~h6$zone)
syn.HSD=TukeyHSD(t3,'h6$zone');syn.HSD
plot(syn.HSD) 

#Bancs a un effet sur la germination pour la pop 830 et 815 mais elle n'a pas un effet sur la germ pour la pop 803.
#camera a un effet sur la germination pr la pop 815 mais elle n'a pas un effet sur la germ pour les pop 830 et 803.
#zone n'a aucun effet sur la germination pour ces 3 pop.

