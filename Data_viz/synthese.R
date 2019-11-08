library(readr)
Synthese_donnees_des_semis <- read_delim("C:/Users/User/Desktop/Projets_Malick_fayad-master/Projets_Malick_fayad-master/Data_viz/Synthese_donnees_des_semis.csv", 
                                               "\t", escape_double = FALSE, col_types = cols(`15_j` = col_number(), 
                                                                                                        `16_j` = col_number(), `17_j` = col_number(), 
                                                                                                       `18_j` = col_number(), `19_j` = col_number(), 
                                                                                                        `20_j` = col_number(), `21_j` = col_number(), 
                                                                                                        `5_degres_C_TMG_h` = col_number(), 
                                                                                                        `5_degres_C_TMG_j` = col_number(), 
                                                                                                        Aire_sous_la_courbe = col_number(), 
                                                                                                        Bancs = col_character(), camera = col_character(), 
                                                                                                        zone = col_character()), trim_ws = TRUE)

synth <- DropNA(Synthese_donnees_des_semis)
attach(synth)

library(DataCombine)
synth2=synth[c(8:14)]

acp = PCA(synth2,scale.unit = FALSE, quali.sup=c(4),graph=TRUE)
acp$eig
barplot(acp$eig[,1])
fviz_eig(acp, addlabels = TRUE, ylim = c(0, 60))

#a=synth$`15_j`

a=acp$ind$coord
A=a[,1] #1ere dimension
#bartlett.test(A~Bancs) #nooooooooooooooo
#shapiro.test(lm(A~Bancs)$residuals) #nooooooooooooo
#bartlett.test(A~camera)
#shapiro.test(lm(A~camera)$residuals) #noooooooooooo
#bartlett.test(A~zone)
#shapiro.test(lm(A~zone)$residuals) #nooooooooooooo

kk=cbind(synth2, A)

#anova(lm(A~camera))
#anova(lm(A~Bancs))
#anova(lm(A~zone))

#syn.bonf=pairwise.t.test(A,camera);syn.bonf
#syn.bonf=pairwise.t.test(A,Bancs);syn.bonf
#syn.bonf=pairwise.t.test(A,zone);syn.bonf


#t1=aov(A~camera)
#syn.HSD=TukeyHSD(t1,'camera');syn.HSD
#plot(syn.HSD)

#t2=aov(A~Bancs)
#syn.HSD=TukeyHSD(t2,'Bancs');syn.HSD
#plot(syn.HSD)

#t3=aov(A~zone)
#syn.HSD=TukeyHSD(t3,'zone');syn.HSD
#plot(syn.HSD)

# (faux de le dire) Ni bancs, ni camera, ni zone a un effet sur le germination des graines (pr tous les individus des differents pops)

acp$eig
acp$var$coord
acp$var$contrib
acp$var$cor
acp$var$cos2
acp$ind$coord
acp$ind$cos2
acp$ind$contrib


h1=synth[which(Pop=='830'),]
h2=synth[which(Pop=='803'),]
h3=synth[which(Pop=='812'),]
h4=synth[which(Pop=='Témoin'),]
h5=synth[which(Pop=='802'),]
h6=synth[which(Pop=='815'),]

g1=synth2[which(Pop=='830'),]
g2=synth2[which(Pop=='803'),]
g3=synth2[which(Pop=='812'),]
g4=synth2[which(Pop=='Témoin'),]
g5=synth2[which(Pop=='802'),]
g6=synth2[which(Pop=='815'),]

acp = PCA(g1,scale.unit = FALSE, graph=TRUE)
barplot(acp$eig[,1])
fviz_eig(acp, addlabels = TRUE, ylim = c(0, 60))

a=acp$ind$coord
A=a[,1] #1ere dimension
bartlett.test(A~h1$Bancs)
shapiro.test(lm(A~h1$Bancs)$residuals)
bartlett.test(A~h1$camera)
shapiro.test(lm(A~h1$camera)$residuals)
bartlett.test(A~h1$zone)
shapiro.test(lm(A~h1$zone)$residuals)

acp = PCA(g2,scale.unit = FALSE, graph=TRUE)
a=acp$ind$coord
A=a[,1] #1ere dimension
#bartlett.test(A~h2$Bancs)
#shapiro.test(lm(A~h2$Bancs)$residuals) #noooooooooo
#bartlett.test(A~h2$camera)
#shapiro.test(lm(A~h2$camera)$residuals) #noooooooooo
bartlett.test(A~h2$zone)
shapiro.test(lm(A~h2$zone)$residuals) 

acp = PCA(g3,scale.unit = FALSE, graph=TRUE)
a=acp$ind$coord
A=a[,1] #1ere dimension
bartlett.test(A~h3$Bancs)
shapiro.test(lm(A~h3$Bancs)$residuals)
#bartlett.test(A~h3$camera) #nooooooooooooooooooo
#shapiro.test(lm(A~h3$camera)$residuals)
bartlett.test(A~h3$zone)
shapiro.test(lm(A~h3$zone)$residuals)

acp = PCA(g4,scale.unit = FALSE, graph=TRUE)
a=acp$ind$coord
A=a[,1] #1ere dimension
bartlett.test(A~h4$Bancs)
shapiro.test(lm(A~h4$Bancs)$residuals)
bartlett.test(A~h4$camera)
shapiro.test(lm(A~h4$camera)$residuals)
bartlett.test(A~h4$zone)
shapiro.test(lm(A~h4$zone)$residuals)

acp = PCA(g5,scale.unit = FALSE, graph=TRUE)
a=acp$ind$coord
A=a[,1] #1ere dimension
#bartlett.test(A~h5$Bancs)
#shapiro.test(lm(A~h5$Bancs)$residuals) #noooooooooo
bartlett.test(A~h5$camera)
shapiro.test(lm(A~h5$camera)$residuals)
bartlett.test(A~h5$zone)
shapiro.test(lm(A~h5$zone)$residuals)

acp = PCA(g6,scale.unit = FALSE, graph=TRUE)
a=acp$ind$coord
A=a[,1] #1ere dimension
#bartlett.test(A~h6$Bancs) #nooooooooo
#shapiro.test(lm(A~h6$Bancs)$residuals) #noooooooooo
#bartlett.test(A~h6$camera) #noooooooooo
#shapiro.test(lm(A~h6$camera)$residuals) #nooooooooooo
#bartlett.test(A~h6$zone)
#shapiro.test(lm(A~h6$zone)$residuals) #noooooooooooo

typeof(h1$Bancs) #character
length(h1$Bancs) #176

acp = PCA(g1,scale.unit = FALSE, graph=TRUE)  
a=acp$ind$coord
A=a[,1] #1ere dimension

anova(lm(A~h1$camera))
anova(lm(A~h1$Bancs))
anova(lm(A~h1$zone))

#syn.bonf=pairwise.t.test(A,h1$camera);syn.bonf
#syn.bonf=pairwise.t.test(A,h1$Bancs);syn.bonf
#syn.bonf=pairwise.t.test(A,h1$zone);syn.bonf


t1=aov(A~h1$camera)
syn.HSD=TukeyHSD(t1,'h1$camera');syn.HSD
plot(syn.HSD)

t2=aov(A~h1$Bancs) #yessssssssssssss entre 3 et 4
syn.HSD=TukeyHSD(t2,'h1$Bancs');syn.HSD
plot(syn.HSD)

boxplot(A~h1$Bancs) #entre 3 et 4

t3=aov(A~h1$zone)
syn.HSD=TukeyHSD(t3,'h1$zone');syn.HSD
plot(syn.HSD)


acp = PCA(g2,scale.unit = FALSE, graph=TRUE)

a=acp$ind$coord
A=a[,1] #1ere dimension

#anova(lm(A~h2$camera))
#anova(lm(A~h2$Bancs))
anova(lm(A~h2$zone))

#syn.bonf=pairwise.t.test(A,h1$camera);syn.bonf
#syn.bonf=pairwise.t.test(A,h1$Bancs);syn.bonf
#syn.bonf=pairwise.t.test(A,h1$zone);syn.bonf


#t1=aov(A~h2$camera)
#syn.HSD=TukeyHSD(t1,'h2$camera');syn.HSD
#plot(syn.HSD)

#t2=aov(A~h2$Bancs)
#syn.HSD=TukeyHSD(t2,'h2$Bancs');syn.HSD
#plot(syn.HSD)

t3=aov(A~h2$zone)
syn.HSD=TukeyHSD(t3,'h2$zone');syn.HSD
plot(syn.HSD)


acp = PCA(g3,scale.unit = FALSE, graph=TRUE)

a=acp$ind$coord
A=a[,1] #1ere dimension

#anova(lm(A~h3$camera))
anova(lm(A~h3$Bancs))
anova(lm(A~h3$zone))

#syn.bonf=pairwise.t.test(A,h1$camera);syn.bonf
#syn.bonf=pairwise.t.test(A,h1$Bancs);syn.bonf
#syn.bonf=pairwise.t.test(A,h1$zone);syn.bonf


#t1=aov(A~h3$camera)
#syn.HSD=TukeyHSD(t1,'h3$camera');syn.HSD
#plot(syn.HSD)

t2=aov(A~h3$Bancs)
syn.HSD=TukeyHSD(t2,'h3$Bancs');syn.HSD
plot(syn.HSD)

t3=aov(A~h3$zone)
syn.HSD=TukeyHSD(t3,'h3$zone');syn.HSD
plot(syn.HSD)


acp = PCA(g4,scale.unit = FALSE, graph=TRUE)

a=acp$ind$coord
A=a[,1] #1ere dimension

anova(lm(A~h4$camera))
anova(lm(A~h4$Bancs))
anova(lm(A~h4$zone))

#syn.bonf=pairwise.t.test(A,h1$camera);syn.bonf
#syn.bonf=pairwise.t.test(A,h1$Bancs);syn.bonf
#syn.bonf=pairwise.t.test(A,h1$zone);syn.bonf


t1=aov(A~h4$camera)
syn.HSD=TukeyHSD(t1,'h4$camera');syn.HSD
plot(syn.HSD)

t2=aov(A~h4$Bancs)
syn.HSD=TukeyHSD(t2,'h4$Bancs');syn.HSD
plot(syn.HSD)

t3=aov(A~h4$zone)
syn.HSD=TukeyHSD(t3,'h4$zone');syn.HSD
plot(syn.HSD)

acp = PCA(g5,scale.unit = FALSE, graph=TRUE)

a=acp$ind$coord
A=a[,1] #1ere dimension

anova(lm(A~h5$camera))
#anova(lm(A~h5$Bancs))
anova(lm(A~h5$zone))

#syn.bonf=pairwise.t.test(A,h1$camera);syn.bonf
#syn.bonf=pairwise.t.test(A,h1$Bancs);syn.bonf
#syn.bonf=pairwise.t.test(A,h1$zone);syn.bonf


t1=aov(A~h5$camera) #yesssssssssssss entre 2 et 3
syn.HSD=TukeyHSD(t1,'h5$camera');syn.HSD
plot(syn.HSD)

#t2=aov(A~h5$Bancs)
#syn.HSD=TukeyHSD(t2,'h5$Bancs');syn.HSD
#plot(syn.HSD)

t3=aov(A~h5$zone)
syn.HSD=TukeyHSD(t3,'h5$zone');syn.HSD
plot(syn.HSD)


boxplot(A~h5$camera) #entre 2 et 3


" acp = PCA(g6,scale.unit = FALSE, graph=TRUE)

a=acp$ind$coord
A=a[,1] #1ere dimension

anova(lm(A~h6$camera))
anova(lm(A~h6$Bancs))
anova(lm(A~h6$zone))

#syn.bonf=pairwise.t.test(A,h1$camera);syn.bonf
#syn.bonf=pairwise.t.test(A,h1$Bancs);syn.bonf
#syn.bonf=pairwise.t.test(A,h1$zone);syn.bonf


t1=aov(A~h6$camera) #yessssssssssss
syn.HSD=TukeyHSD(t1,'h6$camera');syn.HSD
plot(syn.HSD)

t2=aov(A~h6$Bancs) #yesssssssssss
syn.HSD=TukeyHSD(t2,'h6$Bancs');syn.HSD
plot(syn.HSD)

t3=aov(A~h6$zone)
syn.HSD=TukeyHSD(t3,'h6$zone');syn.HSD
plot(syn.HSD) "

#Bancs a un effet sur la germination slt pour la pop 830 mais elle n'a pas un effet sur la germ pour les pop 812 et Temoin.
#camera a un effet sur la germination slt pour la pop 802 mais elle n'a pas un effet sur la germ pour les pop 830 et Temoin.
#zone n'a aucun effet sur la germination pour les pop 830, 803, 802, 812 et Temoin.
