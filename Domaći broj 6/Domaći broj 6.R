#Primer1

library(readxl)
library(moments)
library(ggplot2)
library(faraway)
library(dplyr)
library(ez)
library(car)
library(multcomp)
library(reshape)
library(nortest)
library(nlme)

Pr_1 <- read_excel("C:/Users/Korisnik/Downloads/Primer1.xlsx")
Pr_1<- cbind(Pr_1,ispitanik=c(1:nrow(Pr_1)))
View(Pr_1)

terapija_1<- melt(Pr_1,id="ispitanik",measure.vars = c('Paracetamol','Ibuprofen','Diklofenak','Ketoprofen'))
colnames(terapija_1)<-c('Ispitanik','Terapija','Ocena')
View(terapija_1)

#Proveravamo Homogenost i normalnost podataka
shapiro.test(terapija_1$Ocena)
leveneTest(Ocena~Terapija,data = terapija_1)
# Testovi nam pokzuju da su podaci normalno raspore�eni i homogeni

#Grafi�ki prikaz povezanosti:
par(mar=c(5,4,4,2))
boxplot(terapija_1$Ocena~terapija_1$Terapija,main="Pore�enje ocena ispitanika na osnovu
        primenjene terapije",
        xlab = "Terapija",ylab = "Ocene",col.main="red",col="green")

ggplot(terapija_1, aes(Ocena, Ispitanik, colour = Terapija)) + 
  geom_point()+ 
  ggtitle("Uticaj terapije i ispitanika na ocene") +
  xlab("Ocena")+
  ylab("Ispitanik")+
  theme_grey()
qqnorm(terapija_1$Ocena,col='Red',pch=16)
  

#Radimo ezANOVU:
options(scipen = 0)
Pon_av<-ezANOVA(data = terapija_1,dv=Ocena,wid = Ispitanik,within = Terapija,detailed = T,type = 3)
Pon_av
# Na osnovu rezultata zaklju�ujemo dapostoji zna�ajna razlika jer je p=0.02557030 �to je manje od praga zna�ajnosti alpha=0.05,
# Na osnovu Mauchly test proveravamo sferi�nostpa posto je p=0.046<0.05 potrebna je korekcija,
#Po�to pri Sphercity kolekciji gledamo vrednost HFe=0.66 i po�to je manja od 0.75 koristimo vrednost p[GG] korekcije.
# Rezultat GG korekcije nam daje p vrednost=0.0626 �to je ve�e od 0.05 pa mo�emo re�i da smo sipunili zahtev sferi�nosti.

# Po�to nam je ezANOVA pokazala da postoji razlika ali ne izme�u �ega radimo TUKEY HSD  test:
lme_terapija_1<-lme(Ocena~Terapija,random = ~1|Ispitanik,data = terapija_1,method = 'ML')
Tukey_terapija_1<-glht(lme_terapija_1,linfct=mcp(Terapija='Tukey'))
summary(Tukey_terapija_1)
#postoji zna�ajna razlika izme�u Ibuprofena i Paracetamola, kao i izme�u Diklofenka i Paracetamola.

#Grafi�ki prikaz Tukey HSD testa:
par(mar=c(6,11,5,3))
plot(Tukey_terapija_1, xlab='Razlika u srednjim vrednostima ocene dejstva terapije',col='Blue')
# I na grafi�kom prikazu mo�emo uo�iti da zna�ajna razlika postoji izmedju parova koje je TukeyHSD test pokazao.


#primer 2

pr_2<-read_excel('C:/Users/Korisnik/Downloads/Primer2.xlsx',1)
pr_2<-cbind(pr_2)
colnames(pr_2)<-c('Ispitanik','Prole�e','Leto','Jesen','Zima')
glavobolja_1 <-melt(pr_2,id="Ispitanik", measure.vars =c("Prole�e","Leto","Jesen","Zima"))
colnames(glavobolja_1)<-c('Ispitanik','Godi�nje_doba','Broj_glavobolja')
View(glavobolja_1)

#Proveravamo Homogenost i normalnost podataka
shapiro.test(glavobolja_1$Broj_glavobolja)
lillie.test(glavobolja_1$Broj_glavobolja)
leveneTest(Broj_glavobolja~Godi�nje_doba,data = glavobolja_1)
qqnorm(glavobolja_1$Broj_glavobolja,col='Blue',pch=20)
# Testovi nam pokzuju da su podaci homogeni ali da ne postoji normalna raspodela

#Transformacija(1/x)
original<-glavobolja_1$Broj_glavobolja
T_glavobolja_1<-1/glavobolja_1$Broj_glavobolja

#Homogensot i normalnost
shapiro.test(T_glavobolja_1)
leveneTest(T_glavobolja_1~Godi�nje_doba,data=glavobolja_1)
# Transformacija 1/x je pogor�ala situaciju, proba�emo neku drugu transformaciju.

#Transformacija(sqrt(x))
T_glavobolja_2<-sqrt(original)

#Homogenost i normalnost:
shapiro.test(T_glavobolja_2)
lillie.test(T_glavobolja_2)
leveneTest(T_glavobolja_2~Godi�nje_doba,data=glavobolja_1)
qqnorm(T_glavobolja_2,col='green',pch=2)
# Ova transformacija nam je unormalila rezultate  u dovoljnoj meri jer je p=0.052 �to jos uvek ve�e od 0.05


#Grafi�ki prikaz povezanosti:
par(mar=c(5,4,4,2))
boxplot(glavobolja_1$Broj_glavobolja~glavobolja_1$Godi�nje_doba,main="Pore�enje broja glavobolja ispitanika
        na osnovu godi�njih doba",
        xlab = "Godi�nja doba",ylab = "Broj glavobolja",col.main="green",col="light blue")

ggplot(glavobolja_1, aes(Broj_glavobolja, Godi�nje_doba, colour = Ispitanik)) + 
  geom_point()+ 
  ggtitle("Uticaj godi�njih doba i ispitanika na broj glavobolja") +
  xlab("Broj glavobolja")+
  ylab("Godi�nje doba")+
  theme_light()

# Radimo ezANOVA test kao bi videli da li postoji zna�ajna razlika izme�u podataka.
options(scipen = 999)
Output<-ezANOVA(data=glavobolja_1,dv=Broj_glavobolja,wid=Ispitanik,within=Godi�nje_doba,detailed=T,type=2)
Output
# Na osnovu rezultata mo�emo zaklju�iti da postoji zna�ajna razlika jer je p=0.00009 �to je manje od praga zna�ajnosti alpha=0.05.
# Koristimo Mauchly test za odre�ivanje sferi�nosti koji nam pokatuje da nam je potrebna korekcija(p<0.05)
#Po�to pri Sphercity kolekciji gledamo vrednost HFe=0.687 i po�to je manja od 0.75 koristimo vrednost p[GG] korekcije.
# Rezultat GG korekcije nam daje p vrednost=0.001 �to je manje od 0.05 pa mo�emo potvrditi da nismo uspeli da spre�imo gresku tipa I
# GG = Greenhouse-Geisser

ezPlot(data =glavobolja_1 , dv = Broj_glavobolja , wid = Ispitanik , within = Godi�nje_doba , type = 3 ,
       x =Godi�nje_doba, x_lab ="Godi�nje doba" , y_lab = "Glavobolja",)

# Po�to nam je ezANOVA pokazala da postoji razlika ali ne izme�u �ega radimo TUKEY HSD test:
lme_glavobolja_1<-lme(Broj_glavobolja~Godi�nje_doba,random=~1|Ispitanik,data=glavobolja_1,method='ML')
Tukey_glavobolja_1<-glht(lme_glavobolja_1,linfct=mcp(Godi�nje_doba='Tukey'))
summary(Tukey_glavobolja_1)
# Zna�ajna razlika ne postoji jedino izme�u leta i prole�a(time2-time1), kao i zime i jeseni(time4-time3)

#Grafi�ki prikaz rezultata Tukey HSD testa:
par(mar=c(5,8,7,2))
plot(Tukey_glavobolja_1,xlab='Razlika u srednjim vrednostima broja glavobolja 
     u odnosu na godi�nje doba',
     col='Red')
# Na grafi�kom prikazu tako�e mo�emo videti da razlika jedino nije zna�ajna u periodima Zima-jesen i Leto-prole�e.


