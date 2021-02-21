# Napraviti linearni model 
# Proceniti parametre
# selektovati odabrane promenljive koje imaju uticaja na zavisnu promenljivu
# predvidjanje da bude za samo jedan model


# Linearna regresija

library(DAAG)
library(ggplot2)
library(faraway)
library(caTools)
library(GLMsData)
library(MASS)


# 1. zadatak
data <- humanpower1
View(data)

# Kreiranje modela
model_fly <- lm(formula = wattsPerKg ~ o2, data= data)

# Grafièki prikaz - Scatter plot
plot(data$o2, data$wattsPerKg,col='red',xlab='unos kiseonika',ylab = 'Snaga',
     main = 'zavisnot snage od unosa kiseonika')
abline(model_fly)
# Na Grafièkom prikazu možemo uoèiti da postoji jaka linearna veza izmeðu zavisno i nezavisno promenljive.

# Pregled statisticke znacajnosti koeficijenata modela; Uvid u koeficijent determinacije
summary(model_fly)
# na osnovu ove analize možemo zakljuèiti da na osnovu naše nezavisne promenljive možemo predvideti 93% zavisno promenljive.

# plotovanje dijagnostickih plotova na istoj strani
par(mfrow=c(2,2)) # Menjam prostor na 2 x 2 slike
plot(model_fly)
#Na osnovu grafièkog prikaza možemo reæi da je veza linearna, podaci imaju normalnu raspodelu,
#homoscedastiènosti i da nema prediktorskih varijabli koje utièu na model

par(mfrow = c(1,1)) # vraæanje na jednu sliku

# vizualizacija modela - pomocu paketa ggplot
ggplot(data, aes(x=o2, y=wattsPerKg))+
  geom_point()+
  geom_smooth(method = "lm", se = F)
# ggplot nam takdoje pokatuje linearnu zavisnost izmedju nezaviste i zavisno promenljive

# primena modela na novim podacima
y_pred <- predict(model_fly, data.frame(o2 = 35))
y_pred
# Pomoæu našeg modela smo odredili koja bi bila vrednosti nove zavisno promenljive koje nismo imali u podacima.

boxplot(humanpower1$o2, xlab = "Oxygen intake [ml/min/kg]",col='light blue')
range(humanpower1$o2)
# na osnovu box plota kakva je raspodela naših podataka i da ne postoje outlajeri.

## podela za trening i test podatke.
set.seed(221)
split <-  sample.split(humanpower1$o2, SplitRatio = 2/3)
training_set  <-  subset(humanpower1, split == T)
test_set  <-  subset(humanpower1, split == F)

model <- lm(formula = wattsPerKg ~ o2, data = training_set)
summary(model)

#predikcija test podataka na osnovu trening grupe
y_pred <- predict(model_fly, newdata = test_set)
y_pred
# Na osnovu trening podataka i našeg modela došli smo do predpostavljenih rezultata test

#poredjenje predvidjenog sa pravim setom
head(test_set)

ggplot()+
  geom_point(aes(x = training_set$o2, y = training_set$wattsPerKg),
             colour = "red") +
  geom_line(aes(x = training_set$o2, y = predict(model_fly, newdata = training_set)),
            colour = "black")+
  geom_point(aes(x = test_set$o2, y = test_set$wattsPerKg), 
             color = "green")+
  geom_line(aes(x = test_set$o2, y = predict(model_fly, newdata = test_set)),
           colour = "Blue")+
  ggtitle("Snaga u zavisnosti od unosa kiseonika")+
  xlab("Unos kiseonika")+
  ylab("Proizvedena snaga")+
  theme(plot.title = element_text(hjust = 0.5))


par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(model)#nije baš linearan
#############################

# 2.zadatak
data <- data(lungcap, package = 'GLMsData')
data <- lungcap
head(data)

# Tackasti(Scatter) plot - zavisnost dve promenljive od interesa
plot(lungcap$Ht, lungcap$FEV)

# Kreiranje modela
model <- lm(formula = FEV ~ Ht, data = lungcap)

# Pregled statisticke znacajnosti koeficijenata modela
# Uvid u koeficijent determinacije
summary(model)

# Dijagnosticki plotovi
par(mfrow=c(2,2))
plot(model)

# primena modela na novim podacima
y_pred <- predict(model, data.frame(Ht = 50))
y_pred


# plotovanje dijagnostickih plotova na istoj strani
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(model)
par(mfrow = c(1,1))

######################
# 3. zadatak
data <- data(diabetes, package = "faraway")
data <- diabetes
delayedAssign("data", diabetes)

View(diabetes)

#Podela podataka na 2 seta:
set.seed(12)
podela <- sample.split(diabetes$stab.glu, SplitRatio = 0.8)
train <- subset(diabetes, podela == T)
test <- subset(diabetes, podela == F)


model_diabetes <- lm(stab.glu ~ chol+hdl + glyhb + age + height + weight  + bp.1s, data = diabetes)
summary(model_diabetes) 
plot(model_diabetes)

# novi model
model_diabetes <- lm(stab.glu ~ glyhb, data = train)
summary(model_diabetes)

# predvidjanje modela
y_pred_train <- predict(model_diabetes, newdata = train)

ggplot()+
  geom_point(aes(x = train$glyhb, y = train$stab.glu), colour = "red")+
  geom_line(aes(x = train$glyhb, y = y_pred_train))
# ove promenljive bas ne objasnjavaju nista - svescu model na samo dve promenljive

############################################
# 4.zadatak - airquality
data <- airquality
head(data)

# Kreiranje modela
model_air <- lm(formula = Ozone ~ Temp + Solar.R + Wind, data = data)

# Pregled statisticke znacajnosti koeficijenata modela
# Uvid u koeficijent determinacije
summary(model_air)
model_air$coefficients

par(mfrow = c(2,2)) 
plot(model_air)  

# primena modela na novim podacima
predict(model_air, newdata = data.frame(Temp = 65, Solar.R = 299, Wind = 8.6))
predict(model_air, newdata = data.frame(Temp = 73, Solar.R = 320, Wind = 16.6))

#############################################
# 5. zadatak

head(Pima.tr2)

# Kreiranje modela
pima_lm <- lm(formula = bmi ~ glu + bp + skin + age, data = Pima.tr2)

# Pregled statisticke znacajnosti koeficijenata modela; Uvid u koeficijent determinacije
summary(pima_lm)

# Dijagnosticki plotovi
plot(pima_lm)

# nov model
pima_lm <- lm(formula = bmi ~ skin , data = Pima.tr2[-157,])

# Pregled statisticke znacajnosti koeficijenata modela
# Uvid u koeficijent determinacije
summary(pima_lm)

# Plotovanje modela
par(mfrow=c(2,2))
plot(pima_lm)

ggplot(Pima.tr2, aes(x = skin, y = bmi)) +
  geom_point()+
  geom_smooth(method = "lm", col = "red", se = T) 


# primena modela na novim podacima
y_pred <- predict(pima_lm, newdata = data.frame(skin = 32))
y_pred



