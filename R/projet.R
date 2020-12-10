##base de donnees loyer lyon 2018
file.show("Base_OP_2018_L6900/Base_OP_2018_L6900.csv")
obs<- read.csv2("~/Documents/R/ProjetDMStats/Base_OP_2018_L6900/Base_OP_2018_L6900.csv",na = "NA")
obs<=na.omit(obs)
#View(Base_OP_2018_L6900)
str(obs)#on regarde le type de chacunes des variables
row.names(obs)
attach(obs)
loymoym2=na.omit(loyer_moyen)#on enleve les valeurs manquantes
summary(loymoym2)
echLoymoym2=table(loymoym2)
barplot(echLoymoym2,xlab="Loyer moyen par m2", ylab="nombre d'appart",main="Moyenne des loyers de la ville de Lyon par m2",col="darkblue")
lines(density(loymoym2), col="green", lwd=2)#densite estime empiriquement
x=loymoym2
qqnorm(x,col="green",pch=3,lwd=0.7)
qqline(x,col="red",lwd=2)
plot(density(loymoym2), col="green", lwd=2)#densite estime empiriquement

