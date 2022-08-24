
rm(list=ls())
 # install.packages("dplyr")
library(dplyr)
library(gapminder)
 data(gapminder)
str(gapminder)
View(gapminder)
#Punto 1
#implementar programa.

#Exportar el conjunto de datos gapminder en formato “csv”. El 5 %
#de los valores de las columnas lifeEx, pop, y gdpPercap se debe
#reemplazar de forma aleatoria por valores no asignados NA.

gapminder1=gapminder
attach(gapminder1)
dim(gapminder1)
lifeExp1<-lifeExp
lifeExp1[ sample(1704, 0.05 * 1704) ] <- NA
pop1<-pop
pop1[sample(1704,0.05 * 1704) ] <- NA
gdpPercap1=gdpPercap
gdpPercap1[sample(1704,0.05 * 1704) ] <- NA

#crear data.frame()
tabla.n<-data.frame( country,continent, year,lifeExp1,pop1,gdpPercap1)
head(tabla.n)
tail(tabla.n)
#Crear csv
write.csv(tabla.n,"tablataller2.csv", 
          row.names = FALSE)

#1.2importar gapminder csv
#exportamos
write.csv(gapminder,"gap1.csv", 
          row.names = FALSE)
#importamos 
gap<- read.csv("gap1.csv")
View(gap)

#1.3 graficar el diagrama de dispersion lifeEpxpvspop

options(scipen=999)
plot(lifeExp,pop, main=" POBLACION VS ESPERANZA DE VIDA ", xlab="Esperanza de vida",ylab="Poblacion")

#1.4  grafica de dispersion gdpPercapvspop

plot(gdpPercap,pop, main=" POBLACION PRODUCTO VS INTERNO BRUTO ", xlab="PIB",ylab="Poblacion")  

#1.5 grafica diagramas de caja de la variable gdpPercap por continentes desde 1990-2007

data<- gap[(year>=1990 & year<=2007)
             & continent== "Americas"|
              continent== "Asia"|
             continent=="Europe"|
             continent=="Africa"|
             continent=="Oceania", ]
data<- droplevels(data)
boxplot(data$gdpPercap~data$continent)  


boxplot(gdpPercap~year,data=gapminder,main="Producto interno Bruto vs continentes",
        xlab="Años",ylab="PIB")
#Segunda forma de hacerlo

library(gapminder)
library(dplyr)
boxplot1=gapminder%>%select(gdpPercap,continent,year)%>%
  filter(year>=1990 & year<=2007)
boxplot(gdpPercap~continent,data=boxplot1, main = "Producto interno Bruto vs continentes", xlab="Continente",ylab="PIB")

#punto 2 
#implementar programa
#2.1 la diferencia en la media de los datos es estadısticamente significativa.

Exp.a <- gapminder$lifeExp[1:500]
Exp.b <- gapminder$lifeExp[501:1000]

write.csv(Exp.a,"Experimento_a.csv")
write.csv(Exp.b,"Experimento_b.csv")

experimento_a<- read.csv("Experimento_a.csv")

experimento_b<- read.csv("Experimento_b.csv")

t.test(experimento_a,experimento_b, alternative = "two.sided")

#2.2 mostrar en pantalla la correlacíon de Pearson y Spearman de los datos.

exp.a.2<- gapminder$lifeExp[1:500]
exp.b.2<- gapminder$gdpPercap[1:500]

write.csv(exp.a.2,"Experimento_a2.csv")
write.csv(exp.b.2,"Experimento_b2.csv")

experimento_a2<- read.csv("Experimento_a2.csv")

experimento_b2<- read.csv("Experimento_b2.csv")

cor(experimento_a2[,2], experimento_b2[,2], method = "pearson" )

cor(experimento_a2[,2], experimento_b2[,2], method = "spearman" )



#2.3 graficar el diagrama de dispersion y la lınea recta que 
#aproxime los datos calculada por una regresion lineal por mınimos cuadrados.

plot(experimento_b2[,2] ~ experimento_a2[,2])

regresion <- lm(experimento_b2[,2] ~ experimento_a2[,2])
regresion
plot(experimento_b2[,2]~experimento_a2[,2] , xlab='Esperanza de vida', ylab='PIB Percap')
abline(regresion, col=2)


#punto 3

#3. Implementar un programa con las siguientes opciones:
#3.1 Graficar las funciones de densidad y distribucíon de una distrib́ucion
#uniforme.

#DISTRIBUCION DE UNA UNIFORME

par(mfrow=c(1, 2))
dist<- seq(-0.5, 4.5, 0.01)

#densidad
plot(dist,dunif(dist, min= 0 , max=4 )
     ,type= "l ", main= "Funcion de distribucion Uniforme", 
     ylab= "F(X)" , lwd= 2, col= "blue")

#acumulada
plot(punif, -0.5, 4.5, type= "l", main= "Funcion de distribucion Uniforme", 
     ylab= "F(X)" , lwd= 2, col= "blue" )

#3.2 Graficar la funcion de densidad y distribucion de una distribucíon
#Bernoulli.

par(mfrow=c(1, 2))
#DENSIDAD DE VARIABLE BERNOULLI 
x<-0:1
densidad1=dbinom(x, size = 1, prob = 0.7)
plot(densidad1, type= "h", lwd = 2,
     main = "Función de Densidad variable Bernoulli",
     ylab = "probabilidad", xlab = "Número de éxitos")
#DISTRIBUCION DE LA VARIABLE BERNOULLI 
Distribucion=pbinom(x, size = 1, prob = 0.7)
plot(Distribucion, type= "h",  lwd = 2, 
     main = "Función de Distribucion variable Bernoulli",
     ylab = "Acumulada", xlab = "Número de éxitos")

#3.3 Graficar la funcíon de densidad y distribucíon de una distribucion Poisson

#Densidad de Poisson
par(mfrow=c(1, 2))
A<- 0:16
lambda<-4
den_poss<-dpois(x=A, lambda = 4)
plot(den_poss, type= "h", lwd= 5)

#distribución de Poisson
lambda<-4
dist_poss<-ppois(A, lambda = 4, lower.tail = TRUE, log.p = FALSE)
plot(dist_poss,type= "h", lwd= 5)

#3. 4 Graficar la funcion de densidad y distribucion de una distribucion Exponencial.
par(mfrow=c(1, 2))
#Densidad exponencial
T<- seq(0,10, 0.1)
den_exp<-dexp(T,rate = 4)
plot(T, den_exp, type= "l", lwd= 2,)

#distribucion Exponencial

dis_exp<- pexp(T,rate= 4)
plot(T, dis_exp,type= "l")
