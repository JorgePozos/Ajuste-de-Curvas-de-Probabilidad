# -------------- Ajuste de probabilidad de reclamos de responsabilidad civil-----------------
### Paqueterías
library(MASS) #distr de proba
library(actuar)#distr adicionales de proba
library(fitdistrplus) #ajuste de curvas de proba
library(goftest) #probas de bondad de ajuste
library(readxl)
library(ggplot2)

### Importamos los datos
library(readxl)
datos <- read_excel("C:/Users/PC/Desktop/FCFM actuar?a/9? Semestre Oto?o 21/Modelos de P?rdida (JLRG)/1. Variables aleatorias/Proyecto 2 SESA's/datosR.xlsx")

#Ajustamos las unidades de los datos (mdp)
datos$Montos2020 <- datos$Montos2020/1000000 

### Estad?stica descriptiva
#Resumen estad?stico
summary(datos$Montos2020)

# Histograma
ggplot(datos, aes(x = Montos2020, 
                  y = ..density..)) + #..density.. hace que la altura de las barras sea el procentaje
  geom_histogram(binwidth = 10, bins=1000 ,fill="#69b3a2", color="#e9ecef", alpha = 0.9)+ #Binwith le cambia el ancho de las barras
  labs(title = 'Histograma de Montos actualizados al 2020',
       x = 'Monto',
       y = 'Densidad',
       caption = 'Monto de reclamos de responsabilidad civil en mdp')

#Histograma con densidad
ggplot(datos, aes(x = Montos2020, 
                  y = ..density..)) + 
  geom_histogram(binwidth = 10, bins =1000, fill="#69b3a2", color="#e9ecef", alpha = 0.9) +
  geom_density(color = 'steelblue4', lwd = 1)  +
  labs(title = 'Histograma de Montos actualizados al 2020',
       x = 'Monto',
       y = 'Densidad',
       caption = 'Monto de reclamos de responsabilidad civil en mdp')

#Boxplot
ggplot(datos, aes(y = Montos2020)) +
  geom_boxplot(color = 'steelblue4', fill = '#69b3a2')+
  labs(title = 'Diagrama de caja de Montos actualizados al 2020',
       y = 'Monto',
       caption = 'Monto de reclamos de responsabilidad civil en mdp')

#ecdf
ggplot(datos, aes(Montos2020)) +
  stat_ecdf(geom = "step", color = 'steelblue4', lwd = 1.1 )+ 
  labs(title = 'Func. de distribuci?n acumulativa de Montos actualizados al 2015',
       x = 'Monto',
       y = 'ECDF',
       caption = 'Monto de reclamos de responsabilidad civil en mdp')

### Ajuste de curvas de probabilidad

library(rriskDistributions)
x <- datos$Montos2020
res <- fit.cont(x)

# No acepta ninguna distribuci?n (no s? por qu? no funciona bien),
# sin embargo, consideramos las siguientes

###Generamos modelos
#Weibull
(mod1 <- fitdist(datos$Montos2020, 'weibull', method = 'mle'))
summary(mod1) 
#LogNormal
(mod2 <- fitdist(datos$Montos2020, 'lnorm', method = 'mle'))
summary(mod2) 
#Gamma
(mod3 <- fitdist(datos$Montos2020, 'gamma', method = 'mle'))
summary(mod3) 
#Cauchy
(mod4 <- fitdist(datos$Montos2020, 'cauchy', method = 'mle'))
summary(mod4) 
#Pareto
(mod5 <- fitdist(datos$Montos2020, 'pareto', method = 'mle'))
summary(mod5) 
#LogLogistic
(mod6 <- fitdist(datos$Montos2020, 'llogis', method = 'mle'))
summary(mod6) 

#Comparamos los modelos gr?ficamente
#Como se enciman mucho las gr?ficas, comparamos de 3 en 3 los modelos

leyenda <- c('Weibull', 'LogNormal', 'Gamma')
par(mfrow=c(2,2))
denscomp(list(mod1, mod2, mod3), legendtext = leyenda) #comparaci?n funci?n de densidad
cdfcomp(list(mod1, mod2, mod3), legendtext = leyenda) #comparaci?n funci?n de distribuci?n
qqcomp(list(mod1, mod2, mod3), legendtext = leyenda) #cuantiles teo vs emp?ricos
ppcomp(list(mod1, mod2, mod3), legendtext = leyenda) #Probas teo vs emp?ricas
#el QQ-Plot indica que la mejor es la lognormal y el PP-Plot pudiese descartar la gamma

leyenda <- c('Cauchy', 'Pareto', 'LogLogistic')
par(mfrow=c(2,2))
denscomp(list(mod4, mod5, mod6), legendtext = leyenda) #comparaci?n funci?n de densidad
cdfcomp(list(mod4, mod5, mod6), legendtext = leyenda) #comparaci?n funci?n de distribuci?n
qqcomp(list(mod4, mod5, mod6), legendtext = leyenda) #cuantiles teo vs emp?ricos
ppcomp(list(mod4, mod5, mod6), legendtext = leyenda) #Probas teo vs emp?ricas
# el QQ-Plot descarta la pareto y el PP-Plot la cauchy, parece que la mejor es la
#loglogistic

### Pruebas de hip?tesis
#K-S test
(hip1_ks <- ks.test(datos$Montos2020,'pweibull', mod1$estimate[1],mod1$estimate[2]))
(hip2_ks <- ks.test(datos$Montos2020,'plnorm', mod2$estimate[1],mod2$estimate[2]))
(hip3_ks <- ks.test(datos$Montos2020,'pgamma', mod3$estimate[1],mod3$estimate[2]))
(hip4_ks <- ks.test(datos$Montos2020,'pcauchy', mod4$estimate[1],mod4$estimate[2]))
(hip5_ks <- ks.test(datos$Montos2020,'ppareto', mod5$estimate[1],mod5$estimate[2]))
(hip6_ks <- ks.test(datos$Montos2020,'pllogis', mod6$estimate[1],mod6$estimate[2]))

#AD test
(hip1_ad <- ad.test(datos$Montos2020,'pweibull', mod1$estimate[1],mod1$estimate[2]))
(hip2_ad <- ad.test(datos$Montos2020,'plnorm', mod2$estimate[1],mod2$estimate[2]))
(hip3_ad <- ad.test(datos$Montos2020,'pgamma', mod3$estimate[1],mod3$estimate[2]))
(hip4_ad <- ad.test(datos$Montos2020,'pcauchy', mod4$estimate[1],mod4$estimate[2]))
(hip5_ad <- ad.test(datos$Montos2020,'ppareto', mod5$estimate[1],mod5$estimate[2]))
(hip6_ad <- ad.test(datos$Montos2020,'pllogis', mod6$estimate[1],mod6$estimate[2]))

#wilcoxon test
(hip1_w <- wilcox.test(x = datos$Montos2020, 
                       y = rweibull(464,mod1$estimate[1],mod1$estimate[2]), 
                       alternative = "two.sided", mu = 0,
                       paired = FALSE))
(hip2_w <- wilcox.test(x = datos$Montos2020, 
                       y = rlnorm(464,mod2$estimate[1],mod2$estimate[2]), 
                       alternative = "two.sided", mu = 0,
                       paired = FALSE))
(hip3_w <- wilcox.test(x = datos$Montos2020, 
                       y = rgamma(464,mod3$estimate[1],mod3$estimate[2]), 
                       alternative = "two.sided", mu = 0,
                       paired = FALSE))
(hip4_w <- wilcox.test(x = datos$Montos2020, 
                       y = rcauchy(464,mod4$estimate[1],mod4$estimate[2]), 
                       alternative = "two.sided", mu = 0,
                       paired = FALSE))
(hip5_w <- wilcox.test(x = datos$Montos2020, 
                       y = rpareto(464,mod5$estimate[1],mod5$estimate[2]), 
                       alternative = "two.sided", mu = 0,
                       paired = FALSE))
(hip6_w <- wilcox.test(x = datos$Montos2020, 
                       y = rllogis(464,mod6$estimate[1],mod6$estimate[2]), 
                       alternative = "two.sided", mu = 0,
                       paired = FALSE))

#Cramer Von Mises
(hip1_cv <- cvm.test(datos$Montos2020,'pweibull', mod1$estimate[1],mod1$estimate[2]))
(hip2_cv <- cvm.test(datos$Montos2020,'plnorm', mod2$estimate[1],mod2$estimate[2]))
(hip3_cv <- cvm.test(datos$Montos2020,'pgamma', mod3$estimate[1],mod3$estimate[2]))
(hip4_cv <- cvm.test(datos$Montos2020,'pcauchy', mod4$estimate[1],mod4$estimate[2]))
(hip5_cv <- cvm.test(datos$Montos2020,'ppareto', mod5$estimate[1],mod5$estimate[2]))
(hip6_cv <- cvm.test(datos$Montos2020,'pllogis', mod6$estimate[1],mod6$estimate[2]))

#Resumen de pruebas no param?tricas (sin p-values)
gofstat(list(mod1,mod2,mod3, mod4, mod5, mod6),
        fitnames = c('Weibull', ' Lognormal', 'Gamma','Cauchy', 'Pareto', 'Loglogistic'))

### Conclusi?n
# Todas las distribuciones parecen ajustar muy bien
# pero solo la loglogistic, la gamma y la cauchy pasan la prueba de Wilcoxon.
# El menor AIC lo tiene la loglogistica seguida de la pareto
# y el menor BIC lo tiene la loglogistica seguida de la pareto

# Por lo tanto, concluimos que la distribuci?n que mejor se ajusta es 
# la Loglog?stica con par?metro de forma 0.77424504 y de escala 0.04137091

# Datos vs Distribuci?n ajustada
ggplot(datos, aes(x=Montos2020)) +
  geom_histogram(aes(y = ..density..), , binwidth = 10, bins=1000 ,fill="#69b3a2", color="#e9ecef")+
  stat_function(fun=dllogis, args = list(mod6$estimate[1],mod6$estimate[2]), color = 'steelblue4', lwd=.5)+ #encima la densidad
  labs(title = 'Ajuste de curva de probabilidad',
       x = 'Monto',
       y = 'Densidad',
       caption = 'Monto de reclamos de responsabilidad civil en mdp')

