---
title: "Ajuste de probabilidad de reclamos de responsabilidad civil"
author: "Jorge Uriel Barragan Pozos"
date: "9/17/2021"
output:
  beamer_presentation:
    theme: Madrid
    fonttheme: structurebold
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

## Se Cargan Paqueterías

```{r cars, echo=TRUE, message=FALSE}
### Paqueterías
library(MASS) #distr de proba
library(actuar)#distr adicionales de proba
library(fitdistrplus) #ajuste de curvas de proba
library(goftest) #probas de bondad de ajuste
library(readxl)#importar de excel
library(ggplot2)#gráficos 
library(dplyr)#manipulación de df
```

## Se Cargan los Datos

```{r echo=TRUE}
### Importamos Montos ya ajustados a Inflación
library(readxl)
datos <- read_excel("datosR.xlsx")
#Ajustamos las unidades de los datos (mdp)
datos_uni <- datos %>% 
  mutate(Montos_mdp = Montos2020/1000000)
#Escalamos los Datos 
datos_soft <- datos_uni%>%
  mutate(Monto_escala = log10((Montos_mdp) * 10^7))

```

## Se Obtienen los Datos

```{r echo=TRUE}
datos_soft
```

## Histograma

```{r echo=TRUE, message=FALSE, out.width=0}
# Histograma
ggplot(datos_soft, aes(x = Monto_escala , 
                  y = ..density..)) + 
  geom_histogram(fill="#69b3a2", 
      color="#e9ecef", alpha = 0.9) + 
  labs(title = 'Histograma de Montos actualizados al 2020',
       x = 'Monto',
       y = 'Densidad',
       caption = 'Monto de reclamos de 
       responsabilidad civil en mdp')
est_des <- summary(datos_soft$Monto_escala)

```

## Histograma

```{r echo=FALSE, message=FALSE, out.width=260}
# Histograma
ggplot(datos_soft, aes(x = Monto_escala , 
                  y = ..density..)) + 
  geom_histogram(fill="#69b3a2", 
      color="#e9ecef", alpha = 0.9) +
  labs(title = 'Histograma de Montos actualizados al 2020',
       x = 'Monto',
       y = 'Densidad',
       caption = 'Monto de reclamos de 
       responsabilidad civil en mdp') +
  geom_density(alpha = 0.05 ,fill = "green") + 
  geom_vline(aes(xintercept=  est_des[4] ),#Media
            color="blue", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=  est_des[2] ),#Primer Cuartil
            color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=  est_des[3] ), #Mediana
            color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=  est_des[5] ),#Tercer Cuartil
            color="red", linetype="dashed", size=1) 
```

```{r}
### Estad?stica descriptiva
#Resumen estadístico
summary(datos_soft$Monto_escala)

```

```{r message=FALSE, include=FALSE, out.width=250}
ggplot(datos_soft, aes(x = Monto_escala , 
                  y = ..density..)) + 
  geom_histogram(fill="#69b3a2", 
      color="#e9ecef", alpha = 0.9) +
  geom_density(color = 'steelblue4', lwd = 1)  +
  labs(title = 'Histograma de Montos actualizados al 2020',
       x = 'Monto',
       y = 'Densidad',
       caption = 'Monto de reclamos de responsabilidad civil en mdp')
```

## Boxplot

```{r out.width=300}
ggplot(datos_soft, aes(y = Monto_escala)) +
  geom_boxplot(color = 'steelblue4', fill = '#69b3a2')+
  labs(title = 'Diagrama de caja de Montos actualizados al 2020',
       y = 'Monto',
       caption = 'Monto de reclamos de responsabilidad civil en mdp')

```

## ecdf

```{r out.width=300}
ggplot(datos_soft, aes(Monto_escala)) +
  stat_ecdf(geom = "step", color = 'steelblue4', lwd = 1.1 )+ 
  labs(title = 'Func. de distribución acumulativa de Montos actualizados al 2015',
       x = 'Monto',
       y = 'ECDF',
       caption = 'Monto de reclamos de responsabilidad civil en mdp')

```

## Generamos Modelos

```{r modelos, echo=TRUE, warning=FALSE, results='hide'}
mod1 <- fitdist(datos_soft$Monto_escala, "gamma", 
                method = c("mle"))
mod2 <- fitdist(datos_soft$Monto_escala, "weibull", 
                method = "mle")
mod3 <- fitdist(datos_soft$Monto_escala, "pareto", 
                method = "mle",
                start = list(shape = 1, scale =300))
mod4 <- fitdist(datos_soft$Monto_escala, "lnorm", 
                method = "mle")
mod5 <- fitdist(datos_soft$Monto_escala, "norm", 
                method = "mle")
mod6 <- fitdist(datos_soft$Monto_escala, "llogis", 
                method = "mle")
mod7 <- fitdist(datos_soft$Monto_escala, "invgamma", 
                method = "mle")
mod8 <- fitdist(datos_soft$Monto_escala, "invweibull", 
                method = "mle")
```

# Comparamos los Modelos Gráficamente

```{r}
#Como se enciman mucho las gr?ficas, comparamos de 3 en 3 los modelos
leyenda <- c("Gamma", "Weibull", "Pareto", "Lognormal","Normal","loglogistica","Gamma Inversa","Weibull Inversa")
```

## Función de Densidad

```{r}
denscomp(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8), legendtext = leyenda)
```

## Función de Distribución

```{r}
cdfcomp(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8), legendtext = leyenda)  
```

## Cuantiles Teóricos vs Empiricos

```{r}
qqcomp(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8), legendtext = leyenda, xlim = c(0,20))  
```

## Probabilidades Teóricas vs Empiricas

```{r}
ppcomp(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8), legendtext = leyenda)  
```

## Pruebas de hipótesis
```{r echo=TRUE,results='hide', warning=FALSE}
#K-S test
(hip1_ks <- ks.test(datos$Montos2020,
            'pweibull',mod1$estimate[1],mod1$estimate[2]))
(hip2_ks <- ks.test(datos$Montos2020,
            'plnorm', mod2$estimate[1],mod2$estimate[2]))
(hip3_ks <- ks.test(datos$Montos2020,
            'pgamma', mod3$estimate[1],mod3$estimate[2]))
(hip4_ks <- ks.test(datos$Montos2020,
            'pcauchy', mod4$estimate[1],mod4$estimate[2]))
(hip5_ks <- ks.test(datos$Montos2020,
            'ppareto', mod5$estimate[1],mod5$estimate[2]))
(hip6_ks <- ks.test(datos$Montos2020,
            'pllogis', mod6$estimate[1],mod6$estimate[2]))
```

## AD test
```{r echo=TRUE,results='hide', warning=FALSE}
(hip1_ad <- ad.test(datos$Montos2020,
            'pweibull', mod1$estimate[1],mod1$estimate[2]))
(hip2_ad <- ad.test(datos$Montos2020,
            'plnorm', mod2$estimate[1],mod2$estimate[2]))
(hip3_ad <- ad.test(datos$Montos2020,
            'pgamma', mod3$estimate[1],mod3$estimate[2]))
(hip4_ad <- ad.test(datos$Montos2020,
            'pcauchy', mod4$estimate[1],mod4$estimate[2]))
(hip5_ad <- ad.test(datos$Montos2020,
            'ppareto', mod5$estimate[1],mod5$estimate[2]))
(hip6_ad <- ad.test(datos$Montos2020,
            'pllogis', mod6$estimate[1],mod6$estimate[2]))
```

## Wilcoxon test
```{r echo=TRUE,results='hide', warning=FALSE}
(hip1_w <- wilcox.test(x = datos$Montos2020, 
        y = rweibull(464,mod1$estimate[1],mod1$estimate[2]), 
        alternative = "two.sided", mu = 0,paired = FALSE))
(hip2_w <- wilcox.test(x = datos$Montos2020, 
        y = rlnorm(464,mod2$estimate[1],mod2$estimate[2]), 
        alternative = "two.sided", mu = 0,paired = FALSE))
(hip3_w <- wilcox.test(x = datos$Montos2020, 
        y = rgamma(464,mod3$estimate[1],mod3$estimate[2]), 
        alternative = "two.sided", mu = 0,paired = FALSE))
(hip4_w <- wilcox.test(x = datos$Montos2020, 
        y = rcauchy(464,mod4$estimate[1],mod4$estimate[2]), 
        alternative = "two.sided", mu = 0,paired = FALSE))
(hip5_w <- wilcox.test(x = datos$Montos2020, 
        y = rpareto(464,mod5$estimate[1],mod5$estimate[2]), 
        alternative = "two.sided", mu = 0,paired = FALSE))
(hip6_w <- wilcox.test(x = datos$Montos2020, 
        y = rllogis(464,mod6$estimate[1],mod6$estimate[2]), 
        alternative = "two.sided", mu = 0,paired = FALSE))

```

## Cramer Von Mises
```{r echo=TRUE,results='hide', warning=FALSE}
(hip1_cv <- cvm.test(datos$Montos2020,
            'pweibull', mod1$estimate[1],mod1$estimate[2]))
(hip2_cv <- cvm.test(datos$Montos2020,
            'plnorm', mod2$estimate[1],mod2$estimate[2]))
(hip3_cv <- cvm.test(datos$Montos2020,
            'pgamma', mod3$estimate[1],mod3$estimate[2]))
(hip4_cv <- cvm.test(datos$Montos2020,
            'pcauchy', mod4$estimate[1],mod4$estimate[2]))
(hip5_cv <- cvm.test(datos$Montos2020,
            'ppareto', mod5$estimate[1],mod5$estimate[2]))
(hip6_cv <- cvm.test(datos$Montos2020,
            'pllogis', mod6$estimate[1],mod6$estimate[2]))
```




## Pruebas de Bondad de Ajuste

```{r echo=FALSE, paged.print=TRUE, results='hide'}
gofstat(list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8))

```

![](Captura%20de%20Pantalla%202021-09-18%20a%20la(s)%2021.24.43.png)

## Conclusión

El menor AIC lo tiene la loglogistica seguida de la pareto
y el menor BIC lo tiene la loglogistica seguida de la pareto
Por lo tanto, concluimos que la distribuci?n que mejor se ajusta es 
la Loglogística con parámetro de forma 7.833596 y de escala 10.709578


```{r out.width=250}

par(mfrow = c(2,2))
denscomp(mod6) # Función de Densidad
cdfcomp(mod6) #Función de Distribución 
qqcomp(mod6) #cuantiles Teóricos vs Empiricos 
ppcomp(mod6) #Probabilidades Teóricas vs Empiricas 

```
