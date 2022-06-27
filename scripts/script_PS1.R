## PSet 1

#Punto 2
setwd("C:/Users/USER/OneDrive - Universidad de los Andes/Escritorio/BigData/PS1")
rm(list=ls())


require(pacman)
p_load(tidyverse, rvest)

# Scrappear 10 htmls
for (i in 1:10)
{
  assign(paste("datos_ignacio_",i, sep=""), read_html(paste(
    "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html", sep="")))
}

# Ponerlos en formato de tabla
tabla_1 <- datos_ignacio_1 %>%
  html_node(xpath = '/html/body/table') %>%
  html_table()

tabla_2 <- datos_ignacio_2 %>%
  html_node(xpath = '/html/body/table') %>%
  html_table()

tabla_3 <- datos_ignacio_3 %>%
  html_node(xpath = '/html/body/table') %>%
  html_table()

tabla_4 <- datos_ignacio_4 %>%
  html_node(xpath = '/html/body/table') %>%
  html_table()

tabla_5 <- datos_ignacio_5 %>%
  html_node(xpath = '/html/body/table') %>%
  html_table()

tabla_6 <- datos_ignacio_6 %>%
  html_node(xpath = '/html/body/table') %>%
  html_table()

tabla_7 <- datos_ignacio_7 %>%
  html_node(xpath = '/html/body/table') %>%
  html_table()

tabla_8 <- datos_ignacio_8 %>%
  html_node(xpath = '/html/body/table') %>%
  html_table()

tabla_9 <- datos_ignacio_9 %>%
  html_node(xpath = '/html/body/table') %>%
  html_table()

tabla_10 <- datos_ignacio_10 %>%
  html_node(xpath = '/html/body/table') %>%
  html_table()

# Unir tablas
tabla_final <- rbind(tabla_1, tabla_2)
tabla_final <- rbind(tabla_final, tabla_3)
tabla_final <- rbind(tabla_final, tabla_4)
tabla_final <- rbind(tabla_final, tabla_5)
tabla_final <- rbind(tabla_final, tabla_6)
tabla_final <- rbind(tabla_final, tabla_7)
tabla_final <- rbind(tabla_final, tabla_8)
tabla_final <- rbind(tabla_final, tabla_9)
tabla_final <- rbind(tabla_final, tabla_10)

## Exportar 
write.csv(tabla_final ,"tabla_final_ps1.csv")


install.packages("pacman")
install.packages("tidyverse")
install.packages("ggplot")
set.seed(000)

setwd("~/2022-2/BigData")
datos <- read.csv(file = 'tabla_final_ps1.csv')
head(datos)
ls(datos)
df <- datos[datos$ocu == 1,]
df <- df[df$age >= 18,]
df <- df[df$dominio == "BOGOTA",]

#Punto 3

#b
age2 <- df$age^2
ols<- lm(formula = ingtotob ~ age + age2,data=df)

#c
library(stargazer)
stargazer(ols,type = "text",title = "Fitted Model OLS", out = "ols.doc")

#d

library(ggplot2)

pred_ols <- predict(ols)

ggplot(data=df, mapping = aes(x = age, y = pred_ols)) +
geom_point(col = "cadetblue", size = 0.5) +
labs(x= "Edad", y = "Ingreso total observado")+
  theme_minimal()

#e bootstrap
## Bootstrap
sample_coef_intercept <- NULL
sample_coef_x1 <- NULL
sample_erstd_x1 <- NULL
sample_coef_x2 <- NULL
sample_erstd_x2 <- NULL


for (i in 1:1000) {
  sample_d = tabla_final[sample(1:nrow(tabla_final), 0.3*nrow(tabla_final), replace = TRUE), ]
  
  model_bootstrap <- lm(ingtot ~ age + age2, data = sample_d)
  
  sample_coef_intercept <-
    c(sample_coef_intercept, model_bootstrap$coefficients[1])
  
  sample_coef_x1 <-
    c(sample_coef_x1, model_bootstrap$coefficients[2])
  
  sample_erstd_x1 <-
    c(sample_erstd_x1, coef(summary(model_bootstrap))[2, 2])
  
  sample_coef_x2 <-
    c(sample_coef_x2, model_bootstrap$coefficients[3])
  
  sample_erstd_x2 <-
    c(sample_erstd_x2, coef(summary(model_bootstrap))[3, 2])
}

coefs <- rbind(sample_coef_intercept, sample_coef_x1, sample_erstd_x1, 
               sample_coef_x2, sample_erstd_x2)

## Combining the results in a table
means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1), 
               mean(sample_coef_x2))
erstd.boot = c(0,mean(sample_erstd_x1),mean(sample_erstd_x2))
knitr::kable(round(
  cbind(
    sample = coef(summary(reg1))[, c(1,2)],
    bootstrap = means.boot,
    erstdBoots = erstd.boot),4), 
  "simple", caption = "Coefficients in different models")


# confidence interval; 
confint(reg1)

a <-
  cbind(
    quantile(sample_coef_intercept, prob = 0.025),
    quantile(sample_coef_intercept, prob = 0.975))
b <-
  cbind(quantile(sample_coef_x1, prob = 0.025),
        quantile(sample_coef_x1, prob = 0.975))

c <-
  cbind(quantile(sample_coef_x2, prob = 0.025),
        quantile(sample_coef_x2, prob = 0.975))

d <-
  round(cbind(
    sample = confint(reg1),
    boot = rbind(a, b, c)), 4)

colnames(d) <- c("2.5 %", "97.5 %",
                 "2.5 %", "97.5 %")

#Punto 4

install.packages("magrittr")
library(magrittr)

install.packages("dplyr")
library(dplyr)

#a
## 1) Remplazar ingtotob = ingtot cuando ingtotob = 0
tabla_final <- df %>% 
  mutate(ingtotob = case_when(
    ingtotob == 0 ~ ingtot,
    TRUE ~ ingtotob
  ))

## 2) Remplazar los que definitivamente quedan en 0 por NAs

tabla_final$ingtotob[tabla_final$ingtotob == 0] <- NA

## 3) Calcular el log

tabla_final$log_wage <- log(tabla_final$ingtotob)

ols_gender <- lm(formula = log_wage ~ sex, data = tabla_final)
library(stargazer)
stargazer(ols_gender,type = "text",title = "Gender Model OLS", out = "gender.doc")

#b
## 4) Quedarnos solo con las variables que vamos a usar y borrar las filas que tengan missings (perdemos como 200)

## Renaming features
tabla_final <- tabla_final %>% 
  select(, -iof2es)  %>%
  rename(r_jefe_hogar = p6050) %>%
  rename(antiguedad_industria = p6426) %>%
  rename(ingreso_pensiones = iof2)

tabla_final <- tabla_final %>% select(log_wage,sex,age,estrato1,maxEducLevel,
                                      hoursWorkUsual,formal,cuentaPropia, 
                                      oficio,sizeFirm,antiguedad_industria)
tabla_final <- tabla_final %>% drop_na()

## 5) Correr la reg de edad
tabla_final$age2 <- tabla_final$age*tabla_final$age
reg_gap1 <- lm(log_wage ~ age + age2, tabla_final)
pred_ols <- predict(reg_gap1)

## 6) graficar por género
ggplot(data=tabla_final, mapping = aes(x = age, y = pred_ols)) +
  geom_point(col = "cadetblue", size = 0.5) +
  labs(x= "Edad", y = "Ingreso total observado")+
  theme_minimal() + facet_wrap(~sex)
