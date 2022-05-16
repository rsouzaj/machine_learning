setwd("C:/carlos/UERJ/ML MEDICINE/reg lin")

library(readr)

df_cnes_aula <- read_csv("df_cnes_aula.csv")

library(tidyverse)
library(ggplot2)
library(caret)


names(df_cnes_aula)
dim(df_cnes_aula)

pesorn <-  df_cnes_aula  %>% dplyr::select("IDADEMAE","Masculino","Nenhuma","PESO")

str(pesorn) #variáveis numéricas estão OK, mas precisamos transformar as variáveis Masculino e Nenhuma em sim ou não (fator)


summary(pesorn) 
dim(pesorn)

#Transformando variaveis de exposicao categoricas
pesorn$Masculino<-factor(pesorn$Masculino,levels=c(1,0), labels =c("Sim","Não")) #Bebe do sexo masculino
pesorn$Nenhuma<-factor(pesorn$Nenhuma,levels=c(1,0), labels =c("Sim","Não")) #Nenhuma consulta pre-natal

#Boxplots
g1<-ggplot(pesorn, aes(Masculino, PESO)) + 
  geom_boxplot() 
plot(g1)

g2<-ggplot(pesorn, aes(Nenhuma, PESO)) + 
  geom_boxplot() 
plot(g2)

#Dispersao
g3<-ggplot(pesorn, aes(IDADEMAE, PESO)) + 
  geom_point()
plot(g3)

pesorn$IDADEMAE<-ifelse(pesorn$IDADEMAE==99,NA,pesorn$IDADEMAE)


set.seed(789)
particionamento <- createDataPartition(pesorn$PESO, # cria as partições de forma estratificada
                                       p = 0.80,
                                       list = FALSE) #O parâmetro list = FALSE evita que o resultado seja armazenado em formato de lista:

pesorn_train <- pesorn[particionamento, ]
pesorn_test <- pesorn[-particionamento, ] 

#preenchendos dados faltantes pela média nos dados de treinamento. Nessa base, dado faltante, NA=99
summary(pesorn_train)

pesorn_train$IDADEMAE[is.na(pesorn_train$IDADEMAE)]<-mean(pesorn_train$IDADEMAE,na.rm = TRUE)

any(is.na(pesorn_train))

#teste para detectar outlier
library(EnvStats)
test <- rosnerTest(pesorn_train$IDADEMAE,
                   k = 3)
test$all.stats

hist(pesorn_train$IDADEMAE)

test1 <- rosnerTest(pesorn_train$PESO,
                    k = 10)
test1$all.stats

hist(pesorn_train$PESO)

#Padronização variaveis numericas
pesorn_train$IDADEMAE<- scale(pesorn_train$IDADEMAE, center = T)
pesorn_train$PESO<- scale(pesorn_train$PESO, center = T)

cor.test(pesorn_train$IDADEMAE,pesorn_train$PESO)

#Associação entre features categóricas
chisq.test(pesorn_train$Masculino,pesorn_train$Nenhuma) #Não há associação (p>0,05), poderiamos usar as duas variáveis no modelo

#seleção de variáveis

t.test(pesorn_train$PESO~pesorn_train$Masculino)
t.test(pesorn_train$PESO~pesorn_train$Nenhuma) # Existe diferença entre as médias de Peso ao nascer tanto para a variável Masculino quanto para a variável nenhuma

#Ajustando os modelos de Regressão Linear simples e múltipla
modelRLS <- lm(PESO ~ IDADEMAE, data = pesorn_train)
summary(modelRLS)

modelRLM1 <- lm(PESO ~ IDADEMAE + Masculino, data = pesorn_train)
summary(modelRLM1)

modelRLM2 <- lm(PESO ~ IDADEMAE + Masculino + Nenhuma, data = pesorn_train)
summary(modelRLM2)

#Comparando os modelos - Modelo RLM3 é o que apresenta menor AIC, logo pode ser considerado o melhor
AIC(modelRLS)
AIC(modelRLM1)
AIC(modelRLM2)

#Diagnóstico de ajuste do modelo - Análise de resíduos

par(mfrow = c(2, 2))
plot(modelRLM2)

#normalidade residuos
library(MASS)
sresid <- studres(modelRLM2) 
par(mfrow = c(1, 1))
hist(sresid)

library(nortest)
ad.test(sresid)$p.value #Performs the Anderson-Darling test for the composite hypothesis of normality, see e.g. Thode (2002, Sec. 5.1.4)

#outro pacote de análise de resíduos
library(ggfortify)
autoplot(modelRLM2)
par(mfrow = c(1, 1))
plot(modelRLM2, 5)
plot(modelRLM2, 4)

#resultados em tabelas
library(broom)
modelRLM2.diag.metrics<- augment(modelRLM2)
modelRLM2.diag.metrics %>%
  top_n(3, wt = .cooksd)

######################################################################
#Pré-processamento dados de teste

pesorn_test$IDADEMAE[is.na(pesorn_test$IDADEMAE)]<-mean(pesorn_test$IDADEMAE,na.rm = TRUE)

any(is.na(pesorn_test))

#Padronização variaveis numericas
pesorn_test$IDADEMAE<- scale(pesorn_test$IDADEMAE, center = T)
pesorn_test$PESO<- scale(pesorn_test$PESO, center = T)

##Predizer os dados de teste
preditos<- predict.lm(modelRLM2,newdata=pesorn_test)

pesorn_test$preditos<-preditos

# Evaluate model
mse <- mean((pesorn_test$PESO - pesorn_test$preditos)^2)
rmse <- sqrt(mse)
print(mse)
print(rmse)

#feito na aula - predição usando o modelo de regressão linear simples (RLS)
preditos2<- predict.lm(modelRLS,newdata=pesorn_test)
pesorn_test$preditos2<-preditos2
mse2 <- mean((pesorn_test$PESO - pesorn_test$preditos2)^2)
rmse2 <- sqrt(mse2)
print(mse2)
print(rmse2)

####################################################### EXERCÍCIO...
##Verificar se um novo modelo excluindo pesos menores que 500 g tem melhor ajuste (outliers, peso<=500?)
pesorn_500<- pesorn %>% filter(PESO>=500)
