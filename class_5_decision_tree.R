setwd("C:/carlos/UERJ/ML MEDICINE/Aula 2004")

library(readr)

df_cnes_aula <- read_csv("df_cnes_aula.csv")

library(tidyverse)
library(ggplot2)
library(caret)
library(dplyr)


names(df_cnes_aula)
dim(df_cnes_aula)

colnames(df_cnes_aula)<-make.names(colnames(df_cnes_aula))

pesorn<-df_cnes_aula

str(pesorn) #variáveis numéricas estão OK, mas precisamos transformar as variáveis Masculino e Nenhuma em sim ou não (fator)

glimpse(pesorn) #ver os dados

names(pesorn)
summary(pesorn) 
dim(pesorn)

#Transformando variaveis de exposicao categoricas
pesorn$Masculino<-factor(pesorn$Masculino,levels=c(1,0), labels =c("Sim","Não")) #Bebe do sexo masculino
pesorn$Nenhuma<-factor(pesorn$Nenhuma,levels=c(1,0), labels =c("Sim","Não")) #Nenhuma consulta pre-natal
pesorn$de.1.a.3<-factor(pesorn$de.1.a.3,levels=c(1,0), labels =c("Sim","Não"))
pesorn$de.4.a.6<-factor(pesorn$de.4.a.6,levels=c(1,0), labels =c("Sim","Não"))
pesorn$Branca<-factor(pesorn$Branca,levels=c(1,0), labels =c("Sim","Não"))
pesorn$Preta<-factor(pesorn$Preta,levels=c(1,0), labels =c("Sim","Não"))
pesorn$Parda<-factor(pesorn$Parda,levels=c(1,0), labels =c("Sim","Não"))
pesorn$hospital<-factor(pesorn$hospital,levels=c(1,0), labels =c("Sim","Não"))
pesorn$outro_estab<-factor(pesorn$outro_estab,levels=c(1,0), labels =c("Sim","Não"))
pesorn$Vaginal<-factor(pesorn$Vaginal,levels=c(1,0), labels =c("Sim","Não"))



#Boxplots
g1<-ggplot(pesorn, aes(Masculino, PESO)) + 
  geom_boxplot() 
plot(g1)

g2<-ggplot(pesorn, aes(Nenhuma, PESO)) + 
  geom_boxplot() 
plot(g2)

g4<-ggplot(pesorn, aes(Preta, PESO)) + 
  geom_boxplot() 
plot(g4)

#Dispersao
g3<-ggplot(pesorn, aes(IDADEMAE, PESO)) + 
  geom_point()
plot(g3)

#nessa base de dados 99 é dado faltante, se for 99 alterar para NA
pesorn$IDADEMAE<-ifelse(pesorn$IDADEMAE==99,NA,pesorn$IDADEMAE)
pesorn<- pesorn%>% filter(IDADEMAE!=99)


#Particionando treino e teste
set.seed(789)
particionamento <- createDataPartition(pesorn$IDADEMAE, # cria as partições de forma estratificada
                                       p = 0.80,
                                       list = FALSE) #O parâmetro list = FALSE evita que o resultado seja armazenado em formato de lista:

pesorn_train <- pesorn[particionamento, ]
pesorn_test <- pesorn[-particionamento, ] 



#PRÉ-PROCESSAMENTO PARA REGRESSÃO LINEAR
#preenchendos dados faltantes pela média nos dados de treinamento.
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
pesorn_train$PESO<- scale(pesorn_train$PESO, center = T)


#Ajustando os modelos de Regressão Linear simples e múltipla
modelRLS <- lm(IDADEMAE ~ PESO, data = pesorn_train)
summary(modelRLS)

modelRLM1 <- lm(IDADEMAE~PESO + Masculino, data = pesorn_train)
summary(modelRLM1)

modelRLM2 <- lm(IDADEMAE ~ PESO+ Nenhuma, data = pesorn_train)
summary(modelRLM2)

modelRLMG <- lm(IDADEMAE ~ ., data = pesorn_train)
summary(modelRLMG)

modelRLMG1 <- lm(IDADEMAE ~ .-outros-Indígena-Preta-Amarela-Branca-menos.de.22.semanas, data = pesorn_train)
summary(modelRLMG1)


#Comparando os modelos - Modelo RLM3 é o que apresenta menor AIC, logo pode ser considerado o melhor
AIC(modelRLS)
AIC(modelRLM1)
AIC(modelRLM2)
AIC(modelRLMG1)

#Diagnóstico de ajuste do modelo - Análise de resíduos

par(mfrow = c(2, 2))

plot(modelRLMG1)

#normalidade residuos
library(MASS)
sresid <- studres(modelRLMG1) 
par(mfrow = c(1, 1))
hist(sresid)

library(nortest)
ad.test(sresid)$p.value #Performs the Anderson-Darling test for the composite hypothesis of normality, see e.g. Thode (2002, Sec. 5.1.4)

#outro pacote de análise de resíduos
library(ggfortify)
autoplot(modelRLMG1)
par(mfrow = c(1, 1))
plot(modelRLMG1, 5)
plot(modelRLMG1, 4)

#resultados em tabelas
library(broom)
modelRLMG1.diag.metrics<- augment(modelRLM2)
modelRLMG1.diag.metrics %>%
  top_n(3, wt = .cooksd)

######################################################################
#Pré-processamento dados de teste

pesorn_test$IDADEMAE[is.na(pesorn_test$IDADEMAE)]<-mean(pesorn_test$IDADEMAE,na.rm = TRUE)

any(is.na(pesorn_test))

#Padronização variaveis numericas
pesorn_test$PESO<- scale(pesorn_test$PESO, center = T)


##Predizer os dados de teste

preditos<- predict.lm(modelRLMG1,newdata=pesorn_test)

pesorn_test$preditos<-preditos

# Evaluate model
mse <- mean((pesorn_test$IDADEMAE - pesorn_test$preditos)^2)
rmse <- sqrt(mse)
print(mse)
print(rmse)


# CONSTRUÇÃO DOS MODELOS (ÁRVORE DE DECISÃO)
#--------------------------------------------------------

library(tree) #Carregando a biblioteca tree

ArvoreDecisao_modelo01 = tree(IDADEMAE~., data=pesorn_train, method='anova') 

summary(ArvoreDecisao_modelo01)
plot(ArvoreDecisao_modelo01)
text(ArvoreDecisao_modelo01, pretty=0)
ArvoreDecisao_modelo01  


#Predição no conjunto de TESTE
ArvoreDecisao_modelo01_Predicao <- predict(ArvoreDecisao_modelo01, pesorn_test, type = "vector")
pesorn_test$preditosarvore<-ArvoreDecisao_modelo01_Predicao 

# Evaluate model ARVORE
msearvore <- mean((pesorn_test$IDADEMAE - pesorn_test$preditosarvore)^2)
rmsearvore <- sqrt(msearvore)
print(msearvore)
print(rmsearvore)


set.seed (3)
par(mfrow = c(1, 1))
CrossValidation = cv.tree(ArvoreDecisao_modelo01)
# size = número de nós terminais de cada árvore considerada
# alfa(k) =  valor do parâmetro custo-complexidade utilizado
#dev =  taxa de erro na atual partição do k-fold cross validation
names(CrossValidation)
CrossValidation
plot(CrossValidation)

ArvorePodada = prune.tree(ArvoreDecisao_modelo01, best =3)
plot(ArvorePodada)
text (ArvorePodada, pretty =0)
ArvorePodada