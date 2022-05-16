#carregando pacote survival
library(survival)
library(tidyverse)
library(ggplot2)


#carregando banco de dados (dataframe pbc)
data(pbc)

#vendo os nomes das variaveis (features) do dataframe pbc
names(pbc)

#verificando a estrutura do dataframe, dimensao e primeiros pacientes (amostras)
str(pbc)
dim(pbc)
head(pbc)

#Definindo a variável alvo(desfecho): Poderia ser status (classificacão) ou time (regressão)

pbcclas<- pbc %>% filter(status==0 | status==2)
table(pbcclas$status) #vendo os valores do status

pbcclas$status<-factor(pbcclas$status, levels=c(0,2), labels =c("Vivo","Obito"))

#transformando variáveis de exposição categóricas em fatores no r (3 features)

pbcclas$ascites<-factor(pbcclas$ascites, levels=c(1,0), labels =c("Sim","N?o"))
pbcclas$hepato<-factor(pbcclas$hepato, levels=c(1,0), labels =c("Sim","N?o"))
pbcclas$trt<-factor(pbcclas$trt, levels=c(1,2), labels =c("D-penic","Placebo"))

#transformando vari?veis de exposi??o numericas no r (8 features)
str(pbcclas)
pbcclas$age<-as.numeric(pbcclas$age)
pbcclas$bili<-as.numeric(pbcclas$bili)
pbcclas$chol<-as.numeric(pbcclas$chol)
pbcclas$albumin<-as.numeric(pbcclas$albumin)
pbcclas$platelet<-as.numeric(pbcclas$platelet)
pbcclas$copper<-as.numeric(pbcclas$copper)
pbcclas$alk.phos<-as.numeric(pbcclas$alk.phos)
pbcclas$ast<-as.numeric(pbcclas$ast)

pbc<- pbcclas %>% select("age","bili","chol","albumin","platelet","copper",
                         "alk.phos","ast","ascites", "hepato","trt","status")

pbc<-pbc %>% filter(trt!="NA")

#analise exploratoria
summary(pbc)

#Boxplots
g1<-ggplot(pbc, aes(status, age)) + 
  geom_boxplot() 
plot(g1)

g2<-ggplot(pbc, aes(status, bili)) + 
  geom_boxplot() 
plot(g2)

g3<-ggplot(pbc, aes(status, chol)) + 
  geom_boxplot() 
plot(g3)

g4<-ggplot(pbc, aes(status, alk.phos)) + 
  geom_boxplot() 
plot(g4)

library(patchwork) #arrumando os gr?ficos

g1 + g2 + g3 + g4


#dispers?o
g5<-ggplot(pbc, aes(age, bili)) + 
  geom_point()
plot(g5)

g6<-ggplot(pbc, aes(age, chol)) + 
  geom_point()
plot(g6)

#arrumando os gr?ficos
g5+g6

#Incluindo mais uma dimensao e usando cores
gf <- ggplot(pbc, aes(x=status, y=bili, fill=trt)) + 
  geom_boxplot()+
  labs(title="Plot of bili  per status",x="status", y = "bili (mg/dl)")+ theme_classic()
plot(gf)

#Particionando os dados e treino e teste (aula 03)
library(caret)
#paste(names(getModelInfo()))

set.seed(789)
particionamento <- createDataPartition(pbc$status, # cria as partições de forma estratificada
                                       p = 0.75,
                                       list = FALSE) #O parâmetro list = FALSE evita que o resultado seja armazenado em formato de lista:

pbc_train <- pbc[particionamento, ]
pbc_test <- pbc[-particionamento, ]


#preenchendos dados faltantes pela m?dia
summary(pbc_train)

pbc_train$chol[is.na(pbc_train$chol)]<-mean(pbc_train$chol,na.rm = TRUE)
pbc_train$platelet[is.na(pbc_train$platelet)]<-mean(pbc_train$platelet,na.rm = TRUE)
pbc_train$copper[is.na(pbc_train$copper)]<-mean(pbc_train$copper,na.rm = TRUE)

any(is.na(pbc_train))

#valores discrepantes - Outliers
boxage<-ggplot(pbc_train, aes(y=age)) + 
  geom_boxplot()
plot(boxage)

boxplat<-ggplot(pbc_train, aes(y=platelet)) + 
  geom_boxplot()
plot(boxplat)

#teste para detectar outlier
library(EnvStats)
test <- rosnerTest(pbc_train$age,
                   k = 3)
test$all.stats

hist(pbc_train$age)

test1 <- rosnerTest(pbc_train$platelet,
                    k = 4)
test1$all.stats

hist(pbc_train$platelet)

#padronização de variáveis numéricas


pbc_train$age<- scale(pbc_train$age, center = T)
pbc_train$chol<- scale(pbc_train$chol, center = T)
pbc_train$bili<- scale(pbc_train$bili, center = T)
pbc_train$platelet<- scale(pbc_train$platelet, center = T)
pbc_train$albumin<- scale(pbc_train$albumin, center = T)
pbc_train$copper<- scale(pbc_train$copper, center = T)
pbc_train$alk.phos<- scale(pbc_train$alk.phos, center = T)
pbc_train$ast<- scale(pbc_train$ast, center = T)

#correlação entre features numéricas
library(corrplot)
pbctrainnum<- pbc_train %>% select("age","bili","chol","albumin","platelet","copper",
                                   "alk.phos","ast")
k <- cor(pbctrainnum)
corrplot(k, method = "circle")

library(corrgram)
corrgram(pbctrainnum, lower.panel = panel.pts, upper.panel= panel.conf, diag.panel = panel.density)

#associação entre features categóricas
chisq.test(pbc_train$ascites,pbc_train$hepato) #usar somente uma das variáveis
chisq.test(pbc_train$ascites,pbc_train$trt)
chisq.test(pbc_train$hepato,pbc_train$trt)

#seleção de variáveis numéricas
t.test(pbc_train$age~pbc_train$status)
t.test(pbc_train$bili~pbc_train$status)
t.test(pbc_train$chol~pbc_train$status)
t.test(pbc_train$albumin~pbc_train$status)

#seleção de variáveis categóricas
chisq.test(pbc_train$hepato,pbc_train$status)
chisq.test(pbc_train$ascites,pbc_train$status)
chisq.test(pbc_train$trt,pbc_train$status)

str(pbc_train)

RL_mod1 <-train(status~age+bili+chol+albumin+platelet+copper+alk.phos+ast+ascites, data = pbc_train, 
                method = "glm",
                family = binomial(link = "logit"))
RL_mod2 <-train(status~age+bili+chol+albumin+platelet+copper+alk.phos+ast+hepato, data = pbc_train, 
                method = "glm",
                family = binomial(link = "logit"))
summary(RL_mod1)

summary(RL_mod2)


#O modelo RL_mod1 é melhor, pois possui o menor AIC
#Realizando a predição no conjunto de treinamento
pbc_train$hepato<-NULL
yp_treino <- predict(RL_mod1, newdata = pbc_train[,1:9])
table(pbc_train$status,yp_treino)
confusionMatrix(yp_treino, pbc_train$status)

#pre-processamento dos dados de teste

pbc_test$age<-as.numeric(pbc_test$age)
pbc_test$bili<-as.numeric(pbc_test$bili)
pbc_test$chol<-as.numeric(pbc_test$chol)
pbc_test$albumin<-as.numeric(pbc_test$albumin)
pbc_test$platelet<-as.numeric(pbc_test$platelet)
pbc_test$copper<-as.numeric(pbc_test$copper)
pbc_test$alk.phos<-as.numeric(pbc_test$alk.phos)
pbc_test$ast<-as.numeric(pbc_test$ast)

#padronização de variaveis numéricas


pbc_test$age<- scale(pbc_test$age, center = T)
pbc_test$chol<- scale(pbc_test$chol, center = T)
pbc_test$bili<- scale(pbc_test$bili, center = T)
pbc_test$platelet<- scale(pbc_test$platelet, center = T)
pbc_test$albumin<- scale(pbc_test$albumin, center = T)
pbc_test$copper<- scale(pbc_test$copper, center = T)
pbc_test$alk.phos<- scale(pbc_test$alk.phos, center = T)
pbc_test$ast<- scale(pbc_test$ast, center = T)


#preenchendos dados faltantes pela média
summary(pbc_test)

pbc_test$chol[is.na(pbc_test$chol)]<-mean(pbc_test$chol,na.rm = TRUE)
pbc_test$platelet[is.na(pbc_test$platelet)]<-mean(pbc_test$platelet,na.rm = TRUE)
pbc_test$copper[is.na(pbc_test$copper)]<-mean(pbc_test$copper,na.rm = TRUE)

any(is.na(pbc_test))

#Selecionando variáveis do modelo treinado
pbc_test$hepato<- NULL
pbc_test$trt<- NULL

# Predição e matriz de confusão CONJUNTO DE TESTE.
yp <- predict(RL_mod1, newdata = pbc_test[,1:9])
table(pbc_test$status,yp)
confusionMatrix(yp, pbc_test$status)


# Modelando dados usando K-FOLD Cross- Validation

#pre-processamento dos dados totais
pbc$age<-as.numeric(pbc$age)
pbc$bili<-as.numeric(pbc$bili)
pbc$chol<-as.numeric(pbc$chol)
pbc$albumin<-as.numeric(pbc$albumin)
pbc$platelet<-as.numeric(pbc$platelet)
pbc$copper<-as.numeric(pbc$copper)
pbc$alk.phos<-as.numeric(pbc$alk.phos)
pbc$ast<-as.numeric(pbc$ast)

#padronização de variáveis numéricas


pbc$age<- scale(pbc$age, center = T)
pbc$chol<- scale(pbc$chol, center = T)
pbc$bili<- scale(pbc$bili, center = T)
pbc$platelet<- scale(pbc$platelet, center = T)
pbc$albumin<- scale(pbc$albumin, center = T)
pbc$copper<- scale(pbc$copper, center = T)
pbc$alk.phos<- scale(pbc$alk.phos, center = T)
pbc$ast<- scale(pbc$ast, center = T)


#preenchendos dados faltantes pela média
summary(pbc)

pbc$chol[is.na(pbc$chol)]<-mean(pbc$chol,na.rm = TRUE)
pbc$platelet[is.na(pbc$platelet)]<-mean(pbc$platelet,na.rm = TRUE)
pbc$copper[is.na(pbc$copper)]<-mean(pbc$copper,na.rm = TRUE)

any(is.na(pbc))


# Define training control
set.seed(789)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 5)
# Train the model

model_extra<- train(status~age+bili+chol+albumin+platelet+copper+alk.phos+ast+ascites, data = pbc, method = "bayesglm",
                    trControl = train.control)
# Summarize the results
print(model_extra)
