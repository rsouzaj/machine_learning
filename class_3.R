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

#Definindo a variável alvo(desfecho): Pode ser status (para a tarefa de classificação) ou time (para a tarefa de regressão)
#Mas vamos escolher status, pois o objetivo é classficação
pbcclass<- pbc %>% filter(status==0 | status==2)
table(pbcclass$status) #Verificando a quantidade de cada classe da variável de desfecho status

#Colocando rótulo na variável status: Vivo = 0 e 2 = Obito
pbcclass$status<-factor(pbcclass$status, levels=c(0,2), labels =c("Vivo","Óbito"))

#transformando variáveis preditoras categóricas em fatores no r (3 features)

pbcclass$ascites<-factor(pbcclass$ascites, levels=c(1,0), labels =c("Sim","Não"))
pbcclass$hepato<-factor(pbcclass$hepato, levels=c(1,0), labels =c("Sim","Não"))
pbcclass$trt<-factor(pbcclass$trt, levels=c(1,2), labels =c("D-penic","Placebo"))

#transformando variáveis preditoras numéricas no r (8 features)
str(pbcclass)
pbcclass$age<-as.numeric(pbcclass$age)
pbcclass$bili<-as.numeric(pbcclass$bili)
pbcclass$chol<-as.numeric(pbcclass$chol)
pbcclass$albumin<-as.numeric(pbcclass$albumin)
pbcclass$platelet<-as.numeric(pbcclass$platelet)
pbcclass$copper<-as.numeric(pbcclass$copper)
pbcclass$alk.phos<-as.numeric(pbcclass$alk.phos)
pbcclass$ast<-as.numeric(pbcclass$ast)

pbc<- pbcclass %>% select("age","bili","chol","albumin","platelet","copper",
                          "alk.phos","ast","ascites", "hepato","trt","status")

pbc<-pbc %>% filter(trt!="NA")

#--------------------------------------------------------
#ANALISE EXPLORATÓRIA
#--------------------------------------------------------
summary(pbc)

# Gerando gráficos
g1<-ggplot(pbc, aes(status, age)) + 
  geom_boxplot() #boxplot
plot(g1)

g2<-ggplot(pbc, aes(status, bili)) + 
  geom_boxplot() #boxplot
plot(g2)

g3<-ggplot(pbc, aes(status, chol)) + 
  geom_boxplot() #boxplot
plot(g3)

g4<-ggplot(pbc, aes(status, alk.phos)) + 
  geom_boxplot() #boxplot
plot(g4)

library(patchwork) #Biblioteca para arrumar os gráficos

g1 + g2 + g3 + g4 # Colocando todos em uma única tela


#Dispersão
g5<-ggplot(pbc, aes(age, bili)) + 
  geom_point() #gráfico de pontos
plot(g5)

g6<-ggplot(pbc, aes(age, chol)) + 
  geom_point()
plot(g6)

#arrumando os gráficos
g5+g6

#Incluindo mais uma dimensao e usando cores
gf <- ggplot(pbc, aes(x=status, y=bili, fill=trt)) + 
  geom_boxplot()+
  labs(title="Plot of bili  per status",x="status", y = "bili (mg/dl)")+ theme_classic()
plot(gf)

#--------------------------------------------------------
# PARTICIONANDO O CONJUNTO DE DADOS EM TREINO E TESTE
#--------------------------------------------------------
library(caret) # carregando o pacote caret

set.seed(123) # Definindo uma semente para garantir a reprodutibilidade dos experimentos
particionamento <- createDataPartition(pbc$status, # cria as partições de forma estratificada
                                       p = 0.75,
                                       list = FALSE) #O parâmetro list = FALSE evita que o resultado seja armazenado em formato de lista:

pbc_train <- pbc[particionamento, ]
pbc_test <- pbc[-particionamento, ]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#--------------------------------------------------------
#PRÉ-PROCESSAMENTO DOS DADOS NO CONJUNTO DE TREINO
#--------------------------------------------------------

pbc_train %>% ggplot(aes(x=status)) + geom_bar(stat="count")
table(pbc_train$status) #classes desbalanceada


#preenchendos dados faltantes pela média
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

#associação entre features categÓricas
chisq.test(pbc_train$ascites,pbc_train$hepato) #usar somente uma das variáveis: hepato ou ascites
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

pbc_train$hepato<- NULL
pbc_train$trt<- NULL

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#--------------------------------------------------------
# PRÉ-PROCESSAMENTO DOS DADOS NO CONJUNTO DE TESTE
#--------------------------------------------------------




pbc_test$age<-as.numeric(pbc_test$age)
pbc_test$bili<-as.numeric(pbc_test$bili)
pbc_test$chol<-as.numeric(pbc_test$chol)
pbc_test$albumin<-as.numeric(pbc_test$albumin)
pbc_test$platelet<-as.numeric(pbc_test$platelet)
pbc_test$copper<-as.numeric(pbc_test$copper)
pbc_test$alk.phos<-as.numeric(pbc_test$alk.phos)
pbc_test$ast<-as.numeric(pbc_test$ast)

#padronizaçãoo de variáveis numéricas


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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#--------------------------------------------------------
# CONSTRUÇÃO DOS MODELOS (ÁRVORE DE DECISÃO)
#--------------------------------------------------------

library(tree) #Carregando a biblioteca tree

ArvoreDecisao_modelo01 = tree(status~., data=pbc_train, method='class') 

summary(ArvoreDecisao_modelo01)
plot(ArvoreDecisao_modelo01)
text(ArvoreDecisao_modelo01, pretty=0)
ArvoreDecisao_modelo01  # Em cada nó teremos: O critério de divisão (por exemplo, 
#bili<-0.29), o número de observações nesse ramo (131), o desvio (143.0),
#a previsão geral para o ramo (Vivo) e
#a fração de observações nesse ramo que assume valores de Vivo e óbito ( 0.76336 0.23664 ).

#Predição no conjunto de TREINAMENTO
ArvoreDecisao_modelo01_Predicao <- predict(ArvoreDecisao_modelo01, pbc_train[,1:9], type = "class")
table(pbc_train$status,ArvoreDecisao_modelo01_Predicao)
confusionMatrix(ArvoreDecisao_modelo01_Predicao, pbc_train$status)

#Predição no conjunto de TESTE
ArvoreDecisao_modelo01_Predicao <- predict(ArvoreDecisao_modelo01, pbc_test[,1:9], type = "class")
#MatrizConfusao = table(ArvoreDecisao_modelo01_Predicao,pbc_train$status)
table(pbc_test$status,ArvoreDecisao_modelo01_Predicao)
confusionMatrix(ArvoreDecisao_modelo01_Predicao, pbc_test$status)

############################################################
# Como a acurácia ficou muito maior no conjunto de treino do que no conjunto de teste
# isso pode indicar um overfitting (lembrando que as árvores possuem essa tendência de 
# se ajustarem facilmente ao conjunto de treinamento).
# Dessa forma, vamos realizar a poda da árvore, diminuindo sua complexidade, com 
# o uso do k-fold Cross Validation
############################################################

set.seed (3)
CrossValidation = cv.tree(ArvoreDecisao_modelo01, FUN=prune.misclass, K = 10) #a taxa de erro de classificação está guiando o processo de validação cruzada e poda, em vez do padrão para a função cv.tree(), que é o desvio.
# size = número de nós terminais de cada árvore considerada
# alfa(k) =  valor do parâmetro custo-complexidade utilizado
#dev =  taxa de erro na atual partição do k-fold cross validation
names(CrossValidation)
CrossValidation
plot(CrossValidation)

ArvorePodada = prune.misclass(ArvoreDecisao_modelo01, best =5)
plot (ArvorePodada)
text (ArvorePodada, pretty =0)
ArvorePodada
#--------------------------------------------------------
#predição no conjunto de teste com a árvore podada
#--------------------------------------------------------
ArvoreDecisaoPodada_modelo01_Predicao = predict(ArvorePodada, pbc_test[,1:9], type = "class")
#MatrizConfusao = table(ArvoreDecisao_modelo01_Predicao,pbc_train$status)
#MatrizConfusao[1:1]
table(pbc_test$status,ArvoreDecisaoPodada_modelo01_Predicao)
confusionMatrix(ArvoreDecisaoPodada_modelo01_Predicao, pbc_test$status)
