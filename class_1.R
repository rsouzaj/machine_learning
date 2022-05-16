#carregando pacote survival
library(survival)
library(tidyverse)
library(ggplot2)

#carregando banco de dados (dataframe pbc)
data(pbc)

#vendo os nomes das vari�veis (features) do dataframe pbc
names(pbc)

#verificando a estrutura do dataframe, dimens�o e primeiros pacientes (amostras)
str(pbc)
dim(pbc)
head(pbc)

#Definindo a vari�vel alvo(desfecho): Poderia ser status (classifica��o) ou time (regress�o)

pbcclas<- pbc %>% filter(status==0 | status==2)
table(pbcclas$status) #vendo os valores do status

pbcclas$status<-factor(pbcclas$status, levels=c(2,0), labels =c("Obito","Vivo"))

#transformando vari�veis de exposi��o categ�ricas em fatores no r (3 features)

pbcclas$ascites<-factor(pbcclas$ascites, levels=c(1,0), labels =c("Sim","N�o"))
pbcclas$hepato<-factor(pbcclas$hepato, levels=c(1,0), labels =c("Sim","N�o"))
pbcclas$trt<-factor(pbcclas$trt, levels=c(1,2), labels =c("D-penic","Placebo"))

#transformando vari�veis de exposi��o numericas no r (8 features)
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

library(patchwork) #arrumando os gr�ficos

g1 + g2 + g3 + g4


#dispers�o
g5<-ggplot(pbc, aes(age, bili)) + 
  geom_point()
plot(g5)

g6<-ggplot(pbc, aes(age, chol)) + 
  geom_point()
plot(g6)

#arrumando os gr�ficos
g5+g6

#Incluindo mais uma dimensao e usando cores
gf <- ggplot(pbc, aes(x=status, y=bili, fill=trt)) + 
  geom_boxplot()+
  labs(title="Plot of bili  per status",x="status", y = "bili (mg/dl)")+ theme_classic()
plot(gf)

#preenchendos dados faltantes pela m�dia
summary(pbc)

pbc$chol[is.na(pbc$chol)]<-mean(pbc$chol,na.rm = TRUE)
pbc$platelet[is.na(pbc$platelet)]<-mean(pbc$platelet,na.rm = TRUE)
pbc$copper[is.na(pbc$copper)]<-mean(pbc$copper,na.rm = TRUE)

any(is.na(pbc))

#valores discrepantes - Outliers
boxage<-ggplot(pbc, aes(y=age)) + 
  geom_boxplot()
plot(boxage)

boxplat<-ggplot(pbc, aes(y=platelet)) + 
  geom_boxplot()
plot(boxplat)

#teste para detectar outlier
library(EnvStats)
test <- rosnerTest(pbc$age,
                   k = 3)
test$all.stats

hist(pbc$age)

test1 <- rosnerTest(pbc$platelet,
                    k = 4)
test1$all.stats

hist(pbc$platelet)

#padroniz��o de vari�veis num�ricas

hist(pbc$age)
pbc$age<- scale(pbc$age, center = T)
hist(pbc$age)

pbc$chol<- scale(pbc$chol, center = T)
pbc$bili<- scale(pbc$bili, center = T)
pbc$platelet<- scale(pbc$platelet, center = T)
pbc$albumin<- scale(pbc$albumin, center = T)
pbc$copper<- scale(pbc$copper, center = T)
pbc$alk.phos<- scale(pbc$alk.phos, center = T)
pbc$ast<- scale(pbc$ast, center = T)

#correla��o entre features num�ricas
library(corrplot)
pbcnum<- pbc %>% select("age","bili","chol","albumin","platelet","copper",
                        "alk.phos","ast")
k <- cor(pbcnum)
corrplot(k, method = "circle")

library(corrgram)
corrgram(pbcnum, lower.panel = panel.pts, upper.panel= panel.conf, diag.panel = panel.density)

#associa��o entre features categ�ricas
chisq.test(pbc$ascites,pbc$hepato) #usar somente uma das vari�veis
chisq.test(pbc$ascites,pbc$trt)
chisq.test(pbc$hepato,pbc$trt)

#sele��o de vari�veis num�ricas
t.test(pbc$age~pbc$status)
t.test(pbc$bili~pbc$status)
t.test(pbc$chol~pbc$status)
t.test(pbc$albumin~pbc$status)

#sele��o de vari�veis categ�ricas
chisq.test(pbc$hepato,pbc$status)
chisq.test(pbc$ascites,pbc$status)
chisq.test(pbc$trt,pbc$status)
