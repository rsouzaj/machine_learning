library(tidyverse)

### Pacotes

    # tidyverse
    # xtable
    # flextable


## Carregar o banco para análise


banco <- read_csv("EXAMES-PNS-2013-selec.csv", locale = locale(decimal_mark = ","))

## Identificação dos nomes das variáveis, dimensão e estrutura do banco de dados.

names(banco)
dim(banco)
glimpse(banco)


# Desfecho ----------------------------------------------------------------


# Seleção do desfecho:

# Diabetes definição:

#   Hemoglobina glicada : Diretrizes Sociedade Brasileira de Diabetes 2019-2020 – 
            # Aspectos técnicos e laboratoriais de diagnóstico e acompanhamento do diabetes mellitus

# Z034	Hemoglobina Glicosilada (em %)

banco %>% 
  group_by(Q030) %>% 
  summarise(N = n())

banco <- banco %>% 
  mutate(hb_glic = if_else(Z034 >= 6.5, 1, 3))

banco %>% 
  group_by(hb_glic) %>% 
  summarise(N = n())

banco %>% 
  group_by(hb_glic, Q030) %>% 
  summarise(N = n())

# Visualização de relação entre sim e não:

# banco <- banco %>% 
#   filter(!is.na(Z034)) %>% 
#  filter(!is.na(Q030)) %>% 
#   mutate(diabetes = if_else(Z034 >= 6.5 | Q030 == 1 | Q030 == 2 , "Sim", "Não"))


banco <- banco %>% 
  mutate(
    diabetes = if_else((Z034 >= 6.5) | (Q030 == 1) | (Q030 == 2) , "Sim", "Não")
    )


# banco$diabetes <- factor(banco$diabetes, levels = c(1, 2, 3), labels= c("Sim", "Gestação", "Não"))
# table(banco_t$Q030)

banco %>% 
  group_by( Z030) %>% 
  summarise(N = n())

table(banco$diabetes)

# Variáveis preditoras ------------------------------------------------------------



# W00303 -> Circunferência da cintura - Final (em cm)
# W00407 ->	Pressão arterial sistólica  - Final (em mmHg) 
# W00408 -> Pressão arterial diastólica  - Final (em mmHg) 
# regiao -> Região do país 
            # 1	Norte
            # 2	Nordeste
            # 3	Sudeste
            # 4	Sul
            # 5	Centro-Oeste
# VDD004 ->	Nível de instrução mais elevado alcançado (pessoas de 5 anos ou mais de idade)	
#       1	Sem instrução
#       2	Fundamental incompleto ou equivalente
#       3	Fundamental completo ou equivalente
#       4	Médio incompleto ou equivalente
#       5	Médio completo ou equivalente
#       6	Superior incompleto ou equivalente
#       7	Superior completo 
#       .	Não aplicável
# P050	Atualmente, o(a) Sr(a) fuma algum produto do tabaco?	
          # 1	Sim, diariamente          
          # 2	Sim, menos que diariamente
          # 3	Não fumo atualmente       
          # .	Não aplicável
# P027	Com que frequência o(a) Sr(a) costuma consumir alguma bebida alcoólica?	
          # 1	Não bebo nunca
          # 2	Menos de uma vez por mês
          # 3	Uma vez ou mais por mês
          # .	Não aplicável
# P034	Nos últimos três meses, o(a) Sr(a) praticou algum tipo de exercício físico ou esporte? (não considere fisioterapia)	
          # 1	Sim
          # 2	Não
          # .	Não aplicável
# # P027	Com que frequência o(a) Sr(a) costuma consumir alguma bebida alcoólica?	
          # 1	Não bebo nunca
          # 2	Menos de uma vez por mês
          # 3	Uma vez ou mais por mês
          # .	Não aplicável


# P025	Em quantos dias da semana o(a) Sr(a) come alimentos doces, tais como pedaços de bolo ou torta, doces, chocolates, balas, biscoitos ou bolachas doces? (0 = Nunca ou menos de uma vez por semana)
# P018	Em quantos dias da semana o(a) Sr(a) costuma comer frutas? (0 = Nunca ou menos de uma vez por semana)
# P015	Em quantos dias da semana o(a) Sr(a) costuma comer peixe? (0 = Nunca ou menos de uma vez por semana)
# P013	Em quantos dias da semana o(a) Sr(a) costuma comer frango/galinha? (0 = Nunca ou menos de uma vez por semana)
# P011	Em quantos dias da semana o(a) Sr(a) costuma comer carne vermelha (boi, porco, cabrito)? (0 = Nunca ou menos de uma vez por semana)
# P009	Em quantos dias da semana, o(a) Sr(a) costuma comer verdura ou legume cozido, como couve, cenoura, chuchu, berinjela, abobrinha? (sem contar batata, mandioca ou inhame) (0 = Nunca ou menos de uma vez por semana)
# P007	Em quantos dias da semana, o(a) Sr(a) costuma comer salada de alface e tomate ou salada de qualquer outra verdura ou legume cru? (0 =  Nunca ou menos de uma vez por semana)

# 
# C011	Qual é o estado civil de ___? 	
#           1	Casado(a)
#           2	Separado(a) ou desquitado(a) judicialmente
#           3	Divorciado(a)
#           4	Viúvo(a)
#           5	Solteiro(a)
#           .	Não aplicável
# 
# 
# Z032	HDL Colesterol (em mg/dL)
# Z031	Colesterol Total (em mg/dL)
# Z007	Hemoglobina (em g/dL)
# Z008	Hematócrito (em %)
# Z004 	Peso - Final (em kg)
# Z005 	Altura - Final (em cm)

# imc = Z004 / ((Z005)/100)^2)

# Z003 	Cor ou raça 
#         1	Branca
#         2	Preta
#         3	Amarela
#         4	Parda
#         5	Indígena
#         9	Ignorado
# Z001	Sexo	
#         1	Masculino
#         2	Feminino
# Z002	Idade		Anos
        

# Seleção das variáveis ---------------------------------------------------

banco_trab <- banco %>% 
  select(Z001:Z008, C011, W00407, W00408, W00303, regiao, )

# Transformação de variáveis preditoras ----------------------------------------------

# Categóricas em fatores

pbcclass$ascites<-factor(pbcclass$ascites, levels=c(1,0), labels =c("Sim","Não"))

# Variáveis preditoras numéricas

pbcclass$age<-as.numeric(pbcclass$age)


# Missing -----------------------------------------------------------------

### Qual o máximo de NAs aceitáveis?

## alguns estudos sugerem de 5 a 25%. Ref. blibliog:

#' @article{dziura2013strategies,
#'   title={Strategies for dealing with missing data in clinical trials: from design to analysis},
#'   author={Dziura, James D and Post, Lori A and Zhao, Qing and Fu, Zhixuan and Peduzzi, Peter},
#'   journal={The Yale journal of biology and medicine},
#'   volume={86},
#'   number={3},
#'   pages={343},
#'   year={2013},
#'   publisher={Yale Journal of Biology and Medicine}
#' }        

### Avaliação da quantidade de NAs e porcentagem por variável

banco %>% 
  summarise(N = n()) %>% 
  tibble(Variáveis = colnames(banco),
         Missing =  colSums(is.na(banco)),
         Porcentagem = (colSums(is.na(banco))*100/N)
  ) %>% 
  select(Variáveis:Porcentagem) %>% 
  arrange((Porcentagem)) %>% 
  xtable::xtable() %>% 
  flextable::as_flextable()



# Análise exploratória ----------------------------------------------------

summary(banco_trab)

### Gráficos

# Boxplot -> Categórica vs numérica

g1 <- ggplot(banco_trab, aes())+
  geom_boxplot()

g2 <- ggplot(banco_trab, aes())+
  geom_boxplot()

g3 <- ggplot(banco_trab, aes())+
  geom_boxplot()

g4 <- ggplot(banco_trab, aes())+
  geom_boxplot()

library(patchwork) # pacote para arrumar gráficos

g1 + g2 + g3 + g4  # todos os gráficos em uma única tela


# Dispersão ->numérica vs numérica

g5<-ggplot(banco_trab, aes(age, bili)) + 
  geom_point() #gráfico de pontos
plot(g5)

g6<-ggplot(banco_trab, aes(age, chol)) + 
  geom_point()
plot(g6)

g5 + g6


# Particionamento dos dados entre treino e teste --------------------------

library(caret)

set.seed(123) #DEfenindo uma semente para garantir a reprodutibilidade

particionamento <- caret::createDataPartition(
  banco_trab$Z003, 
  p = 0.75,
  list = F   # O parâmetro list evita que o resultado seja armazenado em formato de lista.
)

banco_train <- banco_trab[particionamento, ]
banco_test <- banco_trab[-particionamento, ]


# Pré-processamento dos dados no conjunto de treino -----------------------

pbc_train %>% ggplot(aes(x=status)) + geom_bar(stat="count")



## preenchendos dados faltantes pela média

media_imc <- banco_train %>% 
  filter(imc < 50) 


banco_train$imc[is.na(banco_train$imc)] <- mean(media_imc$imc)

media_vldl <- banco_train %>% 
  filter(vldl <= 100)

banco_train$vldl[which(is.na(banco_train$vldl))] <- mean(media_vldl$vldl, na.rm = T)

banco_train$W00303[which(is.na(banco_train$W00303))] <- mean(banco_train$W00303, na.rm = T)


banco_train$Z031[which(is.na(banco_train$Z031))] <- mean(banco_train$Z031, na.rm = T)

banco_train$Z032[is.na(banco_train$Z032)] <- mean(banco_train$Z032, na.rm = T)

banco_train$Z033[is.na(banco_train$Z033)] <- mean(banco_train$Z033, na.rm = T)


summary(banco_train_pad)


banco_train <- banco_train %>% 
  mutate(
    regiao = replace_na(regiao, '5')
  )



# summary(pbc_train)
# 
# pbc_train$chol[is.na(pbc_train$chol)]<-mean(pbc_train$chol,na.rm = TRUE)
# pbc_train$platelet[is.na(pbc_train$platelet)]<-mean(pbc_train$platelet,na.rm = TRUE)
# pbc_train$copper[is.na(pbc_train$copper)]<-mean(pbc_train$copper,na.rm = TRUE)



# Outliers ----------------------------------------------------------------

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

### Correlação entre variáveis numéricas:


# Correlação entre as variáveis:
#   
#   ```{r correl}
# 
# correl <- banco_trab %>% 
#   select(ends_with("pad")) %>% 
#   cor(use = "pairwise")  # não estava funcionando da outra forma (sem o 'pairwise').
# 
# corrplot::corrplot(correl, method = 'circle')
# 
# ```


### Relação entre as variáveis categóricas e diagnóstico de diabetes:

# As estatísticas de Z001(sexo) e Z003(raça) foram idênticas. 
# Após analizar o banco e rever o dicionário de variáveis, verificamos que estão
# preenchidas com os mesmos valores. Retiramos a variável raça da análise.
# 
# ```{r categorias vs diabetes}
# 
# chisq.test(banco_trab$diabetes, banco_trab$Z001)
# 
# chisq.test(banco_trab$diabetes, banco_trab$Z003)
# 
# chisq.test(banco_trab$diabetes, banco_trab$regiao)
# 
# chisq.test(banco_trab$diabetes, banco_trab$VDD004)
# 
# ```


# Padronização de variáveis numéricas -------------------------------------

pbc_train$age<- scale(pbc_train$age, center = T)

#correlação entre features numéricas
# library(corrplot)
pbctrainnum<- pbc_train %>% select("age","bili","chol","albumin","platelet","copper",
                                   "alk.phos","ast")
k <- cor(pbctrainnum)

corrplot::corrplot(k, method = "circle")

# library(corrgram)
corrgram::corrgram(pbctrainnum, lower.panel = panel.pts, upper.panel= panel.conf, diag.panel = panel.density)

# associação entre features categóricas
chisq.test(pbc_train$ascites,pbc_train$hepato) #usar somente uma das variáveis: hepato ou ascites

#seleção de variáveis numéricas
t.test(pbc_train$age~pbc_train$status)

#seleção de variáveis categóricas
chisq.test(pbc_train$hepato,pbc_train$status)

str(pbc_train)

pbc_train$hepato<- NULL
pbc_train$trt<- NULL



# Regressão logística -----------------------------------------------------
library(caret)

mod_1 <- train(class ~ Z001 + Z002 + imc + Z031   +
                        vldl ,
                      data = banco_train_pad,
                      method = "glm",
                      family = binomial(link = "logit")
)
               
               
              banco_doido <- na.omit(banco_trab) %>% 
                 filter(!is.na(diabetes)) %>% 
                 mutate(diabetes = as.factor(diabetes))              

summary(banco_train)              
              
mod_1 <-  train(diabetes ~ Z001 + Z002 + imc + W00303 + Z031 + Z032 + Z033 +
                 vldl + VDD004 + regiao,
               data = banco_train,
               method = "glm",
               family = binomial(link = "logit"))
               
summary(mod_1)

mod_2 <-  train(diabetes ~ Z001 + Z002 + imc + W00303 + Z031 + Z032 +
                  vldl + VDD004 + regiao,
                data = banco_train,
                method = "glm",
                family = binomial(link = "logit"))

summary(mod_2)

mod_3 <-  train(diabetes ~ Z001 + Z002 + imc+ Z031 + Z032 +
                  vldl + VDD004 + regiao,
                data = banco_train,
                method = "glm",
                family = binomial(link = "logit"))

summary(mod_3)


mod_4 <-  train(diabetes ~ Z001 + Z002  + W00303 + Z031 + Z032 +
                  vldl + VDD004 + regiao,
                data = banco_train,
                method = "glm",
                family = binomial(link = "logit"))

summary(mod_4)

mod_5 <-  train(diabetes ~ Z001 + Z002  + W00303 + Z032 +
                  vldl + VDD004 + regiao,
                data = banco_train,
                method = "glm",
                family = binomial(link = "logit"))

summary(mod_5)


mod_6 <-  train(diabetes ~ Z001 + Z002  + W00303 + Z031 +
                  vldl + VDD004 + regiao,
                data = banco_train,
                method = "glm",
                family = binomial(link = "logit"))

summary(mod_6)


mod_7 <-  train(diabetes ~ Z001 + Z002  + imc + Z031 + Z032 +
                  vldl + VDD004 + regiao,
                data = banco_train,
                method = "glm",
                family = binomial(link = "logit"))

summary(mod_7)








str(banco_train)

RL_mod1 <-train(status~age+bili+chol+albumin+platelet+copper+alk.phos+ast+ascites, data = pbc_train, 
                method = "glm",
                family = binomial(link = "logit"))



RL_mod2 <-train(status~age+bili+chol+albumin+platelet+copper+alk.phos+ast+hepato, data = pbc_train, 
                method = "glm",
                family = binomial(link = "logit"))
summary(RL_mod1)

summary(RL_mod2)


## Verificar o melhor modelo

#Realizando a predição no conjunto de treinamento
pbc_train$hepato<-NULL
yp_treino <- predict(RL_mod1, newdata = pbc_train[,1:9])
table(pbc_train$status,yp_treino)
confusionMatrix(yp_treino, pbc_train$status)


#Realizando a predição no conjunto de treinamento
pbc_train$hepato<-NULL
yp_treino <- predict(RL_mod1, newdata = pbc_train[,1:9])
table(pbc_train$status,yp_treino)
confusionMatrix(yp_treino, pbc_train$status)

## Define training control
set.seed(789)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 5)
# Train the model

model_extra<- train(status~age+bili+chol+albumin+platelet+copper+alk.phos+ast+ascites, data = pbc, method = "bayesglm",
                    trControl = train.control)
# Summarize the results
print(model_extra)



# Construção de árvore de decisão -----------------------------------------


arvore_decisao_mod01 <- tree::tree(VARIAVELDESFECHO~., data = banco_train, method = 'class')

summary(arvore_decisao_mod01)
plot(arvore_decisao_mod01)
text(arvore_decisao_mod01, pretty = 0)


## Predição conjunto de TREINAMENTO

arvore_decisao_mod01_predicao <- predict(arvore_decisao_mod01, banco_train[,1:9], type = 'class')

table(banco_train$VARIAVELDESFECHO, arvore_decisao_mod01_predicao)

caret::confusionMatrix(arvore_decisao_mod01_predicao, banco_train$VARIAVELDESFECHO)

## Predição conjunto TESTE

arvore_decisao_mod01 <- predict(arvore_decisao_mod01, banco_test[,1:9], type = 'class')

table(banco_test$VARIAVELDESFECHO, arvore_decisao_mod01)

caret::confusionMatrix(arvore_decisao_mod01_predicao, banco_test$VARIAVELDESFECHO)

# Análise da acurácia entre os grupos de treino e teste:

## avaliar overfitting e poda da árvore caso necessário.



cv.tree



