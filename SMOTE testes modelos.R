
mod_2 <- train(class ~ Z001 + Z002 + W00303 + Z031 +Z032  + VDD004 + regiao +
                 vldl ,
               data = banco_train_pad,
               method = "glm",
               family = binomial(link = "logit")
)
mod_3 <- train(class ~ Z001 + Z002 + W00303  +Z031  + VDD004 + regiao +
                 vldl ,
               data = banco_train_pad,
               method = "glm",
               family = binomial(link = "logit")
)  
mod_4 <- train(class ~ Z001 + Z002 + W00303  + Z031  + VDD004 + regiao + # com colesterol
                 vldl ,
               data = banco_train_pad,
               method = "glm",
               family = binomial(link = "logit"))


mod_5 <- train(class ~  Z002 + W00303  +Z031    + #com colesterol
                 vldl ,
               data = banco_train_pad,
               method = "glm",
               family = binomial(link = "logit"))

summary(mod_4)

banco_train_2 <- banco_train%>% 
  mutate(
    diabetes = if_else(diabetes == "Sim", 1, 2),
    Z001 = as.numeric(Z001),
    VDD004 = as.numeric(VDD004),
    regiao = as.numeric(regiao)
  )

banco_train_pad$Z032 <- NULL

banco_train_pad$Z033 <- NULL

banco_train_pad$Z032 <- NULL

yp_treino <- predict(mod_4, newdata = banco_train_pad[, 1:9])

table(banco_train_pad$class, yp_treino)

caret::confusionMatrix(as.factor(banco_train_pad$class), yp_treino, positive	
 = "Sim")



summary(yp_treino)
table(banco_train_pad$class)

banco_smote <- smotefamily::SMOTE(
  banco_train_2[, 2:11], 
  unlist(as.numeric(banco_train_2$diabetes)),
  K = 5, dup_size = 7
)


banco_smote <- banco_smote$data


banco_train_2 <- banco_smote %>% 
  mutate(
    class = if_else(class == 1, "Sim", "Não"),
    Z001 = as.factor(round(Z001)),
    VDD004 = as.factor(round(VDD004)),
    regiao = as.factor(round(regiao))
  )

mod_7 <-  train(class ~ Z001 + Z002  + W00303 + Z031 +
                  vldl + regiao,
                data = banco_train_pad,
                method = "glm",
                family = binomial(link = "logit"))

summary(mod_7)

banco_train_pad



# Preparo arvore decisao --------------------------------------------------


banco_train_smote <- banco_train %>% 
  mutate(
    diabetes = if_else(diabetes == "Sim", 1, 2),
    Z001 = as.numeric(Z001),
    VDD004 = as.numeric(VDD004),
    regiao = as.numeric(regiao)
  )


smote_train <- smotefamily::SMOTE(
  banco_train_smote[, 2:11], 
  unlist(as.numeric(banco_train_smote$diabetes)),
  K = 5, dup_size = 6
)


smote_train <- smote_train$data

table(smote_train$class)

banco_train_arv <- smote_train %>% 
  mutate(
    Z001 = as.factor(if_else(Z001 == 1, "M", "F") ),
    class = as.factor(if_else(class == 1, "Sim", "Não") ),
    VDD004 = as.factor(round(VDD004)),
    regiao = as.factor(round(regiao))
  ) %>% 
  select(-imc, -Z032, -Z033)

summary(banco_train_arv)

Arvore_dec_mod_4 <- tree::tree(class~.,data = banco_train_arv, method = 'class')

summary(Arvore_dec_mod_4)

plot(Arvore_dec_mod_4)

text(Arvore_dec_mod_4)

Arvore_dec_mod_4


# Predição no conjunto de Teste -------------------------------------------

banco_test_arv <- banco_test %>% 
  mutate(
    Z001 = as.factor(if_else(Z001 == 1, "M", "F") ),
    diabetes = as.factor(diabetes),
    VDD004 = as.factor(VDD004),
    regiao = as.factor(regiao)
  )

Arv_dec_test_pred <- predict(Arvore_dec_mod_4, banco_test_arv[, 1:8], type = 'class')  

table(banco_test_arv$diabetes, Arv_dec_test_pred)

caret::confusionMatrix(Arv_dec_test_pred, banco_test_arv$diabetes)

set.seed(3)

Cross_validation <- tree::cv.tree(Arvore_dec_mod_4, FUN=prune.misclass, K = 10)

names(Cross_validation)

plot(Cross_validation)

ArvorePodada = prune.misclass(Arvore_dec_mod_4, best =6)

plot(ArvorePodada)
text(ArvorePodada)

ArvorePodada
