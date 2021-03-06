#Bibliotecas Usadas
library(gmodels)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(dplyr)
library(caret)
library(lars)
library(elasticnet)

#leitura dos dados
dados <- read.table("dados.txt", header=T, dec=".")

#dados de treino
dados_treino <- filter(dados, train == TRUE)
#dados de teste
dados_teste <- filter(dados, train == FALSE)

#Criando gráficos
plot_vol <- qplot(dados$lcavol, dados$lpsa, xlab = "lcavol", ylab ="PSA")
plot_weight <- qplot(dados$lweight, dados$lpsa, xlab = "lweight", ylab ="PSA")
plot_age <- qplot(dados$age, dados$lpsa, xlab = "age", ylab ="PSA")
plot_bph <- qplot(dados$lbph, dados$lpsa, xlab = "lbph", ylab ="PSA")
plot_svi <- qplot(dados$svi, dados$lpsa, xlab = "svi", ylab ="PSA")
plot_cp <- qplot(dados$lcp, dados$lpsa, xlab = "lcp", ylab ="PSA")
plot_gleason <- qplot(dados$gleason, dados$lpsa, xlab = "gleason", ylab ="PSA")
plot_pgg45 <- qplot(dados$pgg45, dados$lpsa, xlab = "pgg45", ylab ="PSA")

#Plotando quatro deles
grid.arrange(plot_vol, plot_weight, plot_age, plot_bph, ncol=2, nrow=2)

#Plotando quatro deles 
grid.arrange(plot_svi, plot_cp, plot_gleason, plot_pgg45, ncol=2, nrow=2)

#Correlação das variáveis
nums <- sapply(dados, is.numeric)
correlations <- cor(dados[,nums])
corrplot(correlations, order="hclust", type="lower")

#obtendo a regressão linear
reg_linear <- lm(lpsa ~ lcavol+svi+lcp, data = dados_treino)

#predições
predicoes <- predict(reg_linear, select(dados_teste,lcavol, svi, lcp))

#dataframe com as predições
lm_predicoes <- data.frame(pred = predicoes, obs = dados_teste$lpsa)

ggplot(lm_predicoes, aes(x = pred, y = obs)) + geom_point(alpha = 0.5, position = position_jitter(width=0.2)) + geom_abline(colour = "blue") + ggtitle("Observados X Previstos (validação)")

#resíduos
residuos = dados_teste$lpsa - predicoes

qplot(predicoes, residuos, ylab = "Resíduos", xlab = "Predições") +
  geom_hline(yintercept = 0, color='blue') + ggtitle("Resíduos x Predição")

#mean squared error
RMSE(predicoes, dados_teste$lpsa)

#modelo LASSO para seleção de preditores
lasso.fit <- train(lpsa ~ ., data=select(dados_treino, lcavol, lweight, age, lbph, svi, lcp, gleason, pgg45, lpsa), method='lasso', metric='RMSE',tuneLength=10)

plot(lasso.fit)

#importância das variáveis para o modelo LASSO
plot(varImp(lasso.fit))

#previsões com o modelo LASSO
lasso_predicoes <- predict(lasso.fit, select(dados_teste,lcavol, lweight, age, lbph, svi, lcp, gleason, pgg45))

la_predicoes <- data.frame(pred = lasso_predicoes, obs = dados_teste$lpsa)

ggplot(la_predicoes, aes(x = pred, y = obs)) + geom_point(alpha = 0.5, position = position_jitter(width=0.2)) + geom_abline(colour = "blue") + ggtitle("Observados X Previstos (LASSO)")

#RMSE do modelo LASSO
RMSE(lasso_predicoes, dados_teste$lpsa)

#comparando os modelos
compare <- lm_predicoes
lm_predicoes$model <- "RL"
la_predicoes$model <- "LASSO"

compare <- rbind(lm_predicoes, la_predicoes)

ggplot(compare, aes(x = pred, y = obs)) + 
  geom_point(alpha = 0.5, position = position_jitter(width=0.2)) + 
  facet_grid(. ~ model) + 
  geom_abline() +
  ggtitle("Observado x Previsão (validação)")


