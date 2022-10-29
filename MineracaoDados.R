install.packages("here")
install.packages("tidyverse")
install.packages("tidymodels")
library(here)
library(tidyverse)
library(tidymodels)
install.packages("mlbench")
library(mlbench)
library(janitor)
library(lubridate)
colunas <- list(col_character(), 
                col_date(format="%d/%m/%Y"),
		    col_character(),
                col_factor(),
                col_factor(),
		    col_factor(),
		    col_factor(),
		    col_character(),
		    col_factor(),
		    col_factor(),
		    col_factor(),
		    col_factor(),
		    col_factor(),
		    col_factor(),
		    col_factor(),
		    col_factor(),
		    col_factor(),
                col_factor())

dados <- read_csv2("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2021/INFLUD21-26-09-2022.csv")
dadosnovo <- select (dados, ID_MUNICIP, DT_SIN_PRI,CS_SEXO,NU_IDADE_N,CS_GESTANT,CS_RACA,CS_ESCOL_N,SG_UF,FEBRE,PERD_OLFT,PERD_PALA,OBESIDADE,VACINA_COV,HOSPITAL,UTI,RES_AN,CLASSI_FIN,EVOLUCAO)
dadosnovo <- dadosnovo |> filter( SG_UF == "MG",ID_MUNICIP == "BELO HORIZONTE")
count(dadosnovo)
dados <- janitor::clean_names(dados)
print(dadosnovo,n=20)


dados <- mutate(dadosnovo, IDADE = NU_IDADE_N)

dadosnovo<-transform(dadosnovo,EVOLUCAO=as.numeric(EVOLUCAO))

library(ggplot2)

idade.masculino <- dadosnovo |> filter(CS_SEXO == "M")
idade.feminino <- dadosnovo |> filter(CS_SEXO == "F")

fig <- ggplot(idade.masculino)
fig <- fig + geom_histogram(aes(NU_IDADE_N, fill = CS_SEXO),fill='darkblue',color=c('black'))+xlab("Idade")+ylab("Qtd analisada")+ggtitle("Dados analisados no sexo Masculino")+theme(plot.title = element_text(hjust=0.5))
fig

fig <- ggplot(idade.feminino)
fig <- fig + geom_histogram(aes(NU_IDADE_N, fill = CS_SEXO),fill='darkred',color=c('black'))+xlab("Idade")+ylab("Qtd analisada")+ggtitle("Dados analisados no sexo Feminino")+theme(plot.title = element_text(hjust=0.5))
fig

fig_cx <- ggplot(dadosnovo, aes(NU_IDADE_N, CS_SEXO)) +
          geom_boxplot()
fig_cx


medidasdescritivas <- dadosnovo |> 
          group_by(CS_SEXO) |> 
          summarise(Media = mean(NU_IDADE_N), 
			  Mediana = round(median(NU_IDADE_N), 2),
                    "Mais Novo"  = min(NU_IDADE_N), 
                    "Mais Velho" = max(NU_IDADE_N),
			  Total = n()) |>
          arrange(desc(Media))
          
view(medidasdescritivas)



factor(dadosnovo$EVOLUCAO)
obitos <- dadosnovo |> filter(EVOLUCAO == '2')
dadosnovo
dadosnovo |> group_by(CS_SEXO) |> summarise(media = mean(EVOLUCAO==2), 
                                             contagem = sum(EVOLUCAO==2))


view(obitos)
ggplot(obitos) + geom_boxplot(aes(x = NU_IDADE_N, y = CS_SEXO))
)
obitosM <- obitos |> filter(CS_SEXO=="M")
obitosF <- obitos |> filter(CS_SEXO=="F")

obitosMnovo <- obitos |> filter(NU_IDADE_N <'50')
obitosMidoso <- obitos |> filter(NU_IDADE_N>'50')

obitosFnovo <- obitos |> filter(NU_IDADE_N <'50')
obitosFidoso <- obitos |> filter(NU_IDADE_N>'50')

idadeM <- obitos |> filter(CS_SEXO=="M")
idadeF <- obitos |> filter(CS_SEXO=="F")

obitosnaoobeso <- obitos |> filter(OBESIDADE =='2')
obitosobeso <- obitos |> filter(OBESIDADE=='1')

vacinado <- obitos |> filter(VACINA_COV==1)
naovacinado <- obitos |> filter(VACINA_COV==2)
view(vacinado)
summary(dadosnovo)
wilcox.test(x = obitosM$VACINA_COV, y = obitosF$VACINA_COV, alternative = "greater")

wilcox.test(x = obitosMnovo$VACINA_COV, 
            y = obitosMidoso$VACINA_COV,
            alternative = "greater")

wilcox.test(x = obitosMnovo$CS_RACA, 
            y = obitosMidoso$CS_RACA,
            alternative = "greater")

wilcox.test(x = idadeM$VACINA, 
            y = idadeF$VACINA,
            alternative = "greater")

wilcox.test(x = vacinado$NU_IDADE_N, 
            y = naovacinado$NU_IDADE_N,
            alternative = "greater")


..............

medidasdescritivas <- obitos |> 
          group_by(CS_SEXO) |> 
          summarise(Media = mean(NU_IDADE_N), 
			  Mediana = round(median(NU_IDADE_N), 2),
                    "Mais Novo"  = min(NU_IDADE_N), 
                    "Mais Velho" = max(NU_IDADE_N),
			  Total = n()) |>
          arrange(desc(Media))


medidasdescritivas <- obitos |> 
          group_by(OBESIDADE) |> 
          summarise(Media = mean(NU_IDADE_N), 
			  Mediana = round(median(NU_IDADE_N), 2),
                    "Mais Novo"  = min(NU_IDADE_N), 
                    "Mais Velho" = max(NU_IDADE_N),
			  Total = n()) |>
          arrange(desc(Media))

install.packages("baguette")
library("baguette")
install.packages("mlbench")
library("mlbench")

dadosobito <- select (dadosnovo, CS_SEXO,NU_IDADE_N,OBESIDADE,VACINA_COV,EVOLUCAO)
dadosobito <- dplyr_col_modify(dadosobito,cols(OBESIDADE,VACINA_COV,EVOLUCAO, .default =col_guess() ))
divisao_de_dados <- initial_split(dadosobito)
dados_de_treino <- training(divisao_de_dados)
dados_de_teste  <- testing(divisao_de_dados)
view(dadosobito)
?dplyr_reconstruct
reamostragem <- bootstraps(dados_de_treino, times = 4) #computador pouca memoria para processamento.
dadosobito <- data.frame(dadosobito)
receita <- recipe(EVOLUCAO ~ VACINA_COV + CS_SEXO + NU_IDADE_N + OBESIDADE,
                  data = dadosobito)

arv_decisao <- decision_tree(mode = "classification", tree_depth = tune(), min_n = 10) 
knn <- nearest_neighbor(mode = "classification", neighbors = tune()) 
bag_de_arv <- bag_tree(mode = "classification", tree_depth = tune(), min_n = 10) 
floresta <- rand_forest(mode = "classification", mtry = tune(), min_n = 10, trees = 100) 

combinacoes <- workflow_set(
  preproc = list(receita),
  models = list("arvore" = arv_decisao,
                "knn" = knn,
                "bag_de_arv" = bag_de_arv,
                "floresta" = floresta)
)

res <- combinacoes |> workflow_map(resamples = reamostragem,
                                   grid = 5,
                                   verbose = F
)              

autoplot(res, metric = "roc_auc")

?workflow_map
------Boosting

xg_boost <- boost_tree(mode = "classification", engine  = "xgboost", learn_rate = tune())
#c5_boost <- boost_tree(mode = "classification", engine  = "C5.0", learn_rate = tune())


combinacoes <- workflow_set(
  preproc = list(receita),
  models = list("arvore" = arv_decisao,
                "knn" = knn,
                #"bag_de_arv" = bag_de_arv,
                "floresta" = floresta,
                "xg_boost" = xg_boost)
)

res2 <- combinacoes |> workflow_map(resamples = reamostragem,
                                    grid = 5,
                                    verbose = T, 
)         

autoplot(res2, metric = "roc_auc")


bd<-dadosobito
ptr=2/3 
treino<-sample(1:NROW(bd),as.integer(ptr*NROW(bd))) 
dados.treino<-bd[treino,] 
dados.teste<-bd[-treino,] 
fix(bd) 
#-----------------------------arvores de decisao-------------------------- 
library(rpart) 
arvore<-rpart(EVOLUCAO ~ .,bd) 
#funcao para visualizar a arvore 
mostra.arvore<-function(arvore){ 
  plot(arvore) 
  text(arvore,digits=3,font=6) 
} 
mostra.arvore(arvore1) 
fix(dados.treino) 
#teinar a arvore 
arvore1<-rpart(EVOLUCAO ~.,dados.treino) 
fix(dados.teste) 
#testar a arvore 
previsoes.modelo<-predict(arvore1,dados.teste) 
#matriz de confusao 
matriz.confusao<-table(dados.teste$EVOLUCAO,previsoes.modelo) 
#percentagem de erro 
perc.erro<-
  100*(matriz.confusao[1,2]+matriz.confusao[1,3]+matriz.confusao[1,4]+ 
         matriz.confusao[2,1]+matriz.confusao[2,3]+matriz.confusao[2,4]+ 
         39
       matriz.confusao[3,1]+matriz.confusao[3,2]+matriz.confusao[3,4]+ 
         matriz.confusao[4,1]+matriz.confusao[4,2]+matriz.confusao[4,3])/sum(matriz.confusao) 
avalia.arvore<-function(arv,dados.teste,objectivo=ncol(dados.teste)){ 
  prevs<-predict(arv,dados.teste) 
  #previsoes.modelo<-predict(arv,dados.teste) 
  #matriz.confusao<-
  table(dados.teste$EVOLUCAO,previsoes.modelo) 
  matriz.confusao<-
    table(dados.teste[,objectivo],predict(arv,dados.teste)) 
  erro<-
    100*sum(matriz.confusao[col(matriz.confusao)!=row(matriz.confusao)])/sum(matriz.confusao) 
  list(previsoes=prevs,matriz.confusao=matriz.confusao,perc.erro=erro) 
} 
resultados<-avalia.arvore(arvore1,dados.teste,objectivo=2) 
(mad.lm<-mean(abs(previsoes.modelo-dados.teste$EVOLUCAO)))