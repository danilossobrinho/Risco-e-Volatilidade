#Pacotes necessários

pacotes <- c('dplyr', 'stringr', 'ggplot2', 'matrixStats', 'tidyquant',
             'ggcorrplot', 'cowplot')

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Pacotes usados para uso do RMarkdown
install.packages("knitr")
library(knitr)
install.packages("tinytex")
tinytex::install_tinytex()

#Selecionando as ações da bolsa a serem analisadas
produtos <- c('ITSA4.SA', 'TRIS3.SA', 'CVCB3.SA', 'GFSA3.SA')


#definição de uma data inicial e final
data_ini <- '2022-02-01'
data_fin <- '2022-12-14'

#Carregando os valores de abertura, fechamento, volume entre outros
getSymbols(produtos,
           from = data_ini,
           to = data_fin)
precos <- tq_get(produtos,
                 from = data_ini,
                 tp = data_fin,
                 get = "stock.prices")

#Obsservando os preços dos ativos escolhidos
head(CVCB3.SA[,-5:-6], 3)
head(GFSA3.SA[,-5:-6], 3)
round(head(ITSA4.SA[,-5:-6], 3),2)
head(TRIS3.SA[,-5:-6], 3)

# gráfico da variação de preços dos ativos do nosso estudo
par(mfrow=c(2,2))
barplot(CVCB3.SA$CVCB3.SA.Adjusted, main = 'CVCB3')
barplot(GFSA3.SA$GFSA3.SA.Adjusted, main = 'GFSA3')
barplot(ITSA4.SA$ITSA4.SA.Adjusted, main = 'ITSA4')
barplot(TRIS3.SA$TRIS3.SA.Adjusted, main = 'TRIS3')

#Definindo a variável data como "date"
precos$date <- as.Date(precos$date, "%d/%m,%Y")

#Criando tabela apenas com os valores de fechamento ajustado para cada produto financeiro analisado
fech <- precos %>% filter(precos$symbol==produtos[1]) %>% select(date, adjusted)

for ( i in 2:length(produtos)){
  fechaux <- precos %>% filter(precos$symbol==produtos[i]) %>% select(adjusted)
  fech <- bind_cols(fech, fechaux)
}

fech[2:5] <- round(fech[2:5], 2)

#Criando as tabelas defasadas para o calculo dos retornos dos ativos
names(fech)[2:5] <- produtos
fech <- na.omit(fech)
fechlag <- lag(fech)
fechlag1 <- fechlag[-1,-1]
fech1 <- fech[-1,-1]

############################---RISCO---########################
retorno.dis <- as.matrix(fech1/fechlag1 - 1)
retorno.dis <- as.data.frame(retorno.con)
retorno.dis.med <- mean(retorno.dis[,1])

for(i in 2:length(produtos)){
  retorno.dis.med.aux <- mean(retorno.dis[,i])
  retorno.dis.med <- bind_cols(retorno.dis.med, retorno.dis.med.aux)
}
names(retorno.dis.med) <- produtos

Risco <- sd(retorno.dis[,1])

for (i in 2:length(produtos)){
  Riscoaux <- sd(retorno.dis[,i])
  Risco <- bind_cols(Risco, Riscoaux)
}
names(Risco) <- produtos

RiscRetor <- rbind(Risco, retorno.dis.med)
RiscRetor <- bind_cols(c('Risco', 'Retorno médio'), RiscRetor)
RiscRetor <- as.data.frame(RiscRetor)
row.names(RiscRetor) <- RiscRetor$...1
RiscRetor <- RiscRetor[-1]
View(RiscRetor)

############################---VOLATILIDADE---########################
retorno.con <- as.matrix(log(fech1/fechlag1))
retorno.con <- as.data.frame(retorno.con)
retorno.con.med <- mean(retorno.con[,1])

for(i in 2:length(produtos)){
  retorno.con.med.aux <- mean(retorno.con[,i])
  retorno.con.med <- bind_cols(retorno.con.med, retorno.con.med.aux)
}
names(retorno.con.med) <- produtos

Volatilidade <- sd(retorno.dis[,1])

for(i in 2:length(produtos)){
  Volatilidadeaux <- sd(retorno.dis[,i])
  Volatilidade <- bind_cols(Volatilidade, Volatilidadeaux)
}
names(Volatilidade) <- produtos

VolatRetor <- rbind(Volatilidade, retorno.con.med)
VolatRetor <- bind_cols(c('Volatilidade', 'Retorno médio'), VolatRetor)
VolatRetor <- as.data.frame(VolatRetor)
row.names(VolatRetor) <- VolatRetor$...1
VolatRetor <- VolatRetor[-1]
View(VolatRetor)
