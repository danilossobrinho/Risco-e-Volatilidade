# Contextualização  
Risco e Volatilidade são grandezas similares, que podem muita das vezes serem interpretadas da mesma forma. Ambas são calculadas apartir dos valores dos retornos de ativos de renda variável. Os retornos podem ser obtidos por valores diários, semanais, mensais, tudo dependendo do objetivo da pesquisa, nesse cenário o calculo do Risco satisfaz. Outro caso muito comum nos mercados de bolsas, são as chamadas cotações de alta frequência, ativos que  podem ter as suas cotações negociadas em um curtíssimo espaço de tempo, de um em um minuto ou qualquer outro intervalo de tempo contínuo.  
Neste relatório, vamos entender como podemos observar os valores de um ativo, fazer o cálculo dos retornos (contínuos e discretos), gerar as medidas de retorno médio e observar os seus riscos e volatilidades *com o R*

# Carregando os pacotes

```r
library(dplyr)
library(stringr)
library(ggplot2)
library(matrixStats)
library(tidyquant)
library(ggcorrplot)
```

Caso não tenha os pacotes necessários, é preciso baixa-los pelo pelo comando ``install.packages("nome do pacote")``  

# Preparando os dados
## Selecionando as ações do mercado de bolsas
Um passo fundamental é definir a carteira de ações que será estudada. Será escolhido 4 produtos financeiros de forma aleatória e especificamente didática, não há nenhum tipo de viés ou intenção de recomendação destes ativos.  
  
Definindo os produtos e o período  

```r
produtos <- c('ITSA4.SA', 'TRIS3.SA', 'CVCB3.SA', 'GFSA3.SA')
data_ini <- '2022-02-01'
data_fin <- '2022-12-14'
```
  
Utilizando os objetos criados anteriormente, e as funções ``getSymbols`` (função que busca os valores dos ativos no [Yahoo Finance](https://finance.yahoo.com/) relacionando com os stickers armazenados em **produtos**) e ``tq_get`` (função que transforma os dados capturados no [Yahoo Finance](https://finance.yahoo.com/) em formato tibble com os resultados empilhados), escrevemos o scrip:

```r
getSymbols(produtos,
           from = data_ini,
           to = data_fin)
precos <- tq_get(produtos,
                 from = data_ini,
                 tp = data_fin,
                 get = "stock.prices")
```


  
Foram gerados 5 objetos, sendo 4 deles, tabelas com os valores das ações, para cada ativo, durante um dia de operação da bolsa de valores. Outra tabela que o código nos deu, foi definida como **precos**, será esta que manipularemos para a obtenção dos retornos.
  
  
  
  
Uma visão geral dos valores de abertura, maior valor diário, menor valor diário e valor de fechamento, praticados no início do período em estudo

```
##            CVCB3.SA.Open CVCB3.SA.High CVCB3.SA.Low CVCB3.SA.Close
## 2022-02-01         14.40         14.67        14.12          14.28
## 2022-02-02         14.35         14.54        13.35          13.43
## 2022-02-03         13.48         13.98        13.16          13.71
```

```
##            GFSA3.SA.Open GFSA3.SA.High GFSA3.SA.Low GFSA3.SA.Close
## 2022-02-01         18.45         18.81        18.09          18.45
## 2022-02-02         18.36         19.35        18.36          18.63
## 2022-02-03         18.54         19.44        18.27          19.26
```

```
##            ITSA4.SA.Open ITSA4.SA.High ITSA4.SA.Low ITSA4.SA.Close
## 2022-02-01          9.25          9.33         9.23           9.28
## 2022-02-02          9.29          9.29         9.08           9.13
## 2022-02-03          9.15          9.21         9.05           9.15
```

```
##            TRIS3.SA.Open TRIS3.SA.High TRIS3.SA.Low TRIS3.SA.Close
## 2022-02-01          6.38          6.61         6.38           6.38
## 2022-02-02          6.38          6.52         6.32           6.41
## 2022-02-03          6.41          6.60         6.25           6.57
```
A seguir, um gráfico demonstando o valor de fechamento das ações ao longo do período estudado, neste é possível observar se as ações segue uma tendência de valorização, desvalorização ou estabilização.
![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
  
  
## Manipulando as tabelas
Nesta pesquisa, usaremos o valor de fechamento ajustado (coluna *adjusted* da tabela **precos**) . A nossa tabela então será composta por 5 colunas, sendo a primeira a data, e as demais o valor de interesse, para isso usaremos o script a seguir:


```r
fech <- precos %>% filter(precos$symbol==produtos[1]) %>% select(date, adjusted)

for ( i in 2:length(produtos)){
  fechaux <- precos %>% filter(precos$symbol==produtos[i]) %>% select(adjusted)
  fech <- bind_cols(fech, fechaux)
}

fech[2:5] <- round(fech[2:5], 2)
names(fech)[2:5] <- produtos
```



|date       | ITSA4.SA| TRIS3.SA| CVCB3.SA| GFSA3.SA|
|:----------|--------:|--------:|--------:|--------:|
|2022-02-01 |     8.76|     6.03|    14.28|    18.45|
|2022-02-02 |     8.62|     6.06|    13.43|    18.63|
|2022-02-03 |     8.64|     6.21|    13.71|    19.26|
|2022-02-04 |     8.82|     5.74|    13.01|    18.45|
|2022-02-07 |     8.70|     5.55|    12.81|    18.45|
|2022-02-08 |     8.71|     5.49|    13.06|    19.17|
|2022-02-09 |     8.57|     5.30|    13.52|    18.99|
|2022-02-10 |     8.66|     5.28|    13.85|    18.72|
|2022-02-11 |     9.03|     5.24|    13.20|    18.18|
|2022-02-14 |     9.00|     5.22|    13.35|    17.91|
  
  
## Criando tabelas defasadas para calculo dos Retornos
Para quem ja estudou variáveis em estatística, deve esta familizarizado com o termo "variáveis discretas" e "variáveis contínuas", a primeira são valores de fácil mesuração, como número de filhos, idade, ou número de dormitórios em uma casa, já as variáveis contínuas podem assumir qualquer valor, portando a abordagem em análises deve ser diferentes.  
Retornos discretos e contínuos seguem a mesma lógica das variáveis discretas e contínuas, e isso veremos nos próximos tópicos.
  
Para podermos calcularmos os retornos, precisamos criar uma tabela defasada. Uma tabela defasada nada mais é do que uma tabela com a ausência do primeiro valor. Nos proximos tópicos, ficará claro o porque devemos executar este passo, segue o script:

```r
fech <- na.omit(fech)
fechlag <- lag(fech)
fechlag1 <- fechlag[-1,-1]
fech1 <- fech[-1,-1]
```

# Retornos Discretos e Risco
Agora iremos calcular os retornos discretos, aqueles que podem ser calculados caso sua variação seja diária, semanal ou mensal. É atravez dos retornos discrtetos que podemos obter o valor do Risco da ação. A fórmula é a seguinte (Onde "R" é o retorno e "P" o preço da ação):
$$R_t= \frac{P_t}{P_{t-1}}-1$$

E é aqui que se faz importante a criação das tabelas defasadas, pois de acordo com a fórmula acima, precisamos dividir o valor de um período pelo valor do período anterior, após isso subtrair uma unidade, e nossas tabelas **fech1** e **fechlag1** estão exatamente assim. A posição das suas observações estão sempre uma linha de diferença entre uma tabela e outra (para isso foi necessário remover a ultima linha de **fech1** e a primeira linha de **fechlag1**.  
  
Veja o script, usando as tabelas citadas acima e a fórmula apresentada:

```r
retorno.dis <- as.matrix(fech1/fechlag1 - 1)
retorno.dis <- as.data.frame(retorno.dis)
```

|   ITSA4.SA|   TRIS3.SA|   CVCB3.SA|   GFSA3.SA|
|----------:|----------:|----------:|----------:|
| -0.0159817|  0.0049751| -0.0595238|  0.0097561|
|  0.0023202|  0.0247525|  0.0208488|  0.0338164|
|  0.0208333| -0.0756844| -0.0510576| -0.0420561|
| -0.0136054| -0.0331010| -0.0153728|  0.0000000|
|  0.0011494| -0.0108108|  0.0195160|  0.0390244|

Veja que como qualquer ação analisada em um curto período, elas possuem variações positivas e negativas, e o risco é calculado apartir da amplitude dessas variações. Se um produto financeiro varia mais vezes de forma positiva, podemos concluir que este produto tem um retorno médio positivo. 

```r
retorno.dis.med <- mean(retorno.dis[,1])

for(i in 2:length(produtos)){
  retorno.dis.med.aux <- mean(retorno.dis[,i])
  retorno.dis.med <- bind_cols(retorno.dis.med, retorno.dis.med.aux)
}
names(retorno.dis.med) <- produtos

Risco <- sd(retorno.dis[,1])*100

for (i in 2:length(produtos)){
  Riscoaux <- sd(retorno.dis[,i])*100
  Risco <- bind_cols(Risco, Riscoaux)
}
names(Risco) <- produtos

RiscRetor <- rbind(Risco, retorno.dis.med)
RiscRetor <- bind_cols(c('Risco', 'Retorno médio'), RiscRetor)
RiscRetor <- as.data.frame(RiscRetor)
row.names(RiscRetor) <- RiscRetor$...1
RiscRetor <- RiscRetor[-1]
```
  
  
Segue o nosso retorno médio, e pelos resultados, não está muito animador, quem investiu nessas ações nesse período teve um pequeno prejuíso.  
O Risco está apresentado em valor percentual, e podemos notar que o risco dos ativos desta carteira é baixo. Bom mas se o risco é baixo, porque eu tive prejuíso?  
O risco baixo não garante que os retornos de um ativo financeiro irá ser positivo. O baixo risco apenas garante que a ação em quetão não tem uma variação alta no seus valores de mercado, portanto, podemos concluir que baixos riscos significam baixos retornos ou baixos prejuísos, como foi o caso.

|              |   ITSA4.SA|   TRIS3.SA|   CVCB3.SA|   GFSA3.SA|
|:-------------|----------:|----------:|----------:|----------:|
|Risco         |  1.5573293|  3.3236077|  4.8029911|  4.1985494|
|Retorno médio | -0.0002657| -0.0022993| -0.0050624| -0.0045026|

# Retornos Contínuos e Volatilidade
O Retorno contínuo é obtido através dos logarítimos da divisão dos preços do período P~t~ e P~t-1~. É calculado desta forma pois as variações acontecem em períodos muito curto, e teoricamente, P^n^ tende ao infinito. Segue a formula:
$$R_{continuo}=ln\frac{P_t}{P_{t-1}}$$
  
  
  
Segue o valor dos retornos contínuos, há de se observar que os valores são muito semelhantes com o obtidos pelo método dos retornos discretos, pois foram usados os mesmos banco de dados com o período de fechamente relativamente longo para o método (fechamentos diários):

```r
retorno.con <- as.matrix(log(fech1/fechlag1))
retorno.con <- as.data.frame(retorno.con)
```

|   ITSA4.SA|   TRIS3.SA|   CVCB3.SA|   GFSA3.SA|
|----------:|----------:|----------:|----------:|
| -0.0161108|  0.0049628| -0.0613689|  0.0097088|
|  0.0023175|  0.0244511|  0.0206345|  0.0332572|
|  0.0206193| -0.0787017| -0.0524072| -0.0429660|
| -0.0136988| -0.0336613| -0.0154922|  0.0000000|
|  0.0011488| -0.0108697|  0.0193280|  0.0382822|
  
A única diferença em comparação ao calculo dos Retornos discretos ja foi exposta acima, portanto, os proximos passos são praticamente os mesmos, segue o script:
  

```r
retorno.con.med <- mean(retorno.con[,1])

for(i in 2:length(produtos)){
  retorno.con.med.aux <- mean(retorno.con[,i])
  retorno.con.med <- bind_cols(retorno.con.med, retorno.con.med.aux)
}
names(retorno.con.med) <- produtos

Volatilidade <- sd(retorno.dis[,1])*100

for(i in 2:length(produtos)){
  Volatilidadeaux <- sd(retorno.dis[,i])*100
  Volatilidade <- bind_cols(Volatilidade, Volatilidadeaux)
}
names(Volatilidade) <- produtos

VolatRetor <- rbind(Volatilidade, retorno.con.med)
VolatRetor <- bind_cols(c('Volatilidade', 'Retorno médio'), VolatRetor)
VolatRetor <- as.data.frame(VolatRetor)
row.names(VolatRetor) <- VolatRetor$...1
VolatRetor <- VolatRetor[-1]
```

Sem muitas mudanças em comparação com o Retorno contínuo e o Risco, os resultados apresentados foram praticamentes iguais, isso se dá pelo fato do uso da mesma carteira e pela periodicidade dos fechamentos .

|              |  ITSA4.SA|   TRIS3.SA|   CVCB3.SA|   GFSA3.SA|
|:-------------|---------:|----------:|----------:|----------:|
|Volatilidade  |  1.557329|  3.3236077|  4.8029911|  4.1985494|
|Retorno médio | -0.000386| -0.0028505| -0.0062164| -0.0053956|
