install.packages("dplyr")

viagens <- read.csv(
    file = "projeto-viagem/dados/viagem_sample.csv"
    sep = ';',
  dec = ','
)

#Visualizando as primeiras linhas do dataset - verificar se dados foram carregados corretamente
head(viagens)
View(viagens)


#Resumo do dataset - valores min, max, media, mediana...
summary(viagens)

#Summary de uma coluna especifica
summary(viagens$Valor.passagens)

#Verificar tipo dos dados
library(dplyr)
glimpse(viagens)

### TRANSFORMAÇÃO DOS DADOS OBTIDOS

#Convertendo o tipo do dato para tipo Date
viagem_sample$`Período - Data de início`<- as.Date(viagem_sample$`Período - Data de início`, "%d/%m/%Y")
viagem_sample$`Período - Data de fim`<- as.Date(viagem_sample$`Período - Data de fim`, "%d/%m/%Y")

#Formatando a data de inicio - para utilizar apenas Ano/Mês
viagem_sample$data.inicio.formatada<- format(viagem_sample$`Período - Data de início`, "%Y-%m")
viagem_sample$data.inicio.formatada

### EXPLORAÇÃO DOS DADOS

#Gerando histograma da coluna passagens
hist(viagem_sample$`Valor passagens`)

#Filtrando os valores das passagens - apenas passagens entre 200 e 600
passagens_fitro <- viagens %>%
  select(Valor.passagens) %>%
  filter(Valor.passagens >= 200 & Valor.passagens <= 600)

passagens_fitro
hist(passagens_fitro$Valor.passagens)

#Verificando os valores min, max, média... da coluna valor
summary(viagens$Valor.passagens)

#Visualizando os valores em um boxplot
boxplot(viagens$Valor.passagens)

#Visualizando os valores das passagens - filtro de 200 a 600
boxplot(passagens_fitro$Valor.passagens)

#Calculando o desvio padrão
sd(viagens$Valor.passagens)

#Verificar se existem valores não preenchidos nas colunas do dataframe
colSums(is.na(viagens))

#Verifcar a quantidade de categorias da coluna Situação 
#Converter para factor
viagens$Situação <- factor(viagens$Situação)
str(viagens$Situação)

#Verificar quantidade de registros em cada categoria
table(viagens$Situação)

#Obtendo os valores em percentual de cada categoria
prop.table(table(viagens$Situação))*100

### Visualização dos resultados

##### 1 - Qual é o valor gasto por órgão em passagens?

#Criando um dataframe com os 7 órgãos que gastam mais
p1 <- viagens %>%
  group_by(Nome.do.órgão.superior) %>%
  summarise(n = sum(Valor.passagens)) %>%
  arrange(desc(n)) %>%
  top_n(7)

#Alterando o nome das colunas
names(p1) <- c("orgao", "valor")
p1

#Plotando os dados com o ggplot
#install.packages("ggplot2")
library(ggplot2)

ggplot(p1, aes(x = reorder(orgao, valor), y = valor))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x = "Valor", y = "Órgãos")

######### 2 - Qual é o valor gasto por cidade?

#Criando um dataframe com as 15 cidades que gastam mais
p2 <- viagens %>%
  group_by(Destinos) %>%
  summarise(n = sum(Valor.passagens)) %>%
  arrange(desc(n)) %>%
  top_n(15)

p2

#Alterando o nome das colunas
names(p2) <- c("destino", "valor")
p2

#Criando o gráfico
ggplot(p2, aes(x = reorder(destino, valor), y = valor))+
  geom_bar(stat = "identity", fill = "#0ba791")+
  geom_text(aes(label = valor), vjust = 0.3, size = 3)+
  coord_flip()+
  labs(x = "Destino", y = "Valor")

########## 3 - Qual é a quantidade de viagens por mês?
 
p3 <- viagens %>%
  group_by(data.inicio.formatada) %>%
  summarise(qtd = n_distinct(Identificador.do.processo.de.viagem))

head(p3)

#Criando o gráfico

