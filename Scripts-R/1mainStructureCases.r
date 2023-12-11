#Vetores 

#Criando um vetor
?c()

cidade <-c("Brasília",
           "São Paulo",
           "Rio de Janeiro",
           "Porto Alegre")

#Visualizando os dados do vetor
cidade

temperatura <- c(32,22,35,17)

regiao <- c(1,2,2,3)



#Acessando o primeiro elemento
cidade[1]

#Acessando um intervalo de elementos
temperatura[1:3]

#Copiando um vetor
cidade2 <- cidade
cidade2

#Excluindo o segundo elemento da consulta
temperatura[-2]

#Altereando um vetor
cidade2[3] <- "Belo Horizonte"

#Adicionando um novo elemento
cidade2[5] <- "Curitiba"
cidade2 

#Deletando o vetor
cidade2 <- NULL
cidade2

#Fatores 
?factor

UF <- factor(c("DF", "SP", "RJ", "RS"))
UF


grau.instrucao <- factor(c("Nível Médio",
                           "Superior",
                           "Nivel Médio",
                           "Fundamental"),
                         levels = c("Fundamental",
                                    "Nivel Médio",
                                    "Superior"),
                         ordered = TRUE)

grau.instrucao


#Listas
?list()

pessoa <- list(sexo = "M", cidade = "Brasília", idade = 20)

pessoa

#Acessando o primeiro elemento da lista
pessoa[1]

#Acessando o valor do primeiro elemento da lista
pessoa[[1]]

#Editando a lista
pessoa[["idade"]] <- 22
pessoa

#Deletando elemento da lista
pessoa[["idade"]] <- NULL
pessoa

#Criando novamente a lista
pessoa <- list(sexo = "M", cidade = "Bras?lia", idade = 20)

#Filtrando elementos da lista
pessoa[c("cidade", "idade")]

#Lista de listas
cidades <- list(cidade = cidade,
               temperatura = temperatura,
               regiao = regiao)
cidades


#Criando um data frame com vetores

df <- data.frame(cidade,temperatura)
df

df2 <- data.frame(cidades)
df2

#Filtrando valores do data frame
#Recuperando o valor da linha 1, coluna 2
df[1,2]

#Recuperando todas as linhas da primeira coluna
df[, 1]

#Recuperando a Primeira linha e todas as colunas
df[1,]

#Selecionando as 3 primeiras linhas da primeira e ultima coluna
df2[c(1:3), c(1,3)]


#Verificando o nome das colunas
names(df)

#Verificando numero de linhas x colunas
dim(df)

#Verificando os tipos de dados
str(df)


#Acessar uma coluna do dataframe
df$cidade
df['cidade']

?matrix()

#Matrizes

#Criando uma matriz
m <- matrix(seq(1:9), nrow = 3)
m

m2 <- matrix(seq(1:25), 
             ncol = 5,
             byrow = TRUE,
             dimnames = list(c(seq(1:5)),
                             c('A','B','C','D','E'))
             )
m2

#Filtrando a matriz
m2[c(1:2), c("B","C")]