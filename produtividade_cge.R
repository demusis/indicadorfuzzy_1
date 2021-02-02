library(factoextra)
library(FuzzyR)
library(lattice)
library(NbClust)
library(pracma)
library(readxl)


# Define diretório de trabalho
setwd("C:/Users/54538351172/OneDrive/CGE/Monitoramento")

# Lê dados
dados <- read_excel("TOTAIS_SCI_MONITORAMENTO.xlsx")


# Estudo do número de agrupamentos geral.
escalonados_dados <- scale(dados[,3:8])

fviz_nbclust(escalonados_dados, 
             kmeans, 
             method = "wss", k.max = 24) + 
  theme_minimal() + 
  ggtitle("Método de Elbow")


res.nbclust <- NbClust(escalonados_dados,
                       distance = "euclidean",
                       min.nc = 2, 
                       max.nc = 9, 
                       method = "complete", 
                       index ="all")
factoextra::fviz_nbclust(res.nbclust) +
  theme_minimal() + 
  ggtitle("Número de agrupamentos")


# Cria sistema fuzzy.
# a <- newfis ('hyper', fisType = " mamdani ", defuzzMethod = "centroid " )
m1_fis <- newfis('C')

# QTDE_RECOMENDACOES (3)
## Cria variável fuzzy
m1_fis <- FuzzyR::addvar(m1_fis, 
                         'input', 
                         'QTDE_RECOMENDACOES', 
                         c(min(dados[,3]), max(dados[,3])))

## Análise de agrupamentos
kmm = kmeans(dados[,3], 3, nstart = 50,iter.max = 50) 
aux <- cbind(dados[,3], kmm$cluster)
colnames(aux) <- c('v', 'c')

agregado_aux <-
  do.call(data.frame,
          aggregate(v ~ c, aux,
                    function(x) {
                      c(mn = min(x),
                        q1 = quantile(x, probs = 1/4),
                        md = median(x),
                        q1 = quantile(x, probs = 3/4),
                        mx = max(x))
                    }))
colnames(agregado_aux) <-
  c('cluster', 'minimo', 'q1', 'mediano', 'q3', 'maximo')

agregado_aux <- agregado_aux[order(agregado_aux$minimo),]
agregado_aux$cluster <- 1:nrow(agregado_aux)

## Fuzificação.
m1_fis <- addmf(m1_fis,
                'input',
                1,
                'baixo',
                'trapmf',
                c(agregado_aux[1, 'minimo'], 
                  agregado_aux[1, 'minimo'],
                  agregado_aux[1, 'q3'],
                  agregado_aux[2, 'q1'])
)

m1_fis <- addmf(m1_fis,
                'input',
                1,
                'medio',
                'trapmf',
                c(agregado_aux[1, 'q3'], 
                  agregado_aux[2, 'q1'],
                  agregado_aux[2, 'q3'],
                  agregado_aux[3, 'q1'])
)

m1_fis <- addmf(m1_fis,
                'input',
                1,
                'alto',
                'trapmf',
                c(agregado_aux[2, 'q3'], 
                  agregado_aux[3, 'q1'],
                  agregado_aux[3, 'maximo'],
                  agregado_aux[3, 'maximo']))

plotmf(m1_fis, "input" , 1)


