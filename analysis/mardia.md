# Teste de Mardia para Normalidade Multivariada

## O que é Teste de Mardia?
O Teste de Mardia avalia a normalidade conjunta de um conjunto de variáveis através de duas medidas: assimetria multivariada (skewness) e curtose multivariada (kurtosis). Enquanto testes univariados avaliam cada variável isoladamente, o teste de Mardia considera as relações entre elas, capturando padrões de dependência não-lineares. Essa análise é essencial antes de técnicas como Análise Discriminante ou MANOVA, que assumem normalidade multivariada nos dados.

## Código Completo
```r
if (!require("MVN")) install.packages("MVN")
library(MVN)
variaveis_selecionadas <- c(
   "nivel_popularidade_norm",
  "dancabilidade_norm", 
  "duracao_ms_norm",
  "energia_norm",
  "vivacidade_norm",
  "discursividade_norm",
  "popularidade_album_norm",
  "acusticidade_norm",
  "intensidade_sonora_mod_norm",
  "valencia_norm",
  "ordem_no_album_norm"
)

dataset_filtrado <- dataset[, variaveis_selecionadas]
dataset_filtrado <- dataset_filtrado[, colnames(dataset_filtrado) != "nivel_popularidade_norm"]
mardia_result <- mvn(data = dataset_filtrado, mvnTest = "mardia")
print(mardia_result$multivariateNormality)
```
