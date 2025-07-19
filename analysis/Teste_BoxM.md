# Teste de Levene para Homogeneidade de Variâncias

## O que é o Teste de Levene?
O Teste M de Box é um teste estatístico multivariado utilizado para verificar a igualdade de múltiplas matrizes de variância-covariância. Em resumo, o teste de Box é crucial para avaliar a homogeneidade das matrizes de covariância antes de aplicar outras análises multivariadas.

## Código Completo
```r

data_clean$pop_categoria <- cut(
    data_clean$nivel_popularidade_norm, 
    breaks = quantile(data_clean$nivel_popularidade_norm, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
    include.lowest = TRUE, 
    labels = c("Baixa", "Média", "Alta")
)

# Verifique a distribuição das observações por categoria
table(data_clean$pop_categoria)
```
