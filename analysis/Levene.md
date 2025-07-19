# Teste de Levene para Homogeneidade de Variâncias

## O que é o Teste de Levene?

O Teste de Levene é um teste estatístico que verifica se **variâncias são iguais** (homocedasticidade) entre diferentes grupos. É uma etapa crítica antes de realizar uma ANOVA, pois uma das suposições fundamentais da ANOVA é que os grupos comparados tenham variâncias similares.

## Código Completo
```r
leveneTest(nivel_popularidade_norm ~ popularidade_album_norm, dataset_normalizado)


listadfs <- list()

listadfs[[variaveis[1]]] <- split(dataset_normalizado, dataset_normalizado$explicito)
listadfs[[variaveis[2]]] <- split(dataset_normalizado, dataset_normalizado$acusticidade_norm)
listadfs[[variaveis[3]]] <- split(dataset_normalizado, dataset_normalizado$dancabilidade_norm)
listadfs[[variaveis[4]]] <- split(dataset_normalizado, dataset_normalizado$duracao_ms_norm)
listadfs[[variaveis[5]]] <- split(dataset_normalizado, dataset_normalizado$energia_norm)
listadfs[[variaveis[6]]] <- split(dataset_normalizado, dataset_normalizado$instrumentalidade)
listadfs[[variaveis[7]]] <- split(dataset_normalizado, dataset_normalizado$tonalidade)
listadfs[[variaveis[8]]] <- split(dataset_normalizado, dataset_normalizado$vivacidade_norm)
listadfs[[variaveis[9]]] <- split(dataset_normalizado, dataset_normalizado$intensidade_sonora_mod)
listadfs[[variaveis[10]]] <- split(dataset_normalizado, dataset_normalizado$modo)
listadfs[[variaveis[11]]] <- split(dataset_normalizado, dataset_normalizado$discursividade_norm)
listadfs[[variaveis[12]]] <- split(dataset_normalizado, dataset_normalizado$compasso)
listadfs[[variaveis[13]]] <- split(dataset_normalizado, dataset_normalizado$valencia_norm)
listadfs[[variaveis[14]]] <- split(dataset_normalizado, dataset_normalizado$genero)
listadfs[[variaveis[15]]] <- split(dataset_normalizado, dataset_normalizado$popularidade_artista_norm)
listadfs[[variaveis[16]]] <- split(dataset_normalizado, dataset_normalizado$popularidade_album_norm)

variavel <- c()
nome <- c()
tamanho <- c()
variancia <- c()
normalidade <- c()

dfResposta <- data.frame(variavel, nome, tamanho, variancia, normalidade)

for(i in 1:length(listadfs)){
  
  variavel <- names(listadfs)[i]
  nomesInternos <- names(listadfs[[i]])
  
  for(j in 1:length(listadfs[[i]])){

    nome <- nomesInternos[j]
    tamanho <- nrow(listadfs[[i]][[j]])
    variancia <- var(listadfs[[i]][[j]]$nivel_popularidade_norm)
    
    if(!is.na(variancia)){
      normalidade <- shapiro.test(listadfs[[i]][[j]]$nivel_popularidade_norm)$p.value
    } else {
      normalidade <- NA
    }
    dfPasso <- data.frame(variavel, nome, tamanho, variancia, normalidade)
    
    dfResposta <- rbind(dfResposta, dfPasso)
  }
}

dfResposta
write.csv(dfResposta, "resultados.csv")
```