
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NAPRMN

<!-- badges: start -->
<!-- badges: end -->

## Introdução

Esta ferramenta foi desenvolvida para agrupar um conjunto de funções
auxiliares para processamento e analise de dados de RMN para
metabolômica. Este pacote funciona como complemento para outras
ferramentas já estabelecidas, sendo elas:

- ***PepsNMR***: Realiza o processamento de dados de RMN e fornece
  ferramentas para o pré-processamento dos dados
  - Adiciona uma função atualizada para visualização dos espectros,
    permitindo a visualização interativa com o auxilio do pacote plotly;
  - Implementa uma função auxiliar para alinhamento dos espectros
    utilizando o algoritmo CLUPA s com base na implementação do pacote
    speaq;
  - Implementa uma função para integração manual dos sinais;
  - Implementa uma função para conversão automatica da matriz de saida
    do PepsNMR em formato adequado (Dataframe) para ser exportado em
    xlsx ou utilizado em outras ferramentas de analise.
- ***ROPLS***: Implementa metodos multivariados de analise ao R,
  incluindo PCA, PLS-DA. OPLS-DA e os metodos de regressão equivalentes.
  - Implementa uma função voltada para facilitar a extração dos dados
    dos modelos (Scores, Loadings e VIP) no formato de dataframe,
    facilitando a plotagem dos resultados em gráficos personalizados;
  - Implementa funções extras para visualização dos scores e loadings de
    cada tipo de modelo (Ainda em processo de implementação).
- Implementa funções de normalização, escalonamento e transfomação de
  dados com base nas funções implementadas no pacote MetaboanalystR;
- Implementa uma função de limpeza de dados, voltada para remover
  variáveis constantes e não informativas dos dados a serem analisados;
- Implementa funções para uso do metodo STOSY para visualização de
  sinais correlacionados.

## Instalação do pacote

A script abaixo faz a instalação da ferramenta a partir do github, além
de fazer o download das dependências necessárias.

``` r
if (!require("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")}

if (!require("remotes", quietly = TRUE)){
  install.packages("remotes")}

BiocManager::install("Juniorrod2/NAPRMN_package")
```

## 
