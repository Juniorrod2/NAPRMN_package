Readme
================
Núcleo de Análises e Pesquisa em Ressonância Magnética Nuclear

## Introdução

Esta ferramenta foi desenvolvida para agrupar um conjunto de funções
auxiliares para processamento e analise de dados de RMN para
metabolômica. Este pacote funciona como complemento para outras
ferramentas já estabelecidas, sendo elas:

- ***PepsNMR***: Adciona uma função atualizada para visualização dos
  espectros, permitindo a visualização interativa com o auxilio do
  pacote plotly, além de implementar funções auxiliares para alinhamento
  dos espectros utilizando o algoritmo CLUPA (spectra_aling()) com base
  na implementação do pacote speaq e funções para integração manual dos
  sinais;
- ***ROPLS***: Implementa funcionalidades para facilitar a extração dos
  dados dos modelos no formato de dataframe, facilitando a plotagem dos
  resultados em gráficos personalizados;
- Implementa funções de normalização, escalonamento e transfomação de
  dados com base nas funções implementadas no pacote MetaboanalystR.

## Instalação do pacote

A script abaixo faz a instalação da ferramenta a partir do github, além
de fazer o dowload das dependências necessárias.

``` r
if (!require("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")}

if (!require("remotes", quietly = TRUE)){
  install.packages("remotes")}

BiocManager::install("Juniorrod2/NAPRMN_package")
```
