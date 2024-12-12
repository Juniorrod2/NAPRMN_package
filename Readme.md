
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NAPRMN

<!-- badges: start -->
<!-- badges: end -->

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

## Example (Em construção)

This is a basic example which shows you how to solve a common problem:

``` r
library(NAPRMN)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.
