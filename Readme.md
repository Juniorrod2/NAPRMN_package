
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NAPRMN

<!-- badges: start -->

<!-- Add more badges as needed e.g. Travis CI, CRAN, License -->

![GitHub last
commit](https://img.shields.io/github/last-commit/Juniorrod2/NAPRMN_package)
![GitHub repo
size](https://img.shields.io/github/repo-size/Juniorrod2/NAPRMN_package)
<!-- badges: end -->

## Sobre o Pacote (PT-BR)

**NAPRMN** fornece um conjunto avançado de funções para o processamento,
normalização e análise de espectros de Ressonância Magnética Nuclear
(RMN). O pacote inclui métodos para correção de fase, alinhamento,
integração espectral, normalização de dados e análise estatística,
auxiliando estudos metabolômicos e demais aplicações que usam
espectroscopia de RMN como ferramenta analítica.

O pacote complementa ferramentas já conhecidas, como PepsNMR, ROPLS e
MetaboanalystR, oferecendo integração e funções adicionais que facilitam
o fluxo analítico.

## About the Package (EN)

**NAPRMN** provides a comprehensive set of functions for processing,
normalization, and analysis of Nuclear Magnetic Resonance (NMR) spectra.
It features advanced methods for phase correction, spectral alignment,
integration, statistical normalization, and exploratory analysis,
streamlining workflows for metabolomics and other NMR-based
applications.

## Recursos Principais / Key Features

- Correção automática de fase (`autophase`)
- Alinhamento espectral avançado (`Spectra_align`)
- Integração manual e automática de picos (`NMR_integration`)
- Normalização e limpeza de dados espectrais (`DataNormalization`,
  `CleanDataMatrix`)
- Conversão de matrizes para formatos analíticos
  (`NMRMatrixAsDataframe`)
- Visualização interativa dos espectros com plotly
  (`plot_interactive_Spectra`)
- Extração de resultados de modelos estatísticos (`extract_ropls_data`,
  `Plot_scores`, `Plot_loading`)
- Métodos de análise multivariada (PCA, PLS-DA, OPLS-DA com ROPLS)
- Implementação do STOCSY para análise de correlação espectral
  (`stocsy`, `stocsy_by_region`)

## Compatibilidade

O NAPRMN se integra diretamente com: - **PepsNMR**: para
pré-processamento de espectros RMN. - **ROPLS**: métodos multivariados,
extração facilitada dos resultados. - **MetaboanalystR**: funções para
normalização, escalonamento e transformação de dados.

## Instalação

Requer R e os pacotes BiocManager e remotes.

``` r
if (!require("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

if (!require("remotes", quietly = TRUE)){
  install.packages("remotes")
}

BiocManager::install("Juniorrod2/NAPRMN_package")
```

## Funções Disponíveis

### Pré-processamento, Limpeza e Normalização

- **CleanDataMatrix(data)**  
  Remove colunas constantes ou inválidas (NA) de uma matriz numérica.

  ``` r
  cleaned <- CleanDataMatrix(data)
  ```

- **DataNormalization(data, rowNorm, transNorm, scaleNorm, ref)**  
  Normaliza matriz de variáveis espectrais por métodos como quantile,
  PQN, sum, median e aplica transformações log/raiz, escalonamento
  autoscaling, pareto etc.

  ``` r
  norm <- DataNormalization(data, rowNorm="Quantile", scaleNorm="Autoscaling")
  ```

------------------------------------------------------------------------

### Integração e Bucketing Espectral

- **NMR_integration(Spectrum_data, integration_intervals)**  
  Integra áreas sob o espectro em regiões definidas pelo usuário (ex:
  metabolitos).

  ``` r
  region <- data.frame(metabolito=c("Leucina"),ppm_i=c(0.94),ppm_f=c(0.97))
  integrals <- NMR_integration(Spectrum_data, region)
  ```

------------------------------------------------------------------------

### Conversão e Suporte à Estrutura PepsNMR

- **NMRMatrixAsDataframe(NMR_matrix, DirNames=FALSE)**  
  Converte matriz para dataframe organizado para exportação ou análise.

------------------------------------------------------------------------

### Correção de Fase e Alinhamento Espectral

- **autophase(spectra, phasingMethod, absorptionOnly, withBC)**  
  Realiza faseamento automático dos espectros via métodos do NMRphasing.

  ``` r
  phased <- autophase(spectra)
  ```

- **compare_phasing_methods(spectra, fid_info, ref, absorptionOnly,
  withBC, phasingMethods)**  
  Compara vários métodos de faseamento em um espectro de referência.

- **Spectra_align(spectra, nDivRange_ppm, maxShifts_ppm,
  baselineThreshold, SNR.Th, show_info)**  
  Alinha espectros usando o algoritmo CluPA (speaq) para corrigir
  variações de deslocamento químico.

  ``` r
  aligned <- Spectra_align(spectra)
  ```

- **calculate_spectrum_resolution(spectra, npoints, ppm)**  
  Calcula resolução espectral ou converte entre pontos/ppm.

------------------------------------------------------------------------

### Análise Estatística Multivariada (ROPLS)

- **extract_ropls_data(ropls_object, bins_roundPrecision,
  VIP_roundPrecision)**  
  Extrai scores e loadings de modelos PCA, PLS-DA ou OPLS-DA da ropls.

- **Plot_scores(model, groups, comp, …)**  
  Plota scores dos modelos multivariados.

  ``` r
  Plot_scores(model)
  ```

- **plot_scores_overview(PCA_object, …)**  
  Gera múltiplos gráficos de scores para pares de componentes.

- **Plot_loading(model, comp, …)**  
  Plota os loadings (coeficientes de cada variável) para componentes
  selecionados.

  ``` r
  Plot_loading(model)
  ```

- **Plot_loading2(model, comp, …)**  
  Variante para quando variáveis não representam espectros contínuos.

- **plot_loading_overview(PCA_object, …)**  
  Cria gráficos de loading para múltiplos componentes.

- **plot_loading_lines(model, comp, interactive, VIP_values_scale,
  theme)**  
  Gráficos de barras para loadings, coloridos por VIP quando aplicável.

- **plot_slines(TrainingData, model, comp, interactive)**  
  Plota S-lines (covariância/correlação) entre variáveis e scores, útil
  para interpretar contribuição de variáveis nas análises.

------------------------------------------------------------------------

### Visualização Interativa de Espectros

- **plot_interactive_spectra(Spectrum_data, plot_resolution,
  limit_n_points, plot_only, Spectrum_window)**  
  Gera gráficos interativos dos espectros de RMN.

  ``` r
  plot_interactive_spectra(Spectrum_data)
  ```

- **plot_spectra(Spectrum_data, plot_resolution, …)**  
  Similar ao anterior, gera gráficos estáticos/flexíveis.

- **overlaid_bins_plot(full_spectra, bin_data, bin_width, …)** Combina
  visualização do espectro completo com intensidades de bins.

- **plot_raster_spectrum(spec_data, plot_region, spec_resolution,
  plot_resolution)** Cria um gráfico raster (imagem) dos espectros para
  inspeção de padrões globais/anomalias.

  ``` r
  plot_raster_spectrum(spec_data)
  ```

------------------------------------------------------------------------

### STOCSY (Statistical Total Correlation Spectroscopy)

- **stocsy(spectra, driver_peak, mode, ref_spectrum)**  
  Gera gráfico interativo STOCSY correlacionando o sinal de referência
  com todos os demais.

  ``` r
  stocsy(spectra, driver_peak=0.945)
  ```

- **stocsy_by_region(spectra, driver_peak, mode, spectrum_resolution,
  ref_spectrum)**  
  Variante do STOCSY para regiões específicas do espectro.

------------------------------------------------------------------------

## Exemplos de Fluxo de Trabalho

``` r
# 1. Limpeza e Normalização
cleaned <- CleanDataMatrix(raw_data)
normed <- DataNormalization(cleaned, rowNorm="Quantile", scaleNorm="Autoscaling")

# 2. Integração
region <- data.frame(metabolito=c("Leucina"), ppm_i=c(0.94), ppm_f=c(0.97))
integrals <- NMR_integration(Spectrum_data, region)

# 3. Correção de Fase e Alinhamento
phased <- autophase(spectra)
aligned <- Spectra_align(phased)

# 4. Visualização Interativa
plot_interactive_spectra(aligned)
plot_raster_spectrum(aligned, plot_region = c(0.5, 4))

# 5. Análise Multivariada
model <- ropls::opls(normed, Y, predI=2) # Modelo PLS-DA, por exemplo
Plot_scores(model)
Plot_loading(model)
plot_slines(normed, model)
```

------------------------------------------------------------------------

## Contribuições

Contribuições são bem-vindas! Sinta-se livre para abrir issues e pull
requests.

## Licença

<!-- Se houver uma licença, adicione aqui. Exemplo: -->

Este projeto está licenciado sob a Licença MIT - veja o arquivo LICENSE
para detalhes.

------------------------------------------------------------------------

*Para suporte ou dúvidas técnicas, contate o mantenedor do pacote via
GitHub.*
