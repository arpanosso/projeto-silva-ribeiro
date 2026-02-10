
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Análise Terra Preta - Silva Ribeiro

## Carregando Pacotes

``` r
library(tidyverse)
library(readxl)
library(vegan)
```

## Importar o BD do excel

``` r
df <- read_xlsx("data-raw/dados_terrapreta.xlsx") |> 
  rename(if_var = IF) |> 
  janitor::clean_names()
```

## Salvando na Versão de R

``` r
write_rds(df,"data/terra-preta.rds")
```

## Carregando o Banco de Dados do R

``` r
data_set <- read_rds("data/terra-preta.rds")
glimpse(data_set)
#> Rows: 40
#> Columns: 19
#> $ tratamentos <chr> "MTA", "MTA", "MTA", "MTA", "MTA", "MTA", "MTA", "MTA", "M…
#> $ ds          <dbl> 1.179139, 1.209172, 1.223709, 1.279122, 1.222785, 1.211284…
#> $ pt          <dbl> 0.4850923, 0.4719773, 0.4656292, 0.4414315, 0.4660326, 0.4…
#> $ mic         <dbl> 0.2553810, 0.2348952, 0.2427746, 0.2404692, 0.2433800, 0.2…
#> $ mac         <dbl> 0.22971136, 0.23708213, 0.22285460, 0.20096234, 0.22265261…
#> $ ksat        <dbl> 13.170000, 18.560000, 10.970000, 10.000000, 13.175000, 11.…
#> $ if_var      <dbl> 62.85714, 79.36508, 47.94521, 74.64789, 66.20383, 61.61321…
#> $ dmp         <dbl> 3.117068, 3.935556, 2.773950, 2.961878, 3.197113, 2.437374…
#> $ iee         <dbl> 8.564444, 9.031176, 8.579939, 8.512417, 8.671994, 8.704527…
#> $ n           <dbl> 0.1590000, 0.1710000, 0.1610000, 0.1580000, 0.1622500, 0.1…
#> $ s           <dbl> 0.05960000, 0.07550000, 0.06170000, 0.06380000, 0.06515000…
#> $ p_h         <dbl> 4.800000, 5.100000, 5.100000, 5.100000, 5.025000, 5.220398…
#> $ ctc         <dbl> 159.3300, 145.9300, 169.2400, 156.8400, 157.8350, 158.7280…
#> $ v_percent   <dbl> 0.6171468, 0.6573700, 0.6927440, 0.6812038, 0.6621161, 0.7…
#> $ mos         <dbl> 60.00000, 57.00000, 56.00000, 62.00000, 58.75000, 60.35167…
#> $ p           <dbl> 203.00000, 184.00000, 209.00000, 163.00000, 189.75000, 187…
#> $ k           <dbl> 0.8000000, 0.7000000, 0.8000000, 0.7000000, 0.7500000, 0.6…
#> $ ca          <dbl> 87.00000, 86.00000, 105.00000, 96.00000, 93.50000, 103.023…
#> $ mg          <dbl> 10.530000, 9.230000, 11.440000, 10.140000, 10.335000, 10.6…
```

## Estatística Descritiva

### Hitograma

``` r
nome_vars <- data_set |> select(-tratamentos) |> names()
walk(nome_vars,~{
  plot_hist <- data_set |> 
  ggplot(aes(x=!!sym(.x), y= ..density..)) + 
  geom_histogram(color="black", fill="aquamarine4",
                 bins = 12) +
  theme_minimal()
  print(plot_hist)
})
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-10.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-6-18.png)<!-- -->

### Boxplot

``` r
walk(nome_vars,~{
  plot_box <- data_set |> 
    ggplot(aes(x=tratamentos, y= !!sym(.x),fill=tratamentos)) + 
    geom_boxplot() +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(x="Tratamentos")
  print(plot_box)
})
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-10.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-18.png)<!-- -->

### Tabela da Estatistica Descriva

``` r
estat_names <- c("Min","Q1","Med","Media","Q3",
                 "Max","DP","CV","Skn","Krt")
estat_desc <- function(x){
  x<-na.omit(x)
  m <- mean(x,na.rm = TRUE)
  md <- median(x)
  mini <- min(x,na.rm = TRUE)
  q1 <- quantile(x,.25)
  q3 <- quantile(x,.75)
  maxi <- max(x,na.rm = TRUE)
  dp <- sd(x,na.rm = TRUE)
  cv <- 100*dp/m
  ass <- agricolae::skewness(x)
  curt <- agricolae::kurtosis(x)
  c(mini,q1,md,m,q3,maxi,dp,cv,ass,curt)
}

data_set |> 
  group_by(tratamentos) |> 
  reframe( across(
    .cols = ds:mg,
    .fns = estat_desc,
    .names = "{.col}"
  )) |>
  ungroup() |> 
  add_column(estat = rep(estat_names,4)) |> 
  relocate(estat) |> 
  writexl::write_xlsx("output/estatistica-descritiva.xlsx")
```

### Matrix de Correlação com o Corrplot - Por tratamentos

``` r
data_set |> 
  filter(tratamentos == "LAR") |> 
  select(ds:mg) |> 
  cor(use = "complete.obs") |> 
  corrplot::corrplot( method = "color",
         outline = T,,
         addgrid.col = "darkgray",cl.pos = "r", tl.col = "black",
         tl.cex = 1, cl.cex = 1, type = "upper", bg="azure2",
         diag = FALSE,
         # addCoef.col = "black",
         cl.ratio = 0.2,
         cl.length = 5,
         number.cex = 0.8)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
data_set |> 
  filter(tratamentos == "MAM") |> 
  select(ds:mg) |> 
  cor(use = "complete.obs") |> 
  corrplot::corrplot( method = "color",
         outline = T,,
         addgrid.col = "darkgray",cl.pos = "r", tl.col = "black",
         tl.cex = 1, cl.cex = 1, type = "upper", bg="azure2",
         diag = FALSE,
         # addCoef.col = "black",
         cl.ratio = 0.2,
         cl.length = 5,
         number.cex = 0.8)
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
data_set |> 
  filter(tratamentos == "MTA") |> 
  select(ds:mg) |> 
  cor(use = "complete.obs") |> 
  corrplot::corrplot( method = "color",
         outline = T,,
         addgrid.col = "darkgray",cl.pos = "r", tl.col = "black",
         tl.cex = 1, cl.cex = 1, type = "upper", bg="azure2",
         diag = FALSE,
         # addCoef.col = "black",
         cl.ratio = 0.2,
         cl.length = 5,
         number.cex = 0.8)
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
data_set |> 
  filter(tratamentos == "QAF") |> 
  select(ds:mg) |> 
  cor(use = "complete.obs") |> 
  corrplot::corrplot( method = "color",
         outline = T,,
         addgrid.col = "darkgray",cl.pos = "r", tl.col = "black",
         tl.cex = 1, cl.cex = 1, type = "upper", bg="azure2",
         diag = FALSE,
         # addCoef.col = "black",
         cl.ratio = 0.2,
         cl.length = 5,
         number.cex = 0.8)
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- --> \## Análise
Multivariada

### Análise de Cluster

### Análise de Componentes Principais

### Análise de Redundância

### PLS - Partial Least Squares (mínimos quadrados parciais)
