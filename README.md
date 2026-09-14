# CYAN PREDICTION API

API Plumber en R pour la prediction de la variable **CYAN** a partir de
donnees Excel, avec quatre algorithmes de machine learning : Random Forest,
Regression Lineaire Multiple, Reseau de Neurones et KNN.

## Installation

```r
install.packages(c(
  "plumber", "tidyr", "Metrics", "randomForest", "neuralnet",
  "foreach", "doParallel", "openxlsx", "ggplot2", "gridExtra",
  "zip", "htmltools", "openssl", "jose", "corrplot", "caret"
))
```

## Configuration

1. Copier `.Renviron.example` en `.Renviron` :

```bash
cp .