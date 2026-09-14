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
cp .Renviron.example .Renviron
```

2. Generer une cle JWT :

```bash
openssl rand -hex 32
```

3. Coller la valeur dans `JWT_SECRET` du fichier `.Renviron`.
4. Renseigner `CYAN_ROOT_DIR` (dossier de travail de l'API).


## Lancement

```r
source("plumber_entrypoint.R")
```

L'API ecoute par defaut sur `http://localhost:8000`.

## Endpoints

| Methode | Route | Description |
|---------|-------|-------------|
| POST | `/upload` | Envoie un fichier Excel et retourne un cookie JWT |
| GET  | `/analysis` | Lance le pre-traitement + modelisation |
| GET  | `/download` | Telecharge l'archive ZIP des resultats |

### Exemple

```bash
# Upload
curl -X POST "http://localhost:8000/upload?file=/chemin/vers/data.xlsx" -c cookies.txt

# Analyse
curl "http://localhost:8000/analysis?methods=rf,mlr&var=CYAN&offset=2" -b cookies.txt

# Telechargement
curl "http://localhost:8000/download" -b cookies.txt -o results.zip
```

## Structure

```
.
├── plumber.R              # Definition de l'API (endpoints)
├── ALLFONCTIONS.R         # Moteur de modelisation
├── plumber_entrypoint.R   # Script de lancement
├── .Renviron.example      # Modele de configuration
├── .gitignore
└── README.md
```

## Limitations connues

- Les variables globales (`<<-`) rendent l'API non reentrante : deux requetes
  simultanees peuvent interferer. Un refactor vers un stockage par session est
  recommande pour un usage en production.
- La recherche exhaustive des combinaisons de variables est en O(2^n).
  Limiter le nombre de variables significatives pour eviter une explosion
  combinatoire.

## Licence

A definir.