# Point d'entree pour lancer l'API
library(plumber)

pr <- plumber::plumb("plumber.R")
pr$run(host = "0.0.0.0", port = as.integer(Sys.getenv("PORT", 8000)))