# ============================================================================
#  API PLUMBER - PREDICTION CYAN
#  Endpoints : /upload, /analysis, /download
# ============================================================================

library(class)
library(tidyr)
library(Metrics)
library(randomForest)
library(neuralnet)
library(foreach)
library(doParallel)
library(openxlsx)
library(ggplot2)
library(gridExtra)
library(zip)
library(plumber)
library(htmltools)
library(openssl)
library(jose)
library(grDevices)
library(corrplot)
library(caret)

# --- Cle JWT ---
JWT_SECRET <- Sys.getenv("JWT_SECRET")
if (nchar(JWT_SECRET) == 0) {
  stop("La variable d'environnement JWT_SECRET n'est pas definie. ",
       "Copiez .Renviron.example en .Renviron et renseignez-la.")
}

# Conversion d'une chaine hexadecimale en raw
hex_to_raw <- function(hex) {
  hex <- gsub("[^0-9a-fA-F]", "", hex)
  if (nchar(hex) %% 2 != 0) stop("JWT_SECRET doit avoir un nombre pair de caracteres hex.")
  as.raw(strtoi(substring(hex, seq(1, nchar(hex), 2),
                          seq(2, nchar(hex), 2)), 16L))
}
JWT_KEY <- hex_to_raw(JWT_SECRET)

# --- Repertoire racine de travail ---
root <<- Sys.getenv("CYAN_ROOT_DIR", unset = file.path(tempdir(), "cyan_api"))
if (!dir.exists(root)) dir.create(root, recursive = TRUE)

# --- Fichier des fonctions de modelisation ---
FUNCTIONS_FILE <- Sys.getenv(
  "CYAN_FUNCTIONS_FILE",
  unset = file.path(getwd(), "ALLFONCTIONS.R")
)

# --- Taille maximale de fichier (Mo) ---
MAX_FILE_MB <- as.numeric(Sys.getenv("CYAN_MAX_FILE_MB", unset = "10"))

# ---------------------------------------------------------------------------
#  METADONNEES DE L'API
# ---------------------------------------------------------------------------

#* @apiTitle CYAN PREDICTION
#* @apiDescription API de prediction de la variable CYAN a partir d'un fichier Excel.

# ===========================================================================
#  ENDPOINT : /upload
# ===========================================================================

#* Upload d'un fichier Excel et initialisation de la session
#* @param file Chemin du fichier Excel sur le serveur (ex: D:/Chemin.xlsx)
#* @post /upload
api.upload.data <- function(req, res, file) {

  # --- Validation de l'entree ---
  if (is.null(file) || nchar(file) == 0) {
    res$status <- 400
    return(list(error = "Le parametre 'file' doit etre fourni."))
  }

  if (!file.exists(file)) {
    res$status <- 400
    return(list(error = paste0("Fichier introuvable sur le serveur : ", file)))
  }

  if (file.info(file)$size > MAX_FILE_MB * 1024^2) {
    res$status <- 413
    return(list(error = paste0("Fichier trop volumineux (> ", MAX_FILE_MB, " Mo).")))
  }

  # --- Creation du dossier de session unique ---
  bnwe <- tools::file_path_sans_ext(basename(file))
  destination_directory <- paste0(root, "/", bnwe, round(as.numeric(Sys.time())))
  while (dir.exists(destination_directory)) {
    destination_directory <- paste0(root, "/", bnwe, round(as.numeric(Sys.time())))
  }
  dir.create(destination_directory, recursive = TRUE)

  destination_file <- file.path(
    destination_directory,
    paste0("data_", round(as.numeric(Sys.time())), ".xlsx")
  )
  while (file.exists(destination_file)) {
    destination_file <- file.path(
      destination_directory,
      paste0("data_", round(as.numeric(Sys.time())), ".xlsx")
    )
  }

  # --- Copie du fichier ---
  a <- tryCatch(
    file.copy(file, destination_file, overwrite = TRUE),
    error = function(e) 1
  )

  if (!isTRUE(a)) {
    res$status <- 500
    return(list(error = "Echec de la copie du fichier."))
  }

  # --- Generation du token JWT ---
  claim <- jose::jwt_claim(
    path        = destination_file,
    dir         = destination_directory,
    session_key = 123456,
    exp         = as.numeric(Sys.time()) + 3600 * 24   # +24h
  )
  jwt <- jose::jwt_encode_hmac(claim, secret = JWT_KEY)
  res$setCookie("token", jwt)

  list(message = "Fichier telecharge avec succes.",
       session_dir = destination_directory)
}

# ===========================================================================
#  ENDPOINT : /analysis
# ===========================================================================

#* Analyse et modelisation des donnees
#* @param methods Methodes d'analyse separees par des virgules (rf, mlr, ann, knn)
#* @param var Variable a predire
#* @param offset Decalage des colonnes a considerer comme variables
#* @get /analysis
#* @serializer text
function(req, res, methods, var, offset) {

  # --- Chargement des fonctions ---
  if (!file.exists(FUNCTIONS_FILE)) {
    res$status <- 500
    return(paste0("Fichier de fonctions introuvable : ", FUNCTIONS_FILE))
  }
  source(FUNCTIONS_FILE, local = FALSE)

  # --- Verification du token ---
  if (is.null(req$cookies$token)) {
    res$status <- 401
    return("Token manquant.")
  }
  claims <- tryCatch(
    jose::jwt_decode_hmac(req$cookies$token, secret = JWT_KEY),
    error = function(e) NULL
  )
  if (is.null(claims)) {
    res$status <- 401
    return("Token invalide ou expire.")
  }

  file_path   <- claims$path
  session_dir <- claims$dir

  # --- Validation des methodes ---
  models <- c("rf", "mlr", "ann", "knn")
  m2 <- trimws(unlist(strsplit(methods, ",")))
  invalid <- setdiff(m2, models)
  if (length(invalid) > 0) {
    res$status <- 400
    return(paste0("Methodes invalides : ", paste(invalid, collapse = ", "),
                  ". Disponibles : ", paste(models, collapse = ", ")))
  }
  mn <- models[match(m2, models)]

  # --- Pre-traitement ---
  VAR.TO.PREDICT <<- var
  root <<- session_dir

  pt <- pre.traitement(file_path, offset)

  load.dataset        <<- pt$load.dataset
  vector.datas.combine <<- pt$combine.vars
  normalized_data     <<- pt$normalized_data
  data_to_analyze     <<- pt$data_to_analyze

  # --- Modelisation ---
  ms  <- models.sim(mn)
  pot <- post.traitement(mn, ms)

  # --- Archivage ---
  zip_file <- file.path(session_dir, "data_and_images.zip")
  files_to_zip <- setdiff(list.files(session_dir), basename(zip_file))
  zip::zip(zip_file, files = files_to_zip, root = session_dir)

  paste0("Analyse terminee. Archive disponible via /download.")
}

# ===========================================================================
#  ENDPOINT : /download
# ===========================================================================

#* Telechargement de l'archive ZIP des resultats
#* @get /download
#* @serializer contentType list(type="application/zip")
function(req, res) {

  if (is.null(req$cookies$token)) {
    res$status <- 401
    return("Token manquant.")
  }
  claims <- tryCatch(
    jose::jwt_decode_hmac(req$cookies$token, secret = JWT_KEY),
    error = function(e) NULL
  )
  if (is.null(claims)) {
    res$status <- 401
    return("Token invalide ou expire.")
  }

  session_dir <- claims$dir
  zip_file <- file.path(session_dir, "data_and_images.zip")

  if (!file.exists(zip_file)) {
    res$status <- 404
    return("Archive introuvable. Lancez d'abord /analysis.")
  }

  res$setHeader(
    "Content-Disposition",
    paste0('attachment; filename="cyan_results_',
           format(Sys.time(), "%Y%m%d_%H%M%S"), '.zip"')
  )

  readBin(zip_file, "raw", n = file.info(zip_file)$size)
}