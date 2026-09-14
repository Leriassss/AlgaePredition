# ============================================================================
#  FONCTIONS DE MODELISATION - CYAN
# ============================================================================

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# ---------------------------------------------------------------------------
#  GENERATION DE LA FORMULE ET DES JEUX DE DONNEES POUR UNE COMBINAISON
# ---------------------------------------------------------------------------
processing.modeling <- function(i, j) {
  splicing_index     <- load.dataset[, i]
  calibration_datas  <- normalized_data[splicing_index, ]
  validation_datas   <- normalized_data[-splicing_index, ]
  var_calibration    <- data_to_analyze[VAR.TO.PREDICT][splicing_index, ]
  var_validation     <- data_to_analyze[VAR.TO.PREDICT][-splicing_index, ]

  loop_formula <- as.formula(paste0(VAR.TO.PREDICT, " ~ ", vector.datas.combine[j]))
  loop_params  <- unlist(strsplit(vector.datas.combine[j], "+", fixed = TRUE))

  loop_data <- data.frame(
    subset(calibration_datas, select = loop_params),
    CYAN = var_calibration
  )
  loop_validation_data <- data.frame(
    subset(validation_datas, select = loop_params),
    CYAN = var_validation
  )

  names(loop_data)[ncol(loop_data)] <- VAR.TO.PREDICT
  names(loop_validation_data)[ncol(loop_validation_data)] <- VAR.TO.PREDICT

  list(
    formule          = loop_formula,
    data.calage      = loop_data,
    data.validation  = loop_validation_data
  )
}

# ---------------------------------------------------------------------------
#  PRE-TRAITEMENT
# ---------------------------------------------------------------------------
pre.traitement <- function(path, offset = 1) {

  load_data <- tidyr::drop_na(openxlsx::read.xlsx(path))
  data_to_analyze <- load_data[, offset:ncol(load_data)]

  if (ncol(load_data) >= 1500 || nrow(load_data) >= 1500) {
    stop(list(message = "Nombre de donnees trop important"), status = 400)
  }

  # --- Correlations ---
  cor_matrix <- cor(data_to_analyze[, -(ncol(data_to_analyze))])

  jpeg(file.path(root, "MatriceDeCorrelation.jpeg"))
  corrplot::corrplot(cor_matrix)
  dev.off()

  highly_correlated <- caret::findCorrelation(cor_matrix, cutoff = 0.8)
  if (length(highly_correlated) != 0) {
    data_to_analyze <- data_to_analyze[, -highly_correlated]
  }

  correls <- apply(data_to_analyze, 2, function(x) {
    cors <- cor.test(as.numeric(x), as.numeric(data_to_analyze[[VAR.TO.PREDICT]]))
    c(cors$estimate, cors$p.value)
  })
  row.names(correls) <- c("cor", "p.value")

  correls.signif <- correls[, which(correls[2, ] < 0.05 & abs(correls[1, ]) >= 0.5)]
  data_to_analyze <- subset.data.frame(data_to_analyze,
                                       select = colnames(correls.signif))

  # --- Boxplots ---
  jpeg(file.path(root, "boxplot_log.jpeg"))
  boxplot(log(data_to_analyze), ylim = c(-6, 10),
          main = "Boites a moustache des variables\n(Echelle logarithmique)",
          na.action = NULL, col = rainbow(8, s = 0.4, v = 0.9), las = 2)
  dev.off()

  jpeg(file.path(root, "boxplot.jpeg"))
  bxplt_data <- boxplot(data_to_analyze,
                        main = "Boites a moustache des variables",
                        col = rainbow(8, s = 0.4, v = 0.9), las = 2)
  dev.off()

  clone_dta <- data_to_analyze
  for (i in 1:ncol(clone_dta)) {
    clone_dta[which(clone_dta[, i] %in% bxplt_data$out), ] <- NA
  }
  data_to_analyze <- tidyr::drop_na(clone_dta)

  # --- Normalisation ---
  normalized_data <- as.data.frame(lapply(
    data_to_analyze[-(which(names(data_to_analyze) == VAR.TO.PREDICT))],
    FUN = function(vec) normalize(vec)
  ))

  # --- Jeux de donnees (70%) ---
  length.data.calibrage <- ceiling(nrow(normalized_data) * 0.7)
  set.seed(123)
  load.dataset <- data.frame(
    1:length.data.calibrage,
    replicate(10, sample(1:nrow(normalized_data),
                         size = length.data.calibrage, replace = FALSE),
              simplify = FALSE)
  )
  names(load.dataset) <- 1:ncol(load.dataset)

  # --- Combinaisons de variables ---
  parameters <- as.character(names(normalized_data))
  datas.combine <- lapply(1:length(parameters),
                          function(n) combn(parameters, n, simplify = FALSE))
  vector.datas.combine <- unlist(lapply(datas.combine,
                                        function(x) sapply(x, paste, collapse = "+")))

  openxlsx::write.xlsx(
    list(
      correls.signif         = correls.signif,
      data_used              = data_to_analyze,
      normalized_data        = normalized_data,
      possibles_vars_combine = vector.datas.combine
    ),
    file.path(root, "pre_traitement.xlsx")
  )

  list(
    correls.signif    = correls.signif,
    data_to_analyze   = data_to_analyze,
    vars.boxplot.log  = file.path(root, "boxplot_log.jpeg"),
    vars.boxplot      = file.path(root, "boxplot.jpeg"),
    normalized_data   = normalized_data,
    load.dataset      = load.dataset,
    combine.vars      = vector.datas.combine
  )
}

# ---------------------------------------------------------------------------
#  MODELISATION
# ---------------------------------------------------------------------------
models.sim <- function(vec) {
  models <- c("rf", "mlr", "ann", "knn")
  models_formula <- c(
    "random.forest(looping)",
    "multi.linear.regression(looping)",
    "artificial.neural.network(looping)",
    "k.nearest.neighbor(looping)"
  )
  formulas <- models_formula[match(vec, models)]
  list_models <- list()

  for (looping in 1:length(vector.datas.combine)) {
    mod_res <- list()
    for (i in 1:length(formulas)) {
      mod_res[[i]] <- eval(parse(text = formulas[i]))
    }
    names(mod_res) <- vec
    list_models[[looping]] <- mod_res
  }
  list_models
}

# ---------------------------------------------------------------------------
#  POST-TRAITEMENT
# ---------------------------------------------------------------------------
post.traitement <- function(model_names, mr) {

  rmses <- sapply(model_names, function(model_name) {
    sapply(mr, FUN = function(vec) vec[[model_name]]$rmse.calage)
  })
  row.names(rmses) <- vector.datas.combine

  prediction_all <- lapply(1:length(vector.datas.combine), FUN = function(rmse_line) {
    data.frame(
      sapply(model_names, FUN = function(model_name) {
        if (model_name == "knn") {
          mr[[rmse_line]][[model_name]]$estimated
        } else {
          model <- mr[[rmse_line]][[model_name]]$best.model
          datas <- processing.modeling(1, rmse_line)
          as.numeric(predict(model, datas$data.validation))
        }
      }),
      valeurs_reelles = as.numeric(
        processing.modeling(1, 1)$data.validation[VAR.TO.PREDICT][, 1]
      )
    )
  })
  names(prediction_all) <- vector.datas.combine

  rmse.validation.best <- matrix(ncol = length(prediction_all),
                                 nrow = length(model_names))
  for (i in 1:length(prediction_all)) {
    datac <- prediction_all[[i]]
    data_m <- datac[-which(names(datac) == "valeurs_reelles")]
    rmse_values <- apply(data_m, 2, FUN = function(vect) {
      Metrics::rmse(as.numeric(vect), as.numeric(datac["valeurs_reelles"][, 1]))
    })
    rmse.validation.best[, i] <- rmse_values
  }
  row.names(rmse.validation.best) <- model_names
  colnames(rmse.validation.best) <- vector.datas.combine
  rmse.validation.best <- data.frame(rmse.validation.best)

  best.rmses <- apply(rmse.validation.best, 1, which.min)

  predicted_values <- data.frame(
    mapply(FUN = function(table, b) table[b],
           prediction_all[best.rmses], names(best.rmses))
  )
  names(predicted_values) <- model_names

  predictions_data <- data.frame(
    predicted_values,
    valeurs_reelles = as.numeric(
      processing.modeling(1, 1)$data.validation[VAR.TO.PREDICT][, 1]
    )
  )

  combinaisons.models <- data.frame(
    var = apply(data.frame(t(best.rmses)), 2,
                FUN = function(vec) vector.datas.combine[vec])
  )

  rmse_calage_validation <- data.frame(t(rmse.validation.best), rmses)
  names(rmse_calage_validation) <- c(
    paste0("rmse_validation_", model_names),
    paste0("rmse_calage_", model_names)
  )

  results <- lapply(
    subset.data.frame(predictions_data, select = -ncol(predictions_data)),
    FUN = function(vec) {
      data.frame(Observed = predictions_data[, ncol(predictions_data)],
                 Predicted = vec)
    }
  )

  # --- Graphiques seuils ---
  seuils <- data.frame(Seuil = c(5, 10, 15),
                       Couleur = c("blue", "green", "red"))

  plots <- lapply(model_names, function(model_name) {
    model_data <- results[[model_name]]
    ggplot(model_data, aes(x = Observed, y = Predicted)) +
      geom_abline(intercept = 0, slope = 1, color = "gray") +
      labs(title = paste("Seuils de prediction de", VAR.TO.PREDICT, "-", model_name)) +
      theme_minimal() +
      geom_hline(data = seuils, aes(yintercept = Seuil, color = Couleur)) +
      geom_vline(data = seuils, aes(xintercept = Seuil, color = Couleur)) +
      geom_point() +
      geom_text(data = seuils, aes(x = -2, y = Seuil,
                                   label = paste("Seuil =", Seuil)),
                vjust = -0.5, hjust = 0) +
      scale_color_identity() +
      theme(panel.grid.major = element_line(color = "black"),
            panel.grid.minor = element_blank())
  })

  plots_graphs <- lapply(names(results), function(model_name) {
    model_data <- results[[model_name]]
    ggplot(data.frame(model_data, x = 1:nrow(model_data)), aes(x = x)) +
      geom_point(aes(y = Observed, color = "Obs"), shape = 15) +
      geom_line(aes(y = Observed, color = "Obs")) +
      geom_point(aes(y = Predicted, color = "Pred"), shape = 15) +
      geom_line(aes(y = Predicted, color = "Pred")) +
      labs(x = "Sites", y = "Abondance (Ind/mL)",
           title = paste0("CYAN - Modele ", model_name)) +
      scale_color_manual(values = c("Obs" = "blue", "Pred" = "red"),
                         labels = c("Observees", "Predites")) +
      theme(panel.grid.major = element_line(color = "black"),
            panel.grid.minor = element_blank(),
            axis.title.x = element_text(face = "bold", size = 17, hjust = 0.5),
            axis.title.y = element_text(face = "bold", size = 14, hjust = 0.5),
            plot.title   = element_text(face = "bold", size = 16, hjust = 0.5)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black")
  })

  jpeg(file.path(root, "seuils.jpeg"), width = 1300, height = 700, quality = 100)
  grid.arrange(grobs = plots, ncol = ceiling(length(model_names) / 2))
  dev.off()

  jpeg(file.path(root, "visualisations.jpeg"), width = 1300, height = 700, quality = 100)
  grid.arrange(grobs = plots_graphs, ncol = ceiling(length(model_names) / 2))
  dev.off()

  # --- Sauvegarde des modeles ---
  pv <- mapply(FUN = function(table, b) {
    c <- table[b]
    save(c, file = file.path(root, paste0("model_", b, "_object.RData")))
  }, ms[best.rmses], names(best.rmses))

  openxlsx::write.xlsx(
    list(
      predictions_data       = predictions_data,
      predictions_for_all    = data.frame(prediction_all),
      rmse.validation.best   = rmse.validation.best,
      combinaisons.models    = combinaisons.models,
      rmse_calage_validation = rmse_calage_validation,
      obs_pred               = data.frame(results)
    ),
    file.path(root, "analysis_results.xlsx"),
    colNames = TRUE, rowNames = TRUE
  )

  list(
    best.rmses             = best.rmses,
    predictions_data       = predictions_data,
    prediction_all         = prediction_all,
    rmse.validation.best   = rmse.validation.best,
    combinaisons.models    = combinaisons.models,
    rmse_calage_validation = rmse_calage_validation,
    obs_pred               = results,
    seuils                 = file.path(root, "seuils.jpeg"),
    visualisation          = file.path(root, "visualisations.jpeg")
  )
}

# ---------------------------------------------------------------------------
#  RANDOM FOREST
# ---------------------------------------------------------------------------
random.forest <- function(j) {
  rf.models <- list()
  params <- list()
  i <- 1
  while (i <= ncol(load.dataset)) {
    loop_infos <- processing.modeling(i, j)
    params[[i]] <- list(calibration_datas = loop_infos$data.calage,
                        validation_datas  = loop_infos$data.validation)

    set.seed(123)
    rf.models[[i]] <- randomForest::randomForest(
      loop_infos$formule, data = loop_infos$data.calage, ntree = 500
    )
    names(rf.models)[i] <- paste("MJ", i, sep = "")
    i <- i + 1
  }

  vars.jeux <- lapply(params, FUN = function(vec) vec$calibration_datas)

  contig.rmse <- as.data.frame(sapply(rf.models, FUN = function(model) {
    lapply(vars.jeux, FUN = function(jeu) {
      prediction <- as.numeric(as.array(
        predict(model, jeu[-(which(names(jeu) == VAR.TO.PREDICT))])
      ))
      Metrics::rmse(as.numeric(jeu[VAR.TO.PREDICT][, 1]), prediction)
    })
  }))
  contig.rmse <- apply(contig.rmse, 2, as.numeric, as.array)
  row.names(contig.rmse) <- paste0("J", 1:length(vars.jeux))

  classify.rmse <- t(apply(contig.rmse, 1, rank))
  mean.classify <- rank(colMeans(classify.rmse))
  rf.rmse <- diag(contig.rmse)
  best.rank.model <- which.min(mean.classify)

  model.rf <- rf.models[[best.rank.model]]

  cat("Simulation Random Forest terminee !\n")
  list(best.model = model.rf,
       rmse.calage = rf.rmse[[best.rank.model]])
}

# ---------------------------------------------------------------------------
#  MLR
# ---------------------------------------------------------------------------
multi.linear.regression <- function(j) {
  mlr.models <- list()
  i <- 1
  while (i <= ncol(load.dataset)) {
    loop_infos <- processing.modeling(i, j)
    mlr.models[[i]] <- lm(loop_infos$formule, data = loop_infos$data.calage)
    names(mlr.models)[i] <- paste("MJ", i, sep = "")
    i <- i + 1
  }

  vars.jeux <- lapply(mlr.models, FUN = function(vec) vec$model)

  contig.rmse <- as.data.frame(sapply(mlr.models, FUN = function(model) {
    lapply(vars.jeux, FUN = function(jeu) {
      prediction <- as.numeric(as.array(
        predict(model, jeu[-(which(names(jeu) == VAR.TO.PREDICT))])
      ))
      Metrics::rmse(as.numeric(jeu[VAR.TO.PREDICT][, 1]), prediction)
    })
  }))
  contig.rmse <- apply(contig.rmse, 2, as.numeric, as.array)
  row.names(contig.rmse) <- paste0("J", 1:length(vars.jeux))

  classify.rmse <- t(apply(contig.rmse, 1, rank))
  mean.classify <- rank(colMeans(classify.rmse))
  mlr.rmse <- diag(contig.rmse)
  best.rank.model <- which.min(mean.classify)

  model.mlr <- mlr.models[[best.rank.model]]

  cat("Simulation Multi Linear Regression terminee !\n")
  list(best.model = model.mlr,
       rmse.calage = mlr.rmse[[best.rank.model]])
}

# ---------------------------------------------------------------------------
#  KNN
# ---------------------------------------------------------------------------
k.nearest.neighbor <- function(j) {
  knn.rmse <- numeric()
  knns <- list()
  i <- 1
  while (i <= ncol(load.dataset)) {   # <- corrige (etait "i <= 1")
    loop_infos <- processing.modeling(i, j)
    n_for_modeling <- ceiling(sqrt(nrow(loop_infos$data.calage)))
    optim <- 1
    while (optim <= (n_for_modeling + 1)) {
      trs <- loop_infos$data.calage
      tes <- loop_infos$data.validation
      set.seed(123)
      knn.n <- class::knn(
        train = trs[-(which(names(trs) == VAR.TO.PREDICT))],
        test  = tes[-(which(names(tes) == VAR.TO.PREDICT))],
        cl    = loop_infos$data.calage[VAR.TO.PREDICT][, 1],
        k     = optim
      )
      knn.rmse[length(knn.rmse) + 1] <- Metrics::rmse(
        loop_infos$data.validation[VAR.TO.PREDICT][, 1],
        as.numeric(array(knn.n))
      )
      knns[[length(knns) + 1]] <- as.numeric(array(knn.n))
      optim <- optim + 1
    }
    i <- i + 1
  }

  knn.rmse <- t(as.data.frame(knn.rmse))
  cat("Simulation K Nearest Neighbor terminee !\n")

  list(
    optim          = which.min(knn.rmse),
    estimated      = knns[[which.min(knn.rmse)]],
    rmse.calage    = knn.rmse[which.min(knn.rmse)],
    rmse.validation = knn.rmse[which.min(knn.rmse)]
  )
}

# ---------------------------------------------------------------------------
#  ANN
# ---------------------------------------------------------------------------
artificial.neural.network <- function(j) {
  num_cores <- detectCores()
  cl <- makeCluster(num_cores)
  on.exit(stopCluster(cl), add = TRUE)

  clusterExport(cl, varlist = c(
    "normalized_data", "processing.modeling", "load.dataset",
    "data_to_analyze", "vector.datas.combine", "VAR.TO.PREDICT"
  ))
  registerDoParallel(cl)

  ann.models <- foreach(i = 1:ncol(load.dataset), .combine = 'c') %dopar% {
    loop_infos <- processing.modeling(i, j)
    tryCatch({
      set.seed(123)
      jeu <- list(neuralnet::neuralnet(
        loop_infos$formule,
        data = loop_infos$data.calage,
        hidden = 3, stepmax = 1e+10
      ))
      names(jeu) <- paste0("J", i)
      jeu
    }, error = function(e) {
      jeu <- list(NA)
      names(jeu) <- paste0("J", i)
      jeu
    })
  }

  # NB : neuralnet n'expose pas $data directement ; on reconstruit les jeux
  vars.jeux <- lapply(1:ncol(load.dataset), function(i) {
    processing.modeling(i, j)$data.calage
  })

  contig.rmse <- as.data.frame(sapply(ann.models, FUN = function(model) {
    lapply(vars.jeux, FUN = function(jeu) {
      if (is.null(model) || length(model) == 0 || is.na(model[[1]])) return(NA)
      prediction <- as.numeric(as.array(
        predict(model, jeu[-(which(names(jeu) == VAR.TO.PREDICT))])
      ))
      Metrics::rmse(as.numeric(jeu[VAR.TO.PREDICT][, 1]), prediction)
    })
  }))
  contig.rmse <- apply(contig.rmse, 2, as.numeric, as.array)
  colnames(contig.rmse) <- paste0("M", 1:length(vars.jeux))

  classify.rmse <- t(apply(contig.rmse, 1, rank, na.last = "keep"))
  mean.classify <- rank(colMeans(classify.rmse, na.rm = TRUE))
  ann.rmse <- diag(contig.rmse)
  best.rank.model <- which.min(mean.classify)

  model.ann <- ann.models[[best.rank.model]]

  cat("Simulation Artificial Neural Network terminee !\n")
  list(best.model = model.ann,
       rmse.calage = ann.rmse[[best.rank.model]])
}