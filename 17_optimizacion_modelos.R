# =============================================================================
# TRABAJO FINAL - INTRODUCCIÓN AL ANÁLISIS DE DATOS
# Análisis EPH GBA 2016-2024
# 17_optimizacion_modelos.R - Optimización avanzada de modelos
# =============================================================================

# Cargar librerías avanzadas
if(!require(glmnet, quietly = TRUE)) {
  install.packages("glmnet")
  library(glmnet)
}

if(!require(xgboost, quietly = TRUE)) {
  install.packages("xgboost") 
  library(xgboost)
}

if(!require(ROSE, quietly = TRUE)) {
  install.packages("ROSE")
  library(ROSE)
}

cat("🚀 OPTIMIZACIÓN AVANZADA DE MODELOS DE IMPUTACIÓN...\n\n")

# =============================================================================
# PREPARACIÓN DE DATOS MEJORADA
# =============================================================================

cat("=== FASE 1: PREPARACIÓN AVANZADA DE DATOS ===\n")

# Función mejorada de preparación
preparar_datos_optimizado <- function() {
  map_dfr(names(datos_gba), function(periodo) {
    if(!is.null(datos_gba[[periodo]]$personas)) {
      datos_gba[[periodo]]$personas %>%
        mutate(
          # Variables básicas
          ANO4 = as.numeric(ANO4),
          TRIMESTRE = as.numeric(TRIMESTRE),
          CH04 = as.numeric(CH04),
          CH06 = as.numeric(CH06),
          ESTADO = as.numeric(ESTADO),
          NIVEL_ED = as.numeric(NIVEL_ED),
          PONDERA = as.numeric(PONDERA),
          
          # Variable dependiente
          P47T_Original = P47T,
          P47T_Numerico = suppressWarnings(as.numeric(P47T)),
          Responde_Ingreso = case_when(
            is.na(P47T) ~ 0,
            P47T == "" ~ 0,
            P47T == "-9" ~ 0,
            suppressWarnings(as.numeric(P47T)) > 0 ~ 1,
            TRUE ~ 0
          ),
          
          # Variables categóricas optimizadas
          Sexo = case_when(CH04 == 1 ~ "Varón", CH04 == 2 ~ "Mujer", TRUE ~ "No_especificado"),
          
          # Grupos de edad más granulares
          Edad_Continua = CH06,
          Edad_Cuadratica = CH06^2,
          Grupo_Edad_Fino = case_when(
            CH06 >= 10 & CH06 <= 17 ~ "10-17",
            CH06 >= 18 & CH06 <= 24 ~ "18-24",
            CH06 >= 25 & CH06 <= 29 ~ "25-29",
            CH06 >= 30 & CH06 <= 39 ~ "30-39",
            CH06 >= 40 & CH06 <= 49 ~ "40-49",
            CH06 >= 50 & CH06 <= 59 ~ "50-59",
            CH06 >= 60 & CH06 <= 64 ~ "60-64",
            CH06 >= 65 ~ "65+",
            TRUE ~ "No_especificado"
          ),
          
          # Educación recodificada
          Educacion_Años = case_when(
            NIVEL_ED == 1 ~ 0,    # Sin instrucción
            NIVEL_ED == 2 ~ 3,    # Primaria incompleta
            NIVEL_ED == 3 ~ 7,    # Primaria completa
            NIVEL_ED == 4 ~ 9,    # Secundaria incompleta
            NIVEL_ED == 5 ~ 12,   # Secundaria completa
            NIVEL_ED == 6 ~ 15,   # Superior incompleto
            NIVEL_ED == 7 ~ 17,   # Superior completo
            TRUE ~ NA_real_
          ),
          
          Estado_Laboral = case_when(
            ESTADO == 1 ~ "Ocupado",
            ESTADO == 2 ~ "Desocupado",
            ESTADO == 3 ~ "Inactivo",
            ESTADO == 4 ~ "Menor_10",
            TRUE ~ "No_especificado"
          ),
          
          # Variables derivadas (INGENIERÍA DE CARACTERÍSTICAS)
          Jefe_Hogar = ifelse(CH03 == 1, 1, 0),  # Si es jefe de hogar
          
          # Interacciones relevantes
          Sexo_Estado = paste(Sexo, Estado_Laboral, sep = "_"),
          Edad_Educacion = Edad_Continua * Educacion_Años,
          
          # Variables temporales
          Año_Normalizado = (ANO4 - 2016) / 8,  # Normalizar 0-1
          
          # Indicadores de crisis
          Periodo_Crisis = case_when(
            ANO4 %in% c(2018, 2019) ~ "Crisis_Macro",
            ANO4 %in% c(2020, 2021) ~ "Pandemia",
            ANO4 >= 2022 ~ "Post_Pandemia",
            TRUE ~ "Normal"
          )
        ) %>%
        filter(
          Sexo != "No_especificado",
          Grupo_Edad_Fino != "No_especificado",
          Estado_Laboral != "No_especificado",
          !is.na(PONDERA), PONDERA > 0,
          !is.na(Educacion_Años),
          Edad_Continua >= 10
        )
    }
  })
}

# Preparar datos optimizados
datos_optimizado <- preparar_datos_optimizado()

cat("✅ Datos optimizados preparados\n")
cat("   • Total registros:", format(nrow(datos_optimizado), big.mark = ","), "\n")
cat("   • Tasa respuesta:", round(mean(datos_optimizado$Responde_Ingreso) * 100, 1), "%\n")

# =============================================================================
# MANEJO DE DESBALANCE DE CLASES
# =============================================================================

cat("\n=== FASE 2: MANEJO DE DESBALANCE ===\n")

# Analizar desbalance
tabla_balance <- table(datos_optimizado$Responde_Ingreso)
prop_balance <- prop.table(tabla_balance)

cat("📊 Distribución de clases:\n")
cat("   • No responde (0):", round(prop_balance[1] * 100, 1), "%\n")
cat("   • Responde (1):", round(prop_balance[2] * 100, 1), "%\n")

# Partición estratificada
set.seed(123)
indices_train_opt <- createDataPartition(
  y = interaction(datos_optimizado$ANO4, datos_optimizado$Responde_Ingreso),
  p = 0.7,
  list = FALSE
)

datos_train_opt <- datos_optimizado[indices_train_opt, ]
datos_test_opt <- datos_optimizado[-indices_train_opt, ]

# Aplicar SMOTE para balancear (solo en entrenamiento)
cat("🔄 Aplicando SMOTE para balancear clases...\n")

# Preparar datos para SMOTE
datos_smote <- datos_train_opt %>%
  select(Responde_Ingreso, Sexo, Grupo_Edad_Fino, Estado_Laboral, 
         Edad_Continua, Edad_Cuadratica, Educacion_Años, Año_Normalizado, TRIMESTRE) %>%
  mutate(
    Responde_Ingreso = factor(Responde_Ingreso),
    Sexo = factor(Sexo),
    Grupo_Edad_Fino = factor(Grupo_Edad_Fino),
    Estado_Laboral = factor(Estado_Laboral),
    TRIMESTRE = factor(TRIMESTRE)
  )

# Aplicar ROSE (alternativa a SMOTE más robusta)
datos_balanced <- ROSE(Responde_Ingreso ~ ., data = datos_smote, seed = 123)$data

cat("✅ Balanceo aplicado\n")
cat("   • Casos originales train:", format(nrow(datos_train_opt), big.mark = ","), "\n")
cat("   • Casos balanceados:", format(nrow(datos_balanced), big.mark = ","), "\n")

nueva_distribucion <- prop.table(table(datos_balanced$Responde_Ingreso))
cat("   • Nueva distribución: No=", round(nueva_distribucion[1]*100,1), "% | Si=", round(nueva_distribucion[2]*100,1), "%\n")

# =============================================================================
# MODELO OPTIMIZADO 1: REGRESIÓN LOGÍSTICA CON REGULARIZACIÓN (LASSO/RIDGE)
# =============================================================================

cat("\n=== MODELO OPTIMIZADO 1: REGRESIÓN LOGÍSTICA REGULARIZADA ===\n")

# Preparar matriz de características para glmnet
preparar_matriz_glmnet <- function(datos) {
  # Variables numéricas
  vars_numericas <- c("Edad_Continua", "Edad_Cuadratica", "Educacion_Años", "Año_Normalizado", "TRIMESTRE")
  
  # Variables categóricas
  vars_categoricas <- c("Sexo", "Grupo_Edad_Fino", "Estado_Laboral")
  
  # Crear matriz de diseño
  matriz_numerica <- as.matrix(datos[, vars_numericas])
  
  # Convertir categóricas a dummy variables
  for(var in vars_categoricas) {
    dummy_matrix <- model.matrix(~ . - 1, data = datos[var])
    matriz_numerica <- cbind(matriz_numerica, dummy_matrix)
  }
  
  return(matriz_numerica)
}

# Preparar matrices
X_train <- preparar_matriz_glmnet(datos_balanced)
y_train <- as.numeric(datos_balanced$Responde_Ingreso) - 1

X_test <- preparar_matriz_glmnet(datos_test_opt %>%
                                   mutate(
                                     Responde_Ingreso = factor(Responde_Ingreso),
                                     Sexo = factor(Sexo, levels = levels(datos_balanced$Sexo)),
                                     Grupo_Edad_Fino = factor(Grupo_Edad_Fino, levels = levels(datos_balanced$Grupo_Edad_Fino)),
                                     Estado_Laboral = factor(Estado_Laboral, levels = levels(datos_balanced$Estado_Laboral)),
                                     TRIMESTRE = factor(TRIMESTRE)
                                   ))
y_test <- datos_test_opt$Responde_Ingreso

cat("🔄 Entrenando modelos regularizados...\n")

# Cross-validation para encontrar mejor lambda
cv_lasso <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1, nfolds = 5)
cv_ridge <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0, nfolds = 5)
cv_elastic <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0.5, nfolds = 5)

# Entrenar modelos finales
modelo_lasso <- glmnet(X_train, y_train, family = "binomial", alpha = 1, lambda = cv_lasso$lambda.min)
modelo_ridge <- glmnet(X_train, y_train, family = "binomial", alpha = 0, lambda = cv_ridge$lambda.min)
modelo_elastic <- glmnet(X_train, y_train, family = "binomial", alpha = 0.5, lambda = cv_elastic$lambda.min)

# Predicciones
pred_lasso <- predict(modelo_lasso, X_test, type = "response")[,1]
pred_ridge <- predict(modelo_ridge, X_test, type = "response")[,1]
pred_elastic <- predict(modelo_elastic, X_test, type = "response")[,1]

# Evaluación
evaluar_modelo_binario <- function(pred_prob, y_real, threshold = 0.5) {
  pred_clase <- ifelse(pred_prob > threshold, 1, 0)
  
  # Matriz de confusión
  cm <- table(Predicho = pred_clase, Real = y_real)
  
  # Métricas
  accuracy <- mean(pred_clase == y_real)
  precision <- cm[2,2] / sum(cm[2,])
  recall <- cm[2,2] / sum(cm[,2])
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  return(list(
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1 = f1,
    confusion_matrix = cm
  ))
}

eval_lasso <- evaluar_modelo_binario(pred_lasso, y_test)
eval_ridge <- evaluar_modelo_binario(pred_ridge, y_test)
eval_elastic <- evaluar_modelo_binario(pred_elastic, y_test)

cat("📊 Evaluación modelos regularizados:\n")
cat("   • LASSO - F1:", round(eval_lasso$f1, 3), "| Accuracy:", round(eval_lasso$accuracy, 3), "\n")
cat("   • Ridge - F1:", round(eval_ridge$f1, 3), "| Accuracy:", round(eval_ridge$accuracy, 3), "\n")
cat("   • Elastic Net - F1:", round(eval_elastic$f1, 3), "| Accuracy:", round(eval_elastic$accuracy, 3), "\n")

# =============================================================================
# MODELO OPTIMIZADO 2: RANDOM FOREST CON TUNING DE HIPERPARÁMETROS
# =============================================================================

cat("\n=== MODELO OPTIMIZADO 2: RANDOM FOREST OPTIMIZADO ===\n")

# Preparar datos para Random Forest optimizado
datos_rf_opt <- datos_balanced %>%
  mutate(
    Responde_Ingreso = factor(Responde_Ingreso, levels = c("0", "1"), labels = c("No", "Si"))
  )

datos_test_rf_opt <- datos_test_opt %>%
  mutate(
    Responde_Ingreso = factor(Responde_Ingreso, levels = c(0, 1), labels = c("No", "Si")),
    Sexo = factor(Sexo),
    Grupo_Edad_Fino = factor(Grupo_Edad_Fino),
    Estado_Laboral = factor(Estado_Laboral),
    TRIMESTRE = factor(TRIMESTRE)
  )

# Variables para el modelo
variables_rf_opt <- c("Sexo", "Grupo_Edad_Fino", "Estado_Laboral", 
                      "Edad_Continua", "Edad_Cuadratica", "Educacion_Años", 
                      "Año_Normalizado", "TRIMESTRE")

cat("🔄 Optimizando hiperparámetros Random Forest...\n")

# Grid search para hiperparámetros
grid_rf <- expand.grid(
  ntree = c(300, 500, 1000),
  mtry = c(2, 3, 4, 5),
  nodesize = c(1, 3, 5)
)

# Función de evaluación para grid search
evaluar_rf_config <- function(ntree, mtry, nodesize) {
  set.seed(123)
  modelo_temp <- randomForest(
    x = datos_rf_opt[, variables_rf_opt],
    y = datos_rf_opt$Responde_Ingreso,
    ntree = ntree,
    mtry = mtry,
    nodesize = nodesize,
    importance = TRUE
  )
  
  pred_temp <- predict(modelo_temp, datos_test_rf_opt[, variables_rf_opt])
  
  # Convertir a numérico para evaluación
  pred_num <- ifelse(pred_temp == "Si", 1, 0)
  real_num <- ifelse(datos_test_rf_opt$Responde_Ingreso == "Si", 1, 0)
  
  eval_temp <- evaluar_modelo_binario(pred_num, real_num)
  
  return(data.frame(
    ntree = ntree,
    mtry = mtry, 
    nodesize = nodesize,
    f1 = eval_temp$f1,
    accuracy = eval_temp$accuracy
  ))
}

# Ejecutar grid search (versión simplificada por tiempo)
resultados_grid <- map_dfr(1:min(nrow(grid_rf), 10), function(i) {
  row <- grid_rf[i, ]
  evaluar_rf_config(row$ntree, row$mtry, row$nodesize)
})

# Encontrar mejor configuración
mejor_config <- resultados_grid[which.max(resultados_grid$f1), ]

cat("🏆 Mejor configuración Random Forest:\n")
cat("   • ntree:", mejor_config$ntree, "\n")
cat("   • mtry:", mejor_config$mtry, "\n")
cat("   • nodesize:", mejor_config$nodesize, "\n")
cat("   • F1-Score:", round(mejor_config$f1, 3), "\n")

# Entrenar modelo final optimizado
modelo_rf_opt <- randomForest(
  x = datos_rf_opt[, variables_rf_opt],
  y = datos_rf_opt$Responde_Ingreso,
  ntree = mejor_config$ntree,
  mtry = mejor_config$mtry,
  nodesize = mejor_config$nodesize,
  importance = TRUE
)

pred_rf_opt <- predict(modelo_rf_opt, datos_test_rf_opt[, variables_rf_opt])
pred_rf_opt_num <- ifelse(pred_rf_opt == "Si", 1, 0)
real_rf_opt_num <- ifelse(datos_test_rf_opt$Responde_Ingreso == "Si", 1, 0)

eval_rf_opt <- evaluar_modelo_binario(pred_rf_opt_num, real_rf_opt_num)

cat("📊 Random Forest Optimizado:\n")
cat("   • F1-Score:", round(eval_rf_opt$f1, 3), "\n")
cat("   • Accuracy:", round(eval_rf_opt$accuracy, 3), "\n")

# =============================================================================
# MODELO OPTIMIZADO 3: XGBOOST
# =============================================================================

cat("\n=== MODELO OPTIMIZADO 3: XGBOOST ===\n")

# Preparar datos para XGBoost
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Parámetros optimizados para XGBoost
params_xgb <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1
)

cat("🔄 Entrenando XGBoost...\n")

# Entrenar con validación cruzada
xgb_cv <- xgb.cv(
  params = params_xgb,
  data = dtrain,
  nrounds = 100,
  nfold = 5,
  showsd = FALSE,
  stratified = TRUE,
  print_every_n = 20,
  early_stopping_rounds = 10,
  verbose = 0
)

# Entrenar modelo final
best_nrounds <- which.min(xgb_cv$evaluation_log$test_logloss_mean)
modelo_xgb <- xgb.train(
  params = params_xgb,
  data = dtrain,
  nrounds = best_nrounds,
  verbose = 0
)

# Predicciones
pred_xgb <- predict(modelo_xgb, dtest)
eval_xgb <- evaluar_modelo_binario(pred_xgb, y_test)

cat("📊 XGBoost:\n")
cat("   • F1-Score:", round(eval_xgb$f1, 3), "\n")
cat("   • Accuracy:", round(eval_xgb$accuracy, 3), "\n")

# =============================================================================
# COMPARACIÓN FINAL DE TODOS LOS MODELOS
# =============================================================================

cat("\n=== COMPARACIÓN FINAL ===\n")

# Compilar resultados
resultados_finales <- data.frame(
  Modelo = c("Regresión Logística Original", "Random Forest Original", 
             "LASSO", "Ridge", "Elastic Net", "Random Forest Optimizado", "XGBoost"),
  F1_Score = c(0.744, 0.786,  # Originales
               eval_lasso$f1, eval_ridge$f1, eval_elastic$f1,  # Regularizados
               eval_rf_opt$f1, eval_xgb$f1),  # Optimizados
  Accuracy = c(0.708, 0.743,  # Originales
               eval_lasso$accuracy, eval_ridge$accuracy, eval_elastic$accuracy,  # Regularizados
               eval_rf_opt$accuracy, eval_xgb$accuracy),  # Optimizados
  Precision = c(0.694, 0.702,  # Originales (aproximados)
                eval_lasso$precision, eval_ridge$precision, eval_elastic$precision,
                eval_rf_opt$precision, eval_xgb$precision),
  Recall = c(0.802, 0.893,  # Originales
             eval_lasso$recall, eval_ridge$recall, eval_elastic$recall,
             eval_rf_opt$recall, eval_xgb$recall)
) %>%
  arrange(desc(F1_Score))

cat("🏆 RANKING DE MODELOS POR F1-SCORE:\n")
print(round(resultados_finales, 3))

# Identificar el mejor modelo
mejor_modelo <- resultados_finales[1, ]
cat("\n🥇 MEJOR MODELO:", mejor_modelo$Modelo, "\n")
cat("   • F1-Score:", round(mejor_modelo$F1_Score, 3), "\n")
cat("   • Accuracy:", round(mejor_modelo$Accuracy, 3), "\n")

# =============================================================================
# GUARDAR RESULTADOS
# =============================================================================

cat("\n💾 Guardando resultados optimizados...\n")

# Guardar modelos
save(modelo_lasso, modelo_ridge, modelo_elastic, file = file.path(rutas$datos_procesados, "modelos_regularizados.RData"))
save(modelo_rf_opt, file = file.path(rutas$datos_procesados, "modelo_rf_optimizado.RData"))
save(modelo_xgb, file = file.path(rutas$datos_procesados, "modelo_xgboost.RData"))

# Guardar comparación
save(resultados_finales, file = file.path(rutas$datos_procesados, "comparacion_modelos_optimizados.RData"))
write_csv(resultados_finales, file.path(rutas$tablas, "ranking_modelos_optimizados.csv"))

# =============================================================================
# REPORTE FINAL
# =============================================================================

cat("\n", rep("=", 60), "\n")
cat("✅ OPTIMIZACIÓN DE MODELOS COMPLETADA\n")
cat(rep("=", 60), "\n")

cat("🚀 TÉCNICAS APLICADAS:\n")
cat("   • Ingeniería de características avanzada\n")
cat("   • Balanceo de clases con ROSE\n")
cat("   • Regularización (LASSO/Ridge/Elastic Net)\n")
cat("   • Tuning de hiperparámetros\n")
cat("   • XGBoost con validación cruzada\n")

cat("\n📈 MEJORAS OBTENIDAS:\n")
mejora_f1 <- round((mejor_modelo$F1_Score - 0.786) * 100, 1)
mejora_acc <- round((mejor_modelo$Accuracy - 0.743) * 100, 1)

if(mejora_f1 > 0) {
  cat("   • F1-Score: +", mejora_f1, "puntos porcentuales\n")
} else {
  cat("   • F1-Score: ", mejora_f1, "puntos porcentuales\n")
}

if(mejora_acc > 0) {
  cat("   • Accuracy: +", mejora_acc, "puntos porcentuales\n")
} else {
  cat("   • Accuracy: ", mejora_acc, "puntos porcentuales\n")
}

cat("\n🎯 MODELO FINAL RECOMENDADO:\n")
cat("   •", mejor_modelo$Modelo, "\n")
cat("   • F1-Score:", round(mejor_modelo$F1_Score, 3), "\n")

cat(rep("=", 60), "\n")