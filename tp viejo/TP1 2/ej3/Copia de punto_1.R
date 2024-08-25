# Load the necessary libraries for data analysis and visualization
library(ggplot2)  # For creating plots
library(dplyr)    # For data manipulation
run_experiment <- function(datasets_to_pred, filepath) {
  exp_results <- list()  # Almacenar los resultados del experimento
  i <- 1  # Inicializar contador para los resultados del experimento
  
  for (dtp in datasets_to_pred) {
    for (prop_NAs in c(0, 0.7)) {
      print(c(dtp$dataset_name, "No", prop_NAs))
      
      data_df <- read.csv(dtp$file_path)  # Cargar el conjunto de datos
      
      # Convertir todas las columnas a variables categóricas
      data_df <- data_df %>%
        mutate_all(as.factor)
      
      # Crear el árbol de regresión
      tree <- rpart(dtp$target_col ~ ., data = data_df, control = rpart.control(minsplit = 2, minbucket = 1, maxdepth = 10, cp = 0, xval = 0))
      
      # Obtener la importancia de las variables
      variable_importance <- tree$variable.importance
      
      for (vars_removed in 1:5) {
        # Identificar las variables menos importantes
        less_important_vars <- names(sort(variable_importance, decreasing = FALSE)[1:vars_removed])
        
        # Eliminar las variables menos importantes del DataFrame
        data_df_reduced <- data_df %>% select(-one_of(less_important_vars))
        
        # Realizar el experimento para la configuración actual
        res_tmp <- est_auc_across_depths_no_par(
          data.frame(dataset_name = dtp$dataset_name, prop_NAs = prop_NAs, IMPUTED = "No"),
          data_df_reduced,
          dtp$target_col,
          preprocess_control,
          max_maxdepth = 30,
          prop_val = 0.25,
          val_reps = 30
        )
        
        res_tmp$vars_removed <- vars_removed
        exp_results[[i]] <- res_tmp
        rm(res_tmp)  # Limpiar resultado temporal
        i <- i + 1  # Incrementar contador de resultados
      }
    }
  }
  
  # Combinar los resultados del experimento en un solo marco de datos
  exp_results <- do.call(rbind, exp_results)
  
  # Guardar los resultados del experimento en un archivo
  write.table(exp_results, filepath, row.names = FALSE, sep = "\t")
}

# Cargar los conjuntos de datos
datasets_to_pred <- list(
  list(file_path = "/ruta/al/archivo.csv", target_col = "nombre_de_la_variable_objetivo", dataset_name = "nombre_del_conjunto_de_datos")
)

# Ejecutar el experimento
if (RERUN_EXP == TRUE) {
  run_experiment(datasets_to_pred, "/ruta/del/archivo_de_resultados.txt")
}

# Constants and global variables
PARALLELIZE <- TRUE # Set the option for parallelization of computations
N_THREADS <- 6     # Define the number of threads for parallel processing
N_BINS <- 10        # Define the number of bins for discretization
RERUN_EXP <- TRUE   # Set the option to rerun the experiment

# Load provided functions
source("/Users/ezequielkaplan/Downloads/TP1 2/ej3/provided_functions.R")

# Load the datasets
datasets_to_pred <- list(
  load_df("/Users/ezequielkaplan/Downloads/TP1\ 2/ej3/data/customer_churn.csv", "Churn", "churn")
)

# Run the experiment
if (RERUN_EXP == TRUE) {
  exp_results_file <- "/Users/ezequielkaplan/Downloads/TP1\ 2/src/outputs/tables/sample_exp.txt"
  
  exp_results <- list()  # Store experiment results
  
  for (num_vars_to_remove in 1:5) {
    data_df <- read.csv("/Users/ezequielkaplan/Downloads/TP1 2/src/data/customer_churn.csv")
    
    # Convertir todas las columnas a variables categóricas
    data_df <- data_df %>%
      mutate_all(as.factor)
    
    # Crear el árbol de regresión
    tree <- rpart(churn ~ ., data = data_df, control = rpart.control(minsplit = 2, minbucket = 1, maxdepth = 10, cp = 0, xval = 0))
    
    # Obtener la importancia de las variables
    variable_importance <- tree$variable.importance
    
    # Identificar las variables menos importantes
    less_important_vars <- names(sort(variable_importance, decreasing = FALSE)[1:num_vars_to_remove])
    
    # Eliminar las variables menos importantes del DataFrame
    data_df_reduced <- data_df %>% select(-one_of(less_important_vars))
    
    # Run experiment for reduced dataset
    run_experiment(datasets_to_pred, exp_results_file)
  }
}

# Plot the experiment results
plot_exp_results( "/Users/ezequielkaplan/Downloads/TP1\ 2/src/outputs/tables/sample_exp.txt", "/Users/ezequielkaplan/Downloads/TP1\ 2/src/outputs/plots/sample_exp.jpg", width=5, height=4)
