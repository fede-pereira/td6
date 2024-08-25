# Load the necessary libraries for data analysis and visualization
library(ggplot2)  # For creating plots
library(dplyr)    # For data manipulation

eliminar_columnas_menos_importantes <- function(dataset_list, cant_elimino) {
  data_df <- dataset_list$data_df
  var_to_predict <- dataset_list$var_to_predict

  # Calcular importancia de las variables
  tree <- rpart(data_df[[var_to_predict]] ~ ., data = data_df,
                control = rpart.control(minsplit = 2, minbucket = 1, maxdepth = 10, cp = 0, xval = 0))
  variable_importance <- tree$variable.importance
  # Identificar las columnas menos importantes a eliminar
  less_important_vars <- names(sort(variable_importance, decreasing = FALSE)[1:cant_elimino])
  
  # Excluir la columna de la variable a predecir del proceso de eliminación
  less_important_vars <- setdiff(less_important_vars, var_to_predict)
  
  # Eliminar las columnas menos importantes
  data_df <- data_df %>% select(-one_of(less_important_vars))
  
  # Actualizar el dataset en la lista
  dataset_list$data_df <- data_df

  return(dataset_list)
}


# Constants and global variables
PARALLELIZE <- TRUE # Set the option for parallelization of computations
N_THREADS <- 3     # Define the number of threads for parallel processing
N_BINS <- 10        # Define the number of bins for discretization
RERUN_EXP <- TRUE   # Set the option to rerun the experiment

# Load provided functions
source("/Users/ezequielkaplan/Downloads/TP1\ 2/src/provided_functions.R")

# Run an experiment to evaluate the performance of a predictive model under different conditions.
run_experiment <- function(datasets_to_pred, filepath) {
  exp_results <- list()  # Store experiment results
  i <- 1  # Initialize counter for experiment results
  
  for (dtp in datasets_to_pred) {
    for (impute in c("No")) {
      for (prop_NAs in c(0,1,4,5)) {

        dtp_modified <- eliminar_columnas_menos_importantes(dtp, prop_NAs)


        preprocess_control <- list(
          prop_NAs = 0,
          impute_NAs = FALSE,
          treat_NAs_as_new_levels = FALSE,
          do_ohe = FALSE,
          discretize = FALSE,
          n_bins = N_BINS,
          ord_to_numeric = FALSE,
          prop_switch_y = 0
        )
        
        if (PARALLELIZE) {
          res_tmp <- est_auc_across_depths(dtp_modified, preprocess_control,
                                           max_maxdepth = 30, prop_val = 0.25,
                                           val_reps = 30)
        } else {
          res_tmp <- est_auc_across_depths_no_par(dtp_modified, preprocess_control,
                                                  max_maxdepth = 30, prop_val = 0.25,
                                                  val_reps = 30)
        }
        
        res_tmp$IMPUTED <- impute
        res_tmp$prop_NAs <- prop_NAs
        exp_results[[i]] <- res_tmp
        rm(res_tmp)  # Clean up temporary result
        i <- i + 1  # Increment result counter
      }
    }
  }
  
  exp_results <- do.call(rbind, exp_results)
  
  write.table(exp_results, filepath, row.names = FALSE, sep = "\t")
}

# Plot the results of the sample experiment using ggplot2.
plot_exp_results <- function(filename_exp_results, filename_plot, width, height) {
  exp_results <- read.table(filename_exp_results, header = TRUE, sep = "\t")
  
  data_for_plot <- exp_results %>%
    group_by(dataset_name, prop_NAs, IMPUTED, maxdepth) %>%
    summarize(mean_auc = mean(auc), .groups = 'drop')
  
  g <- ggplot(data_for_plot, aes(x = maxdepth, y = mean_auc, color = IMPUTED)) +
    geom_line() +
    theme_bw() +
    xlab("Maximum tree depth") +
    ylab("AUC (estimated through repeated validation)") +
    ylim(0.4, 1) +  # Establecer el rango del eje y
    facet_grid(dataset_name ~ prop_NAs, scales = "free_y") +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA))
  
  ggsave(filename_plot, g, width = width, height = height)
}


# Load the datasets
datasets_to_pred <- list(
  load_df("/Users/ezequielkaplan/Downloads/TP1\ 2/src/data/customer_churn.csv", "Churn", "churn"),
  load_df("/Users/ezequielkaplan/Downloads/TP1\ 2/src/data/heart.csv", "Heart", "HeartDisease"),
  load_df("/Users/ezequielkaplan/Downloads/TP1\ 2/src/data/HousePricePrediction.csv", "House", "Mayor_2")
)

# Run the experiment
if (RERUN_EXP) {
  run_experiment(datasets_to_pred, "/Users/ezequielkaplan/Downloads/TP1\ 2/src/outputs/tables/sample_exp.txt")
}

# Plot the experiment results
plot_exp_results("/Users/ezequielkaplan/Downloads/TP1\ 2/src/outputs/tables/sample_exp.txt",
                 "/Users/ezequielkaplan/Downloads/TP1\ 2/src/outputs/plots/sample_exp.jpg", width = 5, height = 4)
