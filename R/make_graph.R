#' Create an interactive graph making experience
#' @param data A data frame containing the data to plot
#' @export
make_graph <- function(data) {
  # Charger les packages nécessaires
  library(rlang)
  library(dplyr)
  
  # Helper function to get yes/no input
  get_yes_no <- function(prompt) {
    response <- tolower(readline(prompt))
    return(response == "y" || response == "yes" || response == "")
  }
  
  # Helper function to get variable input with validation
  get_variable_input <- function(prompt, valid_vars) {
    cat("\nAvailable variables:\n", paste(valid_vars, collapse = ", "), "\n")
    while(TRUE) {
      var <- readline(prompt)
      if(var %in% valid_vars || (var == "none")) {
        return(var)
      }
      cat("Invalid variable name. Please choose from the available variables.\n")
    }
  }
  
  # Helper function to get renamed values
  get_renamed_values <- function(original_values) {
    renamed_values <- vector("character", length(original_values))
    names(renamed_values) <- original_values
    
    if(get_yes_no("Do you want to rename these values? (Y/n): ")) {
      cat("\nEnter new names for each value:\n")
      for(i in seq_along(original_values)) {
        value <- original_values[i]
        new_name <- readline(sprintf("New name for '%s': ", value))
        if(new_name != "") {
          renamed_values[i] <- new_name
        } else {
          renamed_values[i] <- value
        }
      }
      return(setNames(renamed_values, original_values))
    }
    return(NULL)
  }
  
  # 1. Ask if user wants to make a barplot
  if(!get_yes_no("Do you want to make a barplot? (Y/n): ")) {
    cat("Only barplots are supported at the moment.\n")
    return(NULL)
  }
  
  # Get available variable names
  valid_vars <- names(data)
  
  # 2. Get X variable
  x_var <- get_variable_input("Which variable do you want on the X axis?: ", valid_vars)
  
  # Display possible values for X variable
  x_values <- unique(data[[x_var]])
  cat("\nThe variable '", x_var, "' has the following values and counts:\n", sep = "")
  x_value_counts <- data %>%
    count(.data[[x_var]]) %>%
    arrange(desc(n))
  print(x_value_counts)
  
  # Confirm X variable choice
  if(!get_yes_no("Do you want to proceed with this X variable? (Y/n): ")) {
    cat("Let's choose the X variable again.\n")
    x_var <- get_variable_input("Which variable do you want on the X axis?: ", valid_vars)
  }
  
  # 3. Get Y variable and summarization method
  y_var <- get_variable_input("Which variable do you want to summarize on the Y axis?: ", valid_vars)
  
  # Ask if user wants to see possible values of Y variable
  if(get_yes_no("Do you want to see the possible values of this variable? (Y/n): ")) {
    y_values <- unique(data[[y_var]])
    cat("\nThe variable '", y_var, "' has the following values and counts:\n", sep = "")
    y_value_counts <- data %>%
      count(.data[[y_var]]) %>%
      arrange(desc(n))
    print(y_value_counts)
  }
  
  # Demander si l'utilisateur souhaite créer une variable binaire basée sur des conditions
  create_binary_var <- get_yes_no("Do you want to create a binary variable based on specific values of this variable? (Y/n): ")
  
  if(create_binary_var) {
    # Afficher les valeurs uniques de la variable y_var
    unique_values <- unique(data[[y_var]])
    cat("\nUnique values in '", y_var, "':\n", paste(unique_values, collapse = ", "), "\n", sep = "")
    
    # Demander les valeurs qui seront codées en 1
    values_for_one <- readline("Enter the values that should be coded as 1, separated by commas: ")
    values_for_one <- strsplit(values_for_one, ",")[[1]]
    values_for_one <- trimws(values_for_one)
    
    # Créer la nouvelle variable binaire
    new_y_var <- paste0(y_var, "_binary")
    data[[new_y_var]] <- ifelse(data[[y_var]] %in% values_for_one, 1, 0)
    
    # Mettre à jour y_var pour utiliser la nouvelle variable binaire
    y_var <- new_y_var
    valid_vars <- names(data)
    
    # Afficher un message de confirmation
    cat("\nNew binary variable '", y_var, "' has been created based on '", y_var, "'.\n", sep = "")
    
    # Demander si l'utilisateur veut voir les valeurs de la nouvelle variable
    if(get_yes_no("Do you want to see the values of the new variable? (Y/n): ")) {
      print(table(data[[y_var]], useNA = "ifany"))
    }
  }
  
  # Demander à l'utilisateur de spécifier les variables de regroupement
  cat("\nNow, specify the variables you want to group by.")
  group_vars <- c()
  
  repeat {
    group_var <- get_variable_input("Enter a grouping variable (or 'none' to finish): ", valid_vars)
    if(group_var == "none") {
      break
    } else {
      group_vars <- c(group_vars, group_var)
    }
  }
  
  if(length(group_vars) == 0) {
    group_vars <- c(x_var)
  }
  
  # Ask how to summarize the Y variable
  cat("\nHow do you want to summarize the variable on the Y axis?")
  cat("\nOptions:")
  cat("\n1. **Mean**: Calculate the average of the variable. Use this for numerical variables.")
  cat("\n2. **Sum**: Calculate the total sum of the variable. Useful for counting occurrences or totals.")
  cat("\n3. **Custom function**: Enter your own R function to summarize the variable (e.g., median, sd).")
  summary_choice <- readline("\nEnter your choice (1/2/3): ")
  
  summary_function <- NULL
  if(summary_choice == "1") {
    summary_function <- "mean"
    cat("\nYou have chosen to calculate the **mean** of '", y_var, "'.\n", sep = "")
  } else if(summary_choice == "2") {
    summary_function <- "sum"
    cat("\nYou have chosen to calculate the **sum** of '", y_var, "'.\n", sep = "")
  } else if(summary_choice == "3") {
    summary_function <- readline("Enter the R function to summarize (e.g., median, sd): ")
    cat("\nYou have chosen to use the custom function '", summary_function, "'.\n", sep = "")
  } else {
    stop("Invalid choice for summary function.")
  }
  
  # Confirm choices
  cat("\nSummary of your choices:")
  cat("\n- X variable:", x_var)
  cat("\n- Grouping variables:", paste(group_vars, collapse = ", "))
  cat("\n- Y variable:", y_var)
  cat("\n- Summary function:", summary_function, "\n")
  
  if(!get_yes_no("Do you want to proceed with these choices? (Y/n): ")) {
    cat("Let's start over.\n")
    return(make_graph(data))
  }
  
  # 4. Get fill variable
  fill_var <- get_variable_input("Which variable do you want to use for fill? (or 'none'): ", valid_vars)
  if (fill_var == "none") {
    fill_var <- NULL
  } else {
    # Display possible values for fill variable
    fill_values <- unique(data[[fill_var]])
    cat("\nThe variable '", fill_var, "' has the following values and counts:\n", sep = "")
    fill_value_counts <- data %>%
      count(.data[[fill_var]]) %>%
      arrange(desc(n))
    print(fill_value_counts)
  }
  
  # 5. Get variable labels and renaming
  # For X variable
  x_values <- unique(data[[x_var]])
  x_rename <- get_renamed_values(x_values)
  
  # For fill variable (if used)
  fill_rename <- NULL
  if(!is.null(fill_var)) {
    fill_values <- unique(data[[fill_var]])
    fill_rename <- get_renamed_values(fill_values)
  }
  
  # 6. Get y-axis title
  y_title <- readline("Enter title for Y axis (press Enter for default): ")
  if(y_title == "") {
    y_title <- paste0("Summary of ", y_var)
  }
  
  # 7. Ask about logo
  logo <- get_yes_no("Do you want to add the Datagotchi logo? (Y/n): ")
  
  # 8. Ask about sample size in caption
  n <- get_yes_no("Do you want to show sample size in the caption? (Y/n): ")
  
  # 9. Get caption
  caption <- readline("Enter caption (press Enter for none): ")
  
  # 10. Get save path
  save_path <- readline("Enter path to save the plot (press Enter for none): ")
  if(save_path == "") save_path <- NULL
  
  # 11. Get weight variable
  weight_var <- get_variable_input("Which variable should be used for weights? (or 'none'): ", valid_vars)
  if(weight_var == "none") weight_var <- NULL
  
  # Préparer les données pour le graphique
  summary_fun <- match.fun(summary_function)
  
  if (is.null(weight_var)) {
    plot_data <- data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(y_value = summary_fun(.data[[y_var]], na.rm = TRUE), .groups = 'drop')
  } else {
    plot_data <- data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(y_value = summary_fun(.data[[y_var]] * .data[[weight_var]], na.rm = TRUE), .groups = 'drop')
  }
  
  # Mettre à jour y_var pour le graphique
  y_var <- "y_value"
  
  # Create the plot using datagotchi_barplot
  result <- datagotchi_barplot(
    data = plot_data,
    x = x_var,
    y = y_var,
    fill = fill_var,
    summary_function = NULL,  # Déjà résumé
    weight = NULL,            # Déjà pris en compte
    x_rename = x_rename,
    fill_rename = fill_rename,
    logo = logo,
    caption = caption,
    save_path = save_path,
    y_title = y_title
    # Add other parameters if necessary
  )
  
  # Display the plot
  print(result)
  
  return(result)
}
