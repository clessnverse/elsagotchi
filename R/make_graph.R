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
  
# Demander si l'utilisateur souhaite recoder ou regrouper les catégories
recode_y <- get_yes_no("Do you want to recode or group categories of this variable? (Y/n): ")

if (recode_y) {
  # Afficher les valeurs uniques
  unique_values <- unique(data[[y_var]])
  
  # Convertir les valeurs uniques en caractères, en gérant les NA
  unique_values_char <- sapply(unique_values, function(x) {
    if (is.na(x)) {
      "NA"
    } else {
      as.character(x)
    }
  })
  
  cat("\nUnique values in '", y_var, "':\n", paste(unique_values_char, collapse = ", "), "\n", sep = "")
  
  # Initialiser un mapping pour les nouvelles catégories
  new_categories <- list()
  cat("\nFor each category, enter the new category name (leave blank to keep the original name):\n")
  for (i in seq_along(unique_values)) {
    value <- unique_values[i]
    value_char <- unique_values_char[i]
    new_value <- readline(sprintf("New category for '%s': ", value_char))
    if (new_value == "") {
      new_value <- value_char
    }
    if (is.na(value)) {
      new_categories[["NA"]] <- new_value
    } else {
      new_categories[[value_char]] <- new_value
    }
  }
  
  # Créer une nouvelle variable avec les catégories regroupées
  new_y_var <- readline("Enter a name for the new recoded variable: ")
  
  # Fonction de mappage avec gestion des NA
  data[[new_y_var]] <- vapply(data[[y_var]], function(x) {
    if (is.na(x)) {
      if ("NA" %in% names(new_categories)) {
        new_categories[["NA"]]
      } else {
        NA_character_
      }
    } else {
      x_char <- as.character(x)
      if (x_char %in% names(new_categories)) {
        new_categories[[x_char]]
      } else {
        NA_character_
      }
    }
  }, FUN.VALUE = character(1))
  
  # Mettre à jour y_var pour utiliser la nouvelle variable
  y_var <- new_y_var
  valid_vars <- names(data)
  
  # Afficher les nouvelles catégories et leurs fréquences
  if (get_yes_no("Do you want to see the new categories and counts? (Y/n): ")) {
    y_value_counts <- data %>%
      count(.data[[y_var]]) %>%
      arrange(desc(n))
    print(y_value_counts)
  }
}
  
  # Ask how to summarize the Y variable
  cat("\nHow do you want to summarize the variable on the Y axis?")
  cat("\nOptions:")
  cat("\n1. **Mean**: Calculate the average of the variable. Use this for numerical variables.")
  cat("\n2. **Sum**: Calculate the total sum of the variable. Useful for counting occurrences or totals.")
  cat("\n3. **Proportion of specific values**: Calculate the percentage of specific categories within each group.")
  cat("\n4. **Custom function**: Enter your own R function to summarize the variable (e.g., median, sd).")
  summary_choice <- readline("\nEnter your choice (1/2/3/4): ")
  
  summary_function <- NULL
  if(summary_choice == "1") {
    summary_function <- "mean"
    cat("\nYou have chosen to calculate the **mean** of '", y_var, "'.\n", sep = "")
    cat("This is appropriate if '", y_var, "' is a numerical variable.\n", sep = "")
  } else if(summary_choice == "2") {
    summary_function <- "sum"
    cat("\nYou have chosen to calculate the **sum** of '", y_var, "'.\n", sep = "")
    cat("This can be used to count total occurrences or aggregate values.\n")
  } else if(summary_choice == "3") {
    # Get specific values for which to calculate the proportion
    unique_values <- unique(data[[y_var]])
    cat("\nUnique values in '", y_var, "':\n", paste(unique_values, collapse = ", "), "\n", sep = "")
    specific_values <- readline("Enter the values for which you want to calculate the proportion, separated by commas: ")
    specific_values <- strsplit(specific_values, ",")[[1]]
    specific_values <- trimws(specific_values)
    
    # Create a binary variable where specific values are coded as 1
    data[[paste0(y_var, "_binary")]] <- ifelse(data[[y_var]] %in% specific_values, 1, 0)
    y_var <- paste0(y_var, "_binary")
    summary_function <- "mean"
    cat("\nYou have chosen to calculate the **proportion** of '", paste(specific_values, collapse = ", "), "' in '", y_var, "'.\n", sep = "")
  } else if(summary_choice == "4") {
    summary_function <- readline("Enter the R function to summarize (e.g., median, sd): ")
    cat("\nYou have chosen to use the custom function '", summary_function, "' to summarize '", y_var, "'.\n", sep = "")
  } else {
    stop("Invalid choice for summary function.")
  }
  
  # Confirm Y variable and summary function
  cat("\nSummary of your choices:")
  cat("\n- X variable:", x_var)
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
  
  # Create the plot using datagotchi_barplot
  result <- datagotchi_barplot(
    data = data,
    x = x_var,
    y = y_var,
    fill = fill_var,
    summary_function = summary_function,
    weight = weight_var,
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
