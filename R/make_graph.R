#' Create an interactive graph making experience
#' @param data A data frame containing the data to plot
#' @export
make_graph <- function(data) {
  # Helper function to get yes/no input
  get_yes_no <- function(prompt) {
    response <- tolower(readline(prompt))
    return(response == "y" || response == "yes" || response == "")
  }
  
  # Helper function to get variable input with validation
  get_variable_input <- function(prompt, valid_vars) {
    cat("\nAvailable variables:", paste(valid_vars, collapse = ", "), "\n")
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
      for(value in original_values) {
        new_name <- readline(sprintf("New name for '%s': ", value))
        if(new_name != "") {
          renamed_values[value] <- new_name
        } else {
          renamed_values[value] <- value
        }
      }
      return(renamed_values)
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
  
  # 3. Get Y variable and type
  cat("\nFor the Y axis, you can either:")
  cat("\n1. Calculate proportions from a binary variable")
  cat("\n2. Use direct values from a variable")
  y_type <- readline("\nEnter your choice (1/2): ")
  
  if(y_type == "1") {
    y_var <- get_variable_input("Which binary variable do you want to calculate proportions from?: ", valid_vars)
    percents <- TRUE
  } else {
    y_var <- get_variable_input("Which variable do you want on the Y axis?: ", valid_vars)
    percents <- FALSE
  }
  
  # 4. Get fill variable
  fill_var <- get_variable_input("Which variable do you want to use for fill? (or 'none'): ", valid_vars)
  
  # 5. Get variable labels and renaming
  # For X variable
  x_values <- unique(data[[x_var]])
  cat("\nCurrent values for", x_var, ":", paste(x_values, collapse = ", "), "\n")
  x_rename <- get_renamed_values(x_values)
  
  # For fill variable (if used)
  fill_rename <- NULL
  if(fill_var != "none") {
    fill_values <- unique(data[[fill_var]])
    cat("\nCurrent values for", fill_var, ":", paste(fill_values, collapse = ", "), "\n")
    fill_rename <- get_renamed_values(fill_values)
  }
  
  # 6. Get y-axis title
  y_title <- readline("Enter title for Y axis (press Enter for default): ")
  if(y_title == "") {
    y_title <- if(percents) "Proportion (%)" else "Count"
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
    x = !!sym(x_var),
    y = !!sym(y_var),
    fill = if(fill_var != "none") !!sym(fill_var) else NULL,
    y_title = y_title,
    percents = percents,
    x_rename = x_rename,
    fill_rename = fill_rename,
    logo = logo,
    colorset = "issues",  # Could make this configurable too
    n = n,
    caption = caption,
    theme = "datagotchi_light",  # Could make this configurable too
    save_path = save_path,
    weight = if(!is.null(weight_var)) !!sym(weight_var) else NULL
  )
  
  return(result)
}

# Example usage:
# data_filtered <- read.csv("your_data.csv")
# make_graph(data_filtered)

