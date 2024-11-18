#' Create a standardized Datagotchi bar plot
#' 
#' @param data A data frame containing the data to plot
#' @param x Character. The x-axis variable name
#' @param y Character. The y-axis variable name (if computing proportions, this will be the variable to compute proportions from)
#' @param fill Character. The fill variable name
#' @param y_title Character. Title for y-axis
#' @param percents Logical. Whether to display values as percentages
#' @param x_rename Named vector. Renaming values for x variable
#' @param fill_rename Named vector. Renaming values for fill variable
#' @param logo Logical. Whether to add the Datagotchi logo
#' @param colorset Character. Name of the color set to use ("issues", "parties", "custom")
#' @param n Logical. Whether to display sample size in caption
#' @param caption Character. Caption text (will be appended with n if n=TRUE)
#' @param theme Character. Name of the theme to use
#' @param save_path Character. Path where to save the plot
#' @param weight Character. Optional. Name of the weight variable for weighted calculations
#' @export
datagotchi_barplot <- function(data, 
                              x,
                              y,
                              fill,
                              y_title = "Proportion",
                              percents = TRUE,
                              x_rename = NULL,
                              fill_rename = NULL,
                              logo = TRUE,
                              colorset = "issues",
                              n = TRUE,
                              caption = NULL,
                              theme = "datagotchi_light",
                              save_path = NULL,
                              weight = NULL) {
  
  # Load required packages
  require(dplyr)
  require(ggplot2)
  require(grid)
  require(png)
  
  # Convert inputs to character
  x_var <- deparse(substitute(x))
  y_var <- deparse(substitute(y))
  fill_var <- deparse(substitute(fill))
  weight_var <- if(!is.null(weight)) deparse(substitute(weight)) else NULL
  
  # Define color sets
  color_palettes <- list(
    issues = c(
      "Afro-Américain" = "#4B0082",
      "Latino-Américain" = "#FF7F50",
      "Caucasien" = "#B0C4DE"
    ),
    parties = c(
      "Démocrate" = "#0000FF",
      "Républicain" = "#FF0000",
      "Indépendant" = "#808080"
    )
  )
  
  # Data preparation
  prepared_data <- data %>%
    # Remove NA values
    filter(!is.na(!!sym(x_var)),
           !is.na(!!sym(y_var)),
           !is.na(!!sym(fill_var)))
  
  # Rename categories if specified
  if(!is.null(x_rename)) {
    prepared_data[[x_var]] <- factor(prepared_data[[x_var]], 
                                   levels = names(x_rename),
                                   labels = unname(x_rename))
  }
  
  if(!is.null(fill_rename)) {
    prepared_data[[fill_var]] <- factor(prepared_data[[fill_var]], 
                                      levels = names(fill_rename),
                                      labels = unname(fill_rename))
  }
  
  # Calculate proportions
  if(percents) {
    summarized_data <- prepared_data %>%
      group_by(!!sym(x_var), !!sym(fill_var)) %>%
      summarise(
        proportion = if(!is.null(weight_var)) {
          weighted.mean(!!sym(y_var), w = !!sym(weight_var), na.rm = TRUE)
        } else {
          mean(!!sym(y_var), na.rm = TRUE)
        },
        n = n(),
        .groups = "drop"
      )
  } else {
    summarized_data <- prepared_data
  }
  
  # Calculate total n for caption if needed
  if(n) {
    total_n <- nrow(prepared_data)
    if(!is.null(caption)) {
      caption <- paste0(caption, "\n(n = ", total_n, ")")
    } else {
      caption <- paste0("(n = ", total_n, ")")
    }
  }
  
  # Create the plot
  p <- ggplot(summarized_data, 
              aes(x = !!sym(x_var), 
                  y = if(percents) proportion else !!sym(y_var),
                  fill = !!sym(fill_var))) +
    geom_bar(stat = if(percents) "identity" else "count",
             position = "dodge",
             width = 0.55) +
    geom_text(aes(label = if(percents) 
                  scales::percent(proportion, accuracy = 1)
                  else after_stat(count)), 
              position = position_dodge(width = 0.55),
              vjust = -0.5,
              size = 15,
              family = "PixelOperatorSC",
              fontface = "bold") +
    labs(x = "",
         y = y_title,
         caption = caption) +
    scale_fill_manual(values = color_palettes[[colorset]]) +
    if(percents) scale_y_continuous(labels = scales::percent) else scale_y_continuous() +
    get(paste0("theme_", theme))() +
    theme(
      plot.title = element_text(size = 60),
      axis.text = element_text(size = 40),
      axis.text.x = element_text(size = 40),
      axis.title = element_text(size = 45),
      legend.text = element_text(size = 50),
      legend.title = element_text(size = 55),
      strip.background = element_rect(color = "black", fill = "black"),
      plot.caption = element_text(
        size = 35,
        family = "PixelOperatorSC",
        hjust = 0,
        vjust = -3,
        margin = margin(t = 30, b = 5, l = 5, r = 20),
        lineheight = 0.3
      ),
      plot.margin = margin(t = 15, r = 40, b = 20, l = 20),
      panel.border = element_rect(color = "black", fill = NA, size = 2)
    )
  
  # Save the plot if path is provided
  if(!is.null(save_path)) {
    # Save initial plot
    ggsave(save_path, plot = p, width = 10, height = 8, dpi = 300)
    
    # Add logo if requested
    if(logo) {
      # Read the saved plot and logo
      plot_img <- readPNG(save_path)
      logo_img <- readPNG("path/to/logo.png") # You'll need to specify the actual logo path
      
      # Create new device
      png(save_path, width = 1000, height = 800)
      
      # Draw plot and logo
      grid.raster(plot_img)
      grid.raster(logo_img, x = 0.92, y = 0.06, 
                 width = unit(2, "inches"), 
                 height = unit(1, "inches"))
      
      # Close device
      dev.off()
    }
  }
  
  return(p)
}
