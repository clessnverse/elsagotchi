#' Create a standardized bar plot for survey data
#' 
#' @param data A data frame containing the survey data
#' @param x Character. The name of the column to use for x-axis categories
#' @param y Character. The name of the column to use for y-axis values (defaults to count/frequency if NULL)
#' @param fill Character. Optional. The name of the column to use for fill colors
#' @param position Character. The position adjustment ("stack", "dodge", "fill"). Defaults to "dodge"
#' @param coord_flip Logical. Whether to flip the coordinates. Defaults to FALSE
#' @param sort Logical. Whether to sort the bars by value. Defaults to FALSE
#' @param labels Logical. Whether to show value labels on bars. Defaults to TRUE
#' @param label_size Numeric. Size of the bar labels. Defaults to 3
#' @param title Character. Optional plot title
#' @param subtitle Character. Optional plot subtitle
#' @param xlab Character. Optional x-axis label
#' @param ylab Character. Optional y-axis label 
#'
#' @return A ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_col geom_text position_dodge position_stack position_fill coord_flip labs
#' @importFrom dplyr count group_by summarise arrange
#' @importFrom clessnize datagotchi_theme_light
#'
datagotchi_barplot <- function(data, 
                              x, 
                              y = NULL,
                              fill = NULL,
                              position = "dodge",
                              coord_flip = FALSE,
                              sort = FALSE,
                              labels = TRUE,
                              label_size = 3,
                              title = NULL,
                              subtitle = NULL,
                              xlab = NULL,
                              ylab = NULL) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  if (!x %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", x))
  }
  if (!is.null(y) && !y %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", y))
  }
  if (!is.null(fill) && !fill %in% names(data)) {
    stop(sprintf("Column '%s' not found in data", fill))
  }
  if (!position %in% c("stack", "dodge", "fill")) {
    stop("'position' must be one of: 'stack', 'dodge', 'fill'")
  }
  
  # Prepare the data
  if (is.null(y)) {
    # If no y variable is specified, count frequencies
    plot_data <- data %>%
      count(.data[[x]], .data[[fill]]) %>%
      setNames(c(x, fill, "n"))
    y <- "n"
  } else {
    plot_data <- data
  }
  
  # Sort data if requested
  if (sort) {
    plot_data <- plot_data %>%
      group_by(.data[[x]]) %>%
      summarise(total = sum(.data[[y]])) %>%
      arrange(desc(total)) %>%
      left_join(plot_data, by = x)
  }
  
  # Create the base plot
  p <- ggplot(plot_data)
  
  # Add the bars with appropriate aesthetics
  if (is.null(fill)) {
    p <- p + geom_col(aes(x = .data[[x]], 
                         y = .data[[y]]),
                     position = position)
  } else {
    p <- p + geom_col(aes(x = .data[[x]], 
                         y = .data[[y]],
                         fill = .data[[fill]]),
                     position = position)
  }
  
  # Add labels if requested
  if (labels) {
    if (position == "dodge") {
      pos <- position_dodge(width = 0.9)
    } else if (position == "stack") {
      pos <- position_stack()
    } else {
      pos <- position_fill()
    }
    
    p <- p + geom_text(
      aes(x = .data[[x]],
          y = .data[[y]],
          label = round(.data[[y]], 1),
          group = if (!is.null(fill)) .data[[fill]] else NULL),
      position = pos,
      vjust = -0.5,
      size = label_size
    )
  }
  
  # Apply the theme and optional modifications
  p <- p + 
    datagotchi_theme_light() +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab %||% x,
      y = ylab %||% y
    )
  
  # Flip coordinates if requested
  if (coord_flip) {
    p <- p + coord_flip()
  }
  
  return(p)
}

# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
