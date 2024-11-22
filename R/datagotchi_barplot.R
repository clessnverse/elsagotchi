#' Create a standardized bar plot for survey data
#' 
#' @param data A data frame containing the survey data
#' @param x Character. The name of the column to use for x-axis categories
#' @param y Character. The name of the column to use for y-axis values (defaults to count/frequency if NULL)
#' @param fill Character. Optional. The name of the column to use for fill colors
#' @param percents Logical. Whether to calculate percentages for the y-axis. Defaults to FALSE
#' @param weight Character. Optional. The name of the column to use for weights
#' @param x_rename Named vector. Optional. New names for x-axis categories
#' @param fill_rename Named vector. Optional. New names for fill categories
#' @param logo Logical. Whether to add the Datagotchi logo. Defaults to FALSE
#' @param caption Character. Optional caption text
#' @param save_path Character. Optional path to save the plot
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
datagotchi_barplot <- function(data, 
  x, 
  y = NULL,
  fill = NULL,
  percents = FALSE,
  weight = NULL,
  x_rename = NULL,
  fill_rename = NULL,
  logo = FALSE,
  caption = NULL,
  save_path = NULL,
  position = "dodge",
  coord_flip = FALSE,
  sort = FALSE,
  labels = TRUE,
  label_size = 3,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  y_title = NULL) {

# Charger les packages nécessaires
library(ggplot2)
library(dplyr)
library(rlang)  # Pour utiliser .data
library(grid)   # Pour ajouter le logo
library(png)    # Pour lire les fichiers PNG

# Validation des entrées
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
if (!is.null(weight) && !weight %in% names(data)) {
stop(sprintf("Column '%s' not found in data", weight))
}
if (!position %in% c("stack", "dodge", "fill")) {
stop("'position' must be one of: 'stack', 'dodge', 'fill'")
}

# Renommer les valeurs si nécessaire
if (!is.null(x_rename)) {
data[[x]] <- factor(data[[x]], levels = names(x_rename), labels = x_rename)
}
if (!is.null(fill) && !is.null(fill_rename)) {
data[[fill]] <- factor(data[[fill]], levels = names(fill_rename), labels = fill_rename)
}

# Préparer les données
if (is.null(y)) {
# Si aucune variable y n'est spécifiée, compter les fréquences ou les pourcentages
if (percents) {
if (is.null(fill)) {
plot_data <- data %>%
count(.data[[x]], wt = if (!is.null(weight)) .data[[weight]] else NULL, name = "n") %>%
mutate(perc = n / sum(n) * 100)
} else {
plot_data <- data %>%
count(.data[[x]], .data[[fill]], wt = if (!is.null(weight)) .data[[weight]] else NULL, name = "n") %>%
group_by(.data[[fill]]) %>%
mutate(perc = n / sum(n) * 100)
}
y_var <- "perc"
} else {
if (is.null(fill)) {
plot_data <- data %>%
count(.data[[x]], wt = if (!is.null(weight)) .data[[weight]] else NULL, name = "n")
} else {
plot_data <- data %>%
count(.data[[x]], .data[[fill]], wt = if (!is.null(weight)) .data[[weight]] else NULL, name = "n")
}
y_var <- "n"
}
} else {
# Si une variable y est spécifiée, utiliser ses valeurs
plot_data <- data
y_var <- y
}

# Calculer les étiquettes
plot_data <- plot_data %>%
mutate(label = if (percents) {
paste0(round(.data[[y_var]], 1), '%')
} else {
round(.data[[y_var]], 1)
})

# Créer le graphique de base
p <- ggplot(plot_data)

# Ajouter les barres avec les esthétiques appropriées
if (is.null(fill)) {
p <- p + geom_col(aes_string(x = x, y = y_var), position = position)
} else {
p <- p + geom_col(aes_string(x = x, y = y_var, fill = fill), position = position)
}

# Ajouter les labels si demandé
if (labels) {
if (position == "dodge") {
pos <- position_dodge(width = 0.9)
} else if (position == "stack") {
pos <- position_stack()
} else {
pos <- position_fill()
}

p <- p + geom_text(
aes_string(x = x, y = y_var, label = 'label'),
position = pos,
vjust = -0.5,
size = label_size
)
}

# Appliquer le thème et les modifications optionnelles
p <- p + 
datagotchi_theme_light() +
labs(
title = title,
subtitle = subtitle,
x = xlab %||% x,
y = ylab %||% y_title %||% y_var,
caption = caption
)

# Inverser les coordonnées si demandé
if (coord_flip) {
p <- p + coord_flip()
}

# Ajouter le logo si demandé
if (logo) {
# Chemin vers le logo (à adapter selon l'emplacement réel)
logo_path <- "path/to/Logo.PNG"  # Remplacez par le chemin réel du logo
if (file.exists(logo_path)) {
logo_img <- png::readPNG(logo_path)
logo_grob <- grid::rasterGrob(logo_img, x = 0.9, y = 0.1, width = unit(2, "inches"))
p <- p + annotation_custom(logo_grob)
} else {
warning("Logo file not found at specified path.")
}
}

# Sauvegarder le graphique si un chemin est fourni
if (!is.null(save_path)) {
ggsave(filename = save_path, plot = p, width = 10, height = 8, dpi = 300)
}

return(p)
}