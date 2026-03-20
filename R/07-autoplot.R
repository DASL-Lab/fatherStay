
#' Plot the nowcasts
#' 
#' @param multidadnow A multidadnow object.
#' @param last_n The number of most recent observations to plot (in addition to the nowcasts). Defaults to all.
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.multidadnow <- function(multidadnow, se = FALSE, last_n = Inf, alpha = 0.2) {
  plot_data <- multidadnow$data[!is.na(multidadnow$data[, multidadnow$response]), ]
  
  training_data <- plot_data[plot_data$model == "Training", ]
  if (last_n < nrow(training_data)) {
    training_data <- training_data[(nrow(training_data) - last_n):nrow(training_data), ]
  }
  training_data$model <- NULL

  plot_data <- plot_data[plot_data$model != "Training", ]

  g <- ggplot2::ggplot()
    
  if (se) {
    g <- g +
      ggplot2::geom_ribbon(
        data = plot_data,
        mapping = ggplot2::aes(
          x = !!sym(multidadnow$date_col),
          ymin = pi_lower,
          ymax = pi_upper,
          colour = model,
          group = paste0(model, params, formula)
        ),
        alpha = alpha,
        linetype = "dashed",
        fill = "lightgrey"
      )
  }


  if (last_n > 0) {
    g <- g +
      ggplot2::geom_line(
        data = training_data,
        mapping = ggplot2::aes(
          x = !!sym(multidadnow$date_col),
          y = !!sym(multidadnow$response)
        ),
        colour = "black",          # optional: make it stand out
        size   = 1,
        inherit.aes = FALSE
      )
  }
  
  g <- g + ggplot2::geom_line(
      data = plot_data,
      mapping = ggplot2::aes(
        x = !!sym(multidadnow$date_col),
        y = !!sym(multidadnow$response),
        colour = model,
        group = paste0(model, params, formula)
      )
    ) +
      ggplot2::labs(x = "Date", y = "Prediction", colour = "Model")

  g
}



