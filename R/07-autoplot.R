
#' Plot the nowcasts
#' 
#' @param multidadnow A multidadnow object.
#' @param last_n The number of most recent observations to plot (in addition to the nowcasts). Defaults to all.
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.multidadnow <- function(multidadnow, last_n = NULL) {
  plot_data <- data.frame(
    x = multidadnow$models[[1]]$prepped_data$dates_train,
    y = multidadnow$models[[1]]$prepped_data$y_train,
    method = "Training Data"
  )
  if (is.null(last_n)) last_n <- length(dadnow$data$dates_nowcast)
  if (last_n >= nrow(plot_data)) last_n <- nrow(plot_data) - 1
  plot_data <- plot_data[(nrow(plot_data)- last_n):nrow(plot_data), ] 

  for (i in 1:length(multidadnow$models)) {
    if (multidadnow$models[[i]]$model_id %in% plot_data$method) {
      new_name <- paste0(multidadnow$models[[i]]$model_id, "_", i)
    } else {
      new_name <- multidadnow$models[[i]]$model_id
    }
    plot_data <- rbind(plot_data, 
      data.frame(
        x = multidadnow$models[[i]]$prepped_data$dates_nowcast,
        y = multidadnow$models[[i]]$prediction,
        method = new_name
      )
    )
  }

  ggplot2::ggplot(data = plot_data) +
    ggplot2::aes(x = x, y = y, colour = method) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Date", y = "Prediction", colour = "Model")
}



