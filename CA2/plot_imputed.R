#' Plot Imputation Results
#'
#' This function is adapted from code by Hans Ekkehard Plesser.
#'
#' @param orig A numeric vector or time series containing the NAs.
#' @param filled A numeric vector or time series with imputed values.
#' @return Returns NULL. Called for plotting.
#' @export


plot_imputed <- function(orig, filled) {
  # Imputation plot
  p <- imputeTS::ggplot_na_imputations(orig, filled) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank()
    )
  
  # Distribution plot
  q <- tibble::tibble(Original=as.vector(orig), Imputed=filled) |>
    tidyr::pivot_longer(
      tidyr::everything(),
      names_to="Data",
      values_to="Value"
    ) |>
    ggplot2::ggplot(ggplot2::aes(x=Value, color=Data)) +
    ggplot2::geom_density(bw=1) + 
    ggplot2::ylab('Density') +
    ggplot2::theme(legend.position="right")
  
  # Combined
  pc <- cowplot::plot_grid(p, q, nrow=2, ncol=1, rel_heights=c(2, 1))
  print(pc)
}
