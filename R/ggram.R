#' @export
ggram <- function(title = NULL, widths = c(1,1), ...){
  
  temp <- tempfile()
  savehistory(file = temp)

  plot <- last_plot()
  
  code_plot <- code_file_to_code_df(temp) |>
    code_df_to_code_plot()
  
  
  code_plot + plot + patchwork::plot_layout(widths = widths) +
    patchwork::plot_annotation(title = title, ...) & 
    theme(plot.background = element_rect(colour = "black", linewidth = .05))
  
}

