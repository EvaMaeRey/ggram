#' @importFrom ggplot2 ggplot geom_tile scale_fill_manual alpha geom_text theme
#' @importFrom ggplot2 element_rect theme_void scale_y_reverse scale_x_continuous
#' @importFrom ggplot2 geom_vline geom_hline annotate last_plot
#' @importFrom utils loadhistory



# INTERNAL FUNCTIONS ------------------------------------------------------
#' @importFrom utils savehistory
## History ----
save_history <- function(){
  temp <- tempfile()
  savehistory(file = temp)
  temp # return temp for exported functions
}


## Plot ----
code_plot <- function(temp){
  code_file_to_code_df(temp) |>
    code_df_to_code_plot()

}

# patchwork + gt -- Shared in ggram_df_output and ggram_tp_output
plot_patch_gt <- function(code_plot,
                          output,
                          widths = widths,
                          title = title,
                          ...
){
  patchwork::free(code_plot) + gt::gt(output) + patchwork::plot_layout(widths = widths) +
    patchwork::plot_annotation(title = title, ...)
}

# theme -- shared across all 4
shared_theme <- function(clr = "black", lwd = 0.05){
  theme(plot.background = element_rect(colour = clr, linewidth = lwd))
}


## Code ----
code_readLines <- function(temp){
  readLines(temp) |>
    grep(pattern = "ggram", x = _,
         invert = TRUE, value = TRUE) |>
    paste(collapse = "\n")
}

eval_code <- function(code){
  eval(parse(text = code))
}

# EXPORTED FUNCTIONS ------------------------------------------------------

#' @export
ggram <- function(title = NULL, widths = c(1,1), ...){

  temp <- save_history()

  plot <- last_plot()

  code_plot <- code_plot(temp)

  patchwork::free(code_plot) + plot + patchwork::plot_layout(widths = widths) +
  patchwork::plot_annotation(title = title, ...) &
  shared_theme()

}

#' @export
ggram_df_output <- function(title = NULL, widths = c(1,1), ...){

  temp <- save_history()

  code <- code_readLines(temp)

  code_plot <- code_plot(temp)

  output <- eval_code(code)

  plot_patch_gt(code_plot, output, widths, title) & shared_theme()

}

#' @export
ggram_tp_output <- function(title = NULL, widths = c(1,1), ...){

  temp <- save_history()

  code <- code_readLines(temp)

  code_plot <- code_plot(temp)

  output <- eval_code(code) |> dplyr::collect()

  plot_patch_gt(code_plot, output, widths, title) & shared_theme()
}

#' @export
ggram_text_output <- function(title = NULL, widths = c(1,1), ...){

  temp <- save_history()

  code <- code_readLines(temp)

  code_plot <- code_plot(temp)

  output <- eval_code(code) |> utils::capture.output()

  text_output_plot <- output |>
    data.frame(code = _) |>
      dplyr::mutate(row_number = dplyr::row_number()) |>
      ggplot() +
      aes(x = 1,
          y = row_number,
          label = code) +
      geom_text(hjust = 0, family = "mono") +
    stamp_notebook()

  patchwork::free(code_plot) + text_output_plot + patchwork::plot_layout(widths = widths) +
  patchwork::plot_annotation(title = title, ...) &
  shared_theme()

}
