#' @export
ggram <- function(title = NULL, widths = c(1,1), style_output = F, ...){
  
  temp <- tempfile()
  savehistory(file = temp)

  plot <- last_plot()
  
  code_plot <- code_file_to_code_df(temp) |>
    code_df_to_code_plot()
  
  if(style_output){style = stamp_graph_paper()}else{style = NULL}
  
  patchwork::free(code_plot) + plot + style + patchwork::plot_layout(widths = widths) +
    patchwork::plot_annotation(title = title, ...) & 
    theme(plot.background = element_rect(colour = "black", linewidth = .05))
  
}

#' @export
ggram_df_output <- function(title = NULL, widths = c(1.1,1), ...){
  
  temp <- tempfile()
  savehistory(file = temp)

  readLines(temp)[!stringr::str_detect(readLines(temp), "ggram")] |>  
  paste(collapse = "\n") ->
    code
  
  code_plot <- code_file_to_code_df(temp) |>
    code_df_to_code_plot()
  
  eval(parse(text = code)) -> output
  
  
  patchwork::free(code_plot) + gt::gt(output) + patchwork::plot_layout(widths = widths) +
  patchwork::plot_annotation(title = title, ...) & 
  theme(plot.background = element_rect(colour = "black", linewidth = .05))
    
}

ggram_tp_output <- function(title = NULL, widths = c(1,1), ...){
  
  temp <- tempfile()
  savehistory(file = temp)

  readLines(temp)[!stringr::str_detect(readLines(temp), "ggram")] |>  
  paste(collapse = "\n") ->
    code
  
  code_plot <- code_file_to_code_df(temp) |>
    code_df_to_code_plot()
  
  eval(parse(text = code)) |> tidypivot::collect() -> output
  
  
  patchwork::free(code_plot) + gt::gt(output) + patchwork::plot_layout(widths = widths) +
  patchwork::plot_annotation(title = title, ...) & 
  theme(plot.background = element_rect(colour = "black", linewidth = .05))
    
}


#' @export
ggram_text_output <- function(title = NULL, widths = c(1,1), ...){
  
  temp <- tempfile()
  savehistory(file = temp)

  readLines(temp)[!stringr::str_detect(readLines(temp), "ggram")] |>  
  paste(collapse = "\n") ->
    code
  
  code_plot <- code_file_to_code_df(temp) |>
    code_df_to_code_plot()
  
  capture.output(eval(parse(text = code))) -> output
  
  output |>
    data.frame(code = _) |>
      dplyr::mutate(row_number = dplyr::row_number()) |>
      ggplot() + 
      aes(x = 1, 
          y = row_number, 
          label = code) +
      geom_text(hjust = 0, family = "mono") +
    stamp_notebook() ->
    text_output_plot
  
  patchwork::free(code_plot) + text_output_plot + patchwork::plot_layout(widths = widths) +
  patchwork::plot_annotation(title = title, ...) & 
  theme(plot.background = element_rect(colour = "black", linewidth = .05))
    
}
