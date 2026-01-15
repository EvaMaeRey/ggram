
code_df_to_code_plot <- function(code_df, style_specs = specify_code_plot_style()){
  
  stamp_style <- stamp_notebook(style_specs$vline_color, style_specs$hline_color, 
                                style_specs$paper_color, style_specs$width, 
                                style_specs$height, style_specs$accent)
  
  code_df |>
  ggplot() +
    aes(code = code) +
    geom_tile(stat = StatCode) + 
    scale_fill_manual(values = style_specs$highlight_colors, 
                      breaks = c(FALSE, TRUE), guide = "none") +
    geom_text(stat = StatCode, alpha = .7, family = style_specs$family) +
    geom_text(stat = StatCodeLineNumbers, family = style_specs$family) +
    theme(legend.position = "none") + 
    stamp_style
  
}
