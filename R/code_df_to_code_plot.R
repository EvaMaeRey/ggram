code_df_to_code_plot <- function(code_df, 
                                 style = NULL, 
                                 highlight_colors = c(alpha("grey90",.4), alpha("yellow", .4)),
                                 family = "mono",
                                 size = 1,
                                 vline_color = "darkred", 
                                 hline_color = "blue", 
                                 paper_color = alpha("whitesmoke", .1),
                                 width = 35,
                                 height = 20, 
                                 accent = stamp_punched_holes()
                       
                                 
                                 ){
  
  style <- style %||% stamp_notebook(vline_color, hline_color, paper_color, width, height, accent)
  
  code_df |>
  ggplot() +
    aes(code = code) +
    geom_tile(stat = StatCode) + 
    scale_fill_manual(values = highlight_colors, 
                      breaks = c(FALSE, TRUE), guide = "none") +
    geom_text(stat = StatCode, alpha = .7, family = family) +
    geom_text(stat = StatCodeLineNumbers, family = family) +
    theme(legend.position = "none") + 
    style
  
}
