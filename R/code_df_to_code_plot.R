
code_df_to_code_plot <- function(code_df, style = "notebook"){
  
  notebook <- list(stamp_notebook(), stamp_punched_holes())
  
  code_df |>
  ggplot() +
    aes(code = code) +
    geom_tile(stat = StatCode) + 
    scale_fill_manual(values = c(alpha("grey90",.4), alpha("yellow", .4)), 
                      breaks = c(FALSE, TRUE), guide = "none") +
    geom_text(stat = StatCode, alpha = .7, family = "mono") +
    geom_text(stat = StatCodeLineNumbers, family = "mono") +
    theme(legend.position = "none") + 
    notebook
  
}
