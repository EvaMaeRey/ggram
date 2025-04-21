
code_df_to_code_plot <- function(code_df){
  
  code_df |>
  ggplot() +
    aes(code = code) +
    stamp_notebook() +
    geom_tile(stat = StatCode) + 
    scale_fill_manual(values = c(alpha("grey90",.4), alpha("yellow", .4)), 
                      breaks = c(FALSE, TRUE)) +
    geom_text(stat = StatCode, alpha = .7, family = "mono", fontface = "italic") +
    geom_text(stat = StatCodeLineNumbers, family = "mono") +
    theme(legend.position = "none") + 
    stamp_punched_holes()
  
}
