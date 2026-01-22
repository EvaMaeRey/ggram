#' @export
ggram_df_output <- function(title = NULL, widths = c(1.1,1), code = NULL, code_style_args = specify_code_plot_style(), ...){
  
  code <- code %||% clipr::read_clip() # get_code(code = code) # 
  code_plot <- specify_code_plot(code, code_style_args = code_style_args)
  output <- eval(parse(text = code))
  output_plot <- gt::gt(output)
  
  patch_code_and_output(code_plot, output_plot, widths, title, ...)
    
}
