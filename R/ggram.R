#' @export
ggram <- function(title = NULL, widths = c(1,1), code = NULL, style = stamp_notebook(), output_plot = NULL, ...){
  
  code <- get_code()
  code_plot <- specify_code_plot(code, style = style)
  output <- eval(parse(text = code))
  output_plot <- output
  
  patch_code_and_output(code_plot, output_plot, widths, title, ...)
  
}
