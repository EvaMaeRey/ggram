ggram_text_output <- function(title = NULL, widths = c(1.1,1), code = NULL, ...){
  
  code <- get_code(code = code)
  code_plot <- specify_code_plot(code)
  output <- eval(parse(text = code))
  output_text <- capture.output(output)
  output_plot <- specify_textoutput_plot(output_text)
  
  patch_code_and_output(code_plot, output_plot, widths, title, ...)
    
}
