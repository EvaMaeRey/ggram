ggram_tp_output <- function(title = NULL, widths = c(1.1,1), code = NULL, ...){
  
  code <- get_code(code = code)
  code_plot <- specify_code_plot(code)
  output <- eval(parse(text = code))
  tp_output <- output |> tidypivot::collect() 
  output_plot <- gt::gt(tp_output)
  
  patch_code_and_output(code_plot, output_plot, widths, title, ...)
    
}
