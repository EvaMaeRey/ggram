code_file_to_code_df <- function(code = NULL, filepath = ".Rhistory"){
  
  if(is.null(code)){code <- readLines(filepath) |> 
    paste(collapse = "\n") }
  
  code |> 
    styler::style_text() |> 
    as.character() |>
    data.frame(code = _) |>
    filter(!stringr::str_detect(code, "^ggram.+"))
  
}
