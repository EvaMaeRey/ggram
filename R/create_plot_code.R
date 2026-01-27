# for demo purposes only
create_plot_code <- function(){
  
"library(ggplot2)

ggplot(cars) +
  aes(speed, dist) #<< + 
  geom_point()"

  }


code_to_vector <- function(code){
  
  if(code |> is.null()){NULL}else{str_split(code, "\\n")[[1]]}
  
}
