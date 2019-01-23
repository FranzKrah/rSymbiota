# Test if portal has a tick box or table button
is_table_button <- function(remdriver){

  x <- remdriver$getPageSource()[[1]]
  x <- grep("Table Display|Mostrar Tabla", x)
  ifelse(length(x) == 0, FALSE, TRUE)

}
