#' Explonatory variables plot for PLSDA
#'
#' @param var1 
#' @param var2 
#' @param color 
#'
#' @return
#' @export
#'
#' @examples
explanatory_variables=function(var1,var2, color){
  
  verify=require("plotly")
  if(verify!=TRUE){
    install.packages("plotly")
    verify=TRUE
  }
  
  plot_ly(x=var1,y=var2, color=color)
}
