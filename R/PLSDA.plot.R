#' Plot of explanatory variables
#' 
#' @description 
#' 'Plot of explanatory variables 
#' 
#' @param var1 
#' A quantitative variable 
#' 
#' @param var2 
#' A quantitative variable 
#' 
#' @param color 
#' Color of the points in the plot
#'
#' @return a scatter plot
#' @export
#'
#' @examples
#' PLSDA::explanatory_variables(var1 = iris$Sepal.Length,var2=iris$Sepal.Width,color=iris$Species)

explanatory_variables_plot=function(var1,var2, color){
  verify=require("plotly")
  if(verify!=TRUE){
    install.packages("plotly")
    verify=TRUE
  }
  plot_ly(x=var1,y=var2, color=color,type="scatter",mode="markers")%>% layout(title="Scatter plot",
                                                hovermode='closest',dragmode= 'select',
                                                legend=list(title=list(text='Color')),
                                                xaxis = list(
                                                  title = 'Var 1'),
                                                yaxis = list(
                                                  title='Var 2'))
}
