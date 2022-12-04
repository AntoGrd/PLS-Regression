#' PLSDA Scree plot
#' 
#' @description
#' Make a scree plot for the component selection 
#' 
#' @param PLSDA
#' A PLSDA object
#' @return a scree plot which shows which component select
#' @export
#'
#' @examples
#' PLSDA::scree_plot(res)

scree_plot=function(PLSDA){
  
  library(plotly)
  
  # Calculation of correlations 
  ncomp=ncol(PLSDA$X)
  X=PLSDA$X
  corX=cor(X)
  
  # Calculation of the eigenvalues
  eigenvalues=eigen(corX)$values
  test=eigenvalues > 1
  cols <- ifelse(test, "rgba(255, 0, 0, 0.5)", "rgba(0, 0, 255, 0.5)")
  lab=1:ncomp
  # Scree plot
  plot_ly(x=lab, y = eigenvalues,type = "bar",color = I(cols))%>%
    layout(
      xaxis=list(title="Components"),
      yaxis=list(title="Eigenvalues"),
      title="Selection of components"
    )
}
