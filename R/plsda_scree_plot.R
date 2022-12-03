#' PLSDA Scree plot
#' 
#' @description
#' Make a scree plot for the component selection 
#' 
#' @param acp 
#' A ACP model
#' @return a scree plot which shows which component select
#' @export
#'
#' @examples
#' plsda_scree_plot(res)

scree_plot=function(acp){
  
  verify=require("plotly")
  if(verify!=TRUE){
    install.packages("plotly")
    verify=TRUE
  }
  library(plotly)
  
  # Calculation of correlations 
  ncomp=ncol(acp$X)
  X=acp$X
  corX=cor(X)
  
  # Calculation of the eigenvalues
  eigenvalues=eigen(corX)$values
  test=eigenvalues > 1
  cols <- ifelse(test, "rgba(255, 0, 0, 0.5)", "rgba(0, 0, 255, 0.5)")
  lab=paste("Comp.",1:ncomp,sep = "")
  # Scree plot
  plot_ly(x=lab, y = eigenvalues,type = "bar",color = I(cols))%>%
    layout(
      xaxis=list(title="Components"),
      yaxis=list(title="Eigenvalues"),
      title="Selection of components"
    )
}
