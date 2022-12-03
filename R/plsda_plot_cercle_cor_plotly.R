#' Correlation circle plot for PLSDA
#'
#' @description 
#' Correlation circle plot for PLSDA
#' @param PLSDA
#' a PLSDA object
#' @return a correlation circle
#' @export
#'
#' @examples
#' PLSDA::circle_plot(res)

circle_plot <- function(PLSDA){
  
  verify=require("plotly")
  if(verify!=TRUE){
    install.packages("plotly")
    verify=TRUE
  }
  library(plotly)
  
  #get values
  c1=PLSDA$X_loadings[,1]*sqrt(eigen(cor(scale(PLSDA$X)))$values[1])
  c2=PLSDA$X_loadings[,2]*sqrt(eigen(cor(scale(PLSDA$X)))$values[2])
  
  # creation of circle
  liste = list(list(
    x0 = -1, 
    x1 = 1, 
    y0 = -1, 
    y1 = 1, 
    type = "circle"
  ))
  # Creation of the same number of rows as variables (which are zero for the moment)
  for (i in 1:ncol(PLSDA$X)){
    i = i + 1 
    liste[[i]] = list(
      x0 = 0,
      x1 = 0,
      y0 = 0,
      y1 = 0,
      line = list(
        color = "rgb(192, 192, 192)",
        width = 3
      ),
      type = "line")
  }
  # Addition of the basic information (that we could put directly after)
  layout <- list(
    title = "Correlation circle", 
    width = 600, 
    xaxis = list(title = "Component 1"), 
    yaxis = list(title = "Component 2"), 
    height = 600 
  )
  
  # Add row values for each of our columns
  for (i in 1:ncol(PLSDA$X)){
    j = i + 1
    liste[j][[1]]$x1 <- c1[i]
    liste[j][[1]]$y1 <- c2[i]
  }
  
  
  # Graph display
  graph <- plot_ly()
  for (i in 1:ncol(PLSDA$X)){
    # Plotting of points
    graph <- add_trace(graph, mode="markers", name=colnames(PLSDA$X)[i], x=c1[i], y=c2[i])
    # Adding information and lines
    graph <- layout(graph, title=layout$title, width=layout$width, xaxis=layout$xaxis, yaxis=layout$yaxis, height=layout$height, shapes=liste)
  }
  return(graph)
}

