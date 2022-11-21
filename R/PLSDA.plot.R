install.packages("plotly")
library(plotly)

var_explicatives=function(var1,var2, color){
  plot_ly(x=var1,y=var2, color=color)
}

data(iris)

var_explicatives(var1=iris$Sepal.Length, var2=iris$Petal.Length,color=iris$Species)
