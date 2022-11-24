library(plotly)

#Récupération des valeurs
c1=res$X_loadings[,1]*sqrt(eigen(cor(scale(res$X)))$values[1])
c2=res$X_loadings[,2]*sqrt(eigen(cor(scale(res$X)))$values[2])

# Création du cercle
liste = list(list(
  x0 = -1, 
  x1 = 1, 
  y0 = -1, 
  y1 = 1, 
  type = "circle"
))
# Création du même nombre de lignes que de variable (qui sont nulles pour l'instant)
for (i in 1:ncol(res$X)){
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
# Ajout des infos de base (que l'on pourrai mettre directement après)
layout <- list(
  title = "Correlation circle", 
  width = 600, 
  xaxis = list(title = "Component 1"), 
  yaxis = list(title = "Component 2"), 
  height = 600 
  )

# Ajout des valeurs des lignes pour chacunes de nos colonnes
for (i in 1:ncol(res$X)){
  j = i + 1
  liste[j][[1]]$x1 <- c1[i]
  liste[j][[1]]$y1 <- c2[i]
}


# Affichage du graphique
graph <- plot_ly()
for (i in 1:ncol(res$X)){
  # Tracage des points
  graph <- add_trace(p, mode="markers", name=colnames(res$X)[i], x=c1[i], y=c2[i])
  # Ajouts des infos et des lignes
  graph <- layout(p, title=layout$title, width=layout$width, xaxis=layout$xaxis, yaxis=layout$yaxis, height=layout$height, shapes=liste)
}
graph
