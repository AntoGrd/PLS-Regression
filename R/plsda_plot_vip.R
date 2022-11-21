plot.vip<-function(objet){
  #if (class(VIP) != "VIP") {
  #  stop("object must be a VIP !")
  #}
  titre=paste("Variable Importance in Projection ( *:VIP>",objet$threshold,")")
  plot_VIP=barplot(as.matrix(t(objet$VIP)),main=titre,col="blue",ylim=c(0.0,max(objet$VIP)+0.3),xlab="Variable",ylab="VIP")
  seuil=NULL
  seuil[objet$VIP[,1]>objet$threshold]='*'
  text(plot_VIP,t(objet$VIP)+0.15,seuil,cex=2)
  abline(objet$threshold,0,col = "red")
}

plot.vip(resvip)





