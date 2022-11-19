plot.vip<-function(VIP){
  if (class(VIP) != "VIP") {
    stop("object must be a VIP !")
  }
  titre=paste("Variable Importance in Projection ( *:VIP>",instance$threshold,")")
  plot_VIP=barplot(as.matrix(t(instance$VIP)),main=titre,col="blue",ylim=c(0.0,max(instance$VIP)+0.3),xlab="Variable",ylab="VIP")
  seuil=NULL
  seuil[instance$VIP[,1]>instance$threshold]='*'
  text(plot_VIP,t(instance$VIP)+0.15,seuil,cex=2)
  abline(instance$threshold,0,col = "red")
}
