
tertiary_cases <- function(R=1.2, # reproduction number
                            k=0.3, # dispersion
                            b=1, # probability cluster identified
                            q=0.5, # probability contact traced
                            c=0.5, # proportional reduction from tracing index case
                            d=0.5 # probability detection of cases
                           ){
  tertiary_avert <- function(R, k, b, q, c, d){
      R^2*q*c + (1-(1-b*q)*(1-d))*q*c*R^3*(1+1/k)
  }
  
  baseline_tertiary <- tertiary_avert(R,k,1,1,1,1)
  forward_tertiary_avert <- tertiary_avert(R,k,0,q,c,d)
  backward_tertiary_avert <- tertiary_avert(R,k,b,q,c,d)

  list(base=baseline_tertiary,
    forward_avert=forward_tertiary_avert,
    backward_avert=backward_tertiary_avert,
    enhanced_avert=backward_tertiary_avert-forward_tertiary_avert)
}

plotbypc <- function(R=1.2, k=0.3, b=1, d = 0.5, qs=seq(0.0,1,0.1), cs=seq(0.2,1,0.2), noplot=F, panel=rep("",3)){
    qsmat <- matrix(qs,length(qs),length(cs))
    csmat <- matrix(cs,length(qs),length(cs),byrow=T)
    tertiary <- tertiary_cases(R,k,b,qsmat,csmat,d)
    if(noplot){return(tertiary)} # skip plotting
    
    custom_colors <- c(
        rgb(21, 63, 89, maxColorValue = 255),
        rgb(86, 82, 137, maxColorValue = 255),
        rgb(175, 88, 141, maxColorValue = 255),
        rgb(238, 109, 103, maxColorValue = 255),
        rgb(244, 169, 60, maxColorValue = 255)
        )
    cols1 = custom_colors[1:length(cs)]
    cols2 = custom_colors[1:length(cs)]
    cols3 = custom_colors[1:length(cs)]
    white = rgb(255, 255, 255, maxColorValue = 255)
    

    errors <- (tertiary$forward_avert) * 0.10
    matplot(tertiary$forward_avert,x=qs,type="o",pch=19,lty=1,col=cols1,lwd=2,ylab="# Cases averted",xlab="q",ylim=c(0,50),main=c("",paste(c(panel[1],rep(" ",0)),collapse="")))
    legend("topleft",legend=rev(paste("c = ",cs)),pch=19,col=rev(cols1),bty="n", cex=0.8)
    for (i in 1:length(cs)) {
        arrows(x0 = qs, y0 = (tertiary$forward_avert)[,i] - errors[,i], y1 = (tertiary$forward_avert)[,i] + errors[,i], angle = 90, code = 3, length = 0.1, col=rev(cols1[i]))
    }
    
    errors <- (tertiary$backward_avert) * 0.10
    matplot(tertiary$backward_avert,x=qs,type="o",pch=19,lty=1,col=cols2,lwd=2,ylab="# Cases averted",xlab="q",ylim=c(0,50),main=c("",paste(c(panel[2],rep(" ",0)),collapse="")))
    legend("topleft",legend=rev(paste("c = ",cs)),pch=19,col=rev(cols1),bty="n", cex=0.8)
    for (i in 1:length(cs)) {
        arrows(x0 = qs, y0 = (tertiary$backward_avert)[,i] - errors[,i], y1 = (tertiary$backward_avert)[,i] + errors[,i], angle = 90, code = 3, length = 0.1, col=rev(cols1[i]))
    }
    
    barplot(t(tertiary$enhanced_avert), beside=FALSE, col=white, names.arg=qs, xlab="q",xlim=c(1.35,10.65),ylim = c(0, 50), ylab="# Cases averted", main=c("",paste(c(panel[3],rep(" ",0)),collapse="")),border = "white",space=0)
    box()
    legend("topleft",legend=rev(paste("c = ",cs)),pch=19,col=rev(cols1),bty="n", cex=0.8)
    for (i in length(cs):1) {
        barplot(tertiary$enhanced_avert[,i], add = TRUE, col = cols3[i], beside = TRUE,space = 0)
    }
    
    return(tertiary)
}


# source("figure_1.R") # load functions
# par(mfrow=c(2,3),mar=c(4,3,2.5,1),mgp=c(2,0.6,0),las=0,yaxs="i")
# tertiary<-plotbypc(R=1.7,k=0.1,b=0.6,d=0.3, panel=c("(A) Traditional approach","(B) Our approach","(C) Enhancement"))
# tertiary<-plotbypc(R=1.7,k=0.2,b=0.6,d=0.3, panel=c("(D) Traditional approach","(E) Our approach","(F) Enhancement"))

