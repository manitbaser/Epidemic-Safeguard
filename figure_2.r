
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



plotbypc <- function(Rs=seq(0.1,2.5,0.25), k=0.5, b=1, d = 0.5, q=0.5, c=0.5, noplot=F, panel=rep("",3),e1=0,e2=0){
    Rsmat <- matrix(Rs,length(Rs),length(k))
    tertiary <- tertiary_cases(Rsmat,k,b,q,c,d)
    if(noplot){return(tertiary)} # skip plotting
    
    custom_colors <- c(
        rgb(21, 63, 89, maxColorValue = 255),
        rgb(86, 82, 137, maxColorValue = 255),
        rgb(175, 88, 141, maxColorValue = 255),
        rgb(238, 109, 103, maxColorValue = 255),
        rgb(244, 169, 60, maxColorValue = 255)
        )

    cols1 = custom_colors[1:1]
    cols2 = custom_colors[4:4]
    cols3 = custom_colors[5:5]

    matplot(tertiary$enhanced_avert/tertiary$base*100, x = Rs*4, type = "o", pch = 19, lty = 1, col = cols3, ylab = "Efficiency (in %)", xlab = "R", ylim = c(0, 100), main = c("", paste(c(panel[1], rep(" ", 0)), collapse = "")),lwd=2)

    lines(Rs*4, tertiary$backward_avert/tertiary$base*100, type = "o", pch = 19, lty = 1, col = cols2,lwd=2)
    errors <- 0.10 * tertiary$backward_avert/tertiary$base
    arrows(x0 = Rs*4, y0 = ((tertiary$backward_avert/tertiary$base) - errors)*100, y1 = ((tertiary$backward_avert/tertiary$base) + errors)*100, angle = 90, code = 3, length = 0.1, col=rev(cols2))

    lines(Rs*4, tertiary$forward_avert/tertiary$base*100, type = "o", pch = 19, lty = 1, col = cols1,lwd=2)
    errors <- (tertiary$forward_avert/tertiary$base) * 0.10
    arrows(x0 = Rs*4, y0 = ((tertiary$forward_avert/tertiary$base) - errors)*100, y1 = ((tertiary$forward_avert/tertiary$base) + errors)*100, angle = 90, code = 3, length = 0.1, col=rev(cols1))

    legend("topleft", legend = c("Traditional approach", "Our approach", "Enhancement"), col = c(cols1, cols2, cols3), lty = 1, lwd = 2, pch = 19,bty="n", cex=0.8)
    return(tertiary)
}




# source("figure_2.R") # load functions
# par(mfrow=c(2,3),mar=c(4,3,2.5,1),mgp=c(2,0.6,0),las=0,yaxs="i")
# tertiary<-plotbypc(q=0.5,k=0.1,c=0.4,b=0.8,d=0.5,panel=c("(A) q = 0.5, c = 0.4"))
# tertiary<-plotbypc(q=0.5,k=0.1,c=0.7,b=0.8,d=0.5,panel=c("(B) q = 0.5, c = 0.7"))
# tertiary<-plotbypc(q=0.5,k=0.1,c=1,  b=0.8,d=0.5,panel=c("(C) q = 0.5, c = 1.0"))
# tertiary<-plotbypc(q=0.8,k=0.1,c=0.4,b=0.8,d=0.5,panel=c("(D) q = 0.8, c = 0.4"))
# tertiary<-plotbypc(q=0.8,k=0.1,c=0.7,b=0.8,d=0.5,panel=c("(E) q = 0.8, c = 0.7"))
# tertiary<-plotbypc(q=0.8,k=0.1,c=1,  b=0.8,d=0.5,panel=c("(F) q = 0.8, c = 1.0"))
