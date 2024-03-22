library(writexl)

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
    
    results_df <- data.frame(
    R = numeric(),
    k = numeric(),
    b = numeric(),
    d = numeric(),
    q = numeric(),
    c = numeric(),
    panel = character(),
    base = numeric(),
    forward_avert = numeric(),
    backward_avert = numeric(),
    enhanced_avert = numeric()
    )


    # Loop through parameter values and store the results
    for (q in qs) {
        for (c in cs) {
            tertiary <- tertiary_cases(R, k, b, q, c, d)
            result_row <- data.frame(
                R = R,
                k = k,
                b = b,
                d = d,
                q = q,
                c = c,
                panel = panel[1],
                base = tertiary$base,
                forward_avert = tertiary$forward_avert,
                backward_avert = tertiary$backward_avert,
                enhanced_avert = tertiary$enhanced_avert
            )
            results_df <- rbind(results_df, result_row)
        }
    }

    print(panel[1])
    file_name <- paste("results_q_", panel[1], ".xlsx", sep = "")
    print(file_name)
    write_xlsx(results_df, file_name)
    
    return(tertiary)
}


# source("data_1.R") # load functions
# tertiary<-plotbypc(R=2.4,k=0.4,b=0.7,d=0.2, panel=c("ABC"))
# tertiary<-plotbypc(R=1.7,k=0.1,b=0.5,d=0.5, panel=c("DEF"))
