

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



plotbypc <- function(Rs=seq(0.1,2.5,0.25), k=0.5, b=1, d = 0.5, q=0.5, c=0.5, noplot=F, panel=rep("",3),e1=0,e2=0){
    Rsmat <- matrix(Rs,length(Rs),length(k))
    
    results_df <- data.frame(
    R = numeric(),
    k = numeric(),
    b = numeric(),
    d = numeric(),
    q = numeric(),
    c = numeric(),
    panel = character(),
    forward_avert_efficiency = numeric(),
    backward_avert_efficiency = numeric(),
    enhanced_avert_efficiency = numeric()
    )

    for (R in Rs) {
        tertiary <- tertiary_cases(R, k, b, q, c, d)
        result_row <- data.frame(
            R = R,
            k = k,
            b = b,
            d = d,
            q = q,
            c = c,
            panel = panel[1],
            forward_avert_efficiency = tertiary$forward_avert/tertiary$base,
            backward_avert_efficiency = tertiary$backward_avert/tertiary$base,
            enhanced_avert_efficiency = tertiary$enhanced_aver/tertiary$base
        )
        results_df <- rbind(results_df, result_row)
    }

    print(panel[1])
    file_name <- paste("results_R_", panel[1], ".xlsx", sep = "")
    print(file_name)
    write_xlsx(results_df, file_name)
    

    return(tertiary)
}


# source("data_2.R") # load functions
# tertiary<-plotbypc(q=0.7,k=0.2,c=0.5,b=0.5,d=0.2,panel=c("A"))
# tertiary<-plotbypc(q=0.7,k=0.5,c=1,b=0.8,d=0.5,panel=c("B"))
# tertiary<-plotbypc(q=0.8,k=0.4,c=0.8,b=0.5,d=0.2,panel=c("C"))
# tertiary<-plotbypc(q=0.8,k=0.5,c=1,b=0.8,d=0.5,panel=c("D"))
# tertiary<-plotbypc(q=1,k=0.2,c=0.5,b=0.5,d=0.2,panel=c("E"))
# tertiary<-plotbypc(q=1,k=0.5,c=0.8,b=0.8,d=0.5,panel=c("F"))
