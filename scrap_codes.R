combinations<-data.frame(combn(8,2))
All_models<-NULL
for (y in 1:dim(y_var)[2]) {
  for (x in 1:dim(combinations)[2]) {
    
    reg1<-cbind(y_var[c(y)],x_var[,c(combinations[,x])])
    reg1_sum<-summary(lm(reg1))
    model_var<-paste0(names(reg1)[1],"~",(paste0(names(reg1)[-1],collapse ="+")))
    r_sq<-reg1_sum$r.squared
    Adj_rsq<-reg1_sum$adj.r.squared
    d1<-data.frame(cbind(model_var,r_sq,Adj_rsq))
    All_models<-rbind(All_models,d1)
    
  }
}



Model1<-summary(lm(VIX_QoQ_diff~LIBOR+Eq_Ind1_QoQ_pcentchg,data = VIX_transf))
Model2<-summary(lm(VIX_QoQ_diff~Eq_Ind1_QoQ_pcentchg+LIBOR_QoQ_diff,data = VIX_transf))

## VIX Level model
Model3<-summary(lm(VIX~Eq_Ind1_QoQ_diff+LIBOR_QoQ_diff,data = VIX_transf))

## VIX %Chng Model
Model4<-summary(lm(VIX_QoQ_pcentchg~Eq_Ind1_QoQ_diff+LIBOR_QoQ_diff,data = VIX_transf))
Model5<-summary(lm(VIX_QoQ_pcentchg~LIBOR+Eq_Ind1_QoQ_diff,data = VIX_transf))
Model6<-summary(lm(VIX_QoQ_pcentchg~Eq_Ind1_QoQ_pcentchg+Eq_Ind2_QoQ_diff,data = VIX_transf))




runs <- function(n){
  summary <- list(list())
  seq <- sample(c('H','T'),n,replace = TRUE)
  seq1 <- seq[-1]#Sequence without the first character
  seq2 <- seq[-length(seq)] #Sequence without the last character of the sequence
  # Comparing the two sequences is equivalent to comparing each character with its previous character
  TF <- seq1!=seq2
  # Add the first and the last character position since they were not included in seq1 and seq2
  runs <- c(0,which(TF),length(seq))
  # difference of the positions that are not runs
  # gives the length of runs
  runs_table <- cbind(seq[runs[-1]],as.numeric(as.character(diff(runs))))
  tab <- runs_table[which(runs_table[,1]=="H"),]
  max_len <- max(tab[,2])
  summary[[1]] <- seq; 
  summary[[2]] <- runs_table;
  summary[[3]] <- as.numeric(max_len)
  summary[[4]] <- print(paste0("maximum number of heads: ", max_len))
  return(summary)
}
runs_simulation<-runs(1000)


quartz()
hist(x=as.numeric(runs_simulation[[2]][,2]),main="Histogram of runs of heads and tails in the sequence",xlab="Number of runs",ylab="Frequency of the runs")





