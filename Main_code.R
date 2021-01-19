##########################################################
## Question 1: VIX Regression Model
##########################################################
install.packages("zoo")
install.packages("xts")
install.packages("utils")
library(utils)
library(zoo)
library(xts)
library(readr)

setwd("~/Desktop/UBS")
data <- read_csv("data.csv")
Vix_data<-data.frame(t(data[,-1]))
colnames(Vix_data)<-c("Eq_Ind1","Eq_Ind2","VIX","LIBOR") 
rownames(Vix_data)<- c(colnames(data)[-1])
write.csv(Vix_data,"Vix_data.csv")

## VIX na Interpolation to obtain Missing values
Non_na_vix<-which(!is.na(Vix_data$VIX))
Vix_data_TS<-Vix_data[min(Non_na_vix):nrow(Vix_data),]
#Vix_data_TS$Vix_Approx<-na.approx(Vix_data_TS$VIX)

### Removing NA observations - results in quarterly data
Vix_data_TS_Qtrly<-na.omit(Vix_data)

### Outlier detection 
## BOXplots and Scatter plots
attach(Vix_data_TS_Qtrly)
quartz()
boxplot(VIX, col = "Blue",main="VIX")
quartz()
boxplot(Eq_Ind1,col = "Blue",main="Eq_Ind1")
quartz()
boxplot(Eq_Ind2,col = "Blue",main="Eq_Ind2")
quartz()
boxplot(LIBOR,col = "Blue",main="LIBOR")

## Scatterplot
quartz()
plot(VIX, col = "Blue",main="VIX")
quartz()
plot(Eq_Ind1,col = "Blue",main="Eq_Ind1")
quartz()
plot(Eq_Ind2,col = "Blue",main="Eq_Ind2")
quartz()
plot(LIBOR,col = "Blue",main="LIBOR")

quartz()
qqnorm(VIX);qqline(VIX,col=2)
v1<-qqnorm(VIX)

### STD Deviation Approach
# calculate summary statistics
data_mean<-mean(VIX)
data_std <-sd(VIX)
# identify outliers
cut_off = data_std * 3
lower= data_mean - cut_off
upper <-data_mean + cut_off

VIX[which(VIX<lower| VIX >upper)]


quartz()
plot(Vix_data_TS_Qtrly)

QoQ_pcent_chg<-function(x){
  qoq<-c((x[2:length(x)]/x[1:length(x)-1])-1)
  return(qoq)
}

QoQ_diff<-function(x){
  qoq<-c(x[2:length(x)]- x[1:length(x)-1])
  return(qoq)
}

Vix_qoq_pcent_chng<-sapply(Vix_data_TS_Qtrly, function(x)QoQ_pcent_chg(x) )
colnames(Vix_qoq_pcent_chng)<-paste0(colnames(Vix_qoq_pcent_chng),"_QoQ_pcentchg")

Vix_qoq_diff<-sapply(Vix_data_TS_Qtrly, function(x)QoQ_diff(x) )
colnames(Vix_qoq_diff)<-paste0(colnames(Vix_qoq_diff),"_QoQ_diff")

VIX_transf_1<-cbind(Vix_data_TS_Qtrly[-1,],Vix_qoq_pcent_chng,Vix_qoq_diff)
VIX_transf<- VIX_transf_1[is.finite(rowSums(VIX_transf_1)), ]

VIX_Full_model<-lm(VIX_QoQ_pcentchg~.,data = VIX_transf[!names(Vix_transf) %in% c("VIX_QoQ_diff"),],na.action=na.exclude)
VIX_transf[(!names(Vix_transf) %in% c("VIX_QoQ_diff")),]
correlation_test<-cor(VIX_transf)

y_var<-as.data.frame(VIX_transf[,c("VIX_QoQ_diff","VIX_QoQ_pcentchg","VIX")])
x_var<-as.data.frame(VIX_transf[!names(VIX_transf) %in% names(y_var)])

###### Regression to find the best model based on R.sq and Adj.Rsq
##### Running all Combinations of model.


### Function To Generate Combinations of Models
## helper function
factor_to_num<-function(x){
  x1<-as.numeric(as.character(x))
  return(x1)
}

combination_of_models<-function(no_of_X,no_of_var){
  
  combinations<-data.frame(combn(no_of_X,no_of_var))
  All_models<-NULL
  for (y in 1:dim(y_var)[2]) {
    for (x in 1:dim(combinations)[2]) {
      reg1<-cbind(y_var[c(y)],x_var[c(combinations)[[x]]])
      #x_data<-x_var[,c(combinations[,x])]
      #reg1<-cbind(y_var[c(y)],x_var[,c(combinations[,x])])
      reg1_sum<-summary(lm(reg1))
      model_var<-paste0(names(reg1)[1],"~",(paste0(names(reg1)[-1],collapse ="+")))
      r_sq<-reg1_sum$r.squared
      Adj_rsq<-reg1_sum$adj.r.squared
      d1<-data.frame(cbind(model_var,r_sq,Adj_rsq))
      All_models<-rbind(All_models,d1)
    }
  }
  
  All_models[,-1]<-sapply(All_models[,-1], function(x) factor_to_num(x))
  Ranked_rsq_model<-All_models[order(-All_models$r_sq),]
  
  return(Ranked_rsq_model)
}

## 2 Factor Models
Model_2Var<-combination_of_models(9,2)

## 1 Factor Models
Model_1Var<-combination_of_models(9,1)

## VIX Diff Model
m1<-summary(lm(as.formula(as.character(Model_1Var$model_var[1])),data=VIX_transf))


####################################################################################################################
## Question 2: Run of Heads
####################################################################################################################

install.packages("plyr")
install.packages("randtests")
library(plyr)
library(base)
library(randtests)
### Function to generate coin sequence of length n
Random_coin_seq<-function(n){
  
  coin_seq<-rbinom(n,1,0.5)
  mapped_values<-mapvalues(coin_seq, c(0, 1), c("T", "H"))
  return(mapped_values)

  }

### Function for longest run of heads
Function_longest_run_heads<-function(given_sequence){

  test1<-rle(given_sequence)
  Run_length_df<-data.frame(Run_length=test1$lengths,Value=test1$values)
  max_run_heads<-max(Run_length_df[Run_length_df$Value=="H",]$Run_length)
  return(max_run_heads)
}

### Simulation for Longest runs of "heads"
runs_vector<-data.frame()
for (i in 1:1000) {
  seq1<-Random_coin_seq(1000)
  longest_run<-Function_longest_run_heads(seq1)
  runs_vector<-rbind(longest_run,runs_vector)
}
prop.table(table(runs_vector))
quartz()
hist(runs_vector[,1])
runs_vector
quantile(c(runs_vector[,1]),probs = c(0.1, 0.5, 1, 2, 5, 10, 25,50,75,100)/100)

##############################################################################
## for a coin toss: sample space(SS) 2^n
## eg. coin tossed 1 time: SS= 2^1 = 2
##    coin tossed 2 time: SS= 2^2 = 4
##    coin tossed 3 time: SS= 2^3 = 8
##    coin tossed 6 time: SS= 2^6 = 32
##    coin tossed 1000 time: SS= 2^1000 
## probability_h<-d*(d^n-1)/(d-1) 
## d - sample outcomes N-number of runs
## A series of 1000 coin tosses can have 1000-(6-1)=995 sequences of length 6
d=2
n=6
d*(d^n-1)/(d-1) 

### Simulation from Wolfram ALpha problem
## Recursive solution.
## A_n_Runs ; Prob = A_n_runs/2^n

maxruns=6
runs=maxruns+1
n=1000
total<-c()
a_n_runs<-c()
########### Simulation for MAXRun of length 6
for (i in 1:(n+1)) {
  if(i<=runs){
    a_n_runs[i]<-c(2^(i-1))
    total[i]<-a_n_runs[i]
  }else if(i>runs){
    a_n_runs[i]<-total[i-1]+total[i-2]+total[i-3]+total[i-4]+total[i-5]+total[i-6]+total[i-7]
        total[i]<-a_n_runs[i]
  }
}
total_prob_6<-total/(2^n)
total_prob_6[n+1]


############# Simulation for MAXRun of length 5
maxruns=5
runs=maxruns+1
n=1000
total<-c()
a_n_runs<-c()
## Recursive loop formula for MAXRun of length 5
for (i in 1:(n+1)) {
  if(i<=runs){
    a_n_runs[i]<-c(2^(i-1))
    total[i]<-a_n_runs[i]
  }else if(i>runs){
    a_n_runs[i]<-total[i-1]+total[i-2]+total[i-3]+total[i-4]+total[i-5]+total[i-6]
    total[i]<-a_n_runs[i]
  }
}

total_prob_5<-total/(2^n)
total_prob_5[n+1]


## Total Probability of Max_Run length =6 is prob(X<=6)- prob(X<=5)
total_prob_6[n+1]-total_prob_5[n+1]


quartz()
hist(runs_vector[,1])
d2<-as.data.frame(table(runs_vector))
seq1<-Random_coin_seq(100)
longest_run<-Function_longest_run_heads(seq1)



