## Packages to simplify life
require(quantmod)
require(graphics)
require(timeSeries)
require(xts)
require(car)
require(MLmetrics)
install.packages("quantmod")
install.packages("timeSeries")
install.packages("MLmetrics")
install.packages("quantmod")

# References - https://www.statmethods.net/stats/rdiagnostics.html


## Import data
Vix_data_M <- read.csv("Vix_data.csv", header=T, stringsAsFactors=FALSE)


## Remove all missing
Vix_data1 <- na.omit(Vix_data_M)


## creating % difference between quarters
Vix_data1$X <- NULL
Vix_data1$Eq_Ind1_d <- (Delt(Vix_data1$Eq_Ind1))
Vix_data1$Eq_Ind2_d <- (Delt(Vix_data1$Eq_Ind2))
Vix_data1$Libor_d <- (Delt(Vix_data1$LIBOR))
Vix_data1$VIX_d <- (Delt(Vix_data1$VIX))


## Remove all -inf missing
Vix_data2<- Vix_data1[is.finite(rowSums(Vix_data1)), ]

# built a model
summary(lm(VIX_d~.,data = Vix_data2,na.action=na.exclude))


##Final model with significant variable

Model <- lm(VIX_d~Eq_Ind1_d,data = Vix_data2,na.action=na.exclude)
summary(Model)
qqPlot(Model)


## Insample MAPE and MSE
MAPE(Model$fitted.values,Vix_data2$VIX_d)
MSE(Model$fitted.values,Vix_data2$VIX_d)
RMSE(Model$fitted.values,Vix_data2$VIX_d)


## What happens when Eq1 drops to 1300; because this is  a diff model, you'll have to convert the data back to level
## 1300 Eq1 drop ~ -51% drop in eq1_d, therefore it will be -2.52214*-.513766 = 1.29579 increase in vix_d => 11.04 (last value of vix) will jump to 25.32552


##Final model with significant variable + eq2 insignificant variable

Model1 <- lm(VIX_d~Eq_Ind1_d+Eq_Ind2_d,data = Vix_data2,na.action=na.exclude)
summary(Model1)
qqPlot(Model1)

## Do the same valculation for eq2_d here


c1<-data.frame(combn(3,2))

combination_of_models<-function(no_of_X,no_of_var){
  
  combinations<-data.frame(combn(no_of_X,no_of_var))
  All_models<-NULL
  for (y in 1:dim(y_var)[2]) {
    for (x in 1:dim(combinations)[2]) {
      reg1<-cbind(y_var[c(y)],x_var[c(combinations)[[x]]])
      #x_data<-x_var[,c(combinations[,x])]
      print(names(reg1))
      #reg1<-cbind(y_var[c(y)],x_var[,c(combinations[,x])])
      reg1_sum<-summary(lm(reg1))
      model_var<-paste0(names(reg1)[1],"~",(paste0(names(reg1)[-1],collapse ="+")))
      r_sq<-reg1_sum$r.squared
      Adj_rsq<-reg1_sum$adj.r.squared
      d1<-data.frame(cbind(model_var,r_sq,Adj_rsq))
      All_models<-rbind(All_models,d1)
    }
  }
  return(All_models)
}

UNivariate_model<-combination_of_models(8,2)
UNivariate_model
