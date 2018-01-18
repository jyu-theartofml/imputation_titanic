library(titanic)
library(dplyr)
library(mice)
library(Metrics)
library(lattice)
library(ggplot2)
library(grid)
train.data<-data.frame(titanic_train)
subset.data<-train.data %>% select(-c(PassengerId, Name, Ticket, Cabin)) %>% na.omit()

subset.data$Survived<-as.factor(subset.data$Survived)
subset.data$Pclass<-as.factor(subset.data$Pclass)
sapply(subset.data, class)

select.df<-subset.data

missing_n<-c(0.1,0.2,0.3, 0.4)

error_val<-matrix(NA, nrow=4, ncol=2)

for (i in 1:length(missing_n)){
  
  set.seed(42) 
  percentage=missing_n[i]
  missing.qty<-round(dim(select.df)[1]*percentage)
  select.df<-subset.data
  df1<-sample_n(select.df, missing.qty)
  select.idx1<-row.names(df1)
  select.df[select.idx1,]$Pclass<-NA
  
  set.seed(12) 
  df2<-sample_n(select.df, missing.qty)
  select.idx2<-row.names(df2)
  select.df[select.idx2,]$Age<-NA
  
  #### Random Forest imputation ######
  imputed.data<-complete(mice(select.df, method='rf',m=1,maxit=20,ntree=10)) #was 20 maxit, ntree=10
  
  na.idx<-which(is.na(select.df), arr.ind=TRUE)
  imputed_val<-imputed.data[na.idx,c("Pclass","Age")]
  actual_val<-subset.data[na.idx,c("Pclass","Age")]
  missed_pclass<-which(imputed_val$Pclass!=actual_val$Pclass)
  #print(length(missed_pclass))
  error_val[i,1]<-length(missed_pclass)/missing.qty
  
  age_error<-mae(actual_val$Age,imputed_val$Age)
  error_val[i,2]<-age_error
}

result<-data.frame(row.names=c("10% missing", "20% missing",'30% missing','40% missing'),
                   Pclass_error_rate=error_val[,1], Age_error_MAE=error_val[,2])

plot1<-ggplot(data=result,aes(x=row.names(result), y=Pclass_error_rate, group=1))+geom_path()+geom_point(size=0.5, alpha=0.8)+ylab("Passenger class error rate")+xlab("Percentage of NA values")+theme_minimal()

plot2<-ggplot(data=result,aes(x=row.names(result), y=Age_error_MAE, group=1))+geom_path()+geom_point( size=0.5, alpha=0.8)+ylab("Age error (MAE)")+xlab("Percentage of NA values")+theme_minimal()

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

