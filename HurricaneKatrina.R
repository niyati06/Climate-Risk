library(readxl)

my_data<-read.csv('Industry_return_before.csv');
library(tseries);
counter<-400;


data<-select(my_data,-c("Before","Beer","Books","Paper","Meals"));
data=drop_na(data)
col_names<-colnames(data);
my_list <- vector("list", length(colnames(data)))
w<-list(rep(0, length(colnames(data))));
i=1
for (col in colnames(data)){
  print(col)
  x<-auto.arima(data[[col]]);
  #print(col)
  #print(arimaorder(x))
  y<-c(arimaorder(x)[1],arimaorder(x)[3])
  #print(y)
  if(all(y == 0)) {order<-c(1,0);
  
  } else {
    order<-c(1,1)
  }
  #print(order)
  garch_spec <- ugarchspec(mean.model = list(armaOrder = y),
                           variance.model = list(model = "gjrGARCH", garchOrder = order),
                           distribution.model = "sstd")
  fit<-ugarchfit(data[[col]],spec=garch_spec); # Fitting the garch model
  res_std<-residuals(fit,standardize=TRUE); #Finding the standardized residuals
  print(dim(res_std))
  gng<-GNG_fit(res_std,start=c(break1 = -2, break2 =2.4, mean = 0 , sd=0.0115, shape1=0.15, shape2=0.15),break_fix = FALSE); # Doing GNG fit
  check=function(x){mistr::p(mistr::distribution(gng),x)} #Finding the CDF
  num<-as.numeric(res_std); #Extracting the residuals
  w_b<-check(num) # Calling the function
  curve(check,num)
  my_list[[i]]=w_b;
  i=i+1;
  print(i)
}


#Converting into a dataframe

my_df <- data.frame(do.call("cbind", my_list))
names(my_df) <- col_names

print(dim(my_df))
#Finding the correlation coefficient

cor(my_df, method = "kendall");


# Convert the data frame to a matrix
my_df <- as.matrix(my_df)
copula_coef=m <- matrix(0, nrow = 26, ncol = 26)
# Loop through each pair of columns
for(i in 1:(ncol(my_df)-1)){
  for(j in (i+1):ncol(my_df)){
    df=as.matrix(cbind(my_df[,i],my_df[,j]))
    # Fit the copula
    copula_fit <- fitCopula(copula=claytonCopula(dim=2),data=df,method="itau")
    # Store the Kendall's tau value in the matrix
    copula_coef[i,j] <- lambda(claytonCopula(coef(copula_fit)))[1]
    copula_coef[j,i] <- copula_coef[i,j]
  }
}
print((copula_coef))

copula_coef=cbind(copula_coef[,1],rep(0,30),copula_coef[,2:3],
                        rep(0,30),copula_coef[,4:21],rep(0,30),copula_coef[,22:24],
                        rep(0,30),
                        copula_coef[,25:ncol(copula_coef)])
copula_coef=rbind(copula_coef[1,],rep(0,30),copula_coef[2:3,],
                        rep(0,30),copula_coef[4:21,],rep(0,30),copula_coef[22:24,],
                        rep(0,30),
                        copula_coef[25:nrow(copula_coef),])

print(copula_coef)