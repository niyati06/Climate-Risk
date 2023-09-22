library(readxl)
setwd('/Users/niyatisrivastava/Desktop/NCSU/Spring-23/FIM601')
my_data<-read.csv('CaliforniaWildfire_post.csv');
library(tseries);
counter<-400;

data<-select(my_data,-c("X","Smoke","FabPr","ElcEq","Paper","Trans","Rtail"))#,"Util","Oil","Books"))
data=drop_na(data)
col_names<-colnames(data);
my_list <- vector("list", length(colnames(data)))
w<-list(rep(0, length(colnames(data))));
i=1
for (col in colnames(data)){
  x<-auto.arima(data[[col]]);
  print(col)
  print(arimaorder(x))
  y<-c(arimaorder(x)[1],arimaorder(x)[3])
  print(y)
  if(all(y == 0)) {order<-c(1,0);
  
  } else {
    order<-c(1,1)
  }
  print(order)
  garch_spec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = order),
                           mean.model = list(armaOrder = y),
                           distribution.model = "sstd")
  fit<-ugarchfit(data[[col]],spec=garch_spec); # Fitting the garch model
  res_std<-residuals(fit,standardize=TRUE); #Finding the standardized residuals
  gng<-GNG_fit(res_std,start=c(break1 = -2, break2 =2, mean = 0 , sd = 1 ,shape1 = 1, shape2 = 0.5)); # Doing GNG fit
  check=function(x){mistr::p(mistr::distribution(gng),x)} #Finding the CDF
  num<-as.numeric(res_std); #Extracting the residuals
  w_b<-check(num) # Calling the function
  curve(check,num)
  my_list[[i]]=w_b;
  i=i+1;
}


#Converting into a dataframe

my_df <- data.frame(do.call("cbind", my_list))
names(my_df) <- col_names

#Finding the correlation coefficient

cor(my_df, method = "kendall");


# Convert the data frame to a matrix
my_df <- as.matrix(my_df)
copula_coef_after=m <- matrix(0, nrow = 24, ncol = 24)
# Loop through each pair of columns
for(i in 1:(ncol(my_df)-1)){
  for(j in (i+1):ncol(my_df)){
    df=as.matrix(cbind(my_df[,i],my_df[,j]))
    # Fit the copula
    copula_fit <- fitCopula(copula=claytonCopula(dim=2),data=df,method="itau")
    # Store the Kendall's tau value in the matrix
    copula_coef_after[i,j] <- lambda(claytonCopula(coef(copula_fit)))[1]
    copula_coef_after[j,i] <- copula_coef_after[i,j]
  }
}

#Correcting dimensions
copula_coef_after=cbind(copula_coef_after[,1:2],rep(0,30),copula_coef_after[,3:12],
                        rep(0,30),rep(0,30),copula_coef_after[,13:20],rep(0,30),
                        rep(0,30),copula_coef_after[,21],rep(0,30),
                        copula_coef_after[,22:ncol(copula_coef_after)])
copula_coef_after=rbind(copula_coef_after[1:2,],rep(0,30),copula_coef_after[3:12,],
                        rep(0,30),rep(0,30),copula_coef_after[13:20,],
                        rep(0,30),rep(0,30),copula_coef_after[21,],
                        rep(0,30),copula_coef_after[22:nrow(copula_coef_after),])
#Checking hypothesis
result_mat <- (copula_coef_after > copula_coef) * 1
result_mat
colnames(result_mat)=colnames(select(my_data,-c("X")))
rownames(result_mat)=colnames(select(my_data,-c("X")))


#Graphing the nodes
install.packages("igraph")
library(igraph)


# Create a graph object from the matrix
my_graph <- graph.adjacency(result_mat, mode = "undirected", diag = FALSE)
layout=layout.circle(my_graph)

# Set the vertex names
V(my_graph)$names <- colnames(select(my_data,-c("X")))
# Plot the graph
plot(my_graph, vertex.label.size=10,vertex.label.cex = 1,vertex.size = 5,vertex.label.color = "black", vertex.label.dist=-1,edge.arrow.size = 5, layout = layout,vertex.label.degree = -3*pi/4)


#Degree and Centrality Calculation
degree_centrality=degree(my_graph)

df_degree <- data.frame(names = V(my_graph)$name, degree = degree_centrality)

# Plot the degree centrality using ggplot2
ggplot(df_degree, aes(x = names, y = degree)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(x = "Industries", y = "Degree Centrality")+ggtitle("California Wildfire Centrality across Industry")+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5),plot.title = element_text(hjust = 0.5))
