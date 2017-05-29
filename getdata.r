library(MASS)

getdata <-function()
{
  # to access input data 
  # inFile <- input$chosenfile
  # if (is.null(chosenfile))
  #   return(NULL)
  # read.csv(chosenfile$datapath, header = input$header)

Sigma <- diag(1,3)
Sigma
n1=20;
n2=20;
n3=20;
n4=20;
n5=20;
data1<-mvrnorm(n = n1, c(0,0,0), Sigma)
data2<-mvrnorm(n = n2, c(0,5,0), Sigma)
data3<-mvrnorm(n = n3, c(10,10,0), Sigma)
data4<-mvrnorm(n = n4, c(10,10,0), Sigma)
data5<-mvrnorm(n = n5, c(10,10,0), Sigma)

data.df<-data.frame(x=c(data1[,1],data2[,1],data3[,1], data4[,1], data5[,1]),
                    y=c(data1[,2],data2[,2],data3[,2], data4[,2], data5[,2]),
                    z=c(data1[,3],data2[,3],data3[,3], data4[,3], data5[,3]),
                    label=c(rep(1,n1),rep(2,n2),rep(3,n3),rep(4,n4),rep(5,n5)))
data.df$label<-factor(data.df$label)


return(data.df)

}