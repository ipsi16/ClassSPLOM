library(gridExtra)
library(ggplot2)
library(MASS)
library('RColorBrewer')
library('reshape2')

#Get data 
data.df <- getdata()

# Returns LIST of LDA plots
getplotlist <- function(counter, data.df, brushedlines, clickedline)
{
  print(data.df)
  
  uniq_labels <- unique(data.df[,ncol(data.df)])
  comb_labels <- combn(uniq_labels,2)
  
  # Set palette:
  # NAME; No.OF COLORS
  # Accent 8
  # Dark2 8
  # RColorBrewer 3
  # Paired 12
  # Pastel1 9
  # Pastel2 8
  # Set1 9
  # Set2 8
  # Set3 12
  
  coloursvec <- brewer.pal(8, "Dark2")
  palette(coloursvec)
  
  # Replace doPlot with function that returns plots given 2 labels
  p <- lapply(1:ncol(comb_labels), function(i) doPlot(counter, data.df, comb_labels[1, i], comb_labels[2, i],  brushedlines, clickedline, coloursvec))
  
  
  return(p)
}


# Returns diagonal list of plots
# Replace doPlot with LDA generating function
getdiaglist <- function(counter, data.df)
{
  
  uniq_labels <- unique(data.df[,ncol(data.df)])
  comb_labels <- combn(uniq_labels,2)
  coloursvec <- brewer.pal(8, "Dark2")
  palette(coloursvec)
  
  p <- lapply(1:length(uniq_labels), function(i) doPlot(counter, data.df, uniq_labels[i], uniq_labels[i],  NULL, NULL, coloursvec))
  
  return(p)
  
}

 # getauclist Returns trustworthiness plot list
 getauclist <-  function(data.df)
 {
   
   #print(data.df)
   datalabels<-data.df$label
   
   databrutdf <- data.df
   datadim <- ncol(data.df) -1
   databrutdf2D<-data.frame(x=databrutdf[,1],y=databrutdf[,2],label=factor(datalabels))
   ggplot()+geom_point(data=databrutdf2D,aes(x=x,y=y,color=label))
   
   
   nc<-  length(unique(data.df[,ncol(data.df)])) # number of classes
   ndata<-nrow(data.df) # number of data
   data<-as.matrix(data.df[,1:datadim]) # raw data matrix without labels
  
   #print(data)
   #print(datalabels)
   
   
   uniq_labels <- unique(datalabels)
   comb_labels <- combn(uniq_labels,2)
   #print(comb_labels)
   colnames(data.df)<-c(1:datadim,"labels")
   dataStdzdf<-data.df #standardizeData(data.df)
   dataStdz<-as.matrix(dataStdzdf[,1:datadim])
   
   #counter <- 1
   #coloursvec <- brewer.pal(12, "Paired")
   #p <- lapply(1:ncol(comb_labels), function(i) doPlot(counter, data.df,comb_labels[1, i], comb_labels[2, i],  NULL, NULL, coloursvec))
   p <- lapply(1:ncol(comb_labels), function(i) LDA_ROC( dataStdz[which(datalabels== comb_labels[1, i]),] , dataStdz[which(datalabels== comb_labels[2, i]),] ,nboot=2))
#   
   p
   return(p)
 }