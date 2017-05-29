### Extract a submatrix with row and column names and preserving format 
### (especially when extracting a single column in a matrix it becomes a row vector ! in base R)
submat<-function(mat,indr,indc)
{
  # indc and indr can be: a set of positive integers, single 0 or a set of negative integer 
  if (sum(indr)==0) indr<-1:nrow(mat)
  if (sum(indc)==0) indc<-1:ncol(mat)
  nr<-length(indr)
  nc<-length(indc)
  if (sum(indr)<0) nr<-nrow(mat)-length(indr)
  if (sum(indc)<0) nc<-ncol(mat)-length(indc)
  matres<-matrix(mat[indr,indc],nr,nc)
  rownames(matres)<-rownames(mat)[indr]
  colnames(matres)<-colnames(mat)[indc]
  return(matres)
}



### Standardize data before projection
standardizeData<-function(datadf)
{
  library(caret)
  
  ### datadf: NxD data frame (data only with no labels)
  
  ### run dataStdz<-standardizeData(data)
  
  # remove variables with 0 variance (constant)
  nzv <- nearZeroVar(datadf)
  if (length(nzv)>0)
  { 
    filteredDatadf <- datadf[, -nzv]
  } else {
    filteredDatadf <- datadf
  }
  
  # remove highly correlated variables
  dataCor <-  cor(filteredDatadf)
  highlyCorData <- findCorrelation(dataCor, cutoff = .95)
  if (length(highlyCorData)>0) filteredDatadf <- submat(filteredDatadf,0,-highlyCorData)
  
  # rescale and center data
  trans = preProcess(filteredDatadf, method=c("BoxCox", "center", "scale"))
  datadfStdz = predict(trans, filteredDatadf)
  
  # return a data frame
  return(datadfStdz)
}

### LDA ROC using bootstrapping
LDA_ROC<-function(dataStdzLab1,dataStdzLab2,nboot=2)
{
  # bootstrapping
  N1<-nrow(dataStdzLab1)
  N2<-nrow(dataStdzLab2)
  labels12<-rbind(matrix(FALSE,nrow(dataStdzLab1),1),matrix(TRUE,nrow(dataStdzLab2),1))
  dataLDAxBoot<-matrix(0,N1+N2,nboot)
  p<-ggplot() + theme_light()
  # bootstrap samples
  for (t in 1:nboot)
  {
    indboot1<-sample(c(1:N1),N1,replace = T)
    indboot2<-sample(c(1:N2),N2,replace = T)
    dataLDAxBoot[,t]<-ldaX(dataStdzLab1[indboot1,],dataStdzLab2[indboot2,],factor(labels12))
    
    roc<-simple_roc(labels12,dataLDAxBoot[,t])
    p<-p+geom_line(data=roc,aes(x=FPR,y=TPR),col="red",size=1,alpha=0.05)
  }
  # original sample
  dataLDAx<-ldaX(dataStdzLab1,dataStdzLab2,labels12)
  rocinit<-simple_roc(labels12,dataLDAx)
  p<-p+geom_line(data=rocinit,aes(x=FPR,y=TPR),col="blue",size=2,alpha=1)+
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position='none', axis.ticks = element_blank(), axis.text = element_blank())
  
  p <- ggplotly(p, width = 150, height =150) %>% config(displayModeBar = FALSE)
  
  return(p)
}

### ROC plotting
simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

### LDA first axis only
ldaX<-function(dataStdzLab1,dataStdzLab2,labels)
{
  ### preprocess data first using dataStdzdf<-standardizeData(datadf)
  ### dataStdz<-as.matrix(dataStdzdf[,1:datadim])
  ### dataStdzLab1<-dataStdz[which(labels==lab1),]
  ### dataStdzLab2<-dataStdz[which(labels==lab2),]
  ### then run dataLDA<-ldaX(dataStdzLab1,dataStdzLab2)
  
  ### Compute only the firstLDA axis
  
  ### dataStdzLab1: N1xD data matrix N1 data of label lab1 in D dimensions
  ### dataStdzLab2: N2xD data matrix N2 data of label lab2 in D dimensions
  
  ### Return: 
  ### ldaX$x: coordinate of data on 1st LDA axis, sorted by increasing order
  ### ldaX$label: label = 1 or 2 of data in same order as in ldaX$x
  
  
  ### init
  datadim<-ncol(dataStdzLab1) # data dimension
  
  ### STEP 1: compute 1st LDA between classes lab1 and lab2
  ###         and project all data onto this first axis to get the x-axis coordinated for the scatterplot
  
  # get data from classes lab1 and lab2 only
  # consider class lab1 versus class lab2
  datadf.1vs2<-data.frame(rbind(dataStdzLab1,dataStdzLab2))
  datadf.1vs2$label<-factor(labels) # factor to apply LDA function

  
  # compute LDA regarding lab1 and lab2
  r1vs2 <- lda(formula = label ~ ., 
               data = datadf.1vs2, 
               prior = rep(1/2,2))
  
  # project all data onto the 1st lda of lab1 vs lab2
  vlda1 <- r1vs2$scaling[,1] # first lda vector
  v12<-sqrt(vlda1%*%vlda1) # scalar
  cp11<-dataStdzLab1%*%vlda1 # NxD * 1xD -> Nx1
  cp12<-dataStdzLab2%*%vlda1 # NxD * 1xD -> Nx1
  
  ldaX<-rbind(cp11,cp12)/v12[1,1]  # x-axis value : data projected on first LDA

  return(ldaX) # (N1+N2) x 1 matrix with lab1 data then lab2 data
}


ldaClassSPLOM<-function(dataStdz,labels,lab1,lab2)
{
  ### preprocess data first using dataStdzdf<-standardizeData(datadf)
  ### dataStdz<-as.matrix(dataStdzdf[,1:datadim])
  ### then run dataLDA<-ldaClassSPLOM(dataStdz,labels,lab1,lab2)
  
  ### dataStdz: NxD data matrix N data in D dimensions
  ### labels: 1xN labels of the data (integer)
  ### lab1, lab2: 1x1 integer, each one must be already in labels
  
  ### for lab1==lab2 consider class lab1 versus all other classes
  ### for 2 classes, provides on x-axis: the 1st LDA of lab1 vs lab2, 
  ###        and on y-axis: the 1st LDA in orthogonal subspace of x-axis, of lab1 versus lab2
  ### for more than 2 classes, provides on x-axis: the 1st LDA of lab1 vs lab2
  ###        and on y-axis: 1st LDA in orthogonal subspace of x-axis, of lab1 and lab2 versus others 
  
  
  ########### PLOTTING
  ### ggplot()+geom_point(data=dataLDA,aes(x=x,y=y,colour=label)) + coord_fixed() + ggtitle("LDA1 1 vs 2 --- LDA2 (1 and 2) vs others")
  

  
  ### Prepare output
  dataLDA<-NULL
  dataLDA$label<-labels
  
  ### init
  datadim<-ncol(dataStdz) # data dimension
  
  ### STEP 1: compute 1st LDA between classes lab1 and lab2
  ###         ande project all data onto this first axis to get the x-axis coordinated for the scatterplot
  
  # get data from classes lab1 and lab2 only
  if (lab1==lab2) # consider class lab1 versus all other classes
  {
    ind12=which(labels==lab1)
    datadf.1vs2<-data.frame(dataStdz)
    datadf.1vs2$label[ind12]<-1
    datadf.1vs2$label[-ind12]<-2
    datadf.1vs2$label<-factor(datadf.1vs2$label) # factor to apply LDA function
    
  }else{ # consider class lab1 versus class lab2
    ind12<-which((labels==lab1 | labels==lab2))
    datadf.1vs2<-data.frame(dataStdz[ind12,])
    datadf.1vs2$label<-labels[ind12]
    datadf.1vs2$label<-factor(datadf.1vs2$label) # factor to apply LDA function
  }
  
  # compute LDA regarding lab1 and lab2
  r1vs2 <- lda(formula = label ~ ., 
               data = datadf.1vs2, 
               prior = rep(1/2,2))
  
  # project all data onto the 1st lda of lab1 vs lab2
  vlda1 <- r1vs2$scaling[,1] # first lda vector
  v12<-sqrt(vlda1%*%vlda1) # scalar
  cp1<-dataStdz%*%vlda1 # NxD * 1xD -> Nx1
  
  dataLDA$x<-cp1/v12[1,1]  # x-axis value : data projected on first LDA
  
  ### STEP 2: project all data onto the hyperplane orthogonal to 1st lda of lab1 vs lab2
  # x_ortho<-x-((x.v)*v/(v^2))
  dataproj<-dataStdz-((dataLDA$x%*%vlda1)/v12[1,1]) # NxD - (Nx1 * 1xD) / 1x1 -> NxD 
  
  ### STEP 3: Compute PCA of projected data onto orthogonal hyperplane to LDA1 to get a basis
  pca<-prcomp(dataproj)
  
  ### STEP 4: project data onto the first D-1 leading PC 
  dataortho<-dataStdz%*%pca$rotation[,1:(datadim-1)] 
  
  ### STEP 5: Compute LDA 1st LDA of union of classes lab1 and lab2 versus union of all other classes
  ###         within the basis formed by the D-1 leading eigenvectors
  
  ### STEP 6: Project data onto this second axis and use it as y-axis coordinate for the scatterplot
  
  # gather data from classes lab1 and lab2 into lab12 vs all others
  datadf.12vsothers<-data.frame(dataortho)
  datadf.12vsothers$label<-labels
  
  if (length(unique(labels))==2)
  {
    ind12=which(labels==lab1)
  }

  datadf.12vsothers$label[ind12]<-1
  datadf.12vsothers$label[-ind12]<-2
  datadf.12vsothers$label<-factor(datadf.12vsothers$label) # factor to apply LDA function
  
  # compute LDA regarding lab12 vs others
  r12vsothers <- lda(formula = label ~ ., 
                     data = datadf.12vsothers, 
                     prior = c(1,1)/2)
  vlda2 <- r12vsothers$scaling[,1]
  
  v22<-vlda2%*%vlda2 # scalar
  cp2<-dataortho%*%vlda2 # NxD * 1xD -> Nx1
  
  # projection to y-axis
  dataLDA$y<-cp2/sqrt(v22[1,1])
  
  dataLDA<-data.frame(dataLDA)
  dataLDA$label<-factor(dataLDA$label)

  return(dataLDA)
}

pcaClassSPLOM<-function(dataStdz)
{
  ### consider only the data without the labels
  ### preprocess data first using dataStdzdf<-standardizeData(data)
  ### dataStdz<-as.matrix(dataStdzdf[,1:datadim])
  ### get projected data with: dataPCA<-pcaClassSPLOM(dataStdz)
  ### plot these data using:
  # dataPCAdf<-data.frame(dataPCA)
  # dataPCAdf$label<-label # add the label to the data frame
  # dataPCAdf$label<-factor(dataPCAdf$label)
  # ggplot()+ggtitle("PCA")+coord_fixed()+geom_point(data=dataPCAdf,aes(x=PC1,y=PC2,colour=label))

  # PCA: compute principal components of the data
  pca<-prcomp(dataStdz)
  
  ### STEP 4: project data onto the first 2 leading PC (they are already normalized (sum(pca$rotation[,i]^2)=1))
  dataPCA<-dataStdz%*%pca$rotation[,1:2] 
  
  return(dataPCA)
}