load("~/Desktop/LEARNING/M1/S1/Math Stats 1/Project/men7988_cell.RData")
base1=test
base2=subset(base1,base1$year88==1)
base3=base2[,c(7,18,19)]
library(mice)

dummy_na = function(elt)
{
  x = dim(length(elt)) 
  x[which(!is.na(elt))] = 1
  x[which(is.na(elt))] = 0
  return(x)
}

#calculate ???
B_C=base3[,c(1)]

#number of amputed data sets to create
M=2

onlywagetpattern= c(0,1,1) #only ampute waget.

#create a matrix to assign our imputed data into.
amputed_list = list()

#Loop: generate M amputed data sets.
for (j in 1:M) {
  ampute1<- ampute(base3,prop=0.40,mech="MCAR",patterns = onlywagetpattern) # I think this has to be an arrow, I'm not sure why.
  #assigned each data frame generated into the list
  amputed_list[[j]] = ampute1$amp
}

#Note: to access a data frame in our list, for example the first data frame, you type amputed_list[[1]]

#Create a list of data frames for imputation by regression
RegImp_list <- list()
#Loop: to run over all amputed sets.
for(j in 1:M){
#impute data to a new df using simple regression Imputation
  RegImp_list[[j]] <- regressionImp(waget~ educ + exper, data=amputed_list[[j]])
}



#Initialize objects for variance and s.e. calculation
std_error= matrix(nrow=74661,ncol = M)
var= matrix(nrow= 74661,ncol = M)
#Loop: Calculate the Standard Error and Variance for ???
for(j in 1:M){

#NOTE: this is unfinished.
std_error[,j]= base4[,c(1)] - B_C
var[,j]= std_error[,j]^2
}
est_bias= 1/M * sum(std_error[1])
est_var=1/M * sum(var[1])

#QUESTION 2 #######
# B numbers of imputation
B = 5

#list of bootstrap imputed dataframes
bootimp_list <- list()

#loop for generating M imputed datasets
for(j in 1:M){
boot_temp <- mice(amputed_list[[j]], m=B, method="norm.boot")
bootimp_list[[j]] <- boot_temp$imp

}

#QUESTION 3 #######

# PCA methods
# PCA has one objective
#1. Summary a dataset (whith a lot of variable) by a new dataset with less variables (uncorelated between them) and keep all the information from the initial data set




#!!!!! IMPORTANT NOTE!!!!!!! 
#!!! The varaibles below here need to be modified when we put it in a loop.!!!!


#This work is based on the slides.
cor(base4[,-c(4)])
# PCA is legitimate because variables are corelated

nb <- estim_ncpPCA(base3)
res.comp <- imputePCA(df2, ncp = nb$ncp) #note that nb$ncp = 0
res.comp$completeObs[1:3, ]



library(FactoMineR)
imp1df2pca <- PCA(df2, ncp = nb$ncp) #this pca method only does mean imputation.


plot.PCA(imp1df2pca)