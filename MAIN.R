load("~/Desktop/LEARNING/M1/S1/Math Stats 1/Project/men7988_cell.RData")
base1=test
base2=subset(base1,base1$year88==1)
base3=base2[,c(7,18,19)] # our UNIMPUTED dataset
library(mice)

dummy_na = function(elt)
{
  x = dim(length(elt)) 
  x[which(!is.na(elt))] = 1
  x[which(is.na(elt))] = 0
  return(x)
}

#create parameter estimates using complete data:

B_C=lm(waget ~ exper + educ, data=base3)$coefficients

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

#QUESTION 1 #######
#Create a list of data frames for imputation by regression
RegImp_list <- list()
#Loop: to run over all amputed sets.
for(j in 1:M){
#impute data to a new df using simple regression Imputation
  RegImp_list[[j]] <- regressionImp(waget~ educ + exper, data=amputed_list[[j]])
}

#create parameter estimates for this
RegImp_params <- matrix(nrow=M,ncol=3)
for(j in 1:M){
  RegImp_params[]<- lm(waget ~ exper + educ, data=RegImp_list[[j]])$coefficients
}


my_lms <- lapply(1:M, function(x) lm(waget ~ educ + exper, data <- RegImp_list[[j]]))
sapply(my_lms, coef)


#Initialize objects for variance and s.e. calculation #UNFINISHED
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

cor((amputed_list[[1]])[,-c(4)]) # NEED TO PUT INSIDE A LOOP
# PCA is legitimate because variables are corelated

library(FactoMineR)
resuacp=PCA(amputed_list[[1]])
resuacp$eig
tabeigenvalue=resuacp$eig[,1]
plot(tabeigenvalue)
lines(tabeigenvalue)
# We have 3 criterias
# 1. The cumulative percentage of variance must be greater than 70%, with this criteria we're going to take 2 components
# 2. The eigen-value greater than 1, so we will take 2 components
# 3. When the slope of eigenvalue graph change a lot, so with this criteria we will take 2 components.

result=PCA(amputed_list[[1]],ncp=2)
result$var
result$var$contrib
# threshold=100/number of variable in the first dataset = 100/3 = 33,3%
# The variables which contribuate to the component 1 are waget and educ
# Only exper contribuates to the component 2
# We can see also the component 1 is strongly correlated with waget and educ and the component 2 with exper


#for(j in 1:M){
res.MIPCA <- MIPCA(amputed_list[[1]], ncp = 2, nboot  = B, method="Regularized") #note that nb$ncp = 0
res.MIPCA$res.MI
#}


# GRAPH GENERATION SECTION ######

#A graph of one M from linear regression version. This visualizes the difference in imputed and amputed data.

temp_df <- RegImp_list[[1]]
temp_df$before_imp <- amputed_list[[1]]$waget
temp_df$original_waget <- base3$waget
ggplot() + 
  geom_point(data=base3, aes(waget,educ), colour = 'black' ,position = 'jitter') + 
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(original_waget,educ), colour = 'green', alpha=0.4, position = 'jitter') + #original data that got amputed
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(waget,educ), colour = 'red', alpha=0.4,position = 'jitter') +
  scale_x_continuous(limits = c(0, 100))

#graph with imputed value on Y axis and original value on X axis
ggplot() + 
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(original_waget,waget), colour = 'blue', alpha=0.1)+
  scale_x_continuous(limits = c(0, 50))


# it is clear from our graph that the imputed data is following a linear regression/
# it has no difference in variance.