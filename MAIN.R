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

#create parameter estimates from the M regression-imputed datasets
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



#Sherman Version # slide 46 /61------- 

PCA_list <- list()
for(j in 1:M){
res.MIPCA <- MIPCA(amputed_list[[1]], ncp = 2, nboot  = B, method="Regularized") 
PCA_list[[j]] <- res.MIPCA$res.MI
}
#PCA_list : a list of dataframes. PCA_list[[M]][B][,1] gives you the Bth imputation from the M set.



#create a list object with M matrixes with B columns for holding coefficients from imputations.
PCA_Reg_list <- list()
for(j in 1:M){
  PCA_Reg_list[[j]] <- matrix(nrow=B,ncol=3)
}
#PCA_Reg_list[[M]][B,] gives you the set of coefficients from the Bth imputation of the M set.

# get the regressions and assign them to our object
for(j in 1:M){
  for(i in 1:B){
    pca_temp <- lm(waget ~ educ + exper , data = PCA_list[[j]][[i]])
    PCA_Reg_list[[j]][i,] <- pca_temp$coefficients
  }
}
#PCA_Reg_list[[M]][B,] gives you the three coefficients (Intercept, educ, exper) for the Bth imputation of the Mth set.

# CALCULATE BIAS AND VARIANCE FOR PCA -----

#Calculate Bias:
#See proof in notes that this is valid.
summedCols <- matrix(ncol=3, nrow=M)
for(j in 1:M){summedCols[j,] <- colSums(PCA_Reg_list[[j]])}
summedCols <- (1/B)*summedCols
PCA_Bias <- (1/M)*(colSums(summedCols)) - B_C

#Calculate Variance:
#(1) Calculate variance for each M using the coeffs in B imputations:
PCA_variance <- matrix(ncol=3,nrow=M)
for(j in 1:M){
PCA_variance[j,1] <- var(PCA_Reg_list[[M]][,1]) #intercept
PCA_variance[j,2] <- var(PCA_Reg_list[[M]][,2]) #educ
PCA_variance[j,3] <- var(PCA_Reg_list[[M]][,3]) #exper
}
#Take the average of these:
PCA_var_estimator <- (1/M)*colSums(PCA_variance)




# SHIN'S CODE. POSSIBLY OBSOLETE ----------
# Create four empty matrix to store coefficients from imputed data, std, and bias
betaM <-matrix( nrow = M, ncol = 1)
stdM <- matrix( nrow = M, ncol = 1)
bias  <- matrix( nrow = M, ncol = 1)
est_var <- matrix(nrow = M, ncol = 1)

#res.comp$completeObs[1:3, ]
Imputed_matrix <-matrix( nrow = 74661, ncol = M)

for(j in 1:M){
  res.comp <- imputePCA(amputed_list[[j]],  ncp = 2, method = c("Regularized") )
  Temp<-res.comp[[1]]
  # Get imputed waget (stacking in each column)
  Imputed_matrix[,c(j)] <- Temp[,c(1)]
  
  #Now we want to do lm(Imputed_matrix[,c(j)]~educ_C+exper_C 
  wage_imp <-Imputed_matrix[,c(j)]
  fit <- lm(wage_imp ~educ_C + exper_C , data = Original)
  summary_temp=coef(summary(fit))
  betaM[j,] <- summary_temp[2,c(1) ]
  stdM[j,] <- summary_temp[ 2,c(2) ]
  bias[j,] <- betaC- betaM[j,]
}

est_bias_pca <- 1/M * sum( bias   )


# GRAPH GENERATION SECTION ######

#A graph of one M from linear regression version. This visualizes the difference in imputed and amputed data.

temp_df <- RegImp_list[[1]]
temp_df$before_imp <- amputed_list[[1]]$waget
temp_df$original_waget <- base3$waget
ggplot() + 
  geom_point(data=base3, aes(waget,educ), colour = 'black' ,position = 'jitter') + 
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(original_waget,educ), colour = 'green', alpha=0.4, position = 'jitter', shape = 1) + #original data that got amputed
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(waget,educ), colour = 'red', alpha=0.4,position = 'jitter', shape = 1) +
  scale_x_continuous(limits = c(0, 100))

#graph with imputed value on Y axis and original value on X axis
ggplot() + 
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(original_waget,waget), colour = 'blue', alpha=0.1)+
  scale_x_continuous(limits = c(0, 50))


# it is clear from our graph that the imputed data is following a linear regression/
# it has no difference in variance.


# PCA graphs

# library(Amelia)
#> res.amelia <- amelia(don, m = 100)
#> compare.density(res.amelia, var = "T12")
#> overimpute(res.amelia, var = "maxO3")