#INITIALIZATION #########
load("~/Desktop/LEARNING/M1/S1/Math Stats 1/Project/men7988_cell.RData")
base1=test
base2=subset(base1,base1$year88==1)
base3=base2[,c(7,18,19)] # our UNIMPUTED dataset
base3$waget <- log(base3$waget) # transform wage to log(wage)
library(mice)

#create parameter estimates using complete data:

B_C=lm(waget ~ exper + educ, data=base3)$coefficients

#number of amputed data sets to create
#M=20
M=1
 
onlywagetpattern= c(0,1,1) #only ampute waget.
amputed_list = list() #create a matrix to assign our imputed data into.

#Loop: generate M amputed data sets.
for (j in 1:M) {
  ampute1<- ampute(base3,prop=0.40,mech="MCAR",patterns = onlywagetpattern)
  #assigned each data frame generated into the list
  amputed_list[[j]] = ampute1$amp
}

#save the generated data
save(amputed_list,    file = "FixedData.RData")

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

#Q1: Calculate Bias for RegImp #######
RegImp_Bias_estimate <- (colSums(RegImp_params)*(1/M) - B_C)

#Q2: Calculate Variance for RegImp ######
RegImpmean <- colSums(RegImp_params)*(1/M)
RegImp_var_estimate <- colSums((RegImp_params - RegImpmean)^2)*(1/M)

#QUESTION 2 #######
B = 5# B numbers of imputation
bootimp_list <- list() #list of bootstrap imputed dataframes for loop

#loop for generating M imputed datasets
for(j in 1:M){
  templist <- list()
  for(i in 1:B){
boot_temp <- mice(amputed_list[[j]], m=B, method="norm.boot")
 templist[[i]] <-  complete(boot_temp,i) #you need to use complete(boot_temp, number of imputed set) to generate proper impute data
  }
  bootimp_list[[j]] <- templist
}
# bootimp_list[[M]][[B]] gives you the Bth imputed dataframe the Mth amputed data.

#Run regression on BootImp =====
#construct a list object with M matrixes with B columns for holding coefficients from imputations.
bootimp_Reg_list <- list()
for(j in 1:M){
  bootimp_Reg_list[[j]] <- matrix(nrow=B,ncol=3)
}
#bootimp_Reg_list[[M]][B,] gives you the set of coefficients from the Bth imputation of the M set.

# get the regressions and assign them to our object
for(j in 1:M){
  for(i in 1:B){
    bootimp_temp <- lm(waget ~ educ + exper , data = bootimp_list[[j]][[i]])
    bootimp_Reg_list[[j]][i,] <- bootimp_temp$coefficients
  }
}


#add a part in the loop which saves all of our bootimps.
#
lmdebugtest <- with(boot_temp, lm(waget ~ educ + exper))
pool.mice <- pool(lmdebugtest)
summary(pool.mice)


lmdebugtest <- with(bootimp_temp, lm(waget ~ educ + exper))
pool.mice <- pool(lmdebugtest)
summary(pool.mice)

# CALCULATE BIAS AND VARIANCE FOR BOOTIMP -----

#Calculate Bias:
#See proof in notes that this is valid.
boot_summedCols <- matrix(ncol=3, nrow=M)
for(j in 1:M){boot_summedCols[j,] <- colSums(bootimp_Reg_list[[j]])}
boot_summedCols <- (1/B)*boot_summedCols
boot_Bias <- (1/M)*(colSums(boot_summedCols)) - B_C

#Calculate Variance:
boot_mean <- boot_Bias + B_C
#(1) Calculate variance for each M using the coeffs in B imputations:

boot_variance <- matrix(ncol=3,nrow=M)
for(j in 1:M){
  boot_variance[j,1] <- var(bootimp_Reg_list[[M]][,1]) #intercept
  boot_variance[j,2] <- var(bootimp_Reg_list[[M]][,2]) #educ
  boot_variance[j,3] <- var(bootimp_Reg_list[[M]][,3]) #exper
}
#Take the average of these:
boot_var_estimator <- (1/M)*((colSums((boot_variance - boot_mean)^2)))


#QUESTION 3 #######

# We use NCP = 2 because we have two regressors educ and exper.

PCA_list <- list()
for(j in 1:M){
res.MIPCA <- MIPCA(amputed_list[[1]], ncp = 2, nboot  = B, method="Regularized") 
PCA_list[[j]] <- res.MIPCA$res.MI
}
#PCA_list : a list of dataframes. PCA_list[[M]][B][,1] gives you the Bth imputation from the M set.

#Run regression for PCA --------

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

# CALCULATE BIAS AND VARIANCE FOR PCA (Slide39/61Josse) -----

#Calculate Bias:
#See proof in notes that this is valid.
summedCols <- matrix(ncol=3, nrow=M)
for(j in 1:M){summedCols[j,] <- colSums(PCA_Reg_list[[j]])}
summedCols <- (1/B)*summedCols
PCA_Bias <- (1/M)*(colSums(summedCols)) - B_C

#Calculate Variance:
PCA_mean <- PCA_Bias + B_C
#(1) Calculate variance for each M using the coeffs in B imputations:
PCA_variance <- matrix(ncol=3,nrow=M)
for(j in 1:M){
PCA_variance[j,1] <- var(PCA_Reg_list[[M]][,1]) #intercept
PCA_variance[j,2] <- var(PCA_Reg_list[[M]][,2]) #educ
PCA_variance[j,3] <- var(PCA_Reg_list[[M]][,3]) #exper
}
#Take the average of these:
PCA_var_estimator <- (1/M)*colSums((PCA_variance - PCA_mean)^2)


#load("alldata.RData")
#save.image(file="alldata.RData") 

# GRAPH GENERATION SECTION ######
library(ggplot2)

#A graph of one M from linear regression version. This visualizes the difference in imputed and amputed data.

#placeholder variables for graphing convenience
temp_df <- RegImp_list[[1]]
temp_df$before_imp <- amputed_list[[1]]$waget
temp_df$original_waget <- base3$waget
ggplot() + 
  geom_point(data=base3, aes(waget,educ), colour = 'gray' ,position = 'jitter') + 
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(original_waget,educ), colour = 'blue', alpha=0.4, position = 'jitter', shape = 1) + #original data that got amputed
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(waget,educ), colour = 'red', alpha=0.4,position = 'jitter', shape = 1) +
  scale_x_continuous(limits = c(0, 5))

#A graph of one M from Bootstrap regression version. This visualizes the difference in imputed and amputed data.

#placeholder variables for graphing convenience
temp_df <- bootimp_list[[1]][[1]]
temp_df$before_imp <- amputed_list[[1]]$waget
temp_df$original_waget <- base3$waget
ggplot() + 
  geom_point(data=base3, aes(waget,educ), colour = 'gray' ,position = 'jitter') + 
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(original_waget,educ), colour = 'blue', alpha=0.4, position = 'jitter', shape = 1) + #original data that got amputed
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(waget,educ), colour = 'red', alpha=0.4,position = 'jitter', shape = 1) +
  scale_x_continuous(limits = c(0, 5))




#graph with imputed value on Y axis and original value on X axis
ggplot() + 
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(original_waget,waget), colour = 'blue', alpha=0.1)+
  scale_x_continuous(limits = c(0,2))


ggplotRegression(lm(waget ~ educ + exper, data = base3))

# it is clear from our graph that the imputed data is following a linear regression/
# it has no difference in variance.

#BootImp graph



boot_temp_mat <- matrix(nrow=10,ncol=3)
for(i in 1:5){
boot_temp_mat[i,1] <- as.numeric(rownames(boot_temp$imp$waget)[1])
boot_temp_mat[i+5,1] <- as.numeric(rownames(boot_temp$imp$waget)[331])
boot_temp_mat[i,2] <- t(boot_temp$imp$waget[1,i])
boot_temp_mat[i+5,3] <- t(boot_temp$imp$waget[331,i])
}

boot_temp_df <- as.data.frame(boot_temp_mat)
for(i in 1:5){
boot_temp_df$educ[i] <- base3$educ[boot_temp_mat[1,1]]
boot_temp_df$educ[i+5] <- base3$educ[boot_temp_mat[6,1]]
}

ggplot(base3, aes(x = waget, y = educ)) + 
  geom_point( alpha=0.1, colour="gray") +
  stat_smooth(method = "lm", color="gray") +
  geom_point( data=boot_temp_df, aes(V2[1],educ[1]), color="cyan") + 
  geom_point( data=boot_temp_df, aes(V2[2],educ[2]), color="cyan1") + 
  geom_point( data=boot_temp_df, aes(V2[3],educ[3]), color="cyan2") + 
  geom_point( data=boot_temp_df, aes(V2[4],educ[4]), color="cyan3") + 
  geom_point( data=boot_temp_df, aes(V2[5],educ[5]), color="cyan4") + 
  geom_point( data=boot_temp_df, aes(V3[6],educ[6]), color="red") + 
  geom_point( data=boot_temp_df, aes(V3[7],educ[7]), color="red1") + 
  geom_point( data=boot_temp_df, aes(V3[8],educ[8]), color="red2") + 
  geom_point( data=boot_temp_df, aes(V3[9],educ[9]), color="red3") + 
  geom_point( data=boot_temp_df, aes(V3[10],educ[10]), color="red4")
  

# DEBUG

impdebug <- mice(mammalsleep, maxit = 3)

# PCA graphs

# library(Amelia)
#> res.amelia <- amelia(don, m = 100)
#> compare.density(res.amelia, var = "T12")
#> overimpute(res.amelia, var = "maxO3")