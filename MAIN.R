#INITIALIZATION #########
library(mice)
library(VIM)
library(dplyr)
load("~/Desktop/LEARNING/M1/S1/Math Stats 1/Project/men7988_cell.RData")
base1=test
base2=subset(base1,base1$year88==1)
base3=base2[,c(7,18,19)] # our UNIMPUTED dataset
base3$waget <- log(base3$waget) # transform wage to log(wage)
rm(base2)
rm(base1)


#create parameter estimates using complete data:
B_C=lm(waget ~ exper + educ, data=base3)$coefficients

M=20 #number of amputed data sets to create
B=5 #number of imputations for Q2 & 3
 
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

#Q1: Calculate Bias for RegImp
RegImp_Bias_estimate <- (colSums(RegImp_params)*(1/M) - B_C)

#Q1: Calculate Variance for RegImp
RegImpmean <- colSums(RegImp_params)*(1/M)
RegImp_var_estimate <- colSums((RegImp_params - RegImpmean)^2)*(1/M)

#QUESTION 2 ########
bootimp_list <- list() #list of bootstrap imputed dataframes for loop

#loop for generating M imputed datasets
for(j in 1:M){
  boot_temp <- mice(amputed_list[[j]], m=B, method="norm.boot")
  bootimp_list[[j]] <- boot_temp
}

#Run regression on BootImp
bootimp_Reg_list <- list() #list for holding regression objects
for(j in 1:M){#the pool function calculates s.e. properly, square for variance.
  bootimp_Reg_temp <- with(bootimp_list[[j]], lm(waget ~ educ + exper))
  bootimp_Reg_list[[j]] <- summary(pool(bootimp_Reg_temp))
  bootimp_Reg_list[[j]]$std.error <- summary(pool(bootimp_Reg_temp))$std.error
  colnames(bootimp_Reg_list[[j]])[2] <- "var"
}

#CALCULATE BIAS AND VARIANCE FOR BOOTIMP

#add the coefficients together
boot_summedCols <- bootimp_Reg_list[[1]]
for(j in 2:M){
  boot_summedCols <- boot_summedCols + bootimp_Reg_list[[j]]
}

boot_Bias <- (1/M)*boot_summedCols$estimate - B_C
boot_var <- (1/M)*boot_summedCols$var

#QUESTION 3 #######
library(missMDA)
# We use NCP = 2 because we have two regressors educ and exper.

PCA_list <- list()
for(j in 1:M){
res.MIPCA <- MIPCA(amputed_list[[j]], ncp = 2, nboot  = B, method="Regularized") 
PCA_list[[j]] <- res.MIPCA$res.MI
}

#Run regression for PCA
# get the regressions and assign them to an object that can be used by mice pool function
PCA_Reg_list <- list()
for(j in 1:M){
  templistPCA <- list()
  for(i in 1:B){
    templistPCA[[i]] <- with(PCA_list[[j]][[i]], lm(waget ~ educ + exper))
  }
  PCA_Reg_list[[j]] <- templistPCA
}

#run pool function from mice package to get properly calculated variance across imputations.
PCA_summary_list <- list()
for(j in 1:M){#the pool function calculates s.e., square for variance.
  PCA_summary_list[[j]] <- summary(pool(PCA_Reg_list[[j]]))
  PCA_summary_list[[j]]$std.error <- summary(pool(PCA_Reg_list[[j]]))$std.error
  colnames(PCA_summary_list[[j]])[2] <- "var"
}

# CALCULATE BIAS AND VARIANCE FOR PCA
PCA_summedCols <- PCA_summary_list[[1]]
for(j in 2:M){
  PCA_summedCols <- PCA_summedCols + PCA_summary_list[[j]]
}

PCA_Bias <- (1/M)*PCA_summedCols$estimate - B_C
PCA_var <- (1/M)*PCA_summedCols$var


#load("alldata.RData")
#save.image(file="alldata.RData") 

# GRAPH GENERATION SECTION ######
library(ggplot2)

#A graph of one M from linear regression version. This visualizes the difference in imputed and amputed data. ########

#placeholder variables for graphing convenience
temp_df <- RegImp_list[[1]]
temp_df$before_imp <- amputed_list[[1]]$waget
temp_df$original_waget <- base3$waget
ggplot() + 
  geom_point(data=base3, aes(waget,educ), colour = 'gray' ,position = 'jitter') + 
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(original_waget,educ), colour = 'blue', alpha=0.4, position = 'jitter', shape = 1) + #original data that got amputed
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(waget,educ), colour = 'red', alpha=0.4,position = 'jitter', shape = 1) +
  scale_x_continuous(limits = c(0, 5))

#A graph of one M from Bootstrap regression version. This visualizes the difference in imputed and amputed data. #######

#placeholder variables for graphing convenience
temp_df <- bootimp_list[[1]][[1]]
temp_df$before_imp <- amputed_list[[1]]$waget
temp_df$original_waget <- base3$waget
ggplot() + 
  geom_point(data=base3, aes(waget,educ), colour = 'gray' ,position = 'jitter') + 
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(original_waget,educ), colour = 'blue', alpha=0.4, position = 'jitter', shape = 1) + #original data that got amputed
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(waget,educ), colour = 'red', alpha=0.4,position = 'jitter', shape = 1) +
  scale_x_continuous(limits = c(0, 5))




#graph with imputed value on Y axis and original value on X axis #######
ggplot() + 
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(original_waget,waget), colour = 'blue', alpha=0.1)+
  scale_x_continuous(limits = c(0,2))


ggplotRegression(lm(waget ~ educ + exper, data = base3))

# it is clear from our graph that the imputed data is following a linear regression/
# it has no difference in variance.

#BootImp graph #########

#graph of iterations for bootimp
plot(boot_temp,layout=c(2,1)) #graphs the mean and standard deviation for each iteration. (1 on left, 5 on right

#graph of the imputed data on an x-y plot with educ
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


# PCA graphs

#genearte graphs on a subsample of 100 (large numbers take too long to render or can't render, as every datapoint is drawn)
small_df <- amputed_list[[1]][sample(nrow(amputed_list[[1]]), 100), ]
res.22 <- MIPCA(small_df, ncp = 2, nboot  = B, method="Regularized") 
plot(res.22)


#x-y plot visualation

temp_df <- PCA_list[[1]]
temp_df$before_imp <- amputed_list[[1]]$waget
temp_df$original_waget <- base3$waget
ggplot() + 
  geom_point(data=base3, aes(waget,educ), colour = 'gray' ,position = 'jitter') + 
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(original_waget,educ), colour = 'blue', alpha=0.4, position = 'jitter', shape = 1) + #original data that got amputed
  geom_point(data = subset(temp_df, is.na(temp_df$before_imp)), aes(waget,educ), colour = 'red', alpha=0.4,position = 'jitter', shape = 1) +
  scale_x_continuous(limits = c(0, 5))
