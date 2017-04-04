
setwd("C:/Users/TAH/Google Drive/MSPA/Predict_450/Solo_2/2_Data_R")
load("C:/Users/TAH/Google Drive/MSPA/Predict_450/Solo_2/2_Data_R/efCode.RData")
load("C:/Users/TAH/Google Drive/MSPA/Predict_450/Solo_2/2_Data_R/stc-cbc-respondents-v6.RData")
#Be sure to update these paths.

taskv6=read.csv(file="stc-dc-task-cbc-v6.csv", head=TRUE,sep=",")
#This is the task data.

task.mat=as.matrix(taskv6)
#Create a matrix from task data.
dim(task.mat)


X.mat=efcode.attmat.f(task.mat[,3:7])
#Note: These are the effects coded variables.
#Should produce 108 rows = 3x36, not sure where the 3 comes from?
#Should be 11 columns, not sure where the 11 comes from either?

xvec=c(0,1,2,3,4,5)
efcode.att.f(xvec)
#Test Effects Coding function to see what its doing.






# -- Create price and brand interaction variable.

pricevec=taskv6$price-mean(taskv6$price)
#Gets the vector of prices from taskV5 and center it on its mean.
#Not really sure what this is doing.

pricevec

X.brands=X.mat[,9:11]
#Get the columns from X.mat that represent brand
#They should be the last three columns (check to be sure of this).

X.brands

X.BrandByPrice = X.brands*pricevec
#Check this result by multiplying a single X.brands column and pricevec.

X.matrix=cbind(X.mat,X.BrandByPrice)
#combines the columns of X.mat and X.BrandByPrice.
#Check your result, of course. Do you have 108 rows and 14 columns in X.matrix?

det(t(X.matrix)%*%X.matrix)
#Checks matrix by premultiplying X.matrix by its transpose, 
#then then looks at the determinant of the result.
#You should get a positive number, maybe a very big one.
#You lost me at premultiply.

dim(X.matrix)




# -- Pepare Y data

ydata=resp.data.v5mod[,3:38]
#Gets Y data WITH OUT zowner and sex.

names(ydata)
#Check to see if you have all 36 response variables.

ydata=na.omit(ydata)
#Removes records with missing data

ydata=as.matrix(ydata)
#Converts Y data to a matrix.






# -- Creates Gender Variable

    library (dplyr)
    
    ydata2 <- resp.data.v5mod
    #-ydata2- Assigns responded data to object.
    
    ydata2[is.na(ydata2)] <- 0
    # -ydata2- Converts missing values to zeros.
    
    ydata2$gender <- ifelse(ydata2$Gen == 2,c(1), c(0)) 
    # -ydata2- Assigns 1 for female, zero for male.
    
    gender <-select(ydata2,gender)





# -- Creates zowner variable
    
    ydata2$zowner <- ifelse(ydata2$STCowner >= 1, c(1), c(0))
    # -ydata2- Assigns 1 for any value other greater than or equal to 1. 
    
    zowner <-select(ydata2,zowner)
    # -ydata2- Selects just the fields we want.
    
    names(ydata2)
    # -ydata2- Checks the names of the columns we're left with.
    
    ydata2=as.matrix(ydata2)
    # -ydata2- Convert Ydata2 to a matrix.






# -- Prepare model inputs.

lgtdata = NULL 
for (i in 1:360)  {lgtdata[[i]]=list(y=ydata[i,],X=X.matrix)}
#THis is one of the input sets for rhierMnlDP().
#a list of data for each respondent.  
#Its length equals the number of respondents.
#The data for each respondent is a list with two elements, 
#X.matrix and the respondent's choice responses, from their row in ydata.

#write.csv(lgtdata[[3]], file = "lgtdata_export.csv")
#Export file to .csv to understand lay out. 

length(lgtdata)
#Check to makes sure lgtdata has the correct length.
#Length should be 360.

lgtdata[[3]]
#Looks at data for 3rd respondent






# -- modelling- Prepare the other inputs for test run of the model.

mcmctest=list(R=30000,keep=5)
#R=the number of iterations. 5 is the chain thinning constant.
#Keeping only some iteration results on an ntervalbasis is called thinning.

#Data1=list(p=3,lgtdata=lgtdata)
#This is the data list rhierMnlDP() expects.
#p is choice set size, which is constant 3 for all choice sets and respondnets.

library(bayesm)
library(coda)

#testrun1=rhierMnlDP(Data=Data1,Mcmc=mcmctest)
#Test run with default parameters.








# -- Adds zowner variable to model

zownerc=matrix(scale(zowner,scale=FALSE),ncol=1)

zownerc
#Should be 1 column 360 rows.

mean(zownerc[,1])
# What do you think that mean of zowner will be?

#Data2=list(p=3,lgtdata=lgtdata,Z=zownerc)

#testrun2=rhierMnlDP(Data=Data2,Mcmc=mcmctest)
#performs test run 2 with zowner added.
#be sure to run at least 30,000 (thirty thousand) iterations of 
#the algorithm, and treat at least the first 15,000 of them as y.

#names(testrun2)
#there is now an element called Deltadraw.  Deltadraw is a matrix with 
#number of rows=saved iterations (1000), and number of columns = number 
#of regression predictors in your X.matrix (14)







# -- Adds gender AND zowner, peforms another test run with both

genderc=scale(gender, scale=FALSE)
# center on mean, don't standardize

zmatrix=cbind(zownerc,genderc)
# this binds the two vectors into matrix columns

Data3=list(p=3,lgtdata=lgtdata,Z=zmatrix)







#Test Run 3

testrun3=rhierMnlDP(Data=Data3,Mcmc=mcmctest)
#Once your run is finished, and assuming that everything when correctly, 
#testrun3$Deltadraw will be a matrix with 28 columns.  The first 14 columns 
#are from regressing the betas on zownerc, and the second 14 columns are 
#from regression the betas on genderc.




# -- Create betadraws1 object and add names to it.

dim(testrun3$betadraw)
#outputs the name of the various "output" dimensions of the model.

betadraw1=testrun3$betadraw
#Creates beta draw variable from the test run array.

dim(betadraw1)
# outputs >[1]  360   14 1000 which tells us it has 360 rows, 
#14 variables columns corresponding to X.matrix columns,
# and 1,00 represents the iterations (ran 5,000 kept every 5th one).

namevec=c("RAM_1","RAM_2","Processor_1","Processor_2","Screen_1","Screen_2", 
          "Price_1","Price_2","Brand_1","Brand_2","Brand_3","BP_1","BP_2","BP_3")
#Creates a vector of names for betadraw.

namevec
#Outputs the names we just created.

dimnames(betadraw1)=list(NULL,namevec,NULL)
# Applies the name vector to the betadraw1 object.
# Adds column names, but not row or “block” names.





# -- Do some plots and summary with betadraws.

plot(betadraw1[3,8,])
#Plot chain for 3 respondent and second coefficient, across 1,000 iterations.

plot(density(betadraw1[,8,3000:6000],width=2))
#here's what the distribution of the last 200 sampled values for 
#respondent 3's 2nd beta coefficient looks like.

summary(betadraw1[3,2,3000:6000])
#Some descriptive statistics for the last 200 records for
#respondent 3's 2nd coefficient.

betameans = apply(betadraw1[,,3000:6000],2,mean)
#summarize the samples of the coefficients.  If we do the following, 
# we get the overall means of the coefficients, the means across respondents:

betaMeanChains=apply(betadraw1,c(2:3),mean)
write.csv(betaMeanChains, file = "betaMeanChains_export.csv")

betameans
#write.csv(betameans, file = "betameans_export.csv")


betaMeanChains








# -- Looks at Istardraws.
table(testrun3$Istardraw)





# Works with Delta Draws

dim(testrun3$Deltadraw)

deltas = apply(testrun3$Deltadraw[3000:6000,],2,mean)
#means of each column of Deltadraw we obtained.
#The columns of Deltadraw correspond to coefficients from regressing 
#the 14 MNL betas on zownerc.  The rows are thinned samples from 
#the coefficients' posterior distributions. These coefficients indicate how the betas depend (linearly) on zownerc.


write.csv(deltas, file = "deltas_export.csv")


# -- Summarizing how the "deltas" (covariate coefficinents) are distributed

round(apply(testrun3$Deltadraw[3000:6000,],2,quantile,probs=c(0.10,0.25,0.5,0.75,0.90)),4)
#Here are selected quantiles of our last 200 draws. I've rounded them to 4 significant digits to better fit.






# Beta Mat Section
betamat=testrun3$betadraw[,2,3001:6000]	
# This will be a 360 X 300 matrix
dim(betamat)

getp.f=function(x,y=0){pfcn=ecdf(x)
+    return(pfcn(y))}

zp=apply(betamat,1,getp.f)

betaDiffZero=rep(0,nrow(betamat)) 
# make a vector of zeros
betaDiffZero[zp <= 0.05 | zp>= 0.95] = 1	

respDiffBetas=betamat[betaDiffZero==1,]
table(betaDiffZero)

respDiffBetas

taskv3=read.csv(file="stc-extra-scenarios-v6.csv", head=TRUE,sep=",")


#Run Extra Scenarios 

betameans=apply(testrun3$betadraw[,,3001:6000],c(1,2),mean)

taskv3=read.csv(file="stc-extra-scenarios-v6.csv", head=TRUE,sep=",")

task3.mat=as.matrix(taskv3) 

dim(task3.mat)

dim(task.mat)

X3.mat = efcode.attmat.f(task3.mat[,3:7])
write.csv(X3.mat, file = "X3.mat_THv1.csv")

dim(X3.mat)

dim(X.mat)

pricevec3=taskv3$price-mean(taskv3$price)

X3.brands=X3.mat[,9:11]

X3.BrandByPrice = X3.brands*pricevec3

X3.matrix=as.matrix(cbind(X3.mat,X3.BrandByPrice))

xbeta=X3.matrix%*%t(betameans)

xbeta2=matrix(xbeta,ncol=3,byrow=TRUE)

write.csv(pchoicemat, file = "Extra_Scenarios_xbeta2_THv1.csv")

expxbeta2=exp(xbeta2)

rsumvec=rowSums(expxbeta2)

pchoicemat=expxbeta2/rsumvec

write.csv(pchoicemat, file = "Extra_Scenarios_pchoicemat_THv1.csv")

plot(density(betadraw1[,12,3001:6000],width=2))
plot(density(betadraw1[,13,3001:6000],width=2))
plot(density(betadraw1[,14,3001:6000],width=2))



