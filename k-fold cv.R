rm( list = ls ()) ## clears R history

## CV
setwd <- getwd()
crimedat = read.csv("crime_data.csv", header =T)  # test data
nsize = nrow ( crimedat )
library ( glmnet )
## extracting the design matrix removing the response column
predictors = as.matrix ( crimedat [,-1])
## extracting the response vector
response = crimedat $ CrimeRate

## K- FOLD CV function
CVfunc = function (K, n, myseq , alphaval ) {
  ## K: number of folds
  ## n: sample size
  ## myseq : sequence of lambda values
  ## alphaval : value of alpha ( LASSO or ridge )
  folds = sample ((1:n)%%K + 1) ## create folds
  MSPE = c() ## create empty MSPE vector
  for (i in 1: length ( myseq )){ ## for each lambda value
    tmpMSPE = NULL
    for (k in 1:K) { ## for each fold
      ## create validation set
      validSet = crimedat [ folds ==k ,];
      ## create matrix of covariates in validation set
      tmp_valid = validSet ; tmp_valid $ CrimeRate = NULL ;
      valid = as.matrix (tmp_valid ) # covariates
      ## create training set
      trainSet = crimedat [ folds !=k ,];
      ## create vector of responses and matrix of covariates in training set
      trainresponse = trainSet $ CrimeRate ; # response
      tmp_train = trainSet ; tmp_train $ CrimeRate = NULL ;
      train = as.matrix (tmp_train ) # covariates
      ## fit model on training set
      m1 = glmnet (x=train , y= trainresponse , family ="gaussian", alpha = alphaval ,
                   lambda = myseq [i])
      ## predict on validation set
      pred1 = predict.glmnet (m1, newx = valid )
      ## create vector of MSPE based on CV for current lambda value
      tmpMSPE = c( tmpMSPE , mean (( validSet $ CrimeRate - pred1)^2))
    }
    ## store average MPSE for current lambda value
    MSPE [i] = mean ( tmpMSPE )
  }
  return ( MSPE )
}

set.seed (12345)
lambdaseq = seq(0,5, by=0.01)
n= nrow ( crimedat )

## Run k-fold cv on test data for K = n-1
CVMSPE = CVfunc (K=n-1, n=nsize , myseq = lambdaseq , alphaval = 1)

head ( CVMSPE )

lambda_min = lambdaseq [ which.min( CVMSPE )]
lambda_min

myfit_final = glmnet (x= predictors , y= response , family ="gaussian", alpha =1,
                        lambda = lambda_min)
coef( myfit_final )


