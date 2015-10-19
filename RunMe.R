# requires following packages
require(data.table)
require(plyr)
require(xgboost)
require(Metrics) # for map@10
require(pROC)
require(foreach)
require(doMC)
require(ROCR)

# set path
setwd('[enter the path where you downloaded repo]') # setwd('~/Projects/Recruit-Coupon-Purchase/github-code/')

# run code
source('code/clean.R') # ~ 340 seconds
source('code/functions.R')
source('code/featurize.R') # TAKES A LOT OF TIME AND CONTAINS PARALLEL CODE!
source('code/model_xgboost.R')

# for BPR Matrix Factorization using MyMediaLite library
source('code/model_BPRMFknn.R')

