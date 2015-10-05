# requires following packages
require(data.table)
require(plyr)
require(xgboost)
require(Metrics) # for map@10
require(pROC)
require(foreach)
require(doMC)
require(ROCR)

setwd('~/Projects/Recruit-Coupon-Purchase/')
source('github-code/clean.R') # 340 seconds
source('github-code/functions.R')
source('github-code/featurize.R') # TAKES A LOT OF TIME AND CONTAINS PARALLEL CODE!
source('github-code/model_xgboost.R')

# for BPR Matrix Factorization using MyMediaLite library