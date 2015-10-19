#
# This is utilizes Bayesian Personlized Ranking Matrix Factorization as implemented in MyMediaLite library.
# Mapping between coupon-attribute space to coupon-factors space is done using a knn model based on cosine similarity.
#

#####################################################################################


# ... function to create Matrix Factorization training file and save ...

get.MFtrainfile <- function(w, type, location, category='viewmatrix'){ 
  
  users <- get.users(w=w)
  testcoupons <- get.testcoupons(w=w)
  traincoupons <- get.traincoupons(w=w)
  
  if (type==1){
    ans = weblog[which(weblog$activity_date <= end_list[w] & weblog$activity_date >= start_list[w])]
    ans <- ans[which(ans$coupon_id %in% testcoupons & ans$user_id %in% users)]
    if (category == 'purchasematrix'){
      ans <- ans[which(ans$purchase_flag ==1), ]
    }
    ans <- ans[,c('user_id', 'coupon_id'), with=F]
    ans <- ans[!duplicated(ans)]
  } else if (type==2){
    ans = weblog[which(weblog$activity_date <= end_list[w] & weblog$activity_date >= start_list[w])]
    ans <- ans[which(ans$coupon_id %in% c(testcoupons, traincoupons) & ans$user_id %in% users)]
    if (category == 'purchasematrix'){
      ans <- ans[which(ans$purchase_flag ==1), ]
    }
    ans <- ans[,c('user_id', 'coupon_id'), with=F]
    ans <- ans[!duplicated(ans)]
  } else if (type==3){
    ans <- get.weblog(w=(w-1))
    ans <- ans[which(ans$coupon_id %in% c(traincoupons, testcoupons) & ans$user_id %in% users)]
    if (category == 'purchasematrix'){
      ans <- ans[which(ans$purchase_flag ==1), ]
    }
    ans <- ans[,c('user_id', 'coupon_id'), with=F]
    ans <- ans[!duplicated(ans)]
  }
  
  write.table(ans, location, row.names = F, col.names = F, sep=',', quote = F)
  
}

#####################################################################################

# ... function to factorize training matrix ...

BPRMF.factorize <- function(w, factors=32, reg_u=0.0025, reg_i=0.0025, reg_j=0.0025, iter=30,
                            learn_rate=0.05){
  

  # write shellscript
  shell.str = paste0('#!/bin/sh -e','\n\n','time mono --debug code/MyMediaLite/lib/mymedialite/item_recommendation.exe ',
                     '--training-file=BPRMF_files/trainfile.csv ',
                     '--recommender=MultiCoreBPRMF --recommender-options num_factors=',factors,
                     ' --recommender-options reg_u=',reg_u,
                     ' --recommender-options reg_i=',reg_i,
                     ' --recommender-options reg_j=',reg_j,
                     ' --recommender-options num_iter=',iter,
                     ' --recommender-options learn_rate=',learn_rate,
                     ' --recommender-options max_threads=32',
                     ' --save-model=BPRMF_files/model',
                     ' --save-user-mapping=BPRMF_files/user.mapping',
                     ' --save-item-mapping=BPRMF_files/coupon.mapping')
  write.table(shell.str, 'BPRMF_files/run.sh', col.names = F, row.names = F, quote=F)
  
  # run shellscript
  system(command = 'sh BPRMF_files/run.sh')
  

    
    # load user mapping
    user.map <- read.csv(paste0('BPRMF_files/user.mapping'), header=F, stringsAsFactors=F)
    user.map = data.frame(do.call('rbind', strsplit(user.map$V1, split='\t')))
    user.map$X1 <- as.character(user.map$X1); user.map$X2 <- as.character(user.map$X2)
    rownames(user.map) <- user.map$X1
    
    # load coupon mapping
    coupon.map <- read.csv(paste0('BPRMF_files/coupon.mapping'), header=F, stringsAsFactors=F)
    coupon.map = data.frame(do.call('rbind', strsplit(coupon.map$V1, split='\t')))
    coupon.map$X1 <- as.character(coupon.map$X1); coupon.map$X2 <- as.character(coupon.map$X2)
    rownames(coupon.map) <- coupon.map$X1
    
    # load model
    model <- read.csv(paste0('BPRMF_files/model'), col.names = 'x', stringsAsFactors=F)
    model <- data.frame(do.call('rbind', strsplit(model$x, split=' '))) # ignore the warnings
    
    # User latent factor matrix
    U = model[3:(nrow(user.map)*factors+2), ]
    U$X1 <- as.character(U$X1); U$X3 <- as.numeric(as.character(U$X3))
    U$X1 <- user.map[U$X1, 2]
    U = split(U$X3, U$X1)
    U = data.frame(do.call('rbind', U))
    
    # Coupon latent factor matrix
    C = model[(nrow(user.map)*factors+3):nrow(model), ]
    C = C[(nrow(coupon.map)+3):nrow(C), ]
    C$X1 <- as.character(C$X1); C$X3 <- as.numeric(as.character(C$X3))
    C$X1 = coupon.map[C$X1, 2]
    C = split(C$X3, C$X1)
    C = data.frame(do.call('rbind', C))
    
    return(list('U'=U, 'C'=C))
    
}
  
#####################################################################################

# ... function for KNN mapping between coupon-attributes and coupon-latent-factors ...

# get coupon features 
get.coupon.attributes <- function(coupons){ 
  features.coupon <- data.frame(fread(paste0('features/features.coupon.csv')))
  rownames(features.coupon) <- features.coupon$coupon_id
  features.coupon <- features.coupon[which(features.coupon$coupon_id %in% coupons), c(29:182,219:433)]
  return(features.coupon)
}

# get KNN predictions from Matrix Factors ...
# k = how many nearest neighbours to consider?
# MF = result of BPRMF.factorize()
# w = week to predict for
get.KNNpredict <- function(k, MF, w){
  
  knn.map <- function(k, train.coupon_latent, train.coupon_attributes, test.coupon_attributes){
    
    train.coupon_attributes = t(apply(train.coupon_attributes, 1, function(x)(x/sqrt(sum(x*x)))))
    test.coupon_attributes = t(apply(test.coupon_attributes, 1, function(x)(x/sqrt(sum(x*x)))))
    sim = test.coupon_attributes %*% t(train.coupon_attributes) # cosine similarity matrix
    
    test.coupon_latent <- lapply(1:nrow(sim), function(i){
      knn = sort(sim[i, ], decreasing = T)[1:k]
      latent.vec = colSums(train.coupon_latent[names(knn), ] * matrix(rep(as.vector(knn), ncol(train.coupon_latent)), 
                                                                      byrow=F, ncol=ncol(train.coupon_latent)))/sum(knn)
      return(latent.vec)
    })
    
    test.coupon_latent <- data.frame(do.call('rbind', test.coupon_latent))
    rownames(test.coupon_latent) <- rownames(sim)
    
    return(test.coupon_latent)
  }
  
  testC <- knn.map (k = k, train.coupon_latent = MF$C, 
                    train.coupon_attributes = get.coupon.attributes(coupons = rownames(MF$C)),
                    test.coupon_attributes = get.coupon.attributes(coupons = get.testcoupons(w=w)))
  
  pred <- as.matrix(MF$U) %*% t(as.matrix(testC))
  pred.coupons <- lapply(1:nrow(pred), function(i){
    names(sort(pred[i,], decreasing = T))
  })
  names(pred.coupons) <- rownames(pred)
  
  return(pred.coupons)
}

#####################################################################################



# training data (week 2) : 22782 users, 19368 items, 158933 events, sparsity 99.96398
dir.create('BPRMF_files')

get.MFtrainfile (w=2, type=3, category='purchasematrix', location = 'BPRMF_files/trainfile.csv')

MF = BPRMF.factorize(w=2, factors=160, reg_u=0.001, reg_i=0.001, reg_j=0.001, 
                     iter=3000, learn_rate=0.05) # training time ~ 06 min 15.7857540 sec.

BPRMFknn.res <- get.KNNpredict (k = 8, MF = MF, w=1)

#####################################################################################


# note that BPRMFknn.res contains predictions for 22782 week 2 users, but for submission all 
# 22873 week 1 users must be present.

# For the remaining users we simply assign the 10 most popular coupons predicted above.

# ... find most popular coupons ...
most_popular = count(unlist(lapply(1:length(BPRMFknn.res), function(i){
  return(BPRMFknn.res[[i]][1:10])
})))
 
most_popular = as.character(most_popular[with(most_popular, order(-freq)), ]$x[1:10])

# ... find remaining users ...
rem_users = setdiff(get.users(w=1), names(BPRMFknn.res))
  
# ... add most poular to remaining users ...
BPRMFknn.res.copy = BPRMFknn.res
for (i in 1:length(rem_users)){
  BPRMFknn.res.copy[[rem_users[i]]] <- most_popular
}

# ... create BPRMFknn submission file ...
# ... this should give around 0.0060 on Private leaderboard ...
BPRMFknn.pred <- get.submission(BPRMFknn.res.copy) 
dir.create('submissions')
write.table(BPRMFknn.pred, 'submissions/sub-BPRMFknn.csv', row.names = F, col.names = T, sep = ',', quote = F)


#####################################################################################

# The final model is an ensemble of xgboost and BPRMFknn predictions

# ... function to create ensemble of xgboost and BPRMFknn results ...
get.ensemble <- function(xgb.pred, BPRknn.pred, k=10){
  # shift matching coupons in BPRMFknn[[i]][1:k] and xgboost[[i]][1:k] to starting positions
  # remaining positions are occupied by xgboost predictions.
  predA <- BPRknn.pred
  predB <- xgb.pred[names(BPRknn.pred)]
  predAB <- list()
  for (i in 1:length(predA)){
    predAB[[i]] <- intersect(predB[[i]][1:k], predA[[i]][1:k])
    predAB[[i]] <- c(predAB[[i]], setdiff(predB[[i]], predAB[[i]]))
  }
  names(predAB) <- names(predA)
  pred1 <- xgb.pred[setdiff(names(xgb.pred), names(BPRknn.pred))]
  return(c(pred1, predAB))
}

# ... createensemble predictions and write to disk ...
ensemble.res <- get.ensemble(xgb.pred = xgb.res$pred, 
                                BPRknn.pred = BPRMFknn.res, k = 8) # k = 8 seems optimal value. 

ensemble.pred = get.submission(pred = ensemble.res )
write.table(ensemble.pred, 'submissions/sub-enemble.csv', row.names = F, col.names = T, sep = ',', quote = F)

#####################################################################################




