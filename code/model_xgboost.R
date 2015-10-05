#####################################################################################

xgb.model.make <- function(file='features.user.coupon.w3.k13.seed42.csv',
                           eta=0.05, subsample=0.9, colsample=0.5, depth=7, metric='auc', rounds=1400,
                           cv = F, seed=29, feat.imp=F){
  
  train = data.frame(fread(paste0('features/features.user.coupon.random/', file)))
  Dtrain <- xgb.DMatrix(data=data.matrix(train[,-c(1:3)]), label=train$target, missing=NA)
  parameters <- list(booster='gbtree', 
                     objective = 'binary:logistic',
                     eta = eta, 
                     subsample = subsample,
                     colsample_bytree = colsample,# 0.5 = 0.7709, 1750
                     max_depth = depth, 
                     verbose = 1,
                     eval.metric=metric,
                     nthread=30)
  if (cv ==T){
    xgb.cv(params=parameters, data=Dtrain, 
           nfold=5,
           nrounds = rounds, seed=seed)
  }
  
  xgb.model <- xgb.train(params = parameters, seed =seed, nrounds=rounds, data=Dtrain, verbose=1)
  
  if (feat.imp ==T ){
    xgb.imp <- xgb.importance(feature_names = colnames(train)[-c(1:3)], model=xgb.model)
    return(list('feat.imp' = xgb.imp, 'model'=xgb.model))
  } else {
    return(xgb.model)
  }
  
}
#####################################################################################
load.pairs <- function(w){
  data <- list()
  for (i in 1:16){
    data[[i]] <- data.frame(fread(paste0('features/features.user.coupon.w',w,'.part',i,'.csv')))
  }
  return(data)
}

#####################################################################################

purchase.predict <- function(models, data){
  
  ans <- list()
  for (i in 1:16){
    print(i)
    data.i <- data[[i]]
    Ddata <- xgb.DMatrix(data=data.matrix(data.i[,-c(1:2)]), missing=NA)
    
    tmp <- list()
    for (j in 1:length(models)){
      tmp[[j]] <- predict(models[[j]], Ddata)
    }
    tmp <- data.frame(do.call('cbind', tmp))
    
    data.i$prob <- rowMeans(tmp)
    data.i <- data.i[,colnames(data.i) %in% c('user_id', 'coupon_id', 'prob')]
    
    ans[[i]] <- data.i
  }
  
  ans <- data.frame(do.call('rbind', ans))
  ans <- ans[with(ans, order(user_id, -prob)), ]
  pred = split(ans$coupon_id, ans$user_id)
  prob = split(ans$prob, ans$user_id)
  return(list('pred'= pred, 
              'prob'= prob))
  
}

#####################################################################################

pairs <- load.pairs(w=1)

# ... models ...
# ... only creating 1 XGB model also gives almost similar accuracy ...

model1 <- xgb.model.make(file = 'features.user.coupon.w2.k13.seed42.csv', 
                         eta=0.05, subsample=0.9, colsample=0.5, depth=7, metric='auc', rounds=1000,
                         cv = F, seed=29, feat.imp=F) # train-auc:0.990913+0.000397	test-auc:0.912788+0.001912
model2 <- xgb.model.make(file = 'features.user.coupon.w2.k13.seed42.csv', 
                         eta=0.05, subsample=0.8, colsample=0.2, depth=6, metric='auc', rounds=1000,
                         cv = F, seed=13, feat.imp=F) 
model3 <- xgb.model.make(file = 'features.user.coupon.w2.k13.seed42.csv', 
                         eta=0.05, subsample=0.8, colsample=0.4, depth=6, metric='auc', rounds=1000,
                         cv = F, seed=45, feat.imp=F) 
model4 <- xgb.model.make(file = 'features.user.coupon.w2.k13.seed42.csv', 
                         eta=0.05, subsample=0.9, colsample=0.5, depth=6, metric='auc', rounds=1000,
                         cv = F, seed=89, feat.imp=F) 


model5 <- xgb.model.make(file = 'features.user.coupon.w2.k13.seed54.csv', 
                         eta=0.05, subsample=0.9, colsample=0.4, depth=7, metric='auc', rounds=1000,
                         cv = F, seed=65, feat.imp=F) 
model6 <- xgb.model.make(file = 'features.user.coupon.w2.k13.seed54.csv', 
                         eta=0.05, subsample=0.8, colsample=0.2, depth=6, metric='auc', rounds=1000,
                         cv = F, seed=20, feat.imp=F) 
model7 <- xgb.model.make(file = 'features.user.coupon.w2.k13.seed54.csv', 
                         eta=0.05, subsample=0.9, colsample=0.5, depth=6, metric='auc', rounds=1000,
                         cv = F, seed=54, feat.imp=F) 


model8 <- xgb.model.make(file = 'features.user.coupon.w2.k13.seed13.csv', 
                         eta=0.05, subsample=0.9, colsample=0.4, depth=7, metric='auc', rounds=1000,
                         cv = F, seed=29, feat.imp=F) 
model9 <- xgb.model.make(file = 'features.user.coupon.w2.k13.seed13.csv', 
                         eta=0.05, subsample=0.9, colsample=0.5, depth=6, metric='auc', rounds=1000,
                         cv = F, seed=643, feat.imp=F) 


model10 <- xgb.model.make(file = 'features.user.coupon.w2.k14.seed54.csv', 
                          eta=0.05, subsample=0.9, colsample=0.4, depth=7, metric='auc', rounds=1000,
                          cv = F, seed=10, feat.imp=F)
model11 <- xgb.model.make(file = 'features.user.coupon.w2.k14.seed54.csv', 
                          eta=0.05, subsample=0.8, colsample=0.3, depth=8, metric='auc', rounds=1000,
                          cv = F, seed=11, feat.imp=F)


model12 <- xgb.model.make(file = 'features.user.coupon.w2.k13.seed53.csv', 
                          eta=0.05, subsample=0.8, colsample=0.4, depth=7, metric='auc', rounds=1000,
                          cv = F, seed=32, feat.imp=F) 
model13 <- xgb.model.make(file = 'features.user.coupon.w2.k13.seed53.csv', 
                          eta=0.05, subsample=0.9, colsample=0.5, depth=7, metric='auc', rounds=1000,
                          cv = F, seed=56, feat.imp=F) 

model14 <- xgb.model.make(file = 'features.user.coupon.w2.k12.seed2.csv', 
                          eta=0.05, subsample=0.9, colsample=0.4, depth=7, metric='auc', rounds=1000,
                          cv = F, seed=92, feat.imp=F)
model15 <- xgb.model.make(file = 'features.user.coupon.w2.k12.seed2.csv', 
                          eta=0.05, subsample=0.9, colsample=0.4, depth=5, metric='auc', rounds=1000,
                          cv = F, seed=94, feat.imp=F)

model16 <- xgb.model.make(file = 'features.user.coupon.w2.k13.seed597.csv', 
                          eta=0.05, subsample=0.9, colsample=0.5, depth=7, metric='auc', rounds=1000,
                          cv = F, seed=29, feat.imp=F)
model17 <- xgb.model.make(file = 'features.user.coupon.w2.k13.seed597.csv', 
                          eta=0.05, subsample=0.8, colsample=0.4, depth=6, metric='auc', rounds=1000,
                          cv = F, seed=29, feat.imp=F)

# ... model XGB 1 ...
xgb.res <- purchase.predict(models=list(model1, model2, model3, model4,
                                        model5, model6, model7, model8, model9,
                                        model10, model11, model12, model13, model14,
                                        model15, model16, model17), data=pairs)

xgb.pred <- get.submission(xgb.res$pred)
dir.create('submissions')
write.table(xgb.pred, 'submissions/sub-XGB-1.csv', row.names = F, col.names = T, sep = ',', quote = F)


