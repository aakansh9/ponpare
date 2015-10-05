##################################################################
########################### LOAD DATA ###########################
##################################################################

# ... userlist ...
userlist <- read.csv('data/userlist.csv')
userlist$reg_date <- as.POSIXct(userlist$reg_date)
userlist$withdraw_date <- as.POSIXct(userlist$withdraw_date)
userlist <- userlist[with(userlist, order(reg_date)), ]
userlist$user_reg_pref_name <- as.character(userlist$user_reg_pref_name)
userlist$user_id <- as.character(userlist$user_id)

# ... couponlist ...
couponlist <- read.csv('data/couponlist.csv')
couponlist$disp_from <- as.POSIXct(couponlist$disp_from)
couponlist$disp_end <- as.POSIXct(couponlist$disp_end)
couponlist$valid_from <- as.POSIXct(couponlist$valid_from)
couponlist$valid_end <- as.POSIXct(couponlist$valid_end)
for (i in 12:20){
  couponlist[,i] <- as.factor(couponlist[,i])
}; rm(i)

# ... weblog ...
weblog <- fread('data/weblog.csv', colClasses = c('integer', 'character', 'integer',
                                                  'character', 'character', 'character', 'character',
                                                  'integer', 'character', 'character', 'character', 'character'))
weblog$view_date <- as.POSIXct(weblog$view_date)
weblog$purchase_date <- as.POSIXct(weblog$purchase_date)
weblog$activity_date <- as.POSIXct(weblog$activity_date)

# ... pref.coords ...
pref.coords <- read.csv('data/pref.coords')
pref.coords$pref_name <- as.character(pref.coords$pref_name)
rownames(pref.coords) <- pref.coords$pref_name

# ... couponarea ...
couponarea <- read.csv('data/couponarea.csv')

# ... create 52 weeks ...
end = as.POSIXct('2012-06-30 23:59:59')
end_list <- as.POSIXct(unlist(lapply(0:51, function(i){end - i*7*24*60*60})), origin = '1970-01-01')
start = as.POSIXct('2012-06-24 12:00:00')
start_list <- as.POSIXct(unlist(lapply(0:51, function(i){start - i*7*24*60*60})), origin = '1970-01-01')
rm(start, end)

# ... load user_id and coupon_id hash keys ...
user_id_hash.key <- read.csv('data/keys/user_id_hash.key', row.names = 3)
user_id_hash.key$user_hash <- as.character(user_id_hash.key$user_hash)
coupon_id_hash.key <- read.csv('data/keys/coupon_id_hash.key', row.names = 3)
coupon_id_hash.key$coupon_hash <- as.character(coupon_id_hash.key$coupon_hash)


##################################################################
########################### FEATURIZE COUPON ###########################
##################################################################
dir.create('features')

features.coupon <- couponlist[,c(24,25,1:23)]

# ... discount ...
features.coupon$discount <- features.coupon$catalog_price - features.coupon$discount_price

# ... immediate_use_time ...
features.coupon$immediate_use_time <- (as.numeric(features.coupon$valid_from) - as.numeric(features.coupon$disp_from))/(60*60*24)

# ... genre_cat ...
features.coupon$genre_cat <- features.coupon$capsule_text
levels(features.coupon$genre_cat) <- c('beauty', 'lessons', 'lessons', 
                                       'travel_leisure','sales_gift_other', 'gourmet', 
                                       'travel_leisure', 'beauty','med', 
                                       'sales_gift_other', 'travel_leisure', 'travel_leisure', 
                                       'travel_leisure','travel_leisure', 'lessons', 
                                       'travel_leisure', 'beauty','sales_gift_other',
                                       'travel_leisure', 'beauty', 'travel_leisure', 
                                       'beauty', 'travel_leisure', 'sales_gift_other'
)

# ... one hot genre_cat ...
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~genre_cat-1, features.coupon))))

# ... one hot capsule text ...
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~capsule_text-1, features.coupon))))

# ... one hot genre name ...
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~genre_name-1, features.coupon))))

# ... one hot shop large area name ...
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~shop_large_area_name-1, features.coupon))))

# ... one hot shop pref name ...
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~shop_pref_name-1, features.coupon))))

# ... one hot shop small area name ...
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~shop_small_area_name-1, features.coupon))))

# ... one hot usable_date_xxx_y (eg. mon_0, mon_1, mon_2, mon_3)
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~usable_date_mon-1, features.coupon))))
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~usable_date_tue-1, features.coupon))))
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~usable_date_wed-1, features.coupon))))
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~usable_date_thu-1, features.coupon))))
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~usable_date_fri-1, features.coupon))))
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~usable_date_sat-1, features.coupon))))
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~usable_date_sun-1, features.coupon))))
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~usable_date_holiday-1, features.coupon))))
features.coupon <- data.frame(cbind(features.coupon, data.frame(model.matrix(~usable_date_before_holiday-1, features.coupon))))

# ... one hot listed_small_area_name ...
tmp <- data.table(cbind(couponarea, data.frame(model.matrix(~listed_small_area_name-1, couponarea))))
tmp.names = colnames(tmp)[5:59]
tmp <- tmp[,c(3, 5:59), with=F]
tmp <- tmp[,colSums(.SD),by='coupon_id']
tmp <- data.frame(do.call('rbind', split(tmp$V1, tmp$coupon_id)))
colnames(tmp) <- tmp.names
for(i in 1:ncol(tmp)){
  tmp[,i][which(tmp[,i] > 1)] <- 1
}
tmp$coupon_id <- rownames(tmp)
features.coupon <- join(features.coupon, tmp , by='coupon_id')

# ... one hot listed_pref_name ...
tmp <- data.table(cbind(couponarea, data.frame(model.matrix(~listed_pref_name-1, couponarea))))
tmp.names = colnames(tmp)[5:51]
tmp <- tmp[,c(3, 5:51), with=F]
tmp <- tmp[,colSums(.SD),by='coupon_id']
tmp <- data.frame(do.call('rbind', split(tmp$V1, tmp$coupon_id)))
colnames(tmp) <- tmp.names
for(i in 1:ncol(tmp)){
  tmp[,i][which(tmp[,i] > 1)] <- 1
}
tmp$coupon_id <- rownames(tmp)
features.coupon <- join(features.coupon, tmp , by='coupon_id')

# ... price_rate buckets ...
# <=25, <= 45 , <=50, <=55, <=60, <=65, 70, 75, 80, ... , 90, 95, 100
breaks = c(0,25, seq(45,100, 5))
tmp <- data.frame(matrix(ncol = (length(breaks)-1), nrow = nrow(features.coupon)))
for (i in 1:(length(breaks)-1)){
  tmp[,i] <- as.numeric(features.coupon$price_rate >= breaks[i] & features.coupon$price_rate < breaks[i+1])
}
colnames(tmp) = paste0('price_rate_B', 1:(length(breaks)-1))
features.coupon <- cbind(features.coupon, tmp)

# ... catalog_price buckets ...
breaks = c(0, 1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000,15000,16000,17000,18000,21000,24000,27000,30000,33000,38000,43000,48000,60000,100000,700000)
tmp <- data.frame(matrix(ncol = (length(breaks)-1), nrow = length(features.coupon$catalog_price)))
for (i in 1:(length(breaks)-1)){
  tmp[,i] <- as.numeric(features.coupon$catalog_price >= breaks[i] & features.coupon$catalog_price < breaks[i+1])
}
colnames(tmp) = paste0('catalog_price_B', 1:(length(breaks)-1))
features.coupon <- cbind(features.coupon, tmp)

# ... discount_price buckets ...
breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,15000,20000,25000,30000,40000,110000)
tmp <- data.frame(matrix(ncol = (length(breaks)-1), nrow = nrow(features.coupon)))
for (i in 1:(length(breaks)-1)){
  tmp[,i] <- as.numeric(features.coupon$discount_price >= breaks[i] & features.coupon$discount_price < breaks[i+1])
}
colnames(tmp) = paste0('discount_price_B', 1:(length(breaks)-1))
features.coupon <- cbind(features.coupon, tmp)

# ... discount buckets ...
breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000,15000,16000,17000,18000,19000,20000,30000,40000,50000,100000,650000)
tmp <- data.frame(matrix(ncol = (length(breaks)-1), nrow = nrow(features.coupon)))
for (i in 1:(length(breaks)-1)){
  tmp[,i] <- as.numeric(features.coupon$discount >= breaks[i] & features.coupon$discount < breaks[i+1])
}
colnames(tmp) = paste0('discount_B', 1:(length(breaks)-1))
features.coupon <- cbind(features.coupon, tmp)

# ... disp_period buckets ...
breaks= c(seq(0,7,1), 40)
tmp <- data.frame(matrix(ncol = (length(breaks)-1), nrow = nrow(features.coupon)))
for (i in 1:(length(breaks)-1)){
  tmp[,i] <- as.numeric(features.coupon$disp_period >= breaks[i] & features.coupon$disp_period < breaks[i+1])
}
colnames(tmp) = paste0('disp_period_B', 1:(length(breaks)-1))
features.coupon <- cbind(features.coupon, tmp)

# ... valid_period buckets ...
breaks= c(0,20,40,60,80,100,120,140,160,180,400)
tmp <- data.frame(matrix(ncol = (length(breaks)-1), nrow = nrow(features.coupon)))
for (i in 1:(length(breaks)-1)){
  tmp[,i] <- as.numeric(features.coupon$valid_period >= breaks[i] & features.coupon$valid_period < breaks[i+1])
}
colnames(tmp) = paste0('valid_period_B', 1:(length(breaks)-1))
features.coupon <- cbind(features.coupon, tmp)

# ... immediate_use_time buckets ...
breaks= c(seq(-5,45,5),60,120)
tmp <- data.frame(matrix(ncol = (length(breaks)-1), nrow = nrow(features.coupon)))
for (i in 1:(length(breaks)-1)){
  tmp[,i] <- as.numeric(features.coupon$immediate_use_time >= breaks[i] & features.coupon$immediate_use_time < breaks[i+1])
}
colnames(tmp) = paste0('immediate_use_time_B', 1:(length(breaks)-1))
features.coupon <- cbind(features.coupon, tmp)


# ... save features.coupon ...
rm(tmp,i,breaks,tmp.names)
write.table(features.coupon, 'features/features.coupon.csv', row.names = F, col.names = T, sep=',')

##################################################################
########################### FEATURIZE USER GLOBAL #################
##################################################################

features.user.global <- data.frame(cbind('user_id'=as.character(userlist$user_id),
                                         'gender'=userlist$gender,
                                         'age'=userlist$age))

# ... age, gender ...
features.user.global$age <- as.integer(as.character(features.user.global$age))
features.user.global$gender <- as.numeric(features.user.global$gender)

# ... reg/withdraw date features ...
features.user.global$reg_date <- userlist$reg_date
features.user.global$reg_date_year <- as.POSIXlt(features.user.global$reg_date)$year +1900
features.user.global$reg_date_mon <- as.POSIXlt(features.user.global$reg_date)$mon +1
features.user.global$reg_date_mday <- as.POSIXlt(features.user.global$reg_date)$mday
features.user.global$reg_date_wday <- as.POSIXlt(features.user.global$reg_date)$wday + 1
features.user.global$reg_date_yday <- as.POSIXlt(features.user.global$reg_date)$yday + 1
features.user.global$withdraw_date <- userlist$withdraw_date
features.user.global$withdraw_date_year <- as.POSIXlt(features.user.global$withdraw_date)$year +1900
features.user.global$withdraw_date_mon <- as.POSIXlt(features.user.global$withdraw_date)$mon +1
features.user.global$withdraw_date_mday <- as.POSIXlt(features.user.global$withdraw_date)$mday
features.user.global$withdraw_date_wday <- as.POSIXlt(features.user.global$withdraw_date)$wday + 1
features.user.global$withdraw_date_yday <- as.POSIXlt(features.user.global$withdraw_date)$yday + 1
features.user.global$reg_period_days <- (as.numeric(features.user.global$withdraw_date) - as.numeric(features.user.global$reg_date))/(60*60*24)
features.user.global$days_before_registration <- (as.numeric(features.user.global$reg_date) - min(as.numeric(features.user.global$reg_date)) + 1)/(60*60*24)

# ... user_reg_pref_name features + one-hot ...
features.user.global$user_reg_pref_name <- as.character(userlist$user_reg_pref_name)
tmp <- data.frame(model.matrix(~user_reg_pref_name-1, features.user.global))
tmp$user_id <- features.user.global[rownames(tmp), 'user_id']
features.user.global <- join(features.user.global, tmp, by='user_id')

# ... save ...
write.table(features.user.global, 'features/features.user.global.csv', row.names = F, col.names = T, sep=',')
rm(tmp)

##################################################################
########################### FEATURIZE USER WEEKDEPENDENT #########
##################################################################
# ... function to find affinity vectors ...
# ... there are 8 kinds of affinities that can be calculated ...

affinity <- function(dt, type){
  
  if (type==1){    # only attribute 
    
    return(paste(summary(dt$attribute)/sum(summary(dt$attribute)), collapse=' '))
    
  } else if(type==2){     # attribute and session id
    
    tmp <- dt[,paste(as.vector(summary(attribute))/sum(as.vector(summary(attribute))), collapse=' '),by='session_id']$V1
    tmp <- data.frame(do.call('rbind', strsplit(tmp, split=' ')))
    for(i in 1:ncol(tmp)){tmp[,i] <- as.numeric(as.character(tmp[,i]))}
    return(paste(colMeans(tmp), collapse=' '))
    
  } else if (type==3){    # attribute, session id
    
    tmp <- dt[,paste(as.vector(summary(attribute))/sum(as.vector(summary(attribute))), collapse=' '),by='session_id']$V1
    tmp <- data.frame(do.call('rbind', strsplit(tmp, split=' ')))
    for(i in 1:ncol(tmp)){tmp[,i] <- as.numeric(as.character(tmp[,i]))}
    weights <- dt[,.N,by='session_id']$N
    tmp <- colSums(tmp*weights)/sum(weights)
    return(paste(tmp, collapse=' '))
    
  } else if (type==4){      # attribute, session id, page_serial
    
    # vector for every session
    tmp <- split(dt, dt$session_id)
    tmp <- lapply(1:length(tmp), function(i){
      sess <- tmp[[i]]
      sess$depth <- c(sess$page_serial[1], diff(sess$page_serial))
      sess$depth[which(sess$depth <=0)] <- 2 # coupon was bought!
      v <- as.vector(summary(rep(sess$attribute, sess$depth)))
      return(v/sum(v))
    })
    # combine each session vector 
    tmp <- data.frame(do.call('rbind', tmp))
    return(paste(colMeans(tmp), collapse=' '))
    
    
  } else if (type==5){    # attribute, session id, page_serial
    
    # vector for every session
    tmp <- split(dt, dt$session_id)
    tmp <- lapply(1:length(tmp), function(i){
      sess <- tmp[[i]]
      sess$depth <- c(sess$page_serial[1], diff(sess$page_serial))
      sess$depth[which(sess$depth <=0)] <- 2 # coupon was bought!
      v <- as.vector(summary(rep(sess$attribute, sess$depth)))
      return(v/sum(v))
    })
    # combine each session vector 
    tmp <- data.frame(do.call('rbind', tmp))
    weights <- dt[,.N,by='session_id']$N
    tmp <- colSums(tmp*weights)/sum(weights)
    return(paste(tmp, collapse=' '))
    
    
  } else if (type==6){    # attribute, item count
    
    dt$item_count[which(dt$item_count == 0)] <- 0.5
    v <- summary(rep(dt$attribute, 2*dt$item_count))
    return(paste(v/sum(v), collapse=' '))
    
  } else if (type==7){     # attribute, session id, item count
    
    dt$item_count[which(dt$item_count == 0)] <- 0.5
    # vector for every session
    tmp <- split(dt, dt$session_id)
    tmp <- lapply(1:length(tmp), function(i){
      sess <- tmp[[i]]
      v = summary(rep(sess$attribute, 2*sess$item_count))
      return(v/sum(v))
    })
    # combine each session vector 
    tmp <- data.frame(do.call('rbind', tmp))
    return(paste(colMeans(tmp), collapse=' '))
    
  } else if (type==8){    #attribute, session id, item count
    
    dt$item_count[which(dt$item_count == 0)] <- 0.5
    # vector for every session
    tmp <- split(dt, dt$session_id)
    tmp <- lapply(1:length(tmp), function(i){
      sess <- tmp[[i]]
      v = summary(rep(sess$attribute, 2*sess$item_count))
      return(v/sum(v))
    })
    # combine each session vector 
    tmp <- data.frame(do.call('rbind', tmp))
    weights <- dt[,.N,by='session_id']$N
    tmp <- colSums(tmp*weights)/sum(weights)
    return(paste(tmp, collapse=' '))
    
  }
}


# .... function to find user features which are week dependent ...

featurize.user.weekdependent <- function(w){
  
  users <- get.users(w=w)
  traincoupons <- get.traincoupons(w=w)
  testcoupons <- get.testcoupons(w=w)
  weblog <- get.weblog(w=w)
  
  features.coupon <- features.coupon[which(features.coupon$coupon_id %in% union(traincoupons, testcoupons)), ]
  features.user.global <- features.user.global[which(features.user.global$user_id %in% users), ]
  
  # join features.coupon to weblog 
  weblog <- join(weblog, features.coupon[,which(colnames(features.coupon) %in% 
                                                  c('coupon_id','dataset','capsule_text',
                                                    'genre_name','price_rate','catalog_price',
                                                    'discount_price','disp_period','valid_period',
                                                    'usable_date_mon','usable_date_tue','usable_date_wed',
                                                    'usable_date_thu','usable_date_fri','usable_date_sat',
                                                    'usable_date_sun','usable_date_holiday','usable_date_before_holiday',
                                                    'shop_large_area_name','shop_pref_name','shop_small_area_name',
                                                    'immediate_use_time','genre_cat')
  )], by='coupon_id')
  
  # EMPTY FEAT DATA FRAME
  feat <- data.frame(weblog[,1 , by='user_id']$user_id)
  colnames(feat) <- 'user_id'
  # - END -
  
  # DATE RELATED FEAT
  feat$last_activity_gap <- (as.numeric(start_list[w])-as.numeric(weblog[,max(activity_date),by='user_id']$V1))/(60*60*24)
  feat$first_activity_gap <- (as.numeric(start_list[w])-as.numeric(weblog[,min(activity_date),by='user_id']$V1))/(60*60*24)
  tmp <- data.frame(cbind('user_id'=as.character(features.user.global$user_id), 
                          'is_withdrawn_beforeweekstart'= as.numeric(features.user.global$withdraw_date <= start_list[w]), # ~907
                          'is_registered_afterweekstart' = as.numeric(features.user.global$reg_date > start_list[w]) # ~48
  ))   
  tmp$is_withdrawn_beforeweekstart[is.na(tmp$is_withdrawn_beforeweekstart)] <- 0
  feat <- join(feat, tmp, by='user_id')
  
  feat$activity_length_beforeweekstart <- weblog[,(as.numeric(max(activity_date, na.rm=T))-as.numeric(min(activity_date, na.rm=T)))/60,by='user_id']$V1
  feat$num_unique_activity_dates <- weblog[,length(unique(as.Date(activity_date))),by='user_id']$V1
  
  tmp <- features.user.global[,which(colnames(features.user.global) %in% c('user_id', 'reg_date', 'withdraw_date'))]  
  tmp$withdraw_date[which(is.na(tmp$withdraw_date))] <- start_list[w]
  tmp$max_unique_activity_days <- (as.numeric(tmp$withdraw_date) - as.numeric(tmp$reg_date))/(60*60*24)
  rownames(tmp) <- tmp$user_id
  feat$rel_num_unique_activity_dates <- feat$num_unique_activity_dates/tmp[feat$user_id, ]$max_unique_activity_days
  # - END -
  
  # SESSION_ID RELATED FEAT
  feat$num_unique_sessions <- weblog[,length(unique(session_id)),by='user_id']$V1 # includes count of NA session ids
  # - END -
  
  # PURCHASE RELATED FEAT
  feat$past_unique_purchasedcoupons <- weblog[,sum(item_count),by='user_id']$V1
  feat$past_total_purchasedcoupons <- weblog[,sum(item_count),by='user_id']$V1
  feat$past_unique_viewedcoupons <- weblog[,length(unique(coupon_id)),by='user_id']$V1
  feat$past_unique_purchased_over_viewed <- feat$past_unique_purchasedcoupons/feat$past_unique_viewedcoupons
  feat$past_total_views <- weblog[,.N,by='user_id']$N
  # - END -
  
  # AFFINITY FEATS
  
  ################# remove logs which have no coupon features
  weblog <- weblog[which(weblog$coupon_id %in% union(traincoupons, testcoupons)), ] # has fewer user ids now than in feat.
  
  ################# set NA page_serial to 0
  weblog$page_serial[is.na(weblog$page_serial)] <- 0 # users who purchase without viewing or users who neither viewed nor purchased
  # didn't visit any page.
  ################# make empty feat.tmp
  feat.tmp <- data.frame('user_id'=unique(weblog$user_id))
  
  ################# immediate_use_time mean,sd
  feat.tmp$immediate_use_time_mean <- weblog[,mean(immediate_use_time),by='user_id']$V1
  feat.tmp$immediate_use_time_sd <- weblog[,sd(immediate_use_time),by='user_id']$V1
  
  ################# prices mean,sd
  feat.tmp$price_rate_mean <- weblog[,mean(price_rate),by='user_id']$V1
  feat.tmp$catalog_price_mean <- weblog[,mean(catalog_price),by='user_id']$V1
  feat.tmp$discount_price_mean <- weblog[,mean(discount_price),by='user_id']$V1
  feat.tmp$discount_mean <- weblog[,mean(catalog_price - discount_price),by='user_id']$V1
  feat.tmp$price_rate_sd <- weblog[,sd(price_rate),by='user_id']$V1
  feat.tmp$catalog_price_sd <- weblog[,sd(catalog_price),by='user_id']$V1
  feat.tmp$discount_price_sd <- weblog[,sd(discount_price),by='user_id']$V1
  feat.tmp$discount_sd <- weblog[,sd(catalog_price - discount_price),by='user_id']$V1
  
  ################# disp/valid_period mean,sd
  feat.tmp$disp_period_mean <- weblog[,mean(disp_period),by='user_id']$V1
  feat.tmp$valid_period_mean <- weblog[,mean(valid_period),by='user_id']$V1
  feat.tmp$disp_period_sd <- weblog[,sd(disp_period),by='user_id']$V1
  feat.tmp$valid_period_sd <- weblog[,sd(valid_period),by='user_id']$V1
  
  ################# genre_cat affinity vectors (8)
  get.weblog.subcols <- function(colname){
    weblog.subcols <- as.data.table(as.data.frame(weblog)[,c('purchase_flag','page_serial', 'item_count','session_id', colname,'user_id')])
    weblog.subcols$session_id[is.na(weblog.subcols$session_id)] <- 's0' # weblog.subcols has no NA values now.
    setnames(weblog.subcols, colname, 'attribute')
    weblog.subcols <- split(weblog.subcols, weblog.subcols$user_id)
    weblog.subcols <- weblog.subcols[feat.tmp$user_id]
    return(weblog.subcols)
  }
  
  weblog.subcols = get.weblog.subcols('genre_cat')
  registerDoMC(32)
  print('... now generating genre_cat.Affinity 1 of 8')
  feat.tmp$genre_cat.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  } # 22.7 sec
  print('... now generating genre_cat.Affinity 2 of 8')
  feat.tmp$genre_cat.Affinity2 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=2)
  }
  print('... now generating genre_cat.Affinity 3 of 8')
  feat.tmp$genre_cat.Affinity3 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=3)
  }
  print('... now generating genre_cat.Affinity 4 of 8')
  feat.tmp$genre_cat.Affinity4 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=4)
  }
  print('... now generating genre_cat.Affinity 5 of 8')
  feat.tmp$genre_cat.Affinity5 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=5)
  }
  print('... now generating genre_cat.Affinity 6 of 8')
  feat.tmp$genre_cat.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  print('... now generating genre_cat.Affinity 7 of 8')
  feat.tmp$genre_cat.Affinity7 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=7)
  }
  print('... now generating genre_cat.Affinity 8 of 8')
  feat.tmp$genre_cat.Affinity8 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=8)
  }
  
  
  ################# genre_name affinity vectors (8)
  weblog.subcols = get.weblog.subcols('genre_name')
  registerDoMC(32)
  print('... now generating genre_name.Affinity 1 of 8')
  feat.tmp$genre_name.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  print('... now generating genre_name.Affinity 2 of 8')
  feat.tmp$genre_name.Affinity2 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=2)
  }
  print('... now generating genre_name.Affinity 3 of 8')
  feat.tmp$genre_name.Affinity3 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=3)
  }
  print('... now generating genre_name.Affinity 4 of 8')
  feat.tmp$genre_name.Affinity4 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=4)
  }
  print('... now generating genre_name.Affinity 5 of 8')
  feat.tmp$genre_name.Affinity5 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=5)
  }
  print('... now generating genre_name.Affinity 6 of 8')
  feat.tmp$genre_name.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  print('... now generating genre_name.Affinity 7 of 8')
  feat.tmp$genre_name.Affinity7 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=7)
  }
  print('... now generating genre_name.Affinity 8 of 8')
  feat.tmp$genre_name.Affinity8 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=8)
  }
  
  ################# capsule_text affinity vectors (4)
  weblog.subcols = get.weblog.subcols('capsule_text')
  registerDoMC(32)
  print('... now generating capsule_text.Affinity 1 of 4')
  feat.tmp$capsule_text.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  print('... now generating capsule_text.Affinity 2 of 4')
  feat.tmp$capsule_text.Affinity2 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=2)
  }
  print('... now generating capsule_text.Affinity 3 of 4')
  feat.tmp$capsule_text.Affinity4 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=4)
  }
  print('... now generating capsule_text.Affinity 4 of 4')
  feat.tmp$capsule_text.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  
  ################# shop_large_area_name affinity vectors (8)
  weblog.subcols = get.weblog.subcols('shop_large_area_name')
  registerDoMC(32)
  print('... now generating shop_large_area_name.Affinity 1 of 8')
  feat.tmp$shop_large_area_name.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  print('... now generating shop_large_area_name.Affinity 2 of 8')
  feat.tmp$shop_large_area_name.Affinity2 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=2)
  }
  print('... now generating shop_large_area_name.Affinity 3 of 8')
  feat.tmp$shop_large_area_name.Affinity3 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=3)
  }
  print('... now generating shop_large_area_name.Affinity 4 of 8')
  feat.tmp$shop_large_area_name.Affinity4 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=4)
  }
  print('... now generating shop_large_area_name.Affinity 5 of 8')
  feat.tmp$shop_large_area_name.Affinity5 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=5)
  }
  print('... now generating shop_large_area_name.Affinity 6 of 8')
  feat.tmp$shop_large_area_name.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  print('... now generating shop_large_area_name.Affinity 7 of 8')
  feat.tmp$shop_large_area_name.Affinity7 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=7)
  }
  print('... now generating shop_large_area_name.Affinity 8 of 8')
  feat.tmp$shop_large_area_name.Affinity8 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=8)
  }
  
  ################# shop_pref_name affinity vectors (3)
  weblog.subcols = get.weblog.subcols('shop_pref_name')
  registerDoMC(32)
  print('... now generating shop_pref_name.Affinity 1 of 3')
  feat.tmp$shop_pref_name.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  print('... now generating shop_pref_name.Affinity 2 of 3')
  feat.tmp$shop_pref_name.Affinity2 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=2)
  }
  print('... now generating shop_pref_name.Affinity 3 of 3')
  feat.tmp$shop_pref_name.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  
  ################# shop_small_area_name affinity vectors (3)
  weblog.subcols = get.weblog.subcols('shop_small_area_name')
  registerDoMC(32)
  print('... now generating shop_large_area_name.Affinity 1 of 3')
  feat.tmp$shop_small_area_name.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  print('... now generating shop_large_area_name.Affinity 2 of 3')
  feat.tmp$shop_small_area_name.Affinity2 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=2)
  }
  print('... now generating shop_large_area_name.Affinity 3 of 3')
  feat.tmp$shop_small_area_name.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  
  ################# usable_date_mon affinity vectors (2)
  weblog.subcols = get.weblog.subcols('usable_date_mon')
  registerDoMC(32)
  print('... now generating usable_date_mon.Affinity 1 & 2')
  feat.tmp$usable_date_mon.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  feat.tmp$usable_date_mon.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  
  ################# usable_date_tue - user affinity vectors (2)
  weblog.subcols = get.weblog.subcols('usable_date_tue')
  registerDoMC(32)
  print('... now generating usable_date_tue.Affinity 1 & 2')
  feat.tmp$usable_date_tue.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  feat.tmp$usable_date_tue.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  
  
  ################# usable_date_wed affinity vectors (2)
  weblog.subcols = get.weblog.subcols('usable_date_wed')
  registerDoMC(32)
  print('... now generating usable_date_wed.Affinity 1 & 2')
  feat.tmp$usable_date_wed.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  feat.tmp$usable_date_wed.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  
  ################# usable_date_thu affinity vectors (2)
  weblog.subcols = get.weblog.subcols('usable_date_thu')
  registerDoMC(32)
  print('... now generating usable_date_thu.Affinity 1 & 2')
  feat.tmp$usable_date_thu.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  feat.tmp$usable_date_thu.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  
  ################# usable_date_fri affinity vectors (2)
  weblog.subcols = get.weblog.subcols('usable_date_fri')
  registerDoMC(32)
  print('... now generating usable_date_fri.Affinity 1 & 2')
  feat.tmp$usable_date_fri.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  feat.tmp$usable_date_fri.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  
  ################# usable_date_sat affinity vectors (2)
  weblog.subcols = get.weblog.subcols('usable_date_sat')
  registerDoMC(32)
  print('... now generating usable_date_sat.Affinity 1 & 2')
  feat.tmp$usable_date_sat.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  feat.tmp$usable_date_sat.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  
  ################# usable_date_sun affinity vectors (2)
  weblog.subcols = get.weblog.subcols('usable_date_sun')
  registerDoMC(32)
  print('... now generating usable_date_sun.Affinity 1 & 2')
  feat.tmp$usable_date_sun.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  feat.tmp$usable_date_sun.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  
  ################# usable_date_holiday affinity vectors (2)
  weblog.subcols = get.weblog.subcols('usable_date_holiday')
  registerDoMC(32)
  print('... now generating usable_date_holiday.Affinity 1 & 2')
  feat.tmp$usable_date_holiday.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  feat.tmp$usable_date_holiday.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  
  ################# usable_date_before_holiday affinity vectors (2)
  weblog.subcols = get.weblog.subcols('usable_date_before_holiday')
  registerDoMC(32)
  print('... now generating usable_date_before_holiday.Affinity 1 & 2')
  feat.tmp$usable_date_before_holiday.Affinity1 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=1)
  }
  feat.tmp$usable_date_before_holiday.Affinity6 <- foreach(i = 1:length(weblog.subcols), .combine='c') %dopar% {
    affinity(dt=weblog.subcols[[i]], type=6)
  }
  
  ################# listed_small_area_name_xxx affinity
  ################# listed_pref_name_xxx affinity
  ################# price_rate_B_xxx affinity
  ################# catalog_price_B_xxx affinity
  ################# discount_price_B_xxx affinity
  ################# discount_B_xxx affinity
  ################# disp_period_B_xxx affinity
  ################# valid_period_B_xxx affinity
  ################# immediate_use_time_B_xxx affinity
  print('... now generating features.coupon[ , 219:433 ] Affinity')
  s=219; e=433
  weblog <- join(weblog[,which(names(weblog) %in% c('user_id', 'coupon_id')), with=F], features.coupon[,c(1, s:e)], by='coupon_id')
  tmp1 <- data.frame(matrix(ncol = (e-s+1), nrow = nrow(feat.tmp)))
  for (k in 3:(e-s+3)){
    tmp2 <- data.table(cbind('user_id'= weblog$user_id, 'attr'= weblog[,k,with=F]))
    names(tmp2)[2] = 'attr'
    tmp1[,(k-2)] <- tmp2[,mean(attr, na.rm=T),by='user_id']$V1
  }
  colnames(tmp1) <- paste0(colnames(weblog)[3:(e-s+3)],'.Affinity')
  feat.tmp <- cbind(feat.tmp, tmp1)
  
  # - END -
  
  print('... joining and returning answer \\m/ ')
  
  # join feat and feat.tmp
  feat <- join(feat, feat.tmp, by='user_id') # note some affinity vectors are just NA after joining
  
  # return
  return(feat)
  
}


######### featurize #########

feat <- featurize.user.weekdependent(w=1) # 4000 sec
write.table(feat, 'features/features.user.w1.csv', row.names = F, col.names = T, sep=',')

feat <- featurize.user.weekdependent(w=2)
write.table(feat, 'features/features.user.w2.csv', row.names = F, col.names = T, sep=',')


##################################################################
########################### FEATURIZE USER COUPON PAIRS #################
##################################################################
#####################################################################################
AffinityMatrices.user.coupon <- function(w){
  
  users = get.users(w=w)
  testcoupons = get.testcoupons(w=w)
  
  # load features.coupon
  features.coupon <- read.csv('features/features.coupon.csv')
  features.coupon$disp_from <- as.POSIXct(features.coupon$disp_from)
  features.coupon$disp_end <- as.POSIXct(features.coupon$disp_end)
  features.coupon$valid_from <- as.POSIXct(features.coupon$valid_from)
  features.coupon$valid_end <- as.POSIXct(features.coupon$valid_end)
  rownames(features.coupon) <- features.coupon$coupon_id
  features.coupon <- features.coupon[testcoupons, ]
  for(i in 14:22){features.coupon[,i] <- as.factor(features.coupon[,i])}
  
  # load features.user.weekdependent
  features.user.weekdependent <- data.frame(fread(paste0('features/features.user.w',w,'.csv')))
  features.user.weekdependent$is_withdrawn_beforeweekstart <- as.numeric(features.user.weekdependent$is_withdrawn_beforeweekstart) 
  features.user.weekdependent$is_registered_afterweekstart <- as.numeric(features.user.weekdependent$is_registered_afterweekstart)
  rownames(features.user.weekdependent) <- features.user.weekdependent$user_id
  features.user.weekdependent <- features.user.weekdependent[users, ]
  
  # attribute affinity matrix function
  attribute.affinity.matrix <- function(coupon.cols, user.col){
    c = features.coupon[, coupon.cols]
    if (length(user.col)==1){
      u = as.matrix(do.call('rbind', strsplit(features.user.weekdependent[, user.col], split=' ')))
    } else{
      u = as.matrix(features.user.weekdependent[,user.col])
    }
    storage.mode(u) <- "numeric"
    rownames(u) <- features.user.weekdependent$user_id
    res = u %*% t(c)
    return(res)
  }
  
  # affinity matrices
  genre_cat.Affinity1 <- attribute.affinity.matrix(coupon.cols = 29:34, user.col = 29)
  genre_cat.Affinity2 <- attribute.affinity.matrix(coupon.cols = 29:34, user.col = 30)
  genre_cat.Affinity3 <- attribute.affinity.matrix(coupon.cols = 29:34, user.col = 31)
  genre_cat.Affinity4 <- attribute.affinity.matrix(coupon.cols = 29:34, user.col = 32)
  genre_cat.Affinity5 <- attribute.affinity.matrix(coupon.cols = 29:34, user.col = 33)
  genre_cat.Affinity6 <- attribute.affinity.matrix(coupon.cols = 29:34, user.col = 34)
  genre_cat.Affinity7 <- attribute.affinity.matrix(coupon.cols = 29:34, user.col = 35)
  genre_cat.Affinity8 <- attribute.affinity.matrix(coupon.cols = 29:34, user.col = 36)
  genre_name.Affinity1 <- attribute.affinity.matrix(coupon.cols = 59:71, user.col = 37)
  genre_name.Affinity2 <- attribute.affinity.matrix(coupon.cols = 59:71, user.col = 38)
  genre_name.Affinity3 <- attribute.affinity.matrix(coupon.cols = 59:71, user.col = 39)
  genre_name.Affinity4 <- attribute.affinity.matrix(coupon.cols = 59:71, user.col = 40)
  genre_name.Affinity5 <- attribute.affinity.matrix(coupon.cols = 59:71, user.col = 41)
  genre_name.Affinity6 <- attribute.affinity.matrix(coupon.cols = 59:71, user.col = 42)
  genre_name.Affinity7 <- attribute.affinity.matrix(coupon.cols = 59:71, user.col = 43)
  genre_name.Affinity8 <- attribute.affinity.matrix(coupon.cols = 59:71, user.col = 44)
  capsule_text.Affinity1 <- attribute.affinity.matrix(coupon.cols = 35:58, user.col = 45)
  capsule_text.Affinity2 <- attribute.affinity.matrix(coupon.cols = 35:58, user.col = 46)
  capsule_text.Affinity4 <- attribute.affinity.matrix(coupon.cols = 35:58, user.col = 47)
  capsule_text.Affinity6 <- attribute.affinity.matrix(coupon.cols = 35:58, user.col = 48)
  shop_large_area_name.Affinity1 <- attribute.affinity.matrix(coupon.cols = 72:80, user.col = 49)
  shop_large_area_name.Affinity2 <- attribute.affinity.matrix(coupon.cols = 72:80, user.col = 50)
  shop_large_area_name.Affinity3 <- attribute.affinity.matrix(coupon.cols = 72:80, user.col = 51)
  shop_large_area_name.Affinity4 <- attribute.affinity.matrix(coupon.cols = 72:80, user.col = 52)
  shop_large_area_name.Affinity5 <- attribute.affinity.matrix(coupon.cols = 72:80, user.col = 53)
  shop_large_area_name.Affinity6 <- attribute.affinity.matrix(coupon.cols = 72:80, user.col = 54)
  shop_large_area_name.Affinity7 <- attribute.affinity.matrix(coupon.cols = 72:80, user.col = 55)
  shop_large_area_name.Affinity8 <- attribute.affinity.matrix(coupon.cols = 72:80, user.col = 56)
  shop_pref_name.Affinity1 <- attribute.affinity.matrix(coupon.cols = 81:127, user.col = 57)
  shop_pref_name.Affinity2 <- attribute.affinity.matrix(coupon.cols = 81:127, user.col = 58)
  shop_pref_name.Affinity6 <- attribute.affinity.matrix(coupon.cols = 81:127, user.col = 59)
  shop_small_area_name.Affinity1 <- attribute.affinity.matrix(coupon.cols = 128:182, user.col = 60)
  shop_small_area_name.Affinity2 <- attribute.affinity.matrix(coupon.cols = 128:182, user.col = 61)
  shop_small_area_name.Affinity6 <- attribute.affinity.matrix(coupon.cols = 128:182, user.col = 62)
  
  usable_date_mon.Affinity1 <- attribute.affinity.matrix(coupon.cols = 183:186, user.col = 63)
  usable_date_mon.Affinity6 <- attribute.affinity.matrix(coupon.cols = 183:186, user.col = 64)
  usable_date_tue.Affinity1 <- attribute.affinity.matrix(coupon.cols = 187:190, user.col = 65)
  usable_date_tue.Affinity6 <- attribute.affinity.matrix(coupon.cols = 187:190, user.col = 66)
  usable_date_wed.Affinity1 <- attribute.affinity.matrix(coupon.cols = 191:194, user.col = 67)
  usable_date_wed.Affinity6 <- attribute.affinity.matrix(coupon.cols = 191:194, user.col = 68)
  usable_date_thu.Affinity1 <- attribute.affinity.matrix(coupon.cols = 195:198, user.col = 69)
  usable_date_thu.Affinity6 <- attribute.affinity.matrix(coupon.cols = 195:198, user.col = 70)
  usable_date_fri.Affinity1 <- attribute.affinity.matrix(coupon.cols = 199:202, user.col = 71)
  usable_date_fri.Affinity6 <- attribute.affinity.matrix(coupon.cols = 199:202, user.col = 72)
  usable_date_sat.Affinity1 <- attribute.affinity.matrix(coupon.cols = 203:206, user.col = 73)
  usable_date_sat.Affinity6 <- attribute.affinity.matrix(coupon.cols = 203:206, user.col = 74)
  usable_date_sun.Affinity1 <- attribute.affinity.matrix(coupon.cols = 207:210, user.col = 75)
  usable_date_sun.Affinity6 <- attribute.affinity.matrix(coupon.cols = 207:210, user.col = 76)
  usable_date_holiday.Affinity1 <- attribute.affinity.matrix(coupon.cols = 211:214, user.col = 77)
  usable_date_holiday.Affinity6 <- attribute.affinity.matrix(coupon.cols = 211:214, user.col = 78)
  usable_date_before_holiday.Affinity1 <- attribute.affinity.matrix(coupon.cols = 215:218, user.col = 79)
  usable_date_before_holiday.Affinity6 <- attribute.affinity.matrix(coupon.cols = 215:218, user.col = 80)
  
  listed_small_area_name.Affinity <- attribute.affinity.matrix(coupon.cols = 219:273, user.col = 81:135)
  listed_pref_name.Affinity <- attribute.affinity.matrix(coupon.cols = 274:320, user.col = 136:182)
  price_rate.Affinity <- attribute.affinity.matrix(coupon.cols = 321:333, user.col = 183:195)
  catalog_price.Affinity <- attribute.affinity.matrix(coupon.cols = 334:362, user.col = 196:224)
  discount_price.Affinity <- attribute.affinity.matrix(coupon.cols = 363:378, user.col = 225:240)
  discount.Affinity <- attribute.affinity.matrix(coupon.cols = 379:403, user.col = 241:265)
  disp_period.Affinity <- attribute.affinity.matrix(coupon.cols = 404:411, user.col = 266:273)
  valid_period.Affinity <- attribute.affinity.matrix(coupon.cols = 412:421, user.col = 274:283)
  immediate_use_time.Affinity <- attribute.affinity.matrix(coupon.cols = 422:433, user.col = 284:295)
  
  matrices <- list(genre_cat.Affinity1,
                   genre_cat.Affinity2,
                   genre_cat.Affinity3,
                   genre_cat.Affinity4,
                   genre_cat.Affinity5,
                   genre_cat.Affinity6,
                   genre_cat.Affinity7,
                   genre_cat.Affinity8,
                   genre_name.Affinity1,
                   genre_name.Affinity2,
                   genre_name.Affinity3,
                   genre_name.Affinity4,
                   genre_name.Affinity5,
                   genre_name.Affinity6,
                   genre_name.Affinity7,
                   genre_name.Affinity8,
                   capsule_text.Affinity1,
                   capsule_text.Affinity2,
                   capsule_text.Affinity4,
                   capsule_text.Affinity6,
                   shop_large_area_name.Affinity1,
                   shop_large_area_name.Affinity2,
                   shop_large_area_name.Affinity3,
                   shop_large_area_name.Affinity4,
                   shop_large_area_name.Affinity5,
                   shop_large_area_name.Affinity6,
                   shop_large_area_name.Affinity7,
                   shop_large_area_name.Affinity8,
                   shop_pref_name.Affinity1,
                   shop_pref_name.Affinity2,
                   shop_pref_name.Affinity6,
                   shop_small_area_name.Affinity1,
                   shop_small_area_name.Affinity2,
                   shop_small_area_name.Affinity6,
                   usable_date_mon.Affinity1,
                   usable_date_mon.Affinity6,
                   usable_date_tue.Affinity1,
                   usable_date_tue.Affinity6,
                   usable_date_wed.Affinity1,
                   usable_date_wed.Affinity6,
                   usable_date_thu.Affinity1,
                   usable_date_thu.Affinity6,
                   usable_date_fri.Affinity1,
                   usable_date_fri.Affinity6,
                   usable_date_sat.Affinity1,
                   usable_date_sat.Affinity6,
                   usable_date_sun.Affinity1,
                   usable_date_sun.Affinity6,
                   usable_date_holiday.Affinity1,
                   usable_date_holiday.Affinity6,
                   usable_date_before_holiday.Affinity1,
                   usable_date_before_holiday.Affinity6,
                   listed_small_area_name.Affinity,
                   listed_pref_name.Affinity,
                   price_rate.Affinity,
                   catalog_price.Affinity,
                   discount_price.Affinity,
                   discount.Affinity,
                   disp_period.Affinity,
                   valid_period.Affinity,
                   immediate_use_time.Affinity
  )
  
  names(matrices) <- c('genre_cat.Affinity1',
                       'genre_cat.Affinity2',
                       'genre_cat.Affinity3',
                       'genre_cat.Affinity4',
                       'genre_cat.Affinity5',
                       'genre_cat.Affinity6',
                       'genre_cat.Affinity7',
                       'genre_cat.Affinity8',
                       'genre_name.Affinity1',
                       'genre_name.Affinity2',
                       'genre_name.Affinity3',
                       'genre_name.Affinity4',
                       'genre_name.Affinity5',
                       'genre_name.Affinity6',
                       'genre_name.Affinity7',
                       'genre_name.Affinity8',
                       'capsule_text.Affinity1',
                       'capsule_text.Affinity2',
                       'capsule_text.Affinity4',
                       'capsule_text.Affinity6',
                       'shop_large_area_name.Affinity1',
                       'shop_large_area_name.Affinity2',
                       'shop_large_area_name.Affinity3',
                       'shop_large_area_name.Affinity4',
                       'shop_large_area_name.Affinity5',
                       'shop_large_area_name.Affinity6',
                       'shop_large_area_name.Affinity7',
                       'shop_large_area_name.Affinity8',
                       'shop_pref_name.Affinity1',
                       'shop_pref_name.Affinity2',
                       'shop_pref_name.Affinity6',
                       'shop_small_area_name.Affinity1',
                       'shop_small_area_name.Affinity2',
                       'shop_small_area_name.Affinity6',
                       'usable_date_mon.Affinity1',
                       'usable_date_mon.Affinity6',
                       'usable_date_tue.Affinity1',
                       'usable_date_tue.Affinity6',
                       'usable_date_wed.Affinity1',
                       'usable_date_wed.Affinity6',
                       'usable_date_thu.Affinity1',
                       'usable_date_thu.Affinity6',
                       'usable_date_fri.Affinity1',
                       'usable_date_fri.Affinity6',
                       'usable_date_sat.Affinity1',
                       'usable_date_sat.Affinity6',
                       'usable_date_sun.Affinity1',
                       'usable_date_sun.Affinity6',
                       'usable_date_holiday.Affinity1',
                       'usable_date_holiday.Affinity6',
                       'usable_date_before_holiday.Affinity1',
                       'usable_date_before_holiday.Affinity6',
                       'listed_small_area_name.Affinity',
                       'listed_pref_name.Affinity',
                       'price_rate.Affinity',
                       'catalog_price.Affinity',
                       'discount_price.Affinity',
                       'discount.Affinity',
                       'disp_period.Affinity',
                       'valid_period.Affinity',
                       'immediate_use_time.Affinity')
  return(matrices)
}

#####################################################################################

#####################################################################################

featurize.user.coupon <- function(w){
  
  users = get.users(w=w)
  testcoupons = get.testcoupons(w=w)
  
  # load features.coupon
  features.coupon <- read.csv('features/features.coupon.csv')
  features.coupon$disp_from <- as.POSIXct(features.coupon$disp_from)
  features.coupon$disp_end <- as.POSIXct(features.coupon$disp_end)
  features.coupon$valid_from <- as.POSIXct(features.coupon$valid_from)
  features.coupon$valid_end <- as.POSIXct(features.coupon$valid_end)
  rownames(features.coupon) <- features.coupon$coupon_id
  features.coupon <- features.coupon[testcoupons, ]
  for(i in 14:22){features.coupon[,i] <- as.factor(features.coupon[,i])}
  
  # load features.user.global
  features.user.global <- read.csv('features/features.user.global.csv')
  features.user.global$reg_date <- as.POSIXct(features.user.global$reg_date)
  features.user.global$withdraw_date <- as.POSIXct(features.user.global$withdraw_date)
  
  # load features.user.weekdependent
  features.user.weekdependent <- data.frame(fread(paste0('features/features.user.w',w,'.csv')))
  features.user.weekdependent$is_withdrawn_beforeweekstart <- as.numeric(features.user.weekdependent$is_withdrawn_beforeweekstart) 
  features.user.weekdependent$is_registered_afterweekstart <- as.numeric(features.user.weekdependent$is_registered_afterweekstart)
  rownames(features.user.weekdependent) <- features.user.weekdependent$user_id
  features.user.weekdependent <- features.user.weekdependent[users, ]
  
  # split users
  users.list <- split(users, ceiling(seq_along(users)/1500))
  
  # affinity matrices
  aff <- AffinityMatrices.user.coupon(w=w)
  
  # create features
  for (i in 1:length(users.list)){
    
    nusers <- users.list[[i]]
    user_id <- rep(nusers, rep(length(testcoupons), length(nusers)))
    coupon_id <- rep(testcoupons, length(nusers))
    feat <- data.frame(cbind('user_id'=user_id, 'coupon_id'=coupon_id))
    
    # ... selected coupon features
    feat <- join(feat, features.coupon[,c(1, 5:7,10,13,26,27,29:34,59:127)], by='coupon_id')
    
    # ... selected user.global features
    feat <- join(feat, features.user.global[,c(1:3,5:9,11:17,19:65)], by='user_id')
    
    # ... selected user weekdependent features
    feat <- join(feat, features.user.weekdependent[, c(1:28)], by='user_id')
    
    # ... affintiy features
    tmp <- data.frame(matrix(ncol = length(aff), nrow=nrow(feat)))
    for (j in 1:length(aff)){
      tmp[,j] <- as.vector(t(aff[[j]][nusers, ]))
    }
    colnames(tmp) <- names(aff)
    feat <- cbind(feat, tmp)
    
    # ... other features
    feat$user.coupon.immediate_use_time.affinity <- feat$immediate_use_time_mean - feat$immediate_use_time
    feat$user.coupon.price_rate.affinity <- feat$price_rate - (feat$price_rate_mean - feat$price_rate_sd)
    feat$user.coupon.catalog_price.affinity <- feat$catalog_price_mean + feat$catalog_price_sd - feat$catalog_price
    feat$user.coupon.discount_price.affinity <- feat$discount_price_mean + feat$discount_price_sd - feat$discount_price
    feat$user.coupon.discount.affinity <- feat$discount - (feat$discount_mean - feat$discount_sd)
    feat$user.coupon.disp_period.affinity <- feat$discount_mean - feat$disp_period
    feat$user.coupon.valid_period.affinity <- feat$valid_period_mean - feat$valid_period
    
    # ... write file
    write.table(feat, paste0('features/features.user.coupon.w',w,'.part',i,'.csv'), row.names = F, col.names = T, sep = ',')
  }
  
}

#####################################################################################
featurize.user.coupon.random <- function(w, ks=13, seeds=13){
  
  # load all user.coupon feat
  feat <- list()
  for (i in 1:16){
    feat[[i]] <- fread(paste0('features/features.user.coupon.w',w,'.part',i,'.csv'))
  }
  
  # select only user_id = viewers
  viewers <- get.viewers.anycoupons(w=w)
  for (i in 1:16){
    feat[[i]] <- feat[[i]][which(feat[[i]]$user_id %in% viewers), ]
  }
  
  # rbind feat
  feat <- do.call('rbind', feat)
  rownames(feat) <- paste0(feat$user_id, feat$coupon_id)
  
  # create random user.coupon pairs
  random.pairs <- function(k, seed){
    testcoupons <- get.testcoupons(w=w)
    # ... target = 1
    data1 = weblog[which(weblog$activity_date <= end_list[w] & weblog$activity_date >= start_list[w])]
    data1 <- data1[which(data1$coupon_id %in% testcoupons & data1$user_id %in% viewers)]
    data1 <- data1[,c('user_id', 'coupon_id'), with=F]
    data1 <- data1[!duplicated(data1)]
    data1$target <- rep(1,nrow(data1))
    # ... target = 0
    set.seed(seed); seeds <- sample(1:(10*length(viewers)), length(viewers))
    coupons <- list()
    for (i in 1:length(viewers)){ # k random coupons for each week viewer
      set.seed(seeds[i])                    
      coupons[[i]] <- sample(testcoupons, k)
    }
    coupons <- lapply(1:length(coupons), function(i){ # remove viewed coupons from above list
      setdiff(coupons[[i]], data1$coupon_id[which(data1$user_id == viewers[i])])
    })
    reps <- unlist(lapply(1:length(coupons), function(i){length(coupons[[i]])}))
    data0 <- data.frame(cbind('user_id' = rep(viewers, reps), 'coupon_id'=unlist(coupons)))
    data0$target <- rep(0,nrow(data0))
    data <- rbind(data0, data1)
    
    return(data)
  }
  
  # add features and write
  for (i in 1:length(seeds)){
    df = random.pairs(k=ks[i], seed=seeds[i])
    df <- join(df, feat, by=c('user_id', 'coupon_id'))
    write.table(df, paste0('features/features.user.coupon.random/features.user.coupon.w',w,'.k',ks[i],'.seed',seeds[i],'.csv'),
                row.names = F, col.names = T, sep = ',')
  }
}

#####################################################################################

featurize.user.coupon (w=1)
featurize.user.coupon (w=2)

#####################################################################################
dir.create('features/features.user.coupon.random')
featurize.user.coupon.random (w = 2, ks = c(13,13,13,14,13,12,13), seeds = c(42, 54, 13, 54, 53, 2, 597))


##################################################################
########################### THE END ###########################
##################################################################

