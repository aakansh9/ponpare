# ... get all users(w) ...
get.users <- function(w){
  users <- userlist$user_id[which(userlist$reg_date <= end_list[w])]
  return(users)
}

# ... get weblog(w) ...
get.weblog <- function(w){

  users = get.users (w=w)

  weblog <- weblog[which(weblog$activity_date < start_list[w])]
  weblog <- weblog[which(weblog$user_id %in% users)]
  
  # add left out users to weblog (they register in week w OR registered before and never had weblog)
  tmp <- data.frame(matrix(ncol = ncol(weblog), nrow = length(setdiff(users, unique(weblog$user_id))))); colnames(tmp) <- colnames(weblog)
  tmp$purchase_flag <- rep(0, nrow(tmp))
  tmp$item_count <- rep(0, nrow(tmp))
  tmp$user_id <- setdiff(users, unique(weblog$user_id))
  tmp$activity_date <- userlist$reg_date[which(userlist$user_id %in% tmp$user_id)]
  for (i in c(4:7,10,12)){
    tmp[,i] <- as.character(tmp[,i])
  }
  tmp$page_serial <- as.integer(tmp$page_serial)
  tmp$view_date <- as.POSIXct(tmp$view_date)
  tmp$purchase_date <- as.POSIXct(tmp$purchase_date)
  weblog <- rbind(weblog, tmp)
  
  return(weblog)
}

# ... get traincoupons(w) ...
get.traincoupons <- function(w){
  traincoupons <- as.character(couponlist$coupon_id[which(couponlist$disp_from < start_list[w])])
  return(traincoupons)
}

# ... get testcoupons(w) ...
get.testcoupons <- function(w){
  testcoupons <- as.character(couponlist$coupon_id[which(couponlist$disp_from >= start_list[w] & couponlist$disp_from <= end_list[w])])
  return(testcoupons)
}

# ... get users(w) with no weblog ...
get.users.nolog <- function(w){
  users = get.users (w=w)
  weblog <- weblog[which(weblog$activity_date < start_list[w])]
  weblog <- weblog[which(weblog$user_id %in% users)]
  return(setdiff(users, unique(weblog$user_id)))
}

# ... get users(w) with no train/testcoupon weblog ...
get.users.noweekcouponlog <- function(w){
  users = get.users (w=w)
  traincoupons = get.traincoupons(w=w)
  testcoupons = get.testcoupons(w=w)
  weblog=get.weblog(w=w)
  return(setdiff(users, weblog[which(weblog$coupon_id %in% union(traincoupons, testcoupons)), ]$user_id))
}

# ... get viewers(w) of any week coupons ...
get.viewers.anycoupons <- function(w){
  users = get.users(w=w)
  viewers = unique(weblog$user_id[which(weblog$activity_date >= start_list[w] & weblog$activity_date <= end_list[w])])
  return(intersect(viewers, users))
}

# ... get viewers(w) of test coupons ...
get.viewers.testcoupons <- function(w){
  users = get.users(w=w)
  testcoupons = get.testcoupons(w=w)
  viewers = unique(weblog$user_id[which(weblog$activity_date >= start_list[w] & 
                                          weblog$activity_date <= end_list[w] & 
                                          weblog$coupon_id %in% testcoupons)])
  return(intersect(viewers, users))
}

# ... get purchasers(w) of week test coupons ...
get.purchasers.testcoupons <- function(w){
  users = get.users(w=w)
  testcoupons = get.testcoupons(w=w)
  purchasers = unique(weblog$user_id[which(weblog$activity_date >= start_list[w] & weblog$activity_date <= end_list[w] & (weblog$coupon_id %in% testcoupons)
                                           & weblog$purchase_flag == 1)])
  return(intersect(purchasers, users))
}

# ... get purchasers(w) of any week coupons ...
get.purchasers.anycoupons <- function(w){
  users = get.users(w=w)
  testcoupons = get.testcoupons(w=w)
  traincoupons = get.traincoupons(w=w)
  purchasers = unique(weblog$user_id[which(weblog$activity_date >= start_list[w] & weblog$activity_date <= end_list[w] & (weblog$coupon_id %in% c(testcoupons, traincoupons))
                                           & weblog$purchase_flag == 1)])
  return(intersect(purchasers, users))
}

# ... get view matrix(w) ...
get.viewmatrix <- function(w){

  users <- get.users(w=w)
  viewers <- get.viewers.testcoupons(w=w)
  testcoupons <- get.testcoupons(w=w)
  
  weblog <- weblog[which(weblog$coupon_id %in% testcoupons & weblog$user_id %in% viewers),]
  weblog <- weblog[,c('user_id', 'coupon_id'), with=F]
  weblog <- weblog[!duplicated(weblog)]
  
  viewmatrix = data.frame(matrix(0, ncol = length(testcoupons), nrow = length(users)))
  colnames(viewmatrix) <- testcoupons
  rownames(viewmatrix) <- users
  for (i in 1:nrow(weblog)){
    viewmatrix[as.character(weblog$user_id[i]), as.character(weblog$coupon_id[i])] <- 1
  }
  return(viewmatrix)
}

# ... get purchase matrix(w) ...

# ... get view truth ...
get.viewtruth <- function(w){
  users <- get.users(w=w)
  testcoupons <- get.testcoupons(w=w)
  week.weblog = weblog[which(weblog$activity_date <= end_list[w] & weblog$activity_date >= start_list[w])]
  week.weblog <- week.weblog[which(week.weblog$coupon_id %in% testcoupons & week.weblog$user_id %in% users)]
  week.weblog <- week.weblog[,c('user_id', 'coupon_id'), with=F]
  week.weblog <- week.weblog[!duplicated(week.weblog)]
  truth <- split(week.weblog$coupon_id, week.weblog$user_id)
  return(truth)
}

# ... get purchase truth ...
get.purchasetruth <- function(w){
  users <- get.users(w=w)
  testcoupons <- get.testcoupons(w=w)
  week.weblog = weblog[which(weblog$activity_date <= end_list[w] & weblog$activity_date >= start_list[w])]
  week.weblog <- week.weblog[which(week.weblog$coupon_id %in% testcoupons & week.weblog$user_id %in% users)]
  week.weblog <- week.weblog[which(week.weblog$purchase_flag ==1)]
  week.weblog <- week.weblog[,c('user_id', 'coupon_id'), with=F]
  week.weblog <- week.weblog[!duplicated(week.weblog)]
  truth <- split(week.weblog$coupon_id, week.weblog$user_id)
  return(truth)
}

# ... get map@k ...
get.map <- function(w, truth, pred, k){
  pred = pred[names(truth)]
  users = get.users(w=w)
  map = mapk(k=k, actual = truth, predicted = pred)*length(pred)/length(users)
  return(map)
}

# ... get sub from pred ...
get.submission <- function(pred){
  
  for (i in 1:length(pred)){
    pred[[i]] <- pred[[i]][1:10]
  }
  
  pred <- data.frame(cbind('user_id'=names(pred), data.table(do.call('rbind', pred))))
  for (i in 2:11){
    pred[,i] <- as.character(coupon_id_hash.key[pred[,i], 1])
  }
  pred$user_id <- as.character(user_id_hash.key[pred$user_id, 1])
  pred <- data.frame(cbind('USER_ID_hash'=pred$user_id, 'PURCHASED_COUPONS'=paste0(pred$V1, ' ',pred$V2, ' ',pred$V3, ' ',pred$V4, ' ',pred$V5, ' ',
                                                                                   pred$V6, ' ',pred$V7, ' ',pred$V8, ' ',pred$V9, ' ',pred$V10)))
  return(pred)
}


