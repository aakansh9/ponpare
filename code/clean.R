#########################################
############# load data ################
#########################################
coupon_area_test <- read.csv('raw_data/coupon_area_test.csv')
coupon_area_train <- read.csv('raw_data/coupon_area_train.csv')
coupon_detail_train <- read.csv('raw_data/coupon_detail_train.csv')
coupon_list_test <- read.csv('raw_data/coupon_list_test.csv')
coupon_list_train <- read.csv('raw_data/coupon_list_train.csv')
coupon_visit_train <- read.csv('raw_data/coupon_visit_train.csv')
prefacture_locations <- read.csv('raw_data/prefecture_locations.csv')
user_list <- read.csv('raw_data/user_list.csv')


#########################################
#################### mkdir ###############
#########################################
dir.create('data')
dir.create('data/keys')


#########################################
######## coupon_list_train/test #########
#########################################
colnames(coupon_list_train) <- c('capsule_text', 'genre_name', 'price_rate',
                                 'catalog_price', 'discount_price', 'disp_from',
                                 'disp_end', 'disp_period', 'valid_from', 'valid_end',
                                 'valid_period', 'usable_date_mon', 'usable_date_tue',
                                 'usable_date_wed', 'usable_date_thu', 'usable_date_fri',
                                 'usable_date_sat', 'usable_date_sun', 'usable_date_holiday',
                                 'usable_date_before_holiday', 'large_area_name', 'pref_name',
                                 'small_area_name', 'coupon_id_hash')
colnames(coupon_list_test) <- colnames(coupon_list_train)

# ... capsule text ...
capsule_text_jp <- union(coupon_list_train$capsule_text, coupon_list_test$capsule_text)
capsule_text_en <- c('gourmet', 'hair_salon', 'spa', 'relaxation', 
                     'beauty', 'nail_eye_salon', 'home_delivery_service', 'lesson', 
                     'gift_card', 'other', 'leisure', 'hotel', 'japanese_hotel', 
                     'vacation_rental', 'lodge', 'resort_inn', 'guesthouse', 
                     'japanese_guesthouse', 'public_hotel', 'beauty', 'event', 
                     'web_service', 'health_and_medical_care', 'class', 
                     'correspondence_course')
tmp <- data.frame(cbind('capsule_text_jp'=capsule_text_jp, 'capsule_text_en'=capsule_text_en))
rownames(tmp) <- tmp$capsule_text_jp
write.csv(tmp, 'data/keys/capsule_text.key')
coupon_list_train$capsule_text <- tmp[as.character(coupon_list_train$capsule_text), 2]
coupon_list_test$capsule_text <- tmp[as.character(coupon_list_test$capsule_text), 2]

# ... genre name ...
genre_name_jp <- union(coupon_list_train$genre_name, coupon_list_test$genre_name)
genre_name_en <- c('gourmet', 'hair_salon', 'spa', 'relaxation', 'beauty',
                   'nail_eye_salon', 'home_delivery_service', 'lesson',
                   'gift_card', 'other_coupon', 'leisure', 'hotels', 
                   'health_and_medical_care')
tmp <- data.frame(cbind('genre_name_jp'=genre_name_jp, 'genre_name_en'=genre_name_en))
rownames(tmp) <- tmp$genre_name_jp
write.csv(tmp, 'data/keys/genre_name.key')
coupon_list_train$genre_name <- tmp[as.character(coupon_list_train$genre_name), 2]
coupon_list_test$genre_name <- tmp[as.character(coupon_list_test$genre_name), 2]

# ... disp from/end...
coupon_list_train$disp_from <- as.POSIXct(coupon_list_train$disp_from)
coupon_list_train$disp_end <- as.POSIXct(coupon_list_train$disp_end)
coupon_list_test$disp_from <- as.POSIXct(coupon_list_test$disp_from)
coupon_list_test$disp_end <- as.POSIXct(coupon_list_test$disp_end)

# ... valid from/end ...
coupon_list_train$valid_from <- as.POSIXct(coupon_list_train$valid_from)
coupon_list_train$valid_end <- as.POSIXct(coupon_list_train$valid_end)
coupon_list_test$valid_from <- as.POSIXct(coupon_list_test$valid_from)
coupon_list_test$valid_end <- as.POSIXct(coupon_list_test$valid_end)

# ... large area name ...
large_area_jp <- union(levels(coupon_list_train$large_area_name), levels(coupon_list_test$large_area_name))
large_area_en <- c('chugoku', 'kyushu_okinawa', 'hokushinetsu',
                   'hokkaido', 'shikoku', 'tohoku', 'tokai',
                   'kanto', 'kansai')
tmp <- data.frame(cbind('large_area_jp'=large_area_jp, 'large_area_en'=large_area_en))
rownames(tmp) <- tmp$large_area_jp
write.csv(tmp, 'data/keys/large_area.key')
coupon_list_train$large_area_name <- tmp[as.character(coupon_list_train$large_area_name), 2]
coupon_list_test$large_area_name <- tmp[as.character(coupon_list_test$large_area_name), 2]

# ... pref name ...
pref_name_jp <- unique(c(levels(coupon_list_train$pref_name), 
                         levels(coupon_list_test$pref_name),
                         levels(coupon_area_train$PREF_NAME),
                         levels(coupon_area_test$PREF_NAME),
                         levels(user_list$PREF_NAME)))
pref_name_jp <- pref_name_jp[1:47]
pref_name_en <- c('mie', 'kyoto', 'saga', 'hyogo', 'hokkaido',
                  'chiba', 'wakayama', 'saitama', 'oita', 'osaka',
                  'nara', 'miyagi', 'miyazaki', 'toyama', 'yamaguchi',
                  'yamagata', 'yamanashi', 'gifu', 'okayama', 'iwate' ,
                  'shimane', 'hiroshima', 'tokushima', 'ehime', 'aichi', 
                  'niigata', 'tokyo', 'tochigi', 'okinawa', 'shiga', 
                  'kumamoto', 'ishikawa', 'kanagawa', 'fukui', 'fukuoka', 
                  'fukushima', 'akita', 'gunma', 'ibaraki', 'nagasaki', 'nagano', 
                  'aomori', 'shizuoka', 'kagawa', 'kochi', 'tottori', 'kagoshima')
tmp <- data.frame(cbind('pref_name_jp'=pref_name_jp, 'pref_name_en'=pref_name_en))
rownames(tmp) <- tmp$pref_name_jp
write.csv(tmp, 'data/keys/pref_name.key')
coupon_list_train$pref_name <- tmp[as.character(coupon_list_train$pref_name), 2]
coupon_list_test$pref_name <- tmp[as.character(coupon_list_test$pref_name), 2]
coupon_area_train$PREF_NAME <- tmp[as.character(coupon_area_train$PREF_NAME), 2]
coupon_area_test$PREF_NAME <- tmp[as.character(coupon_area_test$PREF_NAME), 2]
user_list$PREF_NAME <- tmp[as.character(user_list$PREF_NAME), 2]

# ... small area name ...
small_area_jp <- unique(c(levels(coupon_list_train$small_area_name),
                          levels(coupon_list_test$small_area_name),
                          levels(coupon_area_train$SMALL_AREA_NAME),
                          levels(coupon_area_test$SMALL_AREA_NAME),
                          levels(coupon_detail_train$SMALL_AREA_NAME)))
small_area_en <- c('Kita', 'Mie', 'Kyoto', 'Minami-other', 'Saga', 'Hyogo', 
                   'Hokkaido', 'Chiba', 'Wakayama', 'Saitama', 'Oita', 'Nara', 
                   'Miyagi', 'Miyazaki', 'Toyama', 'Yamaguchi', 'Yamagata', 
                   'Yamanashi', 'Gifu', 'Okayama', 'Iwate', 'Shimane', 'Kawasaki-Shonan-Hakone-other', 
                   'Hiroshima', 'Tokushima', 'Ebisu-Meguro-Shinagawa', 'Ehime', 'Aichi', 
                   'Shinjuku-Takadanobaba-Nakano-Kichijoji', 'Niigata', 'Tochigi', 'Yokohama', 
                   'Ikebukuro-Kagurazaka-Akabane',
                   'Okinawa', 'Shibuya-Aoyama-Jiyugaoka', 'Shiga', 'Kumamoto', 'Ishikawa' , 
                   'Fukui', 'Fukuoka', 'Fukushima', 'Akita', 'Tachikawa-Machida-Hachioji-other',
                   'Gunma', 'Ibaraki', 'Akasaka-Roppongi-Azabu', 'Ginza-Shinbashi-Tokyo-Ueno', 
                   'Nagasaki', 'Nagano', 'Aomori', 'Shizuoka', 'Kagawa', 'Kochi', 
                   'Tottori', 'Kagoshima')
tmp <- data.frame(cbind('small_area_jp'=small_area_jp, 'small_area_en'=small_area_en))
rownames(tmp) <- tmp$small_area_jp
write.csv(tmp, 'data/keys/small_area.key')
coupon_list_train$small_area_name <- tmp[as.character(coupon_list_train$small_area_name), 2]
coupon_list_test$small_area_name <- tmp[as.character(coupon_list_test$small_area_name), 2]
coupon_area_train$SMALL_AREA_NAME <- tmp[as.character(coupon_area_train$SMALL_AREA_NAME), 2]
coupon_area_test$SMALL_AREA_NAME <- tmp[as.character(coupon_area_test$SMALL_AREA_NAME), 2]
coupon_detail_train$SMALL_AREA_NAME <- tmp[as.character(coupon_detail_train$SMALL_AREA_NAME), 2]

#########################################
############# id hashes ################
#########################################
# ... coupon id hash ...
A <- levels(coupon_list_train$coupon_id_hash)
X <- levels(coupon_list_test$coupon_id_hash)
B1 <- levels(coupon_area_train$COUPON_ID_hash)
B2 <- levels(coupon_detail_train$COUPON_ID_hash)
C <- levels(coupon_visit_train$VIEW_COUPON_ID_hash)
coupon_hash <- unique(c(A,X,B1,B2,C))
tmp <- data.frame(cbind('coupon_hash'=coupon_hash, 'coupon_id'= paste0('c', 1:length(coupon_hash))))
rownames(tmp) <- tmp$coupon_hash
write.csv(tmp, 'data/keys/coupon_id_hash.key')
coupon_list_train$coupon_id_hash <- tmp[as.character(coupon_list_train$coupon_id_hash), 2]
coupon_list_test$coupon_id_hash <- tmp[as.character(coupon_list_test$coupon_id_hash), 2]
coupon_area_train$COUPON_ID_hash <- tmp[as.character(coupon_area_train$COUPON_ID_hash), 2]
coupon_area_test$COUPON_ID_hash <- tmp[as.character(coupon_area_test$COUPON_ID_hash), 2]
coupon_detail_train$COUPON_ID_hash <- tmp[as.character(coupon_detail_train$COUPON_ID_hash), 2]
coupon_visit_train$VIEW_COUPON_ID_hash <- tmp[as.character(coupon_visit_train$VIEW_COUPON_ID_hash), 2]

# ... user id hash ...
R <- levels(coupon_detail_train$USER_ID_hash)
Q <- levels(coupon_visit_train$USER_ID_hash)
P <- levels(user_list$USER_ID_hash)
user_hash <- unique(c(P,Q,R))
tmp <- data.frame(cbind('user_hash'=user_hash, 'user_id'= paste0('u', 1:length(user_hash))))
rownames(tmp) <- tmp$user_hash
write.csv(tmp, 'data/keys/user_id_hash.key')
user_list$USER_ID_hash <- tmp[as.character(user_list$USER_ID_hash), 2]
coupon_visit_train$USER_ID_hash <- tmp[as.character(coupon_visit_train$USER_ID_hash), 2]
coupon_detail_train$USER_ID_hash <- tmp[as.character(coupon_detail_train$USER_ID_hash), 2]

# ... referrer id hash ...
referrer_hash <- levels(coupon_visit_train$REFERRER_hash)
intersect(referrer_hash, user_hash) # NULL
tmp <- data.frame(cbind('referrer_hash'=referrer_hash, 'referrer_id'=paste0('r', 1:length(referrer_hash))))
rownames(tmp) <- tmp$referrer_hash
write.csv(tmp, 'data/keys/referrer_hash.key')
coupon_visit_train$REFERRER_hash <- tmp[as.character(coupon_visit_train$REFERRER_hash), 2]

# ... purchase id hash ...
T1 <- levels(coupon_visit_train$PURCHASEID_hash)
T2 <- levels(coupon_detail_train$PURCHASEID_hash)
purchase_hash <- unique(c(T1,T2))[-1] # remove  " "
tmp <- data.frame(cbind('purchase_hash'=purchase_hash, 'purchase_id'=paste0('p', 1:length(purchase_hash))))
rownames(tmp) <- tmp$purchase_hash
write.csv(tmp, 'data/keys/purchase_hash.key')
coupon_visit_train$PURCHASEID_hash <- tmp[as.character(coupon_visit_train$PURCHASEID_hash), 2]
coupon_detail_train$PURCHASEID_hash <- tmp[as.character(coupon_detail_train$PURCHASEID_hash), 2]

# ... session id hash ...
tmp <- data.frame(cbind('session_hash'=levels(coupon_visit_train$SESSION_ID_hash), 'sessiosn_id'=paste0('s', 1:length(levels(coupon_visit_train$SESSION_ID_hash)))))
rownames(tmp) <- tmp$session_hash
write.csv(tmp, 'data/keys/session_hash.key')
coupon_visit_train$SESSION_ID_hash <- tmp[as.character(coupon_visit_train$SESSION_ID_hash), 2]

#########################################
############# user_list ################
#########################################
colnames(user_list) <- c('reg_date', 'gender', 'age', 'withdraw_date',
                         'user_reg_pref_name', 'user_id')
user_list$reg_date <- as.POSIXct(user_list$reg_date )
levels(user_list$gender) <- c(0,1)
user_list$withdraw_date <- as.POSIXct(user_list$withdraw_date)

write.table(user_list, 'data/userlist.csv', sep=',', row.names = F, col.names = T)

rm(A, B1, B2, C, P, pref_name_en, pref_name_jp, purchase_hash, large_area_en,
   large_area_jp, genre_name_en, genre_name_jp, capsule_text_en, capsule_text_jp,
   Q, R, referrer_hash, small_area_en, small_area_jp, T1, T2, user_hash, X, coupon_hash, tmp)



#########################################
### coupon_visit_train & detail_train ###
#########################################
# ... coupon visit train ...
colnames(coupon_visit_train) <- c('purchase_flag', 'view_date', 'page_serial',
                                  'referrer_id', 'view_coupon_id', 'user_id',
                                  'session_id', 'purchase_id')
coupon_visit_train$view_date <- as.POSIXct(coupon_visit_train$view_date)
coupon_visit_train$referrer_id <- as.character(coupon_visit_train$referrer_id)
coupon_visit_train$view_coupon_id <- as.character(coupon_visit_train$view_coupon_id)
coupon_visit_train$user_id <- as.character(coupon_visit_train$user_id)
coupon_visit_train$session_id <- as.character(coupon_visit_train$session_id)
coupon_visit_train$purchase_id <- as.character(coupon_visit_train$purchase_id)

# ... coupon detail train ...
colnames(coupon_detail_train) <- c('item_count', 'purchase_date', 'user_res_small_area_name',
                                   'purchase_id', 'user_id', 'purchase_coupon_id')

coupon_detail_train$purchase_date <- as.POSIXct(coupon_detail_train$purchase_date)
coupon_detail_train$purchase_coupon_id <- as.character(coupon_detail_train$purchase_coupon_id)
coupon_detail_train$user_id <- as.character(coupon_detail_train$user_id)
coupon_detail_train$purchase_id <- as.character(coupon_detail_train$purchase_id)
coupon_detail_train$user_res_small_area_name <- as.character(coupon_detail_train$user_res_small_area_name)


# ... prepare master weblog ...
weblog <- join(coupon_visit_train, coupon_detail_train, type='full', by=c('user_id', 'purchase_id'))

str(weblog)

# ... purchase flag ...
weblog$purchase_flag[-which(is.na(weblog$purchase_id))] <- 1
which(is.na(weblog$purchase_flag))

# ... view_date ...
which(is.na(weblog$purchase_date[which(is.na(weblog$view_date))])) #NONE

# ... activity_date = view OR purchase ...
weblog$activity_date <- weblog$view_date
weblog$activity_date[which(is.na(weblog$activity_date))] <- weblog$purchase_date[which(is.na(weblog$view_date))]

# ... purchase_id ...
tmp <- count(weblog$purchase_id)[which(count(weblog$purchase_id)[,2] > 1), 1] #purchase id's which occur more than 1 times
tmp <- as.character(tmp)[-length(tmp)] # 6647 purchase id's
tmp <- data.table(weblog[which(weblog$purchase_id %in% tmp), ])
  # for each such purchase id are purchase_coupon_id, user_id, user_res_small_area_name, purchase_date,
  # item_count, session_id, referrer_id same?
all(tmp[,length(unique(purchase_coupon_id)),by='purchase_id']$V1 == rep(1,6647)) #TRUE
all(tmp[,length(unique(user_id)),by='purchase_id']$V1 == rep(1,6647)) #TRUE
all(tmp[,length(unique(user_res_small_area_name)),by='purchase_id']$V1 == rep(1,6647)) #TRUE
all(tmp[,length(unique(purchase_date)),by='purchase_id']$V1 == rep(1,6647)) #TRUE
all(tmp[,length(unique(item_count)),by='purchase_id']$V1 == rep(1,6647)) #TRUE
all(tmp[,length(unique(session_id)),by='purchase_id']$V1 == rep(1,6647)) #FALSE. Maybe these are cases when user was charged twice or more by mistake! OR modifying the purchase order.
all(tmp[,length(unique(referrer_id)),by='purchase_id']$V1 == rep(1,6647)) #FALSE just 2 cases! (p113772, p31203), the second one has super wierd distinct view dates!!
as.integer(sort(tmp[,max(diff(as.numeric(view_date))),by='purchase_id']$V1)/(60)) # only very few are big values!
  # so remove duplicated purchase_id values
tmp <- count(weblog$purchase_id)[which(count(weblog$purchase_id)[,2] > 1), 1] #purchase id's which occur more than 1 times
tmp <- as.character(tmp)[-length(tmp)] # 6647 purchase id's
tmp <- weblog[which(weblog$purchase_id %in% tmp), c('purchase_id', 'user_id')]
tmp2 <- tmp[duplicated(tmp), ]
weblog <- weblog[setdiff(rownames(weblog), rownames(tmp2)), ]
rm(tmp, tmp2)

# ... item_count ...
which(!is.na(weblog$purchase_id[which(is.na(weblog$item_count))])) # NONE
weblog$item_count[which(is.na(weblog$item_count))] <- 0

# ... purchase_date ...
which(!is.na(weblog$purchase_id[which(is.na(weblog$purchase_date))])) # NONE

# ... user_res_small_area_name ...
which(!is.na(weblog$purchase_id[which(is.na(weblog$user_res_small_area_name))])) # NONE i.e. all purchase ids have res addresses

# ... clean purchase and view coupon id ...
a=weblog$view_coupon_id[-c(which(is.na(weblog$purchase_coupon_id)), which(is.na(weblog$view_coupon_id)))]
b=weblog$purchase_coupon_id[-c(which(is.na(weblog$purchase_coupon_id)), which(is.na(weblog$view_coupon_id)))]
all(a==b) # T
rm(a,b)
pasteMY <- function(x,y){
  if(is.na(x)==T){ 
    return(y)
  } else if (is.na(y)==T){
    return(x)
  } else {
    return(x)
  }
}
weblog$coupon_id <- unlist(lapply(1:nrow(weblog), function(i){
  return(pasteMY(weblog$view_coupon_id[i], weblog$purchase_coupon_id[i]))
}))
which(is.na(weblog$coupon_id))

weblog$view_coupon_id <- NULL
weblog$purchase_coupon_id <- NULL

# ... save ...
write.table(weblog, 'data/weblog.csv', row.names = F, col.names = T, sep = ',')


#########################################
########### coupon_list #################
#########################################
colnames(coupon_list_train)[24] <- 'coupon_id'
coupon_list_train$dataset <- rep(1, nrow(coupon_list_train))
colnames(coupon_list_test)[24] <- 'coupon_id'
coupon_list_test$dataset <- rep(-1, nrow(coupon_list_test))
couponlist <- data.frame(rbind(coupon_list_train, coupon_list_test))
colnames(couponlist)[21:23] <- c('shop_large_area_name', 'shop_pref_name', 'shop_small_area_name')

# ... valid_from and valid_end ...
couponlist[which(couponlist$valid_from - couponlist$disp_from < 0),] # basically none. 7 cases show up because valid_from is from 12:01 AM.
couponlist[which(couponlist$valid_period==0), ] # 44 coupons have valid_from and valid_end within 1 day.
all(which(is.na(couponlist$valid_from)) == which(is.na(couponlist$valid_end))) # TRUE so both are NA or none is NA. So such coupons can be used any time.
couponlist$valid_from[which(is.na(couponlist$valid_from))] <- couponlist$disp_from[which(is.na(couponlist$valid_from))]
couponlist$valid_period[which(is.na(couponlist$valid_period))] <- 365 # max was 179 days
couponlist$valid_end[which(is.na(couponlist$valid_end))] <- couponlist$valid_from[which(is.na(couponlist$valid_end))] + 365*24*60*60
couponlist$valid_period <- as.numeric(couponlist$valid_end - couponlist$valid_from)/(60*60*24)

# ... usable_date_xxx ... it is factor with 4 levels! NOT INT
for (i in 12:20){
  couponlist[,i][which(is.na(couponlist[,i]))] <- 3
}

write.table(couponlist, 'data/couponlist.csv', sep=',', row.names = F, col.names = T)

#########################################
########### coupon_area #################
#########################################
colnames(coupon_area_train) <- c('listed_small_area_name', 'listed_pref_name', 'coupon_id')
coupon_area_train$dataset <- rep(1, nrow(coupon_area_train))
colnames(coupon_area_test) <- c('listed_small_area_name', 'listed_pref_name', 'coupon_id')
coupon_area_test$dataset <- rep(-1, nrow(coupon_area_test))
couponarea <- data.frame(rbind(coupon_area_train, coupon_area_test))
couponarea <- couponarea[with(couponarea, order(coupon_id, listed_pref_name, listed_small_area_name)), ]
write.table(couponarea, 'data/couponarea.csv', sep=',', row.names = F, col.names = T)


#########################################
########### prefac locations ############
#########################################
colnames(prefacture_locations) <- c('pref_name', 'pref_office', 'lat', 'long')
tmp <- read.csv('data/keys/pref_name.key', row.names = 1)
prefacture_locations$pref_name <- tmp[as.character(prefacture_locations$pref_name),2]
write.table(prefacture_locations, 'data/pref.coords', sep=',', row.names = F, col.names = T)

rm(pasteMY, weblog, user_list, tmp, prefacture_locations, coupon_visit_train,
   coupon_detail_train)





