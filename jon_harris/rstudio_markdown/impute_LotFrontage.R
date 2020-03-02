load('train_all.RData')
original_data = read.csv2('train.csv', header=T, sep = ',')

impute_LotFrontage = original_data$LotFrontage
for (neigh in original_data$Neighborhood){
  idx = which(is.na(original_data$LotFrontage) & (original_data$Neighborhood==neigh))
  impute_LotFrontage[idx] =  mean(original_data$LotFrontage[original_data$Neighborhood==neigh], na.rm=T)
}

hp_feat$LotFrontage = impute_LotFrontage
hp_orig$LotFrontage = impute_LotFrontage

save(hp_feat, hp_orig, file = "train_all_v2.RData")






#IMPUTE TEST DATA: LOTFRONTAGE
load('test_all.RData') #load test data
original_test = read.csv2('test.csv', header=T, sep = ',')
impute_LotFrontage = original_test$LotFrontage
for (neigh in original_test$Neighborhood){
  idx = which(is.na(original_test$LotFrontage) & (original_test$Neighborhood==neigh))
  impute_LotFrontage[idx] =  mean(original_test$LotFrontage[original_test$Neighborhood==neigh], na.rm=T)
}
hp_feat_test$LotFrontage = impute_LotFrontage
hp_orig_test$LotFrontage = impute_LotFrontage

#IMPUTE/FEAT_ENGINEER TEST DATA: TOTGARAGECARS
idx = which(is.na(original_test$GarageCars))
original_test$GarageCars[idx] = 0
idx = which(original_test$GarageCars >= 3)
original_test$GarageCars[idx] = 3
hp_feat_test$TotCarGarage = original_test$GarageCars

hp_orig_test$Exterior1st[692] = 11
hp_orig_test$Exterior2nd[692] = 14

save(hp_feat_test, hp_orig_test, file = "test_all_v2.RData")


