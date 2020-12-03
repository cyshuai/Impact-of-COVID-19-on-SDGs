library(randomForest)
library(xgboost)
library(Matrix)
library(e1071)
library(MLmetrics)
covid_sdg<-read.csv(file="code/COVID-SDG/sdg data final_o.csv")
covid_test_2<-read.csv(file="code/COVID-SDG/Test data withoutcovid.csv")
covid_test_3<-read.csv(file="code/COVID-SDG/Test data withcovid.csv")

covid_sdg<-read.csv(file="/Users/abcdefg/Desktop/COVID_SDG/sdg_data.csv")
covid_test_2<-read.csv(file="/Users/abcdefg/Desktop/COVID_SDG/Test data withoutcovid.csv")
covid_test_3<-read.csv(file="/Users/abcdefg/Desktop/COVID_SDG/Test data withcovid.csv")
model<-read.csv(file="/Users/abcdefg/Desktop/COVID_SDG/model_selection.csv")

#normalize data only for svm models 
colmean<-colMeans(covid_sdg[,4:45],na.rm = T)
colmean<-as.vector(colmean)
colsd<-vector()
for (i in 4:45) {
  sd_value<-sd(covid_sdg[,i],na.rm=T)
  colsd<-rbind(colsd,sd_value)
}
covid_sdg_svm<-cbind(covid_sdg[,1:3],scale(covid_sdg[,4:45],center=T,scale=T))

covid_test_2_svm<-vector()
for (i in 4:6) {
  normalied_value_2<-(covid_test_2[,i]-colmean[i-3])/colsd[i-3]
  covid_test_2_svm<-cbind(covid_test_2_svm,normalied_value_2)
}
covid_test_2_svm<-cbind(covid_test_2[,1:3],covid_test_2_svm)

covid_test_3_svm<-vector()
for (i in 4:6) {
  a<-(covid_test_3[,i]-colmean[i-3])/colsd[i-3]
  covid_test_3_svm<-cbind(covid_test_3_svm,a)
}
covid_test_3_svm<-cbind(covid_test_3[,1:3],covid_test_3_svm)


Totalsum_2_ad<-vector()
Totalsum_2_ec<-vector()
Totalsum_2<-vector()
Totalsum_3_ad<-vector()
Totalsum_3_ec<-vector()
Totalsum_3<-vector()
for (i in 1:5) {
  Pre_2<-vector()
  Pre_3<-vector()
  for (j in 7:8) {
    if (model[j-6,]==1) {
      ######train rf model and conduct prediction 
      train_ori<-na.omit(covid_sdg[,c(1,4:6,j)])
      set.seed(i)
      bootstrap_index<-sample(nrow(train_ori),size=nrow(train_ori),replace=TRUE)
      train<-train_ori[bootstrap_index,]
      set.seed(i)
      result<-rfcv(train[,1:4], train[,5],cv.fold=3, ntree=100)
      cv_var_rf<-result$n.var
      cv_error_rf<-result$error.cv
      names(cv_error_rf)<-NULL
      var_num<-cv_var_rf[which.min(cv_error_rf)]
      set.seed(i)
      X=train[,1:4]
      Y=y=train[,5]
      forest<-randomForest(x=X, y=Y,mtry=var_num,ntree=100)
      set.seed(i)
      pre_2<- predict(forest, newdata=covid_test_2[,c(1,4:6)])
      Pre_2<-cbind(Pre_2,pre_2)
      pre_3<- predict(forest, newdata=covid_test_3[,c(1,4:6)])
      Pre_3<-cbind(Pre_3,pre_3)
    } 
    else if (model[j-6,]==2) {
      ######train egb model and conduct prediction 
      train_ori<-na.omit(covid_sdg[,c(1,4:6,j)])
      set.seed(i)
      bootstrap_index<-sample(nrow(train_ori),size=nrow(train_ori),replace=TRUE)
      train<-train_ori[bootstrap_index,]
      y <- train[,5]
      x <- as.matrix(train[,1:4])
      traindata1 <- data.matrix(x)  
      traindata2 <- Matrix(traindata1,sparse=T) 
      traindata3 <- y
      traindata4 <- list(data=traindata2,label=traindata3)
      dtrain <- xgb.DMatrix(data = traindata4$data, label = traindata4$label)
      #choose best eta
      set.seed(i)
      Mean_mse<-vector()
      for (k in c(0.01, 0.05, 0.15, 0.3)) {
        cv <- xgb.cv(data = dtrain, nfold = 3, metrics = list("rmse"), nround=300,
                     max_depth = 6, eta = k, objective = "reg:squarederror",verbose = F)
        mean_mse<-min(cv$evaluation_log$test_rmse_mean)
        Mean_mse<-rbind(Mean_mse,mean_mse)
      }
      value<-which.min(Mean_mse)
      if (value==1) {eta=0.01} else if (value==2) {eta=0.05} else if (value==3) {eta=0.15} else {eta=0.3}
      #train the model
      set.seed(i)
      xgb_mobtosta <- xgboost(data = dtrain, eta=eta, nround=300,verbose = F)
      pre_2 <- predict(xgb_mobtosta, as.matrix(covid_test_2[,c(1,4:6)]))
      Pre_2<-cbind(Pre_2,pre_2)
      pre_3 <- predict(xgb_mobtosta, as.matrix(covid_test_3[,c(1,4:6)]))
      Pre_3<-cbind(Pre_3,pre_3)
    } 
    else {
      ######train svm model and conduct prediction 
      train_ori<-na.omit(covid_sdg_svm[,c(1,4:6,j)])
      set.seed(i)
      bootstrap_index<-sample(nrow(train_ori),size=nrow(train_ori),replace=TRUE)
      train<-train_ori[bootstrap_index,]
      set.seed(i)
      tuneResult <- tune(svm, train.x=as.matrix(train[,1:4]), train.y=train[,5], ranges = list(gamma= c(0.001, 0.05, 0.25, 1, 2.5), cost= c(1,5,25,100)))
      set.seed(i)
      pre_2<- predict(tuneResult$best.model, newdata=covid_test_2_svm[,c(1,4:6)])
      pre_2<-pre_2*colsd[j-3]+colmean[j-3]
      Pre_2<-cbind(Pre_2,pre_2)
      pre_3<- predict(tuneResult$best.model, newdata=covid_test_3_svm[,c(1,4:6)])
      pre_3<-pre_3*colsd[j-3]+colmean[j-3]
      Pre_3<-cbind(Pre_3,pre_3)
    }
  }
  
  Pre_2<-cbind(covid_test_2$Income,Pre_2)
  Pre_2<-as.data.frame(Pre_2)
  Pre_3<-cbind(covid_test_3$Income,Pre_3)
  Pre_3<-as.data.frame(Pre_3)
  
  Aftersum_2_ad<-vector()
  Aftersum_2_ec<-vector()
  Aftersum_2<-vector()
  Aftersum_3_ad<-vector()
  Aftersum_3_ec<-vector()
  Aftersum_3<-vector()
  for (k in 1:6) {
    Sum_2_ad<-subset(Pre_2, V1==1)
    sumdata_2_ad<-Sum_2_ad[((k-1)*38+1):(38*k),]
    aftersum_2_ad<-colSums(sumdata_2_ad)
    Aftersum_2_ad<-rbind(Aftersum_2_ad,aftersum_2_ad)
    Sum_2_ec<-subset(Pre_2, V1==2)
    sumdata_2_ec<-Sum_2_ec[((k-1)*150+1):(150*k),]
    aftersum_2_ec<-colSums(sumdata_2_ec)
    Aftersum_2_ec<-rbind(Aftersum_2_ec,aftersum_2_ec)
    sumdata_2<-Pre_2[((k-1)*188+1):(188*k),]
    aftersum_2<-colSums(sumdata_2)
    Aftersum_2<-rbind(Aftersum_2,aftersum_2)
    Sum_3_ad<-subset(Pre_3, V1==1)
    sumdata_3_ad<-Sum_3_ad[((k-1)*38+1):(38*k),]
    aftersum_3_ad<-colSums(sumdata_3_ad)
    Aftersum_3_ad<-rbind(Aftersum_3_ad,aftersum_3_ad)
    Sum_3_ec<-subset(Pre_3, V1==2)
    sumdata_3_ec<-Sum_3_ec[((k-1)*150+1):(150*k),]
    aftersum_3_ec<-colSums(sumdata_3_ec)
    Aftersum_3_ec<-rbind(Aftersum_3_ec,aftersum_3_ec)
    sumdata_3<-Pre_3[((k-1)*188+1):(188*k),]
    aftersum_3<-colSums(sumdata_3)
    Aftersum_3<-rbind(Aftersum_3,aftersum_3)
  }
  Totalsum_2_ad<-rbind(Totalsum_2_ad,Aftersum_2_ad)
  Totalsum_2_ec<-rbind(Totalsum_2_ec,Aftersum_2_ec)
  Totalsum_2<-rbind(Totalsum_2,Aftersum_2)
  Totalsum_3_ad<-rbind(Totalsum_3_ad,Aftersum_3_ad)
  Totalsum_3_ec<-rbind(Totalsum_3_ec,Aftersum_3_ec)
  Totalsum_3<-rbind(Totalsum_3,Aftersum_3)
  print(i)
}

#add year sign to it
year<-rep(2019:2024,100)
Totalsum_2_ad<-cbind(year,Totalsum_2_ad)
Totalsum_2_ec<-cbind(year,Totalsum_2_ec)
Totalsum_2<-cbind(year,Totalsum_2)
Totalsum_3_ad<-cbind(year,Totalsum_3_ad)
Totalsum_3_ec<-cbind(year,Totalsum_3_ec)
Totalsum_3<-cbind(year,Totalsum_3)

#10%,50%,90% quantile
PI_without_ad<-vector()
for (i in c(2019:2024)) {
  Subsum_2_ad<-Totalsum_2_ad[Totalsum_2_ad[,1]==i,]
  Subsum_2_ad<-as.data.frame(Subsum_2_ad)
  final<-sapply(Subsum_2_ad, function(i) quantile(i, c(0.05,0.5,0.95)))
  PI_without_ad<-rbind(PI_without_ad,final)
}
PI_without_ec<-vector()
for (i in c(2019:2024)) {
  Subsum_2_ec<-Totalsum_2_ec[Totalsum_2_ec[,1]==i,]
  Subsum_2_ec<-as.data.frame(Subsum_2_ec)
  final<-sapply(Subsum_2_ec, function(i) quantile(i, c(0.05,0.5,0.95)))
  PI_without_ec<-rbind(PI_without_ec,final)
}
PI_without<-vector()
for (i in c(2019:2024)) {
  Subsum_2<-Totalsum_2[Totalsum_2[,1]==i,]
  Subsum_2<-as.data.frame(Subsum_2)
  final<-sapply(Subsum_2, function(i) quantile(i, c(0.05,0.5,0.95)))
  PI_without<-rbind(PI_without,final)
}
PI_with_ad<-vector()
for (i in c(2019:2024)) {
  Subsum_3_ad<-Totalsum_3_ad[Totalsum_3_ad[,1]==i,]
  Subsum_3_ad<-as.data.frame(Subsum_3_ad)
  final<-sapply(Subsum_3_ad, function(i) quantile(i, c(0.05,0.5,0.95)))
  PI_with_ad<-rbind(PI_with_ad,final)
}
PI_with_ec<-vector()
for (i in c(2019:2024)) {
  Subsum_3_ec<-Totalsum_3_ec[Totalsum_3_ec[,1]==i,]
  Subsum_3_ec<-as.data.frame(Subsum_3_ec)
  final<-sapply(Subsum_3_ec, function(i) quantile(i, c(0.05,0.5,0.95)))
  PI_with_ec<-rbind(PI_with_ec,final)
}
PI_with<-vector()
for (i in c(2019:2024)) {
  Subsum_3<-Totalsum_3[Totalsum_3[,1]==i,]
  Subsum_3<-as.data.frame(Subsum_3)
  final<-sapply(Subsum_3, function(i) quantile(i, c(0.05,0.5,0.95)))
  PI_with<-rbind(PI_with,final)
}

                
#compile the predicted data to unify the unit 
#global data
PI_without_0.95<-PI_without[c(3,6,9,12,15,18),3:41]
PI_without_0.5<-PI_without[c(2,5,8,11,14,17),3:41]
PI_without_0.05<-PI_without[c(1,4,7,10,13,16),3:41]
PI_with_0.95<-PI_with[c(3,6,9,12,15,18),3:41]
PI_with_0.5<-PI_with[c(2,5,8,11,14,17),3:41]
PI_with_0.05<-PI_with[c(1,4,7,10,13,16),3:41]
unit<-cbind(c(7365945458,7461407139,7535908347,7608572374,7679688914,7749712318),rep(188,6))  #calculate per capita value or mean value
same_unit<-cbind(100,100,1,1,1000,1000000,1,1000,0.0000001,1,1,1,1,1,0.001,0.001,1,1,0.001,0.000000001,1,0.00001,1000,1,0.00001,1,0.001,0.000000001,1,1000,1000,1,0.001,0.001,0.001,1,1000,0.000000001,0.00001)
unit_direction<-c(1,1,2,2,1,1,2,1,3,2,2,2,2,2,2,2,2,2,3,3,2,1,1,2,1,1,1,3,1,1,3,2,1,1,1,1,1,3,1)
                
Af_compile_without_0.5<-vector()
for (i in 1:39) {
  if (unit_direction[i]==1) {af_compile=PI_without_0.5[,i]/unit[,1]}
  else if (unit_direction[i]==2) {af_compile=PI_without_0.5[,i]/unit[,2]}
  else {af_compile=PI_without_0.5[,i]}
  af_compile_new<-af_compile*same_unit[,i]
  Af_compile_without_0.5<-cbind(Af_compile_without_0.5,af_compile_new)
}

Af_compile_with_0.5<-vector()
for (i in 1:39) {
  if (unit_direction[i]==1) {af_compile=PI_with_0.5[,i]/unit[,1]}
  else if (unit_direction[i]==2) {af_compile=PI_with_0.5[,i]/unit[,2]}
  else {af_compile=PI_without_0.5[,i]}
  af_compile_new<-af_compile*same_unit[,i]
  Af_compile_with_0.5<-cbind(Af_compile_with_0.5,af_compile_new)
}

#emde data
PI_without_ec_0.95<-PI_without_ec[c(3,6,9,12,15,18),3:41]
PI_without_ec_0.5<-PI_without_ec[c(2,5,8,11,14,17),3:41]
PI_without_ec_0.05<-PI_without_ec[c(1,4,7,10,13,16),3:41]
PI_with_ec_0.95<-PI_with_ec[c(3,6,9,12,15,18),3:41]
PI_with_ec_0.5<-PI_with_ec[c(2,5,8,11,14,17),3:41]
PI_with_ec_0.05<-PI_with_ec[c(1,4,7,10,13,16),3:41]
unit<-cbind(c(6312936357,6402500798,6473923472,6543815739,6612397551,6680029726),rep(150,6))  #calculate per capita value or mean value
same_unit<-cbind(100,100,1,1,1000,1000000,1,1000,0.0000001,1,1,1,1,1,0.001,0.001,1,1,0.001,0.000000001,1,0.00001,1000,1,0.00001,1,0.001,0.000000001,1,1000,1000,1,0.001,0.001,0.001,1,1000,0.000000001,0.00001)
unit_direction<-c(1,1,2,2,1,1,2,1,3,2,2,2,2,2,2,2,2,2,3,3,2,1,1,2,1,1,1,3,1,1,3,2,1,1,1,1,1,3,1)

Af_compile_without_ec_0.5<-vector()
for (i in 1:39) {
  if (unit_direction[i]==1) {af_compile=PI_without_ec_0.5[,i]/unit[,1]}
  else if (unit_direction[i]==2) {af_compile=PI_without_ec_0.5[,i]/unit[,2]}
  else {af_compile=PI_without_ec_0.5[,i]}
  af_compile_new<-af_compile*same_unit[,i]
  Af_compile_without_ec_0.5<-cbind(Af_compile_without_ec_0.5,af_compile_new)
}
Af_compile_without_ec_0.5<-Af_compile_without_ec_0.5[,c(1,2,5,8,9,11,13,16,17,19,20,22,23,25,26,27,28,29,30,33,34,35,36,37,39)]

Af_compile_with_ec_0.5<-vector()
for (i in 1:39) {
  if (unit_direction[i]==1) {af_compile=PI_with_ec_0.5[,i]/unit[,1]}
  else if (unit_direction[i]==2) {af_compile=PI_with_ec_0.5[,i]/unit[,2]}
  else {af_compile=PI_without_ec_0.5[,i]}
  af_compile_new<-af_compile*same_unit[,i]
  Af_compile_with_ec_0.5<-cbind(Af_compile_with_ec_0.5,af_compile_new)
}
Af_compile_with_ec_0.5<-Af_compile_with_ec_0.5[,c(1,2,5,8,9,11,13,16,17,19,20,22,23,25,26,27,28,29,30,33,34,35,36,37,39)]

#ad data
PI_without_ad_0.95<-PI_without_ad[c(3,6,9,12,15,18),3:41]
PI_without_ad_0.5<-PI_without_ad[c(2,5,8,11,14,17),3:41]
PI_without_ad_0.05<-PI_without_ad[c(1,4,7,10,13,16),3:41]
PI_with_ad_0.95<-PI_with_ad[c(3,6,9,12,15,18),3:41]
PI_with_ad_0.5<-PI_with_ad[c(2,5,8,11,14,17),3:41]
PI_with_ad_0.05<-PI_with_ad[c(1,4,7,10,13,16),3:41]
unit<-cbind(c(1046153388,1052080899,1055215724,1058071788,1060706247,1063197256),rep(38,6))  #calculate per capita value or mean value


Af_compile_without_ad_0.5<-vector()
for (i in 1:39) {
  if (unit_direction[i]==1) {af_compile=PI_without_ad_0.5[,i]/unit[,1]}
  else if (unit_direction[i]==2) {af_compile=PI_without_ad_0.5[,i]/unit[,2]}
  else {af_compile=PI_without_ad_0.5[,i]}
  af_compile_new<-af_compile*same_unit[,i]
  Af_compile_without_ad_0.5<-cbind(Af_compile_without_ad_0.5,af_compile_new)
}
Af_compile_without_ad_0.5<-Af_compile_without_ad_0.5[,c(2,19,20,22,23,24,25,26,27,28,29,33,34,35,38,39)]

Af_compile_with_ad_0.5<-vector()
for (i in 1:39) {
  if (unit_direction[i]==1) {af_compile=PI_with_ad_0.5[,i]/unit[,1]}
  else if (unit_direction[i]==2) {af_compile=PI_with_ad_0.5[,i]/unit[,2]}
  else {af_compile=PI_with_ad_0.5[,i]}
  af_compile_new<-af_compile*same_unit[,i]
  Af_compile_with_ad_0.5<-cbind(Af_compile_with_ad_0.5,af_compile_new)
}
Af_compile_with_ad_0.5<-Af_compile_with_ad_0.5[,c(2,19,20,22,23,24,25,26,27,28,29,33,34,35,38,39)]
