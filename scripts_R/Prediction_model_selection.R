require(mgcv)
single_SV_caller <- c("Manta","Lumpy","GRIDSS","SvABA","Delly")
combine_SV_SVcaller <- c()
for (i in c(1:length(single_SV_caller))){
  combine_SV_SVcaller <- c(combine_SV_SVcaller,paste0(single_SV_caller[i],single_SV_caller[!(c(1:length(single_SV_caller)) %in% i)],"Union"),
                           paste0(single_SV_caller[i],single_SV_caller[!(c(1:length(single_SV_caller)) %in% i)],"Intersect"))
}
SV_caller <- c(single_SV_caller,combine_SV_SVcaller)
dat_all <- read.csv("./data_SV/summary_stat.csv"))

model1_default <- paste0("gam( Sensitivity ~ te(VAF,T_coverage) + te(VAF,BND_threshold) + te(T_coverage,BND_threshold) + te(VAF, N_coverage), data=dat, family=betar(link='logit'), method='REML')")
model2_default <- paste0("gam( Sensitivity ~ te(VAF,T_coverage) +  te(VAF,BND_threshold) + te(VAF, N_coverage), data=dat, family=betar(link='logit'), method='REML')")
model3_default <- paste0("gam( Sensitivity ~ te(VAF,T_coverage) +  te(VAF,BND_threshold) + te(T_coverage,BND_threshold) + N_coverage, data=dat, family=betar(link='logit'), method='REML')")
model4_default <- paste0("gam( Sensitivity ~ te(VAF,T_coverage) +  te(VAF,BND_threshold) + N_coverage, data=dat, family=betar(link='logit'), method='REML')")
model5_default <- paste0("gam( Sensitivity ~ te(VAF,T_coverage) +  BND_threshold + N_coverage, data=dat, family=betar(link='logit'), method='REML')")

model_all <- c(model1_default, model2_default,model3_default, model4_default,model5_default)
model_names_all <- c("model1_default", "model2_default", "model3_default", "model4_default", "model5_default")

data <- dat_all[dat_all$REP != "SVEngine2",]
leave_index <- list(data$VAF == 0.12,
                    data$VAF == 0.25,
                    data$VAF == 0.4,
                    data$VAF == 0.6,
                    data$VAF == 0.75,
                    data$VAF == 0.95,
                    data$T_coverage == 25,
                    data$T_coverage == 38,
                    data$T_coverage == 50,
                    data$T_coverage == 70,
                    data$T_coverage == 85,
                    data$T_coverage == 95,
                    data$N_coverage == 25,
                    data$N_coverage == 38,
                    data$N_coverage == 50,
                    data$N_coverage == 70,
                    data$N_coverage == 85,
                    data$N_coverage == 95,
                    data$BND_threshold == 8,
                    data$BND_threshold == 15,
                    data$BND_threshold == 30,
                    data$BND_threshold == 90,
                    data$BND_threshold == 120,
                    data$BND_threshold == 180,
                    data$VAF == 0.05,
                    data$VAF == 0.1,
                    data$VAF == 0.2,
                    data$VAF == 0.5,
                    data$VAF == 0.8,
                    data$VAF == 1,
                    data$T_coverage == 20,
                    data$T_coverage == 30,
                    data$T_coverage == 45,
                    data$T_coverage == 60,
                    data$T_coverage == 75,
                    data$T_coverage == 90,
                    data$N_coverage == 15,
                    data$N_coverage == 30,
                    data$N_coverage == 45,
                    data$N_coverage == 60,
                    data$N_coverage == 75,
                    data$N_coverage == 90,
                    data$BND_threshold == 2,
                    data$BND_threshold == 5,
                    data$BND_threshold == 50,
                    data$BND_threshold == 100,
                    data$BND_threshold == 150,
                    data$BND_threshold == 200)


model <- model_all
model_names <- model_names_all

RMSE <- c()
MAE <- c()
for (j in c(1: length(model_names))){
  RMSE_caller <- c()
  MAE_caller <- c()
  for(i in c(1:5)){
    RMSE_test <- c()
    MAE_test <- c()
    for (k in c(1:48)){
      dat_train <- data[!leave_index[[k]],]
      dat_test <- data[leave_index[[k]],]
      dat <- dat_train[dat_train$Caller == SV_caller[i],]
      assign(paste0("gamsen_", SV_caller[i]), eval(parse(text=model[j])))
      
      dat <- dat_test[dat_test$Caller == SV_caller[i],]
      RMSE_test <- c(RMSE_test, sqrt(mean((predict(eval(parse(text=paste0("gamsen_",SV_caller[i]))), dat, type = "response",se.fit = T,unconditional = TRUE)$fit - dat$Sensitivity)^2))                          )
      MAE_test <- c(MAE_test, mean(abs(predict(eval(parse(text=paste0("gamsen_",SV_caller[i]))), dat, type = "response",se.fit = T,unconditional = TRUE)$fit - dat$Sensitivity)))
    }
    RMSE_caller <- c(RMSE_caller, mean(RMSE_test))
    MAE_caller <- c(MAE_caller, mean(MAE_test))
  }
  RMSE <- rbind(RMSE, RMSE_caller)
  MAE <- rbind(MAE, MAE_caller)
}
colnames(RMSE) <- SV_caller[1:5]
colnames(MAE) <- SV_caller[1:5]


AIC_caller <- c()
for(i in c(1:5)){
  dat <- data[data$Caller == SV_caller[i],]
  AIC <- c()
  for (j in c(1:length(model_names))){
    assign(paste0("gamsen_", SV_caller[i],j), eval(parse(text=model[j])))
    AIC <- c(AIC, AIC(eval(parse(text=paste0("gamsen_", SV_caller[i],j)))))
  }
  AIC_caller <- cbind(AIC_caller, AIC)
}
colnames(AIC_caller) <- SV_caller[1:5]

df <- data.frame(rbind(cbind(RMSE, model = model_names,value = "RMSE"),
                       cbind(MAE, model = model_names, value= "MAE"),
                       cbind(AIC_caller, model = model_names, value = "AIC")))
rownames(df) <- paste0(df$model,"_",df$value)

write.csv(df,"./data/RMSE_MAE_AIC_sensitivity.csv")


model1_default <- paste0("gam( Precision ~ te(VAF,T_coverage) + te(VAF,BND_threshold) + te(T_coverage,BND_threshold) + N_coverage, data=dat, family=betar(link='logit'), method='REML')")
model2_default <- paste0("gam( Precision ~ te(VAF,T_coverage) + te(VAF,BND_threshold) + N_coverage, data=dat, family=betar(link='logit'), method='REML')")
model3_default <- paste0("gam( Precision ~ te(VAF,T_coverage) + te(T_coverage,BND_threshold) + N_coverage, data=dat, family=betar(link='logit'), method='REML')")
model4_default <- paste0("gam( Precision ~ te(VAF,T_coverage) + BND_threshold + N_coverage, data=dat, family=betar(link='logit'), method='REML')")
model5_default <- paste0("gam( Precision ~ te(VAF,BND_threshold) + T_coverage + N_coverage, data=dat, family=betar(link='logit'), method='REML')")
model6_default <- paste0("gam( Precision ~ VAF + T_coverage + N_coverage + BND_threshold, data=dat, family=betar(link='logit'), method='REML')")

model <- c( model1_default, model2_default, model3_default, model4_default, model5_default, model6_default)
model_names <- c("model1_default", "model2_default", "model3_default", "model4_default", "model5_default", "model6_default")


data <- dat_all[dat_all$REP != "SVEngine2",]
data <- data[!is.na(data$Precision),]
data$Precision_offset <- data$Precision
data$Precision_offset[data$Precision==1] = 0.99999
leave_index <- list(data$VAF == 0.12,
                    data$VAF == 0.25,
                    data$VAF == 0.4,
                    data$VAF == 0.6,
                    data$VAF == 0.75,
                    data$VAF == 0.95,
                    data$T_coverage == 25,
                    data$T_coverage == 38,
                    data$T_coverage == 50,
                    data$T_coverage == 70,
                    data$T_coverage == 85,
                    data$T_coverage == 95,
                    data$N_coverage == 25,
                    data$N_coverage == 38,
                    data$N_coverage == 50,
                    data$N_coverage == 70,
                    data$N_coverage == 85,
                    data$N_coverage == 95,
                    data$BND_threshold == 8,
                    data$BND_threshold == 15,
                    data$BND_threshold == 30,
                    data$BND_threshold == 90,
                    data$BND_threshold == 120,
                    data$BND_threshold == 180,
                    data$VAF == 0.05,
                    data$VAF == 0.1,
                    data$VAF == 0.2,
                    data$VAF == 0.5,
                    data$VAF == 0.8,
                    data$VAF == 1,
                    data$T_coverage == 20,
                    data$T_coverage == 30,
                    data$T_coverage == 45,
                    data$T_coverage == 60,
                    data$T_coverage == 75,
                    data$T_coverage == 90,
                    data$N_coverage == 15,
                    data$N_coverage == 30,
                    data$N_coverage == 45,
                    data$N_coverage == 60,
                    data$N_coverage == 75,
                    data$N_coverage == 90,
                    data$BND_threshold == 2,
                    data$BND_threshold == 5,
                    data$BND_threshold == 50,
                    data$BND_threshold == 100,
                    data$BND_threshold == 150,
                    data$BND_threshold == 200)

RMSE <- c()
MAE <- c()
for (j in c(1: length(model_names))){
  RMSE_caller <- c()
  MAE_caller <- c()
  for(i in c(1:5)){
    RMSE_test <- c()
    MAE_test <- c()
    for (k in c(1:48)){
      dat_train <- data[!leave_index[[k]],]
      dat_test <- data[leave_index[[k]],]
      dat <- dat_train[dat_train$Caller == SV_caller[i],]
      assign(paste0("gampre_off_", SV_caller[i]), eval(parse(text=model[j])))
      
      dat <- dat_test[dat_test$Caller == SV_caller[i],]
      RMSE_test <- c(RMSE_test, sqrt(mean((predict(eval(parse(text=paste0("gampre_off_",SV_caller[i]))), dat, type = "response",se.fit = T,unconditional = TRUE)$fit - dat$Precision)^2))                          )
      MAE_test <- c(MAE_test, mean(abs(predict(eval(parse(text=paste0("gampre_off_",SV_caller[i]))), dat, type = "response",se.fit = T,unconditional = TRUE)$fit - dat$Precision)))
    }
    RMSE_caller <- c(RMSE_caller, mean(RMSE_test))
    MAE_caller <- c(MAE_caller, mean(MAE_test))
  }
  RMSE <- rbind(RMSE, RMSE_caller)
  MAE <- rbind(MAE, MAE_caller)
}
colnames(RMSE) <- SV_caller[1:5]
colnames(MAE) <- SV_caller[1:5]


AIC_caller <- c()
for(i in c(1:5)){
  dat <- data[data$Caller == SV_caller[i],]
  AIC <- c()
  for (j in c(1:length(model_names))){
    assign(paste0("gampre_off_", SV_caller[i],j), eval(parse(text=model[j])))
    AIC <- c(AIC, AIC(eval(parse(text=paste0("gampre_off_", SV_caller[i],j)))))
  }
  AIC_caller <- cbind(AIC_caller, AIC)
}
colnames(AIC_caller) <- SV_caller[1:5]

df <- data.frame(rbind(cbind(RMSE,value = "RMSE", model = model_names),
                       cbind(MAE,value= "MAE", model = model_names), 
                       cbind(AIC_caller,value = "AIC", model = model_names)))
rownames(df) <- paste0(df$model,"_",df$value)
write.csv(df,"./data/RMSE_MAE_AIC_precision.csv")

