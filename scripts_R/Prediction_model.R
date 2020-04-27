require(mgcv)
single_SV_caller <- c("Manta","Lumpy","GRIDSS","SvABA","Delly")
combine_SV_SVcaller <- c()
for (i in c(1:length(single_SV_caller))){
  combine_SV_SVcaller <- c(combine_SV_SVcaller,paste0(single_SV_caller[i],single_SV_caller[!(c(1:length(single_SV_caller)) %in% i)],"Union"),
                           paste0(single_SV_caller[i],single_SV_caller[!(c(1:length(single_SV_caller)) %in% i)],"Intersect"))
}
SV_caller <- c(single_SV_caller,combine_SV_SVcaller)

### Sensitivity models
dat_all <- read.csv("./data_SV/summary_stat.csv")
data <- dat_all
model2 <- paste0("gam( Sensitivity ~ te(VAF,T_coverage) +  te(VAF,BND_threshold) + te(VAF, N_coverage), data=dat, family=betar(link='logit'), method='REML')")
model3 <- paste0("gam( Sensitivity ~ te(VAF,T_coverage) +  te(VAF,BND_threshold) + te(T_coverage,BND_threshold) + N_coverage, data=dat, family=betar(link='logit'), method='REML')")
model5 <- paste0("gam( Sensitivity ~ te(VAF,T_coverage) +  BND_threshold + N_coverage, data=dat, family=betar(link='logit'), method='REML')")
model3_dropNCOV <- paste0("gam( Sensitivity ~ te(VAF,T_coverage) +  te(VAF,BND_threshold) + te(T_coverage,BND_threshold), data=dat, family=betar(link='logit'), method='REML')")
model5_dropNCOV <- paste0("gam( Sensitivity ~ te(VAF,T_coverage) +  BND_threshold, data=dat, family=betar(link='logit'), method='REML')")

for(i in c(1:5)){
  dat <- data[data$Caller == SV_caller[i],]
  if(i == 1){
    assign(paste0("gamsen_", SV_caller[i]), eval(parse(text=model2)))
  }else if (i == 4){
    assign(paste0("gamsen_", SV_caller[i]), eval(parse(text=model3)))
  }else if(i %in% c(2,3,5)){
    assign(paste0("gamsen_", SV_caller[i]), eval(parse(text=model5_dropNCOV)))
  }
}

dat_all <- read.csv("./data_SV/summary_stat_union_intersection.csv")
data <- dat_all
for(i in c(6:length(SV_caller))){
  dat <- data[data$Caller == SV_caller[i],]
  if(grepl(SV_caller[1], SV_caller[i])){
    assign(paste0("gamsen_", SV_caller[i]), eval(parse(text=model2)))
  }else if(grepl(SV_caller[2], SV_caller[i])){
    assign(paste0("gamsen_", SV_caller[i]), eval(parse(text=model5_dropNCOV)))
  }else if(grepl(SV_caller[3], SV_caller[i])){
    assign(paste0("gamsen_", SV_caller[i]), eval(parse(text=model5_dropNCOV)))
  }else if(grepl(SV_caller[4], SV_caller[i])){
    assign(paste0("gamsen_", SV_caller[i]), eval(parse(text=model3)))
  }else if(grepl(SV_caller[5], SV_caller[i])){
    assign(paste0("gamsen_", SV_caller[i]), eval(parse(text=model5_dropNCOV)))
  }
}
save(list = paste0("gamsen_", SV_caller),file = "./data/gamsen.RData")

### Precision models
dat_all <- read.csv("./data_SV/summary_stat.csv")
data <- dat_all
data <- data[!is.na(data$Precision),]
data$Precision_offset <- data$Precision
data$Precision_offset[data$Precision==1] = 0.99999

model1 <- paste0("gam( Precision ~ te(VAF,T_coverage) + te(VAF,BND_threshold) + te(T_coverage,BND_threshold) + N_coverage, data=dat, family=betar(link='logit'), method='REML')")
model2 <- paste0("gam( Precision ~ te(VAF,T_coverage) + te(VAF,BND_threshold) + N_coverage, data=dat, family=betar(link='logit'), method='REML')")
model3 <- paste0("gam( Precision ~ te(VAF,T_coverage) + te(T_coverage,BND_threshold) + N_coverage, data=dat, family=betar(link='logit'), method='REML')")
model2_dropNCOV <- paste0("gam( Precision ~ te(VAF,T_coverage) + te(VAF,BND_threshold), data=dat, family=betar(link='logit'), method='REML')")

for(i in c(1:5)){
  dat <- data[data$Caller == SV_caller[i],]
  if(i %in% c(1)){
    assign(paste0("gampre_off_", SV_caller[i]), eval(parse(text=model1)))
  }else if(i %in% c(2)){
    assign(paste0("gampre_off_", SV_caller[i]), eval(parse(text=model2)))
  }else if(i %in% c(3)){
    assign(paste0("gampre_off_", SV_caller[i]), eval(parse(text=model2_dropNCOV)))
  }else if(i %in% c(4)){
    assign(paste0("gampre_off_", SV_caller[i]), eval(parse(text=model3)))
  }else if(i %in% c(5)){
    assign(paste0("gampre_off_", SV_caller[i]), eval(parse(text=model2)))
  }
}

dat_all <- read.csv("./data_SV/summary_stat_union_intersection.csv")
data <- dat_all
data <- data[!is.na(data$Precision),]
data$Precision_offset <- data$Precision
data$Precision_offset[data$Precision==1] = 0.99999

for(i in c(6:length(SV_caller))){
  dat <- data[data$Caller == SV_caller[i],]
  if(grepl(SV_caller[1], SV_caller[i])){
    assign(paste0("gampre_off_", SV_caller[i]), eval(parse(text=model1)))
  }else if(grepl(SV_caller[2], SV_caller[i])){
    assign(paste0("gampre_off_", SV_caller[i]), eval(parse(text=model2)))
  }else if(grepl(SV_caller[3], SV_caller[i])){
    assign(paste0("gampre_off_", SV_caller[i]), eval(parse(text=model2_dropNCOV)))
  }else if(grepl(SV_caller[4], SV_caller[i])){
    assign(paste0("gampre_off_", SV_caller[i]), eval(parse(text=model3)))
  }else if(grepl(SV_caller[5], SV_caller[i])){
    assign(paste0("gampre_off_", SV_caller[i]), eval(parse(text=model2)))
  }
}
save(list = paste0("gampre_off_", SV_caller),file = "./data/gampre.RData")




