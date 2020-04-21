library(ggplot2)
library(RColorBrewer)
library(ggsci)
require(mgcv)
library(ggpubr)

color_SVCaller <- c(brewer.pal(9, "Set1")[c(1:9)],brewer.pal(12, "Set3")[c(1:12)],brewer.pal(8, "Set2")[c(1:8)],pal_npg("nrc")(10),pal_jco()(10))
single_SV_caller <- c("Manta","Lumpy","GRIDSS","SvABA","Delly")
combine_SV_SVcaller <- c()
for (i in c(1:length(single_SV_caller))){
  combine_SV_SVcaller <- c(combine_SV_SVcaller,paste0(single_SV_caller[i],single_SV_caller[!(c(1:length(single_SV_caller)) %in% i)],"Union"),
                           paste0(single_SV_caller[i],single_SV_caller[!(c(1:length(single_SV_caller)) %in% i)],"Intersect"))
}
SV_caller <- c(single_SV_caller,combine_SV_SVcaller)
names(color_SVCaller) <- SV_caller

SV_caller_label <- c(single_SV_caller,
                     expression(Manta*union(Lumpy)), expression(Manta*union(GRIDSS)), expression(Manta*union(SvABA)), expression(Manta*union(Delly)),
                     expression(Manta*intersect(Lumpy)), expression(Manta*intersect(GRIDSS)), expression(Manta*intersect(SvABA)), expression(Manta*intersect(Delly)),
                     expression(Lumpy*union(Manta)), expression(Lumpy*union(GRIDSS)), expression(Lumpy*union(SvABA)), expression(Lumpy*union(Delly)),
                     expression(Lumpy*intersect(Manta)), expression(Lumpy*intersect(GRIDSS)), expression(Lumpy*intersect(SvABA)), expression(Lumpy*intersect(Delly)),
                     expression(GRIDSS*union(Manta)), expression(GRIDSS*union(Lumpy)), expression(GRIDSS*union(SvABA)), expression(GRIDSS*union(Delly)),
                     expression(GRIDSS*intersect(Manta)), expression(GRIDSS*intersect(Lumpy)), expression(GRIDSS*intersect(SvABA)), expression(GRIDSS*intersect(Delly)),
                     expression(SvABA*union(Manta)), expression(SvABA*union(Lumpy)), expression(SvABA*union(GRIDSS)), expression(SvABA*union(Delly)),
                     expression(SvABA*intersect(Manta)), expression(SvABA*intersect(Lumpy)), expression(SvABA*intersect(GRIDSS)), expression(SvABA*intersect(Delly)),
                     expression(Delly*union(Manta)), expression(Delly*union(Lumpy)), expression(Delly*union(GRIDSS)), expression(Delly*union(SvABA)),
                     expression(Delly*intersect(Manta)), expression(Delly*intersect(Lumpy)), expression(Delly*intersect(GRIDSS)), expression(Delly*intersect(SvABA)))

SV_caller_label2 <-c()
for (i in c(1:length(single_SV_caller))){
  SV_caller_label2 <- c(SV_caller_label2,paste0(single_SV_caller[i],"&#8746;",single_SV_caller[!(c(1:length(single_SV_caller)) %in% i)]),
                        paste0(single_SV_caller[i],"&#8745;",single_SV_caller[!(c(1:length(single_SV_caller)) %in% i)]))
}
SV_caller_label1 <- c(single_SV_caller, SV_caller_label2)
names(SV_caller_label1) <- SV_caller


# load("./data/gamsen_model1_k16.RData")
# load("./data/gamsen_model2_k16.RData")
# load("./data/gamsen_model3_k16.RData")
# load("./data/gam_Log.RData")
# load("./data/gam_Log2.RData")
# load("./data/gamsen_model1_kdefault.RData")
# load("./data/gamsen_model2_kdefault.RData")
# load("./data/gamsen_model2_k10.RData")
# load("./data/gam_k10.RData")
# load("./data/gam_k16.RData")

load("./data/gamsen.RData")
load("./data/gampre.RData")
#load("./data/gamsen_union_intersection.RData")
#load("./data/gampre_Union_Intersection.RData")

# load("./data/gampre_k10.RData")
# load("./data/gampre_k16.RData")
# 
# load("./data/gam_Linear.RData")
# load("./data/gam_Log.RData")
# load("./data/gampre_Linear.RData")
# data <- data.frame(Caller = SV_caller,
#                    T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller)),
#                    N_coverage = input$N_coverage3,
#                    VAF = rep(seq(0.05,1,0.01),each=length(T_coverage_label)*length(SV_caller)),
#                    BND_threshold = input$BND_threshold3)
# 
# data <- data.frame(Caller = SV_caller,
#                    T_coverage = rep(c(20:90), each = length(SV_caller)),
#                    N_coverage = rep(c(20:90), each = length(T_coverage_label)*length(SV_caller)),
#                    VAF = rep(seq(0.05,1,0.01), each = length(T_coverage_label)* length(SV_caller)* length(T_coverage_label)),
#                    BND_threshold = rep(c(2:200), each = length(T_coverage_label)*length(SV_caller)* length(N_coverage_label)))
# 
# data <- data.frame(Caller = SV_caller,
#                    T_coverage = rep(c(20:90), each = length(SV_caller)),
#                    N_coverage = rep(c(20:90), each = length(c(20:90))*length(SV_caller)),
#                    VAF = rep(seq(0.05,1,0.01), each = length(c(20:90))* length(SV_caller)* length(c(20:90))))
# 
# data <- data.frame(Caller = SV_caller,
#                    T_coverage = rep(c(20:90), each = length(SV_caller)),
#                    N_coverage = rep(c(20:90), each = length(c(20:90))*length(SV_caller)),
#                    VAF = rep(seq(0.05,1,0.01), each = length(c(20:90))* length(SV_caller)* length(c(20:90))),
#                    BND_threshold = rep(c(2:10), each = length(c(20:90))* length(SV_caller)* length(c(20:90)) * length(seq(0.05,1,0.01)))
#                                        )
# data <- data.frame(Caller = SV_caller[1],
#                    T_coverage = rep(c(20:90), each = length(SV_caller)),
#                    N_coverage = rep(c(20:90), each = length(c(20:90))*length(SV_caller)),
#                    VAF = rep(seq(0.05,1,0.01), each = length(c(20:90))* length(SV_caller)* length(c(20:90))),
#                    BND_threshold = rep(c(2:100), each = length(c(20:90))* length(SV_caller)* length(c(20:90)) * length(seq(0.05,1,0.01)))
# )
# 
# write.csv(data,"./data/data1.csv", row.names = FALSE)
# 
# df.Sensitivity <- c()
# for (SVCaller_name in SV_caller){
#   newdata <- data[data$Caller == SVCaller_name,]
#   assign("df.Sensitivity_caller",  data.frame(newdata,
#                                               predict(eval(parse(text=paste0("gamsen_", SVCaller_name))),newdata,type = "response",se.fit = T,unconditional = TRUE),
#                                               T_coverage_label = paste0(newdata$T_coverage,"x"),
#                                               N_coverage_label = paste0(newdata$N_coverage,"x"),
#                                               VAF_label = paste0(newdata$VAF),
#                                               BND_label = paste0(newdata$BND_threshold),
#                                               row.names = c(1:(nrow(newdata)))))
#   df.Sensitivity <- rbind(df.Sensitivity, df.Sensitivity_caller)
# }


# #inputs on top
# input_X_axis3 <- "VAF"
# input_measurements3 <- "Sensitivity"
# 
# ##inputs in sliders on side bar
# ### input_VAF3 will be [0.05,1]
# input_T_coverage3 <- 60
# input_N_coverage3 <- 30
# input_BND_threshold3 <- 200
# ##inputs in checkboxes on side bar
# input_SVCaller3 <- c("Manta","Lumpy","GRIDSS", "SvABA", "Delly")
# #input_SVCaller3.1 <- c("Manta","Lumpy")
# input_SVCaller3.1 <- c()
# #input_T_coverage3.1 <- c("30x","60x")
# input_T_coverage3.1 <- ""
# 
# T_coverage_label = paste0(c(input_T_coverage3,60,30),"x")
# line_type <- c("solid","dashed","dotted")
# names(line_type) <- T_coverage_label
# 
# data <- data.frame(Caller = single_SV_caller,
#                       T_coverage = rep(c(input_T_coverage3,60,30), each=length(SV_caller)),
#                       N_coverage = input_N_coverage3,
#                       VAF = rep(seq(0.05,1,0.01),each=length(T_coverage_label)*length(SV_caller)),
#                       BND_threshold = input_BND_threshold3)
# df.Sensitivity <- c()
# for (SVCaller_name in single_SV_caller){
# #for (SVCaller_name in SV_caller){
#   newdata <- data[data$Caller == SVCaller_name,]
#   assign("df.Sensitivity_caller",  data.frame(newdata,
#                                       predict(eval(parse(text=paste0("gamsen_", SVCaller_name))),newdata,type = "response",se.fit = T,unconditional = TRUE),
#                                       T_coverage_label = paste0(newdata$T_coverage,"x"),
#                                       N_coverage_label = paste0(newdata$N_coverage,"x"),
#                                       VAF_label = paste0(newdata$VAF),
#                                       BND_label = paste0(newdata$BND_threshold),
#                                       row.names = c(1:(nrow(newdata)))))
#   df.Sensitivity <- rbind(df.Sensitivity, df.Sensitivity_caller)
# }
# xlabel <- "Tumor purity (VAF)"
# legend_label <- "Tumor coverage"
# Tcov_var <- c(paste0(input_T_coverage3,"x"),input_T_coverage3.1)
# Ncov_var <- paste0(input_N_coverage3,"x")
# VAF_var <- paste0(rep(seq(0,1,0.01)))
# BND_var <- paste0(input_BND_threshold3)
# index <- 8
# 
# tmp1 <- df.Sensitivity
# input_objective3 <- 0.7
# obj_row <- c()
# if(input_X_axis3 %in% c("VAF","N_coverage","BND_threshold")){
#   for(i in 1: length(SV_caller)){
#     for(j in 1: length(T_coverage_label)){
#       tmp = tmp1[(tmp1$Caller %in% SV_caller[i]) &
#                    (tmp1$T_coverage_label %in% T_coverage_label[j]) &
#                    (tmp1$N_coverage_label %in% Ncov_var) &
#                    (tmp1$VAF_label %in% VAF_var) &
#                    (tmp1$BND_threshold %in% BND_var),]
#       obj_row <- c(obj_row,rownames(tmp[tmp$fit>input_objective3,][1,]))
#     }
#   }
# }else if(input_X_axis3 == "T_coverage"){
#   for(i in 1: length(SV_caller)){
#     for(k in 1: length(unique(VAF_label))){
#       tmp = tmp1[(tmp1$Caller %in% SV_caller[i]) &
#                    (tmp1$T_coverage_label %in% Tcov_var) &
#                    (tmp1$N_coverage_label %in% Ncov_var) &
#                    (tmp1$VAF_label %in% unique(VAF_label)[k]) &
#                    (tmp1$BND_threshold %in% BND_var),]
#       obj_row <- c(obj_row,rownames(tmp[tmp$fit>input_objective3,][1,]))
#     }
#   }
# }
# 
# obj <- rep(FALSE,nrow(df.Sensitivity))
# obj[as.numeric(obj_row[obj_row!="NA"])] <- TRUE
# df.Sensitivity <- cbind(df.Sensitivity,obj)
# obj.y <- rep(NA,nrow(df.Sensitivity))
# obj.y[df.Sensitivity$obj] <- df.Sensitivity$fit[df.Sensitivity$obj]
# df.Sensitivity <- cbind(df.Sensitivity,obj.y)
# 
# 
# input_union <- c()
# if(length(input_SVCaller3.1)==1){
#   input_union <- input_SVCaller3.1
# }else{
#   for(i in 1: length(input_SVCaller3.1)){
#     for(j in 1: length(input_SVCaller3.1)){
#       if(j!=i){
#         input_union <- c(input_union,paste0(input_SVCaller3.1[i],input_SVCaller3.1[j],"Union"))
#       }
#     }
#   }
# }
# 
# df = df.Sensitivity[(df.Sensitivity$Caller %in% c(input_SVCaller3,input_union)) &
#                       (df.Sensitivity$T_coverage_label %in% Tcov_var) &
#                       (df.Sensitivity$N_coverage_label %in% Ncov_var) &
#                       (df.Sensitivity$VAF_label %in% VAF_var),]
# 
# ggplot(data=df, aes(x=eval(parse(text=input_X_axis3)), y = fit, group = interaction(Caller,eval(parse(text=colnames(df)[index]))))) +
#   geom_ribbon(aes(ymin = fit-1.96*se.fit,ymax=fit+1.96*se.fit), fill="grey70")+
#   geom_line(data = df, aes(x=eval(parse(text=input_X_axis3)), y = fit,
#                            group = interaction(Caller,eval(parse(text=colnames(df)[index]))),
#                            color = Caller, linetype = eval(parse(text=colnames(df)[index]))), size =2)+
#   geom_hline(yintercept=input_objective3)+
#   geom_point(data=df,aes(x=eval(parse(text=input_X_axis3)), y = obj.y))+
#   #geom_text(aes(x=eval(parse(text=input_X_axis3)), y = obj.y+0.05, label=round(obj.y,2)))+
#   #ggtitle(paste("Sensitivity","across",xlabel))+
#   scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#   scale_x_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#   #scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$Caller],labels = SV_caller_label[SV_caller %in% df$Caller])+
#   scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$Caller])+
#   scale_linetype_manual(values=line_type[names(line_type) %in% unique(df[,index])])+
#   labs(y=input_measurements3[1], x = xlabel, linetype = colnames(df)[index])+
#   theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
#         legend.title=element_text(size=14),legend.text=element_text(size=14))
# 
# 
# df.Precision <- c()
# for (SVCaller_name in single_SV_caller){
# #for (SVCaller_name in SV_caller){
#   newdata <- data[data$Caller == SVCaller_name,]
#   assign("df.Precision_caller",  data.frame(newdata,
#                                               predict(eval(parse(text=paste0("gampre_off_", SVCaller_name))),newdata,type = "response",se.fit = T,unconditional = TRUE),
#                                               T_coverage_label = paste0(newdata$T_coverage,"x"),
#                                               N_coverage_label = paste0(newdata$N_coverage,"x"),
#                                               VAF_label = paste0(newdata$VAF),
#                                               BND_label = paste0(newdata$BND_threshold),
#                                               row.names = c(1:(nrow(newdata)))))
#   df.Precision <- rbind(df.Precision, df.Precision_caller)
# }
# xlabel <- "Tumor purity (VAF)"
# legend_label <- "Tumor coverage"
# Tcov_var <- c(paste0(input_T_coverage3,"x"),input_T_coverage3.1)
# Ncov_var <- paste0(input_N_coverage3,"x")
# VAF_var <- paste0(rep(seq(0,1,0.01)))
# BND_var <- paste0(input_BND_threshold3)
# index <- 8
# 
# tmp1 <- df.Precision
# input_objective3 <- 0.7
# obj_row <- c()
# if(input_X_axis3 %in% c("VAF","N_coverage","BND_threshold")){
#   for(i in 1: length(SV_caller)){
#     for(j in 1: length(T_coverage_label)){
#       tmp = tmp1[(tmp1$Caller %in% SV_caller[i]) &
#                    (tmp1$T_coverage_label %in% T_coverage_label[j]) &
#                    (tmp1$N_coverage_label %in% Ncov_var) &
#                    (tmp1$VAF_label %in% VAF_var) &
#                    (tmp1$BND_threshold %in% BND_var),]
#       obj_row <- c(obj_row,rownames(tmp[tmp$fit>input_objective3,][1,]))
#     }
#   }
# }else if(input_X_axis3 == "T_coverage"){
#   for(i in 1: length(SV_caller)){
#     for(k in 1: length(unique(VAF_label))){
#       tmp = tmp1[(tmp1$Caller %in% SV_caller[i]) &
#                    (tmp1$T_coverage_label %in% Tcov_var) &
#                    (tmp1$N_coverage_label %in% Ncov_var) &
#                    (tmp1$VAF_label %in% unique(VAF_label)[k]) &
#                    (tmp1$BND_threshold %in% BND_var),]
#       obj_row <- c(obj_row,rownames(tmp[tmp$fit>input_objective3,][1,]))
#     }
#   }
# }
# 
# obj <- rep(FALSE,nrow(df.Precision))
# obj[as.numeric(obj_row[obj_row!="NA"])] <- TRUE
# df.Precision <- cbind(df.Precision,obj)
# obj.y <- rep(NA,nrow(df.Precision))
# obj.y[df.Precision$obj] <- df.Precision$fit[df.Precision$obj]
# df.Precision <- cbind(df.Precision,obj.y)
# 
# 
# input_union <- c()
# if(length(input_SVCaller3.1)==1){
#   input_union <- input_SVCaller3.1
# }else{
#   for(i in 1: length(input_SVCaller3.1)){
#     for(j in 1: length(input_SVCaller3.1)){
#       if(j!=i){
#         input_union <- c(input_union,paste0(input_SVCaller3.1[i],input_SVCaller3.1[j],"Union"))
#       }
#     }
#   }
# }
# 
# df = df.Precision[(df.Precision$Caller %in% c(input_SVCaller3,input_union)) &
#                       (df.Precision$T_coverage_label %in% Tcov_var) &
#                       (df.Precision$N_coverage_label %in% Ncov_var) &
#                       (df.Precision$VAF_label %in% VAF_var),]
# 
# ggplot(data=df, aes(x=eval(parse(text=input_X_axis3)), y = fit, group = interaction(Caller,eval(parse(text=colnames(df)[index]))))) +
#   geom_ribbon(aes(ymin = fit-1.96*se.fit,ymax=fit+1.96*se.fit), fill="grey70")+
#   geom_line(data = df, aes(x=eval(parse(text=input_X_axis3)), y = fit,
#                            group = interaction(Caller,eval(parse(text=colnames(df)[index]))),
#                            color = Caller, linetype = eval(parse(text=colnames(df)[index]))), size =2)+
#   geom_hline(yintercept=input_objective3)+
#   geom_point(data=df,aes(x=eval(parse(text=input_X_axis3)), y = obj.y))+
#   #geom_text(aes(x=eval(parse(text=input_X_axis3)), y = obj.y+0.05, label=round(obj.y,2)))+
#   #ggtitle(paste("Sensitivity","across",xlabel))+
#   scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#   scale_x_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#   #scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$Caller],labels = SV_caller_label[SV_caller %in% df$Caller])+
#   scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$Caller])+
#   scale_linetype_manual(values=line_type[names(line_type) %in% unique(df[,index])])+
#   labs(y=input_measurements3[1], x = xlabel, linetype = colnames(df)[index])+
#   theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
#         legend.title=element_text(size=14),legend.text=element_text(size=14))
# 
# pdf(file=paste0("./plot1",".pdf"),width=20,height=12)
# figure1
# dev.off()
