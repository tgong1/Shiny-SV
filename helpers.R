#library(profmem)
#library(lobstr)
#mem_1 <- profmem_begin()
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

SVTYPE_all <- c("DEL","DUP","INV","DINS","FINS","TRA")
color_SVTYPE <- c(brewer.pal(9, "Set1")[c(1:6)])
names(color_SVTYPE) <- SVTYPE_all
SVTYPE_all_label <- c("Deletion","Duplication","Inversion","Domestic insertion","Foreign insertion","Translocation")


SVTYPE_all2 <- c("DEL","DUP","INV","INS","BND")
color_SVTYPE2 <- c(brewer.pal(9, "Set1")[c(1:4,6)])
names(color_SVTYPE2) <- SVTYPE_all2
SVTYPE_all_label2 <- c("Deletion","Duplication","Inversion","Insertion","Translocation/BND")


theme <- theme(axis.text = element_text(size=14), axis.title = element_text(size=14,face="bold"), plot.title = element_text(size=16),
               legend.title = element_text(size=14), legend.text = element_text(size=14))

#SV_caller <- single_SV_caller


#directory <- "/Volumes/personal/tingon/SV_caller_evaluation_project/ShinyApp/SV_evaluation_gam_v6"
#p <- profmem({
load("./data/gamsen_callers.RData")
load("./data/gampre_off_callers.RData")
load("./data/gamF1_score_callers.RData")
load("./data/gamsen_SVTYPE_callers.RData")
load("./data/gampre_off_SVTYPE_callers.RData")
load("./data/gamF1_score_SVTYPE_callers.RData")

load("./data/gamsen_UnionIntersect_callers.RData")
load("./data/gampre_off_UnionIntersect_callers.RData")
load("./data/gamF1_score_UnionIntersect_callers.RData")

load("./data/gamsen_SVTYPE_union_intersect_callers1.RData")
load("./data/gamsen_SVTYPE_union_intersect_callers2.RData")
load("./data/gamsen_SVTYPE_union_intersect_callers3.RData")
load("./data/gampre_off_SVTYPE_union_intersect_callers1.RData")
load("./data/gampre_off_SVTYPE_union_intersect_callers2.RData")
load("./data/gamF1_score_SVTYPE_union_intersect_callers1.RData")
load("./data/gamF1_score_SVTYPE_union_intersect_callers2.RData")

# load("./data/gamsen_SVTYPE_union_intersect_callers.RData")
# load("./data/gampre_off_SVTYPE_union_intersect_callers.RData")
# load("./data/gamF1_score_SVTYPE_union_intersect_callers.RData")
#})
# mem <- mem_used()
# mem_2 <- profmem_end()
#load("./data/gamsen.RData")
#load("./data/gampre.RData")
# load("./data/gamsen_callers_paper.RData")
# load("./data/gampre_off_callers_paper.RData")
# load("./data/gamF1_score_callers_paper.RData")
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

# #inputs on top
# input_X_axis2 <- "VAF"
# input_measurements2 <- "Sensitivity"
# input_type2 <- "DEL"
# ##inputs in sliders on side bar
# ### input_VAF3 will be [0.05,1]
# input_T_coverage2 <- 60
# input_N_coverage2 <- 30
# input_BND_threshold2 <- 200
# ##inputs in checkboxes on side bar
# input_SVCaller2 <- c("Manta")
# #input_SVCaller3.1 <- c("Manta","Lumpy")
# input_SVCaller2.1 <- c()
# #input_T_coverage3.1 <- c("30x","60x")
# input_T_coverage2.1 <- ""
# 
# T_coverage_label = paste0(c(input_T_coverage2,60,30),"x")
# line_type <- c("solid","dashed","dotted")
# names(line_type) <- T_coverage_label
# 
# data <- data.frame(Caller = input_SVCaller2,
#                       T_coverage = rep(c(input_T_coverage2,60,30), each=length(SV_caller)),
#                       N_coverage = input_N_coverage2,
#                       VAF = rep(seq(0.05,1,0.01),each=length(T_coverage_label)*length(SV_caller)),
#                       BND_threshold = input_BND_threshold2)
# input_type <- input_type2
# SVCaller_name <- input_SVCaller2
# df.Sensitivity <- c()
# for (SVTYPE in SVTYPE_all){
#   newdata <- data
#   assign("df.Sensitivity_caller",  data.frame(newdata,
#                                       predict(eval(parse(text=paste0("gamsen_", SVCaller_name, SVTYPE))),newdata,type = "response",se.fit = T,unconditional = TRUE),
#                                       T_coverage_label = paste0(newdata$T_coverage,"x"),
#                                       N_coverage_label = paste0(newdata$N_coverage,"x"),
#                                       VAF_label = paste0(newdata$VAF),
#                                       BND_label = paste0(newdata$BND_threshold),
#                                       SVTYPE = SVTYPE,
#                                       row.names = c(1:(nrow(newdata)))))
#   df.Sensitivity <- rbind(df.Sensitivity, df.Sensitivity_caller)
# }
# xlabel <- "Tumor purity (VAF)"
# legend_label <- "Tumor coverage"
# Tcov_var <- c(paste0(input_T_coverage2,"x"),input_T_coverage2.1)
# Ncov_var <- paste0(input_N_coverage2,"x")
# VAF_var <- paste0(rep(seq(0,1,0.01)))
# BND_var <- paste0(input_BND_threshold2)
# index <- 8
# 
# tmp1 <- df.Sensitivity
# input_objective2 <- 0.7
# obj_row <- c()
# if(input_X_axis2 %in% c("VAF","N_coverage","BND_threshold")){
#   #for(i in 1: length(SV_caller)){
#     for(j in 1: length(T_coverage_label)){
#       tmp = tmp1[(tmp1$Caller %in% SVCaller_name) &
#                    (tmp1$T_coverage_label %in% T_coverage_label[j]) &
#                    (tmp1$N_coverage_label %in% Ncov_var) &
#                    (tmp1$VAF_label %in% VAF_var) &
#                    (tmp1$BND_threshold %in% BND_var),]
#       obj_row <- c(obj_row, rownames(tmp[tmp$fit>input_objective2,][1,]))
#     }
#   #}
# }else if(input_X_axis2 == "T_coverage"){
#   #for(i in 1: length(SV_caller)){
#     for(k in 1: length(unique(VAF_label))){
#       tmp = tmp1[(tmp1$Caller %in% SVCaller_name) &
#                    (tmp1$T_coverage_label %in% Tcov_var) &
#                    (tmp1$N_coverage_label %in% Ncov_var) &
#                    (tmp1$VAF_label %in% unique(VAF_label)[k]) &
#                    (tmp1$BND_threshold %in% BND_var),]
#       obj_row <- c(obj_row,rownames(tmp[tmp$fit>input_objective2,][1,]))
#     }
#  # }
# }
# 
# obj <- rep(FALSE,nrow(df.Sensitivity))
# obj[as.numeric(obj_row[obj_row!="NA"])] <- TRUE
# df.Sensitivity <- cbind(df.Sensitivity,obj)
# obj.y <- rep(NA,nrow(df.Sensitivity))
# obj.y[df.Sensitivity$obj] <- df.Sensitivity$fit[df.Sensitivity$obj]
# df.Sensitivity <- cbind(df.Sensitivity,obj.y)
# 
# # 
# # input_union <- c()
# # if(length(input_SVCaller3.1)==1){
# #   input_union <- input_SVCaller3.1
# # }else{
# #   for(i in 1: length(input_SVCaller3.1)){
# #     for(j in 1: length(input_SVCaller3.1)){
# #       if(j!=i){
# #         input_union <- c(input_union,paste0(input_SVCaller3.1[i],input_SVCaller3.1[j],"Union"))
# #       }
# #     }
# #   }
# # }
# # 
# df = df.Sensitivity[(df.Sensitivity$SVTYPE %in% input_type2) &
#                       (df.Sensitivity$T_coverage_label %in% Tcov_var) &
#                       (df.Sensitivity$N_coverage_label %in% Ncov_var) &
#                       (df.Sensitivity$VAF_label %in% VAF_var),]
# 
# tmp <- cbind(df, SV_caller_label1[match(df$Caller,SV_caller)])
# table <- cbind(tmp[tmp$obj, c(12, ncol(tmp), 2:6)])
# colnames(table) <- c("SV caller","SVTYPE","Tumour coverage","Normal coverage","Tumour purity/VAF","Breakpoint precision threshold","Sensitivity")
# 
# 
# ggplot(data=df, aes(x=eval(parse(text=input_X_axis2)), y = fit, group = interaction(SVTYPE,eval(parse(text=colnames(df)[index]))))) +
#   geom_ribbon(aes(ymin = fit-1.96*se.fit,ymax=fit+1.96*se.fit), fill="grey70")+
#   geom_line(data = df, aes(x=eval(parse(text=input_X_axis2)), y = fit,
#                            group = interaction(SVTYPE,eval(parse(text=colnames(df)[index]))),
#                            color = SVTYPE, linetype = eval(parse(text=colnames(df)[index]))), size =2)+
#   geom_hline(yintercept = input_objective2)+
#   geom_point(data=df, aes(x=eval(parse(text=input_X_axis2)), y = obj.y))+
#   #geom_text(aes(x=eval(parse(text=input_X_axis3)), y = obj.y+0.05, label=round(obj.y,2)))+
#   #ggtitle(paste("Sensitivity","across",xlabel))+
#   scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#   scale_x_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#   #scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$Caller],labels = SV_caller_label[SV_caller %in% df$Caller])+
#   scale_color_manual(values=color_SVTYPE[names(color_SVTYPE) %in% df$SVTYPE])+
#   scale_linetype_manual(values=line_type[names(line_type) %in% unique(df[,index])])+
#   labs(y=input_measurements2[1], x = xlabel, linetype = colnames(df)[index])+
#   theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
#         legend.title=element_text(size=14),legend.text=element_text(size=14))
# 
# 
# 
