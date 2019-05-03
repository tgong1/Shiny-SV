library(ggplot2)
library(RColorBrewer)
library(colorRamps)
library(plyr)
require(mgcv)

#directory <- "/Volumes/personal/tingon/SV_caller_evaluation_project/ShinyApp/SV_evaluation_gam"
#setwd(directory)
# summary_stat_Manta1 <- read.csv("./data/Manta_summary_stat1.csv")
# summary_stat_Manta1 <- summary_stat_Manta1[summary_stat_Manta1$N_coverage!=120,]
# summary_stat_Lumpy1 <- read.csv("./data/Lumpy_summary_stat1.csv")
# summary_stat_GRIDSS1 <- read.csv("./data/GRIDSS_summary_stat1.csv")
# 
# summary_stat_Manta2 <- read.csv("./data/Manta_summary_stat2.csv")
# summary_stat_Lumpy2 <- read.csv("./data/Lumpy_summary_stat2.csv")
# summary_stat_GRIDSS2 <- read.csv("./data/GRIDSS_summary_stat2.csv")
# 
# summary_stat_Manta <- rbind(summary_stat_Manta1,summary_stat_Manta2)
# summary_stat_Lumpy <- rbind( summary_stat_Lumpy1,summary_stat_Lumpy2)
# summary_stat_GRIDSS <- rbind(summary_stat_GRIDSS1,summary_stat_GRIDSS2)
# 
# write.csv(summary_stat_Manta,"summary_stat_Manta.csv",row.names = FALSE)
# write.csv(summary_stat_Lumpy,"summary_stat_Lumpy.csv",row.names = FALSE)
# write.csv(summary_stat_GRIDSS,"summary_stat_GRIDSS.csv",row.names = FALSE)

#summary_stat_Manta <- read.csv("./data/summary_stat_Manta.csv")
#summary_stat_Lumpy <- read.csv("./data/summary_stat_Lumpy.csv")
#summary_stat_GRIDSS <- read.csv("./data/summary_stat_GRIDSS.csv")

#summary_stat_Manta$Precision_offset <- summary_stat_Manta$Precision
#summary_stat_Manta$Precision_offset[summary_stat_Manta$Precision==1] = 0.99999
#summary_stat_GRIDSS$Precision_offset <- summary_stat_GRIDSS$Precision
#summary_stat_GRIDSS$Precision_offset[summary_stat_GRIDSS$Precision==1] = 0.99999

# gam.fit.Sensitivity_Manta <- gam(Sensitivity ~ te(VAF,k=5) + te(T_coverage,k=5) + te(N_coverage,k=5) + BND_threshold + ti(VAF,T_coverage,k=4)+ ti(T_coverage,N_coverage,k=3), 
#                                  family=binomial(link="logit"), data=summary_stat_Manta, weights=Truth)
# gam.fit.Precision_Manta <- gam(Precision ~ te(VAF,k=5) + te(T_coverage,k=5) + te(N_coverage,k=5) + BND_threshold + ti(VAF,T_coverage,k=4)+ ti(T_coverage,N_coverage,k=3) , 
#                                family=binomial(link="logit"), data=summary_stat_Manta, weights=TP_tmp+FP)
# 
# gam.fit.Sensitivity_Lumpy <- gam(Sensitivity ~ te(VAF,k=5) + te(T_coverage,k=5) + te(N_coverage,k=5) + BND_threshold+ ti(VAF,T_coverage,k=4)+ ti(T_coverage,N_coverage,k=3) , 
#                                  family=binomial(link="logit"), data=summary_stat_Lumpy, weights=Truth)
# gam.fit.Precision_Lumpy <- gam(Precision ~ te(VAF,k=5) + te(T_coverage,k=5) + te(N_coverage,k=5)+ BND_threshold+ ti(VAF,T_coverage,k=4) + ti(T_coverage,N_coverage,k=3) , 
#                                family=binomial(link="logit"), data=summary_stat_Lumpy, weights=TP_tmp+FP)
# 
# gam.fit.Sensitivity_GRIDSS <- gam(Sensitivity ~ te(VAF,k=5) + te(T_coverage,k=5) + te(N_coverage,k=5) + BND_threshold+ ti(VAF,T_coverage,k=4)+ ti(T_coverage,N_coverage,k=3) , 
#                                   family=binomial(link="logit"), data=summary_stat_GRIDSS, weights=Truth)
# gam.fit.Precision_GRIDSS <- gam(Precision ~ te(VAF,k=5) + te(T_coverage,k=5) + te(N_coverage,k=5)+ BND_threshold+ ti(VAF,T_coverage,k=4) + ti(T_coverage,N_coverage,k=3) , 
#                                 family=binomial(link="logit"), data=summary_stat_GRIDSS, weights=TP_tmp+FP)

#gam.fit.Sensitivity_Manta <- gam(Sensitivity ~ te(VAF)  + T_coverage + N_coverage + BND_threshold , data=summary_stat_Manta, family=betar(link="logit"), method="REML" )
#gam.fit.Sensitivity_Lumpy <- gam(Sensitivity ~ te(VAF)  + T_coverage + N_coverage + BND_threshold , data=summary_stat_Lumpy, family=betar(link="logit"), method="REML" )
#gam.fit.Sensitivity_GRIDSS <- gam(Sensitivity ~ te(VAF)  + T_coverage + N_coverage + BND_threshold , data=summary_stat_GRIDSS, family=betar(link="logit"), method="REML" )

#gam.fit.Sensitivity_Manta <- gam(Sensitivity ~ s(VAF, k=5, bs = "cr") + T_coverage + N_coverage + BND_threshold , data=summary_stat_Manta, family=betar(link="logit"), method="REML" )
#gam.fit.Sensitivity_Lumpy <- gam(Sensitivity ~ s(VAF, k=5, bs = "cr") + T_coverage + N_coverage + BND_threshold , data=summary_stat_Lumpy, family=betar(link="logit"), method="REML" )
#gam.fit.Sensitivity_GRIDSS <- gam(Sensitivity ~ s(VAF, k=5, bs = "cr") + T_coverage + N_coverage + BND_threshold , data=summary_stat_GRIDSS, family=betar(link="logit"), method="REML" )

#gam.fit.Precision_Manta <- gam(Precision_offset ~ te(VAF)  + T_coverage + N_coverage + te(BND_threshold) , data=summary_stat_Manta, family=betar(link="logit"), method="REML" )
#gam.fit.Precision_Lumpy <- gam(Precision ~ te(VAF)  + T_coverage + N_coverage + te(BND_threshold) , data=summary_stat_Lumpy, family=betar(link="logit"), method="REML" )
#gam.fit.Precision_GRIDSS <- gam(Precision_offset ~ te(VAF)  + T_coverage + N_coverage + te(BND_threshold) , data=summary_stat_GRIDSS, family=betar(link="logit"), method="REML" )

#gam.fit.Precision_Manta <- gam(Precision_offset ~ s(VAF, k=5, bs = "cr") + T_coverage + N_coverage + s(BND_threshold, k=5, bs = "cr") , data=summary_stat_Manta, family=betar(link="logit"), method="REML" )
#gam.fit.Precision_Lumpy <- gam(Precision ~ s(VAF, k=5, bs = "cr") + T_coverage + N_coverage + s(BND_threshold, k=5, bs = "cr") , data=summary_stat_Lumpy, family=betar(link="logit"), method="REML" )
#gam.fit.Precision_GRIDSS <- gam(Precision_offset ~ s(VAF, k=5, bs = "cr") + T_coverage + N_coverage + s(BND_threshold, k=5, bs = "cr") , data=summary_stat_GRIDSS, family=betar(link="logit"), method="REML" )


manta <- read.csv("./data/summary_stat_Manta.csv")
lumpy <- read.csv("./data/summary_stat_Lumpy.csv")
gridss <- read.csv("./data/summary_stat_GRIDSS.csv")
dat <- rbind( cbind(Caller="Manta",manta), cbind(Caller="Lumpy",lumpy), cbind(Caller="GRIDSS",gridss) )
gamsen = gam( Sensitivity ~ s(VAF,by = as.factor(Caller), k=5, bs = "cr") + T_coverage + N_coverage + BND_threshold , data=dat, family=betar(link="logit"), method="REML" )
dat$Precision_offset <- dat$Precision
dat$Precision_offset[dat$Precision==1] = 0.99999
gampre_off = gam( Precision_offset ~ s(VAF,by=as.factor(Caller), k=5, bs = "cr") + T_coverage + N_coverage + s(BND_threshold,by=as.factor(Caller), k=5, bs = "cr") , data=dat, family=betar(link="logit"), method="REML" )


color_SVCaller <- brewer.pal(9, "Set1")[c(2:8)]
SV_caller <- c("Manta","Lumpy","GRIDSS","BreakDancer","CNVKit","Pindel","SvABA")
names(color_SVCaller) <- SV_caller

# input_VAF3 <- 0.5
# input_T_coverage3 <- 85
# input_N_coverage3 <- 40
# input_BND_threshold3 <- 5
# #dataInput3()[[1]] #df.Sensitivity
# input_SVCaller3 <- c("Manta","Lumpy")
# 
# ################## x axis VAF #########
# input_X_axis3 <- "VAF"
# input_measurements3 <- "Sensitivity"
# input_T_coverage3.1 <- "30x"
# input_N_coverage3.1 <- "30x"
# 
# T_coverage_label = paste0(c(input_T_coverage3,60,30),"x")
# line_type <- c("solid","dashed","dotted")
# names(line_type) <- T_coverage_label
# N_coverage_label = paste0(c(input_N_coverage3,60,30),"x")
# #point_shape <- c(0,1,2)
# point_shape <- c(16,15,17)
# names(point_shape) <- N_coverage_label
# 
# newdata <- data.frame(T_coverage = c(input_T_coverage3,60,30),
#                       N_coverage = rep(c(input_N_coverage3,60,30),each=length(T_coverage_label)),
#                       VAF = rep(seq(0,1,0.01),each=length(T_coverage_label)*length(T_coverage_label)),
#                       BND_threshold=input_BND_threshold3)
# xlabel <- "Tumor purity (VAF)"
# df.Sensitivity_Manta <- data.frame(newdata,
#                                    #predict(gam.fit.Sensitivity_Manta,newdata,type = "response",se.fit = T),
#                                    predict(gam.fit.Sensitivity_Manta,newdata,type = "response",se.fit = T,unconditional = TRUE),
#                                    Caller=rep("Manta",each=nrow(newdata)),
#                                    T_coverage_label = paste0(newdata$T_coverage,"x"),
#                                    N_coverage_label = paste0(newdata$N_coverage,"x"),
#                                    
#                                    row.names = c(1:(nrow(newdata))))
# #df.Sensitivity_Manta <- df.Sensitivity_Manta[df.Sensitivity_Manta$T_coverage %in% c(input_T_coverage3,input_T_coverage3.1),]
# df.Sensitivity <- df.Sensitivity_Manta
# 
# df = df.Sensitivity[(df.Sensitivity$Caller %in% input_SVCaller3) & 
#                       (df.Sensitivity$T_coverage_label %in% c(paste0(input_T_coverage3,"x"),input_T_coverage3.1)) &
#                       (df.Sensitivity$N_coverage_label %in% c(paste0(input_N_coverage3,"x"),input_N_coverage3.1)),]
# 
# ggplot(data=df, aes(x=eval(parse(text=input_X_axis3)), y=fit, group = interaction(Caller,eval(parse(text=colnames(df)[8])),eval(parse(text=colnames(df)[9]))))) +
#       geom_ribbon(aes(ymin=fit-1.96*se.fit,ymax=fit+1.96*se.fit), fill="grey70")+
#       geom_line(data=df, aes(x=eval(parse(text=input_X_axis3)), y=fit, group = interaction(Caller,eval(parse(text=colnames(df)[8])),eval(parse(text=colnames(df)[9]))),
#                              color = Caller, linetype = eval(parse(text=colnames(df)[8]))))+
#       geom_point(data=df, aes(x=eval(parse(text=input_X_axis3)), y=fit, group = interaction(Caller,eval(parse(text=colnames(df)[8])),eval(parse(text=colnames(df)[9]))),
#                               color = Caller, shape = eval(parse(text=colnames(df)[9]))))+
#       ggtitle(paste("Sensitivity","across",xlabel))+
#       scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#       #scale_x_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#       scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$Caller])+
#       scale_linetype_manual(values=line_type[names(line_type) %in% unique(df[,8])])+
#       scale_shape_manual(values=point_shape[names(point_shape) %in% unique(df[,9])])+
#       labs(y=input_measurements3[1], x = xlabel, shape = colnames(df)[9], linetype = colnames(df)[8])+
#       theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
#             legend.title=element_text(size=14),legend.text=element_text(size=14))
# 
# 
# ################## x axis Tumor coverage #########
# 
# input_X_axis3 <- "T_coverage"
# input_measurements3 <- "Sensitivity"
# input_VAF3  <- 0.5
# input_VAF3.1  <- c(0.2,0.8)
# 
# VAF_label = paste0(c(input_VAF3,0.2,0.8))
# VAF_label = paste0(c(input_VAF3,0.2,0.8)*100,"%")
# line_type <- c("solid","dashed","dotted")
# names(line_type) <- VAF_label
# 
# newdata <- data.frame(T_coverage = rep(c(20:90),each=length(VAF_label)),
#                       N_coverage = input_N_coverage3,
#                       VAF = c(input_VAF3,0.2,0.8),
#                       BND_threshold = input_BND_threshold3)
# xlabel <- "Tumor coverage"
# df.Sensitivity_Manta <- data.frame(newdata,
#                                    #predict(gam.fit.Sensitivity_Manta,newdata,type = "response",se.fit = T),
#                                    predict(gam.fit.Sensitivity_Manta,newdata,type = "response",se.fit = T,unconditional = TRUE),
#                                    Caller=rep("Manta",each=nrow(newdata)),
#                                    VAF_label = paste0(newdata$VAF*100,"%"),
#                                    row.names = c(1:(nrow(newdata))))
# #df.Sensitivity_Manta <- df.Sensitivity_Manta[df.Sensitivity_Manta$T_coverage %in% c(input_T_coverage3,input_T_coverage3.1),]
# df.Sensitivity <- df.Sensitivity_Manta
# 
# df = df.Sensitivity[(df.Sensitivity$Caller %in% input_SVCaller3) & 
#                       (df.Sensitivity$VAF_label %in% c(paste0(input_VAF3*100,"%"),paste0(input_VAF3.1*100,"%"))),]
# 
# 
# ggplot(data=df, aes(x=eval(parse(text=input_X_axis3)), y=fit, group = interaction(Caller,eval(parse(text=colnames(df)[8]))))) +
#   geom_ribbon(aes(ymin=fit-1.96*se.fit,ymax=fit+1.96*se.fit), fill="grey70")+
#   geom_line(data=df, aes(x=eval(parse(text=input_X_axis3)), y=fit, group = interaction(Caller,eval(parse(text=colnames(df)[8]))),
#                          color = Caller, linetype = eval(parse(text=colnames(df)[8]))))+
#   ggtitle(paste("Sensitivity","across",xlabel))+
#   scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#   #scale_x_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#   scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$Caller])+
#   scale_linetype_manual(values=line_type[names(line_type) %in% unique(df[,8])])+
# 
#   labs(y=input_measurements3[1], x = xlabel, linetype = colnames(df)[8])+
#   theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
#         legend.title=element_text(size=14),legend.text=element_text(size=14))
# 
# 
################## x axis VAF #########
# input_X_axis3 <- "VAF"
# input_measurements3 <- "Sensitivity"
# input_T_coverage3.1 <- "30x"
# input_N_coverage3.1 <- "30x"
# input_VAF3.1 <- "0.2"
# 
# T_coverage_label = paste0(c(input_T_coverage3,60,30),"x")
# line_type <- c("solid","dashed","dotted")
# names(line_type) <- T_coverage_label
# #N_coverage_label = paste0(c(input_N_coverage3,60,30),"x")
# #point_shape <- c(0,1,2)
# #point_shape <- c(16,15,17)
# #names(point_shape) <- N_coverage_label
# 
# #newdata <- data.frame(T_coverage = c(input_T_coverage3,60,30),
# #                      N_coverage = rep(c(input_N_coverage3,60,30),each=length(T_coverage_label)),
# #                      VAF = rep(seq(0,1,0.01),each=length(T_coverage_label)*length(T_coverage_label)),
# #                      BND_threshold=input_BND_threshold3)
# 
# newdata <- data.frame(Caller = SV_caller[1:3],
#                       T_coverage = rep(c(input_T_coverage3,60,30), each=length(SV_caller[1:3])),
#                       N_coverage = input_N_coverage3,
#                       VAF = rep(seq(0,1,0.01),each=length(T_coverage_label)*length(SV_caller[1:3])),
#                       BND_threshold = input_BND_threshold3)
# 
# xlabel <- "Tumor purity (VAF)"
# legend_label <- "Tumor coverage"
# Tcov_var <- c(paste0(input_T_coverage3,"x"),input_T_coverage3.1)
# Ncov_var <- c(paste0(input_N_coverage3,"x"),input_N_coverage3.1)
# VAF_var <- paste0(newdata$VAF)
# index <- 8
# 
# df.Sensitivity <- data.frame(newdata,
#                                    predict(gamsen,newdata,type = "response",se.fit = T,unconditional = TRUE),
#                                    #Caller=rep("Manta",each=nrow(newdata)),
#                                    T_coverage_label = paste0(newdata$T_coverage,"x"),
#                                    N_coverage_label = paste0(newdata$N_coverage,"x"),
#                                    VAF_label = paste0(newdata$VAF),
#                                    row.names = c(1:(nrow(newdata))))
# 
# # df.Sensitivity_Manta <- data.frame(newdata,
# #                                    #predict(gam.fit.Sensitivity_Manta,newdata,type = "response",se.fit = T),
# #                                    predict(gam.fit.Sensitivity_Manta,newdata,type = "response",se.fit = T,unconditional = TRUE),
# #                                    Caller=rep("Manta",each=nrow(newdata)),
# #                                    T_coverage_label = paste0(newdata$T_coverage,"x"),
# #                                    N_coverage_label = paste0(newdata$N_coverage,"x"),
# #                                    VAF_label = paste0(newdata$VAF),
# #                                    row.names = c(1:(nrow(newdata))))
# #df.Sensitivity_Manta <- df.Sensitivity_Manta[df.Sensitivity_Manta$T_coverage %in% c(input_T_coverage3,input_T_coverage3.1),]
# #df.Sensitivity <- df.Sensitivity_Manta
# 
# df = df.Sensitivity[(df.Sensitivity$Caller %in% input_SVCaller3) &
#                       (df.Sensitivity$T_coverage_label %in% Tcov_var) &
#                       (df.Sensitivity$N_coverage_label %in% Ncov_var) &
#                       (df.Sensitivity$VAF_label %in% VAF_var),]
# 
# ggplot(data=df, aes(x=eval(parse(text=input_X_axis3)), y = fit, group = interaction(Caller,eval(parse(text=colnames(df)[index]))))) +
#   geom_ribbon(aes(ymin = fit-1.96*se.fit,ymax=fit+1.96*se.fit), fill="grey70")+
#   geom_line(data = df, aes(x=eval(parse(text=input_X_axis3)), y = fit,
#                         group = interaction(Caller,eval(parse(text=colnames(df)[index]))),
#                          color = Caller, linetype = eval(parse(text=colnames(df)[index]))))+
#   ggtitle(paste("Sensitivity","across",xlabel))+
#   scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#   #scale_x_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#   scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$Caller])+
#   scale_linetype_manual(values=line_type[names(line_type) %in% unique(df[,index])])+
# 
#   labs(y=input_measurements3[1], x = xlabel, linetype = colnames(df)[index])+
#   theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
#         legend.title=element_text(size=14),legend.text=element_text(size=14))
# 
# # 
# # 
# # line_type <- c("solid","longdash","twodash","dashed","dotdash","dotted")
# # names(line_type) <- c("90x","75x","60x","45x","30x","20x")
# # point_shape <- c(15,16,17,0,1,2)
# # names(point_shape) <- c("90x","75x","60x","45x","30x","15x")
# 






