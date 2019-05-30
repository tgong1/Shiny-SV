require(mgcv)

manta <- read.csv("./data_SV/summary_stat_Manta.csv")
lumpy <- read.csv("./data_SV/summary_stat_Lumpy.csv")
gridss <- read.csv("./data_SV/summary_stat_GRIDSS.csv")
dat <- rbind( cbind(Caller="Manta",manta), cbind(Caller="Lumpy",lumpy), cbind(Caller="GRIDSS",gridss) )
gamsen = gam( Sensitivity ~ s(VAF,by = as.factor(Caller), k=6) + T_coverage + N_coverage + BND_threshold , data=dat, family=betar(link="logit"), method="REML" )
dat$Precision_offset <- dat$Precision
dat$Precision_offset[dat$Precision==1] = 0.99999
gampre_off = gam( Precision_offset ~ s(VAF,by=as.factor(Caller), k=6) + T_coverage + N_coverage
                  + s(BND_threshold,by=as.factor(Caller), k=5) , data=dat, family=betar(link="logit"), method="REML" )


MantaLumpyUnion <- read.csv("./data_SV/summary_stat_MantaLumpyUnion.csv")
MantaLumpyIntersect <- read.csv("./data_SV/summary_stat_MantaLumpyIntersect.csv")
MantaGRIDSSUnion <- read.csv("./data_SV/summary_stat_MantaGRIDSSUnion.csv")
MantaGRIDSSIntersect <- read.csv("./data_SV/summary_stat_MantaGRIDSSIntersect.csv")
LumpyMantaUnion <- read.csv("./data_SV/summary_stat_LumpyMantaUnion.csv")
LumpyMantaIntersect <- read.csv("./data_SV/summary_stat_LumpyMantaIntersect.csv")
LumpyGRIDSSUnion <- read.csv("./data_SV/summary_stat_LumpyGRIDSSUnion.csv")
LumpyGRIDSSIntersect <- read.csv("./data_SV/summary_stat_LumpyGRIDSSIntersect.csv")
GRIDSSMantaUnion <- read.csv("./data_SV/summary_stat_GRIDSSMantaUnion.csv")
GRIDSSMantaIntersect <- read.csv("./data_SV/summary_stat_GRIDSSMantaIntersect.csv")
GRIDSSLumpyUnion <- read.csv("./data_SV/summary_stat_GRIDSSLumpyUnion.csv")
GRIDSSLumpyIntersect <- read.csv("./data_SV/summary_stat_GRIDSSLumpyIntersect.csv")

datUnion <- rbind( cbind(Caller= "MantaLumpyUnion",MantaLumpyUnion),
                       cbind(Caller= "MantaGRIDSSUnion",MantaGRIDSSUnion),
                       cbind(Caller= "LumpyMantaUnion",LumpyMantaUnion),
                       cbind(Caller= "LumpyGRIDSSUnion",LumpyGRIDSSUnion),
                       cbind(Caller= "GRIDSSMantaUnion",GRIDSSMantaUnion),
                       cbind(Caller= "GRIDSSLumpyUnion",GRIDSSLumpyUnion))
datIntersect <- rbind( cbind(Caller="MantaLumpyIntersect",MantaLumpyIntersect),
                          cbind(Caller="MantaGRIDSSIntersect",MantaGRIDSSIntersect),
                          cbind(Caller="LumpyMantaIntersect",LumpyMantaIntersect),
                          cbind(Caller="LumpyGRIDSSIntersect",LumpyGRIDSSIntersect),
                          cbind(Caller="GRIDSSMantaIntersect",GRIDSSMantaIntersect),
                          cbind(Caller="GRIDSSLumpyIntersect",GRIDSSLumpyIntersect))

gamsenUnion = gam( Sensitivity ~ s(VAF,by = as.factor(Caller), k=6) + T_coverage + N_coverage
              + BND_threshold , data=datUnion, family=betar(link="logit"), method="REML" )
gamsenIntersect = gam( Sensitivity ~ s(VAF,by = as.factor(Caller), k=6) + T_coverage + N_coverage
                   + BND_threshold , data=datIntersect, family=betar(link="logit"), method="REML" )

datUnion$Precision_offset <- datUnion$Precision
datUnion$Precision_offset[datUnion$Precision==1] = 0.99999
gampre_offUnion = gam( Precision_offset ~ s(VAF,by=as.factor(Caller), k=6) + T_coverage + N_coverage
                       + s(BND_threshold,by=as.factor(Caller), k=5) , data=datUnion, family=betar(link="logit"), method="REML" )

datIntersect$Precision_offset <- datIntersect$Precision
datIntersect$Precision_offset[datIntersect$Precision==1] = 0.99999
gampre_offIntersect = gam( Precision_offset ~ s(VAF,by=as.factor(Caller), k=6) + T_coverage + N_coverage
                          + s(BND_threshold,by=as.factor(Caller), k=5) , data=datIntersect, family=betar(link="logit"), method="REML" )

save(gamsen, gampre_off, gamsenUnion, gamsenIntersect, gampre_offUnion, gampre_offIntersect, file = "./data/gam.RData")


