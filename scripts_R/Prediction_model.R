require(mgcv)

dat <- read.csv("./data_SV/summary_stat.csv")
gamsen = gam( Sensitivity ~ s(VAF,by = as.factor(Caller), k=6) + T_coverage + N_coverage + BND_threshold , data=dat, family=betar(link="logit"), method="REML" )
dat$Precision_offset <- dat$Precision
dat$Precision_offset[dat$Precision==1] = 0.99999
gampre_off = gam( Precision_offset ~ s(VAF,by=as.factor(Caller), k=6) + T_coverage + N_coverage
                  + s(BND_threshold,by=as.factor(Caller), k=5) , data=dat, family=betar(link="logit"), method="REML" )

datUnion <- read.csv("./data_SV/summary_stat_union.csv")
datIntersect <- read.csv("./data_SV/summary_stat_intersect.csv")
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


