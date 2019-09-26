library(ggplot2)
library(ggsci)
library(RColorBrewer)
load("./data/gam.RData")
#color_SVCaller <- c(pal_npg("nrc")(10)[1],pal_npg("nrc")(10)[3:10],pal_jco()(6))
color_SVCaller <- c(pal_npg("nrc")(10)[1],pal_npg("nrc")(10)[3:4],
                    pal_jco()(6)[1],pal_npg("nrc")(10)[6:7], pal_jco()(6)[2],
                    pal_npg("nrc")(10)[9:10],pal_npg("nrc")(10)[5],pal_npg("nrc")(10)[8], 
                    pal_jco()(6)[c(3,6,4,5)])

color_SVCaller <- c(pal_npg("nrc")(10)[1],pal_npg("nrc")(10)[3:4],
                    pal_jco()(6)[1],pal_npg("nrc")(10)[6], 
                    pal_npg("nrc")(10)[5],pal_npg("nrc")(10)[8],
                    
                    pal_npg("nrc")(10)[7],pal_jco()(6)[2],
                    
                    pal_jco()(6)[c(3,6)],
                    
                    pal_npg("nrc")(10)[9:10],
                    
                    
                    pal_jco()(6)[c(4,5)])

#color_SVCaller <- c(brewer.pal(9, "Set1")[c(1:9)],pal_npg("nrc")(10))
single_SV_caller <- c("Manta","Lumpy","GRIDSS")
combine_SV_SVcaller <- c()
for (i in c(1:length(single_SV_caller))){
  combine_SV_SVcaller <- c(combine_SV_SVcaller,paste0(single_SV_caller[i],single_SV_caller[!(c(1:length(single_SV_caller)) %in% i)],"Union"),
                           paste0(single_SV_caller[i],single_SV_caller[!(c(1:length(single_SV_caller)) %in% i)],"Intersect"))
}
SV_caller <- c(single_SV_caller,combine_SV_SVcaller)
names(color_SVCaller) <- SV_caller

SV_caller_label <- c("Manta", "Lumpy", "GRIDSS",
                     expression(Manta*union(Lumpy)), expression(Manta*union(GRIDSS)), 
                     expression(Manta*intersect(Lumpy)), expression(Manta*intersect(GRIDSS)), 
                     expression(Lumpy*union(Manta)), expression(Lumpy*union(GRIDSS)), 
                     expression(Lumpy*intersect(Manta)), expression(Lumpy*intersect(GRIDSS)), 
                     expression(GRIDSS*union(Manta)), expression(GRIDSS*union(Lumpy)), 
                     expression(GRIDSS*intersect(Manta)), expression(GRIDSS*intersect(Lumpy)))

SV_caller_label2 <-c()
for (i in c(1:length(single_SV_caller))){
  SV_caller_label2 <- c(SV_caller_label2,paste0(single_SV_caller[i],"&#8746;",single_SV_caller[!(c(1:length(single_SV_caller)) %in% i)]),
                        paste0(single_SV_caller[i],"&#8745;",single_SV_caller[!(c(1:length(single_SV_caller)) %in% i)]))
}
SV_caller_label1 <- c(single_SV_caller, SV_caller_label2)
names(SV_caller_label1) <- SV_caller

# tweaks <- 
#   tags$head(tags$style(HTML("
#                                  .multicol {
#                                  height: auto;
#                                  -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
#                                  -moz-column-count: 3;    /* Firefox */ 
#                                  column-count: 3; 
#                                  #-moz-column-fill: balanced;
#                                  #-column-fill: balanced;
#                                  } 
#                                  div.checkbox{
#                                  margin-top: 0px;
#                                  margin-bottom: 10px;
#                                  -webkit-margin-after: 0px; 
#                                  }
#                                  ")) 
#   )
# 
# 
