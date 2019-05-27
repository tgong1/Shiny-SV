library(ggplot2)
library(ggsci)

load("./data/gam.RData")
#color_SVCaller <- c(pal_npg("nrc")(10)[1],pal_npg("nrc")(10)[3:10],pal_jco()(6))
color_SVCaller <- c(pal_npg("nrc")(10)[1],pal_npg("nrc")(10)[3:4],
                    pal_jco()(6)[1],pal_npg("nrc")(10)[6:7], pal_jco()(6)[2],
                    pal_npg("nrc")(10)[9:10],pal_npg("nrc")(10)[5],pal_npg("nrc")(10)[8], 
                    pal_jco()(6)[c(3,6,4,5)])
SV_caller <- c("Manta", "Lumpy", "GRIDSS",
               "MantaLumpyUnion", "MantaLumpyIntersect",
               "MantaGRIDSSUnion", "MantaGRIDSSIntersect",
               "LumpyMantaUnion","LumpyMantaIntersect",
               "LumpyGRIDSSUnion","LumpyGRIDSSIntersect",
               "GRIDSSMantaUnion", "GRIDSSMantaIntersect",
               "GRIDSSLumpyUnion", "GRIDSSLumpyIntersect")
names(color_SVCaller) <- SV_caller

SV_caller_label <- c("Manta", "Lumpy", "GRIDSS",
                     expression(Manta*union(Lumpy)), expression(Manta*intersect(Lumpy)),
                     expression(Manta*union(GRIDSS)), expression(Manta*intersect(GRIDSS)),
                     expression(Lumpy*union(Manta)), expression(Lumpy*intersect(Manta)),
                     expression(Lumpy*union(GRIDSS)), expression(Lumpy*intersect(GRIDSS)),
                     expression(GRIDSS*union(Manta)), expression(GRIDSS*intersect(Manta)),
                     expression(GRIDSS*union(Lumpy)), expression(GRIDSS*intersect(Lumpy)))


tweaks <- 
  tags$head(tags$style(HTML("
                                 .multicol {
                                 height: auto;
                                 -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                 -moz-column-count: 3;    /* Firefox */ 
                                 column-count: 3; 
                                 #-moz-column-fill: balanced;
                                 #-column-fill: balanced;
                                 } 
                                 div.checkbox{
                                 margin-top: 0px;
                                 margin-bottom: 10px;
                                 -webkit-margin-after: 0px; 
                                 }
                                 ")) 
  )


controls <-
  #list(h3("Multicolumn checkboxGroupInput"), 
  tags$div(align = 'left', 
           class = 'multicol', checkboxGroupInput(inputId = "SVCaller3.1",
                   label = ("                                                                       "),
                   # choiceNames = list(HTML("Manta","&#8746;","Lumpy"), HTML("Manta","&#8745;","Lumpy"),
                   #                    HTML("Manta","&#8746;","GRIDSS"), 
                   #                    #"Manta∪GRIDSS",
                   #                    HTML("Manta","&#8745;","GRIDSS"),
                   #                    #"Manta∩GRIDSS",
                   #                    HTML("Lumpy","&#8746;","Manta"), HTML("Lumpy","&#8745;","Manta"),
                   #                    HTML("Lumpy","&#8746;","GRIDSS"), HTML("Lumpy","&#8745;","GRIDSS"),
                   #                    HTML("GRIDSS","&#8746;","Manta"), HTML("GRIDSS","&#8745;","Manta"),
                   #                    HTML("GRIDSS","&#8746;","Lumpy"), HTML("GRIDSS","&#8745;","Lumpy")),
                   choiceNames = list(HTML("&#8746;","Lumpy"), HTML("&#8745;","Lumpy"),
                                      HTML("&#8746;","GRIDSS"), 
                                      #"Manta∪GRIDSS",
                                      HTML("&#8745;","GRIDSS"),
                                      #"Manta∩GRIDSS",
                                      HTML("&#8746;","Manta"), HTML("&#8745;","Manta"),
                                      HTML("&#8746;","GRIDSS"), HTML("&#8745;","GRIDSS"),
                                      HTML("&#8746;","Manta"), HTML("&#8745;","Manta"),
                                      HTML("&#8746;","Lumpy"), HTML("&#8745;","Lumpy")),
                   choiceValues = c("MantaLumpyUnion", "MantaLumpyIntersect",
                                    "MantaGRIDSSUnion", "MantaGRIDSSIntersect",
                                    "LumpyMantaUnion","LumpyMantaIntersect",
                                    "LumpyGRIDSSUnion","LumpyGRIDSSIntersect",
                                    "GRIDSSMantaUnion", "GRIDSSMantaIntersect",
                                    "GRIDSSLumpyUnion", "GRIDSSLumpyIntersect"),
                   selected = NULL))

controls <- checkboxGroupInput(inputId = "SVCaller3",
                   label = ("SV Caller(s)"),
                   choiceNames = list("Manta ", "Lumpy ", "GRIDSS "
                                      #   HTML("Manta","&#8746;","Lumpy"), HTML("Manta","&#8745;","Lumpy"),
                                      #   HTML("Manta","&#8746;","GRIDSS"), HTML("Manta","&#8745;","GRIDSS"),
                                      #   HTML("Lumpy","&#8746;","Manta"), HTML("Lumpy","&#8745;","Manta"),
                                      #   HTML("Lumpy","&#8746;","GRIDSS"), HTML("Lumpy","&#8745;","GRIDSS"),
                                      #  HTML("GRIDSS","&#8746;","Manta"), HTML("GRIDSS","&#8745;","Manta"),
                                      #  HTML("GRIDSS","&#8746;","Lumpy"), HTML("GRIDSS","&#8745;","Lumpy")
                   ),
                   choiceValues = c("Manta", "Lumpy", "GRIDSS"
                                    # "MantaLumpyUnion", "MantaLumpyIntersect",
                                    # "MantaGRIDSSUnion", "MantaGRIDSSIntersect",
                                    # "LumpyMantaUnion","LumpyMantaIntersect",
                                    # "LumpyGRIDSSUnion","LumpyGRIDSSIntersect",
                                    # "GRIDSSMantaUnion", "GRIDSSMantaIntersect",
                                    # "GRIDSSLumpyUnion", "GRIDSSLumpyIntersect"
                   ),
                   selected = "Manta",inline=TRUE)








