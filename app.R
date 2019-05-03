library(shiny)
library(shinyjs)
library(gridExtra)
# Source helpers ----
source("helpers.R")

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Evaluation of Structural Variation (SV) calling for cancer genome"),
  sidebarLayout(
    sidebarPanel(
              # conditionalPanel(condition="input.tabselected==1", 
              #                     checkboxGroupInput(inputId = "SVCaller1",
              #                                        label=("Choose SV caller(s)"),
              #                                        choices = c("Manta","Lumpy","GRIDSS"),
              #                                        #choices = c("Manta","Lumpy","GRIDSS","BreakDancer","CNVKit","Pindel","SvABA"),
              #                                        selected = "Manta"),
              #                     sliderInput("VAF1", "Tumor purity (VAF):", 
              #                                 min = 0, max = 1, value = 0.5, step= 0.01),
              #                     sliderInput("T_coverage1", "Tumor coverage:", 
              #                                 min = 20, max = 90, value = 60, step= 0.1),
              #                     sliderInput("N_coverage1", "Normal coverage:", 
              #                                 min = 20, max = 90, value = 30, step= 0.1),
              #                     sliderInput("BND_threshold1", "Breakpoint threshold:", 
              #                                 min = 2, max = 200, value = 5, step= 1)),
                 conditionalPanel(condition="input.tabselected==2",
                                  h1("Installation"),
                                  p("The web app is available on GitHub, so you can install it from your R console:"),
                                  code('runGitHub("SV_simulation_evaluation", "tgong1")'),
                                  helpText("help text here"),
                                  p("Paper link",a("Shiny homepage.", href = "http://shiny.rstudio.com")),
                                  p("Contact", strong("strong"), "some words"),
                                  p("Contact", em("t.gong@garvan.org.au"), "some words")
                                  
                                  #fileInput("file",label = "Please input the bed file"),
                                  #numericInput(inputId = "bkpt_diff3", label = ("Type in required breakpoint resolution"), value = 100 ,min = 1, max = 2000,step=1)
                                  ),
                conditionalPanel(condition="input.tabselected==3", 
                                 checkboxGroupInput(inputId = "SVCaller3",
                                                    label=("Choose SV caller(s)"),
                                                    choices = c("Manta","Lumpy","GRIDSS"),
                                                    #choices = c("Manta","Lumpy","GRIDSS","BreakDancer","CNVKit","Pindel","SvABA"),
                                                    selected = "Manta"),
                                 sliderInput("VAF3", "Tumor purity/VAF:", 
                                             #min = 0, max = 100, value = 50, step= 1, post="%"),
                                            min = 0.05, max = 1, value = 0.5, step= 0.01),
                                 checkboxGroupInput("VAF3.1",
                                                    label="Tumor purity/VAF",
                                                    choices = c("0.2" = 0.2,"0.8"= 0.8),
                                                    selected = NULL),
                                 sliderInput("T_coverage3", "Tumor coverage:", 
                                             min = 20, max = 90, value = 60, step = 0.1, post = "x"),
                                 checkboxGroupInput("T_coverage3.1",
                                                    label="Tumor coverage",
                                                    choices = c("30x","60x"),
                                                    selected = NULL),
                                 sliderInput("N_coverage3", "Normal coverage:", 
                                             min = 15, max = 90, value = 30, step = 0.1, post = "x"),
                                 #checkboxGroupInput("N_coverage3.1",
                                 #                   label="Normal coverage",
                                 #                   choices = c("60x","30x"),
                                 #                   selected = "60x"),
                                 sliderInput("BND_threshold3", "Breakpoint precision threshold:", 
                                             min = 2, max = 200, value = 5, step= 1, post = "bp"))
                
                ),
    
    mainPanel(
      tabsetPanel(
        # tabPanel("Prediction",value = 1, 
        #          checkboxGroupInput("measurements1",
        #                             label="Please choose the evaluation measurement(s)",
        #                             choices = list("Sensitivity","Precision"),
        #                             selected = "Sensitivity",inline=TRUE),
        #          radioButtons("X_axis1",
        #                       label="Evaluation across",
        #                       choices = c("Tumor purity (VAF)" = "VAF","Tumor coverage" = "T_coverage","Normal coverage" = "N_coverage","Breakpoint resolution" = "BND_threshold"),
        #                       selected = "VAF",inline=TRUE),
        #          textOutput("txtOutput1"),
        #          plotOutput("Plot1")),
        tabPanel("Instruction",value = 2),
        tabPanel("Prediction",value = 3, 
                 checkboxGroupInput("measurements3",
                                    label="Please choose the evaluation measurement(s)",
                                    choices = list("Sensitivity","Precision"),
                                    selected = "Sensitivity",inline=TRUE),
                 radioButtons("X_axis3",
                              label="Evaluation across",
                              choices = c("Tumor purity/VAF" = "VAF","Tumor coverage" = "T_coverage","Normal coverage" = "N_coverage","Breakpoint precision threshold" = "BND_threshold"),
                              selected = "VAF",inline=TRUE),
                 textOutput("txtOutput3"),
                 plotOutput("Plot3")),
        id = "tabselected"
      )
    )
  )
)

server <- function(input,output,session)({
  # MeasureInput1 <- reactive({c("Sensitivity","Precision") %in% input$measurements1
  # })
  # observe({if(input$X_axis1 == "VAF"){
  #   disable("VAF1")
  #   enable("T_coverage1")
  #   enable("N_coverage1")
  #   enable("BND_threshold1")
  # }else if(input$X_axis1 == "T_coverage"){
  #   enable("VAF1")
  #   disable("T_coverage1")
  #   enable("N_coverage1")
  #   enable("BND_threshold1")
  # }else if(input$X_axis1 == "N_coverage"){
  #   enable("T_coverage1")
  #   disable("N_coverage1")
  #   enable("VAF1")
  #   enable("BND_threshold1")
  # }else if(input$X_axis1 == "BND_threshold"){
  #   enable("T_coverage1")
  #   enable("N_coverage1")
  #   enable("VAF1")
  #   disable("BND_threshold1")
  # }})
  
  MeasureInput3 <- reactive({c("Sensitivity","Precision") %in% input$measurements3
  })
  observe({if(input$X_axis3 == "VAF"){
    disable("VAF3")
    disable("VAF3.1")
    enable("T_coverage3")
    enable("T_coverage3.1")
    enable("N_coverage3")
    enable("BND_threshold3")
  }else if(input$X_axis3 == "T_coverage"){
    enable("VAF3")
    enable("VAF3.1")
    disable("T_coverage3")
    disable("T_coverage3.1")
    enable("N_coverage3")
    enable("BND_threshold3")
  }else if(input$X_axis3 == "N_coverage"){
    enable("VAF3")
    disable("VAF3.1")
    enable("T_coverage3")
    enable("T_coverage3.1")
    disable("N_coverage3")
    enable("BND_threshold3")
  }else if(input$X_axis3 == "BND_threshold"){
    enable("VAF3")
    disable("VAF3.1")
    enable("T_coverage3")
    enable("T_coverage3.1")
    enable("N_coverage3")
    disable("BND_threshold3")
  }})
  
  
  # dataInput1 <- reactive({
  #   df.Sensitivity_Manta <- c()
  #   df.Sensitivity_Lumpy <- c()
  #   df.Sensitivity_GRIDSS <- c()
  #   df.Precision_Manta <- c()
  #   df.Precision_Lumpy <- c()
  #   df.Precision_GRIDSS <- c()
  #   if(input$X_axis1 == "VAF"){
  #     newdata <- data.frame(T_coverage = input$T_coverage1,
  #                           N_coverage = input$N_coverage1,
  #                           VAF = seq(0.05,1,0.01),
  #                           BND_threshold=input$BND_threshold1)
  #     xlabel <- "Tumor purity (VAF)"
  #   }else if(input$X_axis1 == "T_coverage"){
  #     newdata <- data.frame(T_coverage = c(20:90),
  #                           N_coverage = input$N_coverage1,
  #                           VAF = input$VAF1,
  #                           BND_threshold=input$BND_threshold1)
  #     xlabel <- "Tumor coverage"
  #   }else if(input$X_axis1 == "N_coverage"){
  #     newdata <- data.frame(T_coverage = input$T_coverage1,
  #                           N_coverage = c(15:90),
  #                           VAF = input$VAF1,
  #                           BND_threshold=input$BND_threshold1)
  #     xlabel <- "Normal coverage"
  #   }else if(input$X_axis1 == "BND_threshold"){
  #     newdata <- data.frame(T_coverage = input$T_coverage1,
  #                           N_coverage = input$N_coverage1,
  #                           VAF = input$VAF1,
  #                           BND_threshold=c(2:200))
  #     xlabel <- "Breakpoint precision threshold (bp)"}
  #  
  #     df.Sensitivity_Manta <- data.frame(newdata,
  #                                        #predict(gam.fit.Sensitivity_Manta,newdata,type = "response",se.fit = T),
  #                                        predict(gam.fit.Sensitivity_Manta,newdata,type = "response",se.fit = T,unconditional = TRUE),
  #                                        Caller=rep("Manta",each=nrow(newdata)),
  #                                        row.names = c(1:(nrow(newdata))))
  #     df.Sensitivity_Lumpy <- data.frame(newdata,
  #                                        predict(gam.fit.Sensitivity_Lumpy,newdata,type = "response",se.fit = T,unconditional = TRUE),
  #                                        Caller=rep("Lumpy",each=nrow(newdata)),
  #                                        row.names = c(1:(nrow(newdata))))
  #     df.Sensitivity_GRIDSS <- data.frame(newdata,
  #                                         predict(gam.fit.Sensitivity_GRIDSS,newdata,type = "response",se.fit = T,unconditional = TRUE),
  #                                         Caller=rep("GRIDSS",each=nrow(newdata)),
  #                                         row.names = c(1:(nrow(newdata))))
  #     
  #     df.Precision_Manta <- data.frame(newdata,
  #                                      predict(gam.fit.Precision_Manta,newdata,type = "response",se.fit = T,unconditional = TRUE),
  #                                      Caller=rep("Manta",each=nrow(newdata)),
  #                                      row.names = c(1:(nrow(newdata))))
  #     df.Precision_Lumpy <- data.frame(newdata,
  #                                      predict(gam.fit.Precision_Lumpy,newdata,type = "response",se.fit = T,unconditional = TRUE),
  #                                      Caller=rep("Lumpy",each=nrow(newdata)),
  #                                      row.names = c(1:(nrow(newdata))))
  #     df.Precision_GRIDSS <- data.frame(newdata,
  #                                       predict(gam.fit.Precision_GRIDSS,newdata,type = "response",se.fit = T,unconditional = TRUE),
  #                                       Caller=rep("GRIDSS",each=nrow(newdata)),
  #                                       row.names = c(1:(nrow(newdata))))
  #   
  #   df.Sensitivity <- rbind(df.Sensitivity_Manta,df.Sensitivity_Lumpy,df.Sensitivity_GRIDSS)
  #   df.Sensitivity$Caller <- factor(df.Sensitivity$Caller, levels = SV_caller)
  #   df.Precision <- rbind(df.Precision_Manta,df.Precision_Lumpy,df.Precision_GRIDSS)
  #   df.Precision$Caller <- factor(df.Precision$Caller, levels = SV_caller)
  #   
  #   return(list(df.Sensitivity,df.Precision,xlabel))
  # })
  
  # output$txtOutput1 <- renderText({
  #   paste("Show the",input$measurements1,MeasureInput1()[1],
  #         "of",input$SVCaller1,
  #         'across',input$X_axis1,dataInput1()[[3]],", with",
  #         " VAF=",input$VAF1,
  #         " Tumor coverage =",input$T_coverage1,
  #         " Normal coverage=",input$N_coverage1,
  #         " breakend resolution=",input$BND_threshold1)
  # })
  
  # Sensitivity_plot1 <- reactive({
  #   if(!MeasureInput1()[1]) return(NULL)
  #   df = dataInput1()[[1]][dataInput1()[[1]]$Caller %in% input$SVCaller1,]
  #   ggplot(data=df, aes(x=eval(parse(text=input$X_axis1)), y=fit, group=Caller)) +
  #     
  #     geom_ribbon(aes(ymin=fit-1.96*se.fit,ymax=fit+1.96*se.fit), fill="grey70")+
  #     geom_line(data=df, aes(x=eval(parse(text=input$X_axis1)), y=fit, group=Caller,color = Caller))+
  #     ggtitle(paste("Sensitivity","across",dataInput1()[[3]]))+
  #     scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
  #     #scale_x_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
  #     scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$Caller])+
  #     labs(y=input$measurements1[1],x = dataInput1()[[3]])+
  #     theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
  #           legend.title=element_text(size=14),legend.text=element_text(size=14))
  # })
  # 
  # Precision_plot1 <- reactive({
  #   if(!MeasureInput1()[2])return(NULL)
  #   df = dataInput1()[[2]][dataInput1()[[2]]$Caller %in% input$SVCaller1,]
  #   ggplot(data=df, aes(x=eval(parse(text=input$X_axis1)), y=fit, group=Caller)) +
  #     
  #     geom_ribbon(aes(ymin=fit-1.96*se.fit,ymax=fit+1.96*se.fit), fill="grey70")+
  #     geom_line(data=df, aes(x=eval(parse(text=input$X_axis1)), y=fit, group=Caller,color = Caller))+
  #     ggtitle(paste("Precision","across",dataInput1()[[3]]))+
  #     scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
  #     scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$Caller])+
  #     labs(y=input$measurements1[2],x = dataInput1()[[3]])+
  #     theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
  #           legend.title=element_text(size=14),legend.text=element_text(size=14))
  # })
  # 
  # output$Plot1 <- renderPlot({
  #   ptlist <- list(Sensitivity_plot1(),Precision_plot1())
  #   to_delete <- !sapply(ptlist,is.null)
  #   ptlist <- ptlist[to_delete] 
  #   
  #   if (length(ptlist)==0) return(NULL)
  #   grid.arrange(grobs=ptlist,ncol=length(ptlist))
  # })
  
  dataInput3 <- reactive({
    df.Sensitivity_Manta <- c()
    df.Sensitivity_Lumpy <- c()
    df.Sensitivity_GRIDSS <- c()
    df.Precision_Manta <- c()
    df.Precision_Lumpy <- c()
    df.Precision_GRIDSS <- c()
    
    if(input$X_axis3 == "VAF"){
      T_coverage_label = paste0(c(input$T_coverage3,60,30),"x")
      line_type <- c("solid","dashed","dotted")
      names(line_type) <- T_coverage_label
      
      newdata <- data.frame(Caller = SV_caller[1:3],
                            T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller[1:3])),
                            N_coverage = input$N_coverage3,
                            VAF = rep(seq(0.05,1,0.01),each=length(T_coverage_label)*length(SV_caller[1:3])),
                            BND_threshold = input$BND_threshold3)
      xlabel <- "Tumor purity/VAF"
      Tcov_var <- c(paste0(input$T_coverage3,"x"),input$T_coverage3.1)
      #Ncov_var <- c(paste0(input$N_coverage3,"x"),input$N_coverage3.1)
      Ncov_var <- paste0(input$N_coverage3,"x")
      VAF_var <- paste0(newdata$VAF)
      BND_var <- paste0(input$BND_threshold3)
      index <- 8
      legend_label <- "Tumor coverage"
      x_min <- 0
      x_max <- 1
      x_by <- 0.1
    }else if(input$X_axis3 == "T_coverage"){
      VAF_label = paste0(c(input$VAF3,0.2,0.8))
      line_type <- c("solid","dashed","dotted")
      names(line_type) <- VAF_label
      
      newdata <- data.frame(Caller = SV_caller[1:3],
                            T_coverage = rep(c(20:90),each = length(VAF_label)*length(SV_caller[1:3])),
                            N_coverage = input$N_coverage3,
                            VAF = rep(c(input$VAF3,0.2,0.8), each=length(SV_caller[1:3])),
                            BND_threshold = input$BND_threshold3)
      xlabel <- "Tumor coverage (x)"
      Tcov_var <- paste0(newdata$T_coverage,"x")
      #Ncov_var <- c(paste0(input$N_coverage3,"x"),input$N_coverage3.1)
      Ncov_var <- paste0(input$N_coverage3,"x")
      VAF_var <- c(paste0(input$VAF3),input$VAF3.1)
      BND_var <- paste0(input$BND_threshold3)
      index <- 10
      legend_label <- "Tumor purity/VAF"
      x_min <- 20
      x_max <- 90
      x_by <- 10
    }else if(input$X_axis3 == "N_coverage"){
      T_coverage_label = paste0(c(input$T_coverage3,60,30),"x")
      line_type <- c("solid","dashed","dotted")
      names(line_type) <- T_coverage_label
      
      newdata <- data.frame(Caller = SV_caller[1:3],
                            T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller[1:3])),
                            N_coverage = rep(c(15:90), each = length(T_coverage_label)*length(SV_caller[1:3])),
                            VAF = input$VAF3,
                            BND_threshold = input$BND_threshold3)
      xlabel <- "Normal coverage (x)"
      Tcov_var <- c(paste0(input$T_coverage3,"x"),input$T_coverage3.1)
      Ncov_var <- paste0(newdata$N_coverage,"x")
      VAF_var <- paste0(input$VAF3)
      BND_var <- paste0(input$BND_threshold3)
      index <- 8
      legend_label <- "Tumor coverage"
      x_min <- 15
      x_max <- 90
      x_by <- 15
    }else if(input$X_axis3 == "BND_threshold"){
      T_coverage_label = paste0(c(input$T_coverage3,60,30),"x")
      line_type <- c("solid","dashed","dotted")
      names(line_type) <- T_coverage_label
      
      newdata <- data.frame(Caller = SV_caller[1:3],
                            T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller[1:3])),
                            N_coverage = input$N_coverage3,
                            VAF = input$VAF3,
                            BND_threshold = rep(c(2:200), each = length(T_coverage_label)*length(SV_caller[1:3])))
      xlabel <- "Breakpoint precision threshold (bp)"
      Tcov_var <- c(paste0(input$T_coverage3,"x"),input$T_coverage3.1)
      Ncov_var <- paste0(input$N_coverage3,"x")
      VAF_var <- paste0(input$VAF3)
      BND_var <- paste0(newdata$BND_threshold)
      index <- 8
      legend_label <- "Tumor coverage"
      x_min <- 0
      x_max <- 200
      x_by <- 20
      }
  
    df.Sensitivity <- data.frame(newdata,
                                 predict(gamsen,newdata,type = "response",se.fit = T,unconditional = TRUE),
                                 T_coverage_label = paste0(newdata$T_coverage,"x"),
                                 N_coverage_label = paste0(newdata$N_coverage,"x"),
                                 VAF_label = paste0(newdata$VAF),
                                 BND_label = paste0(newdata$BND_threshold),
                                 row.names = c(1:(nrow(newdata))))
    df.Precision <- data.frame(newdata,
                               predict(gampre_off,newdata,type = "response",se.fit = T, unconditional = TRUE),
                               T_coverage_label = paste0(newdata$T_coverage,"x"),
                               N_coverage_label = paste0(newdata$N_coverage,"x"),
                               VAF_label = paste0(newdata$VAF),
                               BND_label = paste0(newdata$BND_threshold),
                               row.names = c(1:(nrow(newdata))))
    # df.Sensitivity_Manta <- data.frame(newdata,
    #                                    #predict(gam.fit.Sensitivity_Manta,newdata,type = "response",se.fit = T),
    #                                    predict(gam.fit.Sensitivity_Manta,newdata,type = "response",se.fit = T,unconditional = TRUE),
    #                                    Caller=rep("Manta",each=nrow(newdata)),
    #                                    T_coverage_label = paste0(newdata$T_coverage,"x"),
    #                                    N_coverage_label = paste0(newdata$N_coverage,"x"),
    #                                    VAF_label = paste0(newdata$VAF),
    #                                    BND_label = paste0(newdata$BND_threshold),
    #                                    row.names = c(1:(nrow(newdata))))
    # df.Sensitivity_Lumpy <- data.frame(newdata,
    #                                    predict(gam.fit.Sensitivity_Lumpy,newdata,type = "response",se.fit = T,unconditional = TRUE),
    #                                    Caller=rep("Lumpy",each=nrow(newdata)),
    #                                    T_coverage_label = paste0(newdata$T_coverage,"x"),
    #                                    N_coverage_label = paste0(newdata$N_coverage,"x"),
    #                                    VAF_label = paste0(newdata$VAF),
    #                                    BND_label = paste0(newdata$BND_threshold),
    #                                    row.names = c(1:(nrow(newdata))))
    # df.Sensitivity_GRIDSS <- data.frame(newdata,
    #                                     predict(gam.fit.Sensitivity_GRIDSS,newdata,type = "response",se.fit = T,unconditional = TRUE),
    #                                     Caller=rep("GRIDSS",each=nrow(newdata)),
    #                                     T_coverage_label = paste0(newdata$T_coverage,"x"),
    #                                     N_coverage_label = paste0(newdata$N_coverage,"x"),
    #                                     VAF_label = paste0(newdata$VAF),
    #                                     BND_label = paste0(newdata$BND_threshold),
    #                                     row.names = c(1:(nrow(newdata))))
    
    # df.Precision_Manta <- data.frame(newdata,
    #                                  predict(gam.fit.Precision_Manta,newdata,type = "response",se.fit = T, unconditional = TRUE),
    #                                  Caller=rep("Manta",each=nrow(newdata)),
    #                                  T_coverage_label = paste0(newdata$T_coverage,"x"),
    #                                  N_coverage_label = paste0(newdata$N_coverage,"x"),
    #                                  VAF_label = paste0(newdata$VAF),
    #                                  BND_label = paste0(newdata$BND_threshold),
    #                                  row.names = c(1:(nrow(newdata))))
    # df.Precision_Lumpy <- data.frame(newdata,
    #                                  predict(gam.fit.Precision_Lumpy,newdata,type = "response",se.fit = T, unconditional = TRUE),
    #                                  Caller=rep("Lumpy",each=nrow(newdata)),
    #                                  T_coverage_label = paste0(newdata$T_coverage,"x"),
    #                                  N_coverage_label = paste0(newdata$N_coverage,"x"),
    #                                  VAF_label = paste0(newdata$VAF),
    #                                  BND_label = paste0(newdata$BND_threshold),
    #                                  row.names = c(1:(nrow(newdata))))
    # df.Precision_GRIDSS <- data.frame(newdata,
    #                                   predict(gam.fit.Precision_GRIDSS,newdata,type = "response",se.fit = T, unconditional = TRUE),
    #                                   Caller=rep("GRIDSS",each=nrow(newdata)),
    #                                   T_coverage_label = paste0(newdata$T_coverage,"x"),
    #                                   N_coverage_label = paste0(newdata$N_coverage,"x"),
    #                                   VAF_label = paste0(newdata$VAF),
    #                                   BND_label = paste0(newdata$BND_threshold),
    #                                   row.names = c(1:(nrow(newdata))))
    # 
    # df.Sensitivity <- rbind(df.Sensitivity_Manta,df.Sensitivity_Lumpy,df.Sensitivity_GRIDSS)
     df.Sensitivity$Caller <- factor(df.Sensitivity$Caller, levels = SV_caller)
    # df.Precision <- rbind(df.Precision_Manta,df.Precision_Lumpy,df.Precision_GRIDSS)
     df.Precision$Caller <- factor(df.Precision$Caller, levels = SV_caller)
    
    
    return(list(df.Sensitivity,df.Precision,
                xlabel,line_type,Tcov_var,Ncov_var,VAF_var,BND_var,index,legend_label,x_min,x_max,x_by))
  })
  
  #output$txtOutput3 <- renderText({
  #  paste(#"Show the",input$measurements3,MeasureInput3()[1],
   #       "of",input$SVCaller3,
       #   'across',input$X_axis3,dataInput3()[[3]],", with"
          #" VAF=",input$VAF3,
         # " Tumor coverage =",input$T_coverage3,
         # " Normal coverage=",input$N_coverage3,
         # " breakend resolution=",input$BND_threshold3,
         #paste(dataInput3()[[1]]$T_coverage_label,collapse = ","),
         #paste(input$T_coverage3.1,collapse = ","),
         #paste(colnames(dataInput3()[[1]]),collapse = ",")
         #dataInput3()[[4]]
          #head(dataInput3()[[1]])
  #       )
 # })
  
  Sensitivity_plot3 <- reactive({
    if(!MeasureInput3()[1]) return(NULL)
    Tcov_var <- dataInput3()[[5]]
    Ncov_var <- dataInput3()[[6]]
    VAF_var <- dataInput3()[[7]]
    BND_var <- dataInput3()[[8]]
    index <- dataInput3()[[9]]
    xlabel <- dataInput3()[[3]]
    line_type <- dataInput3()[[4]]
    legend_label <- dataInput3()[[10]]
    x_min <- dataInput3()[[11]]
    x_max <- dataInput3()[[12]]
    x_by <- dataInput3()[[13]]
    df = dataInput3()[[1]][(dataInput3()[[1]]$Caller %in% input$SVCaller3) & 
                           (dataInput3()[[1]]$T_coverage_label %in% Tcov_var) &
                           (dataInput3()[[1]]$N_coverage_label %in% Ncov_var) &
                           (dataInput3()[[1]]$VAF_label %in% VAF_var) &
                           (dataInput3()[[1]]$BND_label %in% BND_var),]
    ggplot(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit, group = interaction(Caller,eval(parse(text = colnames(df)[index]))))) +
          geom_ribbon(aes(ymin = fit-1.96*se.fit, ymax = fit+1.96*se.fit), fill = "grey70")+
          geom_line(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit,
                                 group = interaction(Caller,eval(parse(text = colnames(df)[index]))), 
                                 color = Caller, linetype = eval(parse(text = colnames(df)[index]))))+
          ggtitle(paste(input$measurements3[1], "across", xlabel))+
          scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
          scale_x_continuous(breaks = seq(x_min, x_max, by = x_by), limits = c(x_min,x_max))+
          scale_color_manual(values = color_SVCaller[names(color_SVCaller) %in% df$Caller])+
          scale_linetype_manual(values = line_type[names(line_type) %in% unique(df[,index])])+
          labs(y = input$measurements3[1], x = xlabel, linetype = legend_label)+
          theme(axis.text = element_text(size=14), axis.title = element_text(size=14,face="bold"), plot.title = element_text(size=18),
                legend.title = element_text(size=14), legend.text = element_text(size=14))
  })
  
  
  Precision_plot3 <- reactive({
    if(!MeasureInput3()[2]) return(NULL)
    Tcov_var <- dataInput3()[[5]]
    Ncov_var <- dataInput3()[[6]]
    VAF_var <- dataInput3()[[7]]
    BND_var <- dataInput3()[[8]]
    index <- dataInput3()[[9]]
    xlabel <- dataInput3()[[3]]
    line_type <- dataInput3()[[4]]
    df.Precision <- dataInput3()[[2]]
    legend_label <- dataInput3()[[10]]
    x_min <- dataInput3()[[11]]
    x_max <- dataInput3()[[12]]
    x_by <- dataInput3()[[13]]
    df = df.Precision[(df.Precision$Caller %in% input$SVCaller3) & 
                             (df.Precision$T_coverage_label %in% Tcov_var) &
                             (df.Precision$N_coverage_label %in% Ncov_var) &
                             (df.Precision$VAF_label %in% VAF_var)&
                             (df.Precision$BND_label %in% BND_var),]
    ggplot(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit, group = interaction(Caller,eval(parse(text = colnames(df)[index]))))) +
      geom_ribbon(aes(ymin = fit-1.96*se.fit, ymax = fit+1.96*se.fit), fill = "grey70")+
      geom_line(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit,
                               group = interaction(Caller,eval(parse(text = colnames(df)[index]))), 
                               color = Caller, linetype = eval(parse(text = colnames(df)[index]))))+
      ggtitle(paste(input$measurements3[2], "across", xlabel))+
      scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
      scale_x_continuous(breaks = seq(x_min, x_max, by = x_by), limits = c(x_min,x_max))+
      scale_color_manual(values = color_SVCaller[names(color_SVCaller) %in% df$Caller])+
      scale_linetype_manual(values = line_type[names(line_type) %in% unique(df[,index])])+
      labs(y = input$measurements3[2], x = xlabel, linetype = legend_label)+
      theme(axis.text = element_text(size=14), axis.title = element_text(size=14,face="bold"), plot.title = element_text(size=18),
            legend.title = element_text(size=14), legend.text = element_text(size=14))
  })
  
  
  output$Plot3 <- renderPlot({
    ptlist <- list(Sensitivity_plot3(),Precision_plot3())
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    
    if (length(ptlist)==0) return(NULL)
    grid.arrange(grobs=ptlist,ncol=length(ptlist))
  })
  
  
})

shinyApp(ui=ui,server=server)