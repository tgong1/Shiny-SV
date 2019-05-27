library(shiny)
library(shinyjs)
library(gridExtra)

# Source helpers ----
source("helper.R")

ui <- navbarPage(
  title='Shiny-SoSV!',id = "Shiny-SoSV",

  tabPanel(title = "Home", value = "introduction", 
           h2("Shiny-SoSV: A web app for interactive evaluation of somatic structural variant calls", align = "center"),
           h5("Shiny-SoSV is an interactive web app for evaluating somatic structural variant calls derived from short-read whole genome sequencing.", align = "center"),
           #h4("Shiny-SoSV can be access here"),
           #actionButton('jumpToEvaluation', 'Shiny-SoSV'),
           br(),
           p("Acquired structural variants play a significant role in cancer development and evolution. Accurate detection of these, often complex, genomic rearrangements from whole genome sequencing data is influenced by many variables, the effects of which are not always linear. With increasing demand for application of whole genome sequencing in clinical settings, there is an unmet need for clinician scientists to easily make bioinformatically-driven decisions for every unique patient and sample. To address this, we have developed Shiny-SoSV, an interactive web application for evaluating the effects of various variables on somatic structural variant calling, thereby enabling users to quickly make relevant sequencing and bioinformatics decisions."),
           p("If you use this web app, please", em("cite it.")),
           br(),
           actionButton('jumpToInstallation', 'Installation'),
           actionButton('jumpToUserGuide', 'User Guide'),
           actionButton('jumpToUseCase', 'Example Use Cases'),
           p("Installation: Detailed description of how to obtain and install your own copy of Shiny-SoSV."),
           p("User Guide: Description (explanation) of Shiny-SoSV’s web app interface."),
           p("Example Use Cases: Hypothetical scenarios of highlighting the Shiny-SoSV’s utility.")),
  tabPanel(shinyjs::useShinyjs(),title = "Shiny-SoSV", value = "evaluation",#tweaks,
           sidebarLayout(fluid=FALSE,sidebarPanel(
                                      checkboxGroupInput(inputId = "SVCaller3",
                                                        label = ("SV Caller(s)"),
                                                        choiceNames = list("Manta ", "Lumpy ", "GRIDSS "),
                                                        choiceValues = c("Manta", "Lumpy", "GRIDSS"),
                                                        selected = "Manta",inline=TRUE,width='400px'),
                                      checkboxGroupInput(inputId = "SVCaller3.1",
                                                         label = NULL,
                                                         choiceNames = list(HTML("&#8746;","Lumpy"),HTML("&#8746;","Manta"), HTML("&#8746;","Manta")),
                                                         choiceValues = c("MantaLumpyUnion","LumpyMantaUnion","GRIDSSMantaUnion"),
                                                         selected = NULL,inline=TRUE,width='400px'),
                                      checkboxGroupInput(inputId = "SVCaller3.2",
                                                         label = NULL,
                                                         choiceNames = list(HTML("&#8745;","Lumpy"),HTML("&#8745;","Manta"),HTML("&#8745;","Manta")),
                                                         choiceValues = c("MantaLumpyIntersect","LumpyMantaIntersect","GRIDSSMantaIntersect"),
                                                         selected = NULL,inline=TRUE ,width='400px'),
                                      checkboxGroupInput(inputId = "SVCaller3.3",
                                                         label = NULL,
                                                         choiceNames = list(HTML("&#8746;","GRIDSS"),HTML("&#8746;","GRIDSS"),HTML("&#8746;","Lumpy")),
                                                         choiceValues = c("MantaGRIDSSUnion","LumpyGRIDSSUnion","GRIDSSLumpyUnion"),
                                                         selected = NULL,inline=TRUE ,width='400px'),
                                      checkboxGroupInput(inputId = "SVCaller3.4",
                                                         label = NULL,
                                                         choiceNames = list(HTML("&#8745;","GRIDSS"),HTML("&#8745;","GRIDSS"),HTML("&#8745;","Lumpy")),
                                                         choiceValues = c("MantaGRIDSSIntersect","LumpyGRIDSSIntersect","GRIDSSLumpyIntersect"),
                                                         selected = NULL,inline=TRUE ,width='400px'),
                                     
                                      #controls,
                                     sliderInput("VAF3", "Tumor purity/VAF:",
                                                 min = 0.05, max = 1, value = 0.5, step= 0.01),
                                     checkboxGroupInput("VAF3.1",
                                                        label= NULL,
                                                        choices = c("0.2" = 0.2,"0.8"= 0.8),
                                                        selected = NULL),
                                     sliderInput("T_coverage3", "Tumor coverage:", 
                                                 min = 20, max = 90, value = 60, step = 0.1, post = "x"),
                                     checkboxGroupInput("T_coverage3.1",
                                                        label= NULL,
                                                        choices = c("30x","60x"),
                                                        selected = NULL),
                                     sliderInput("N_coverage3", "Normal coverage:", 
                                                 min = 15, max = 90, value = 30, step = 0.1, post = "x"),
                                     sliderInput("BND_threshold3", "Breakpoint precision threshold:", 
                                                 min = 2, max = 200, value = 5, step= 1, post = "bp"),
                                     width=3
                                     ),
          mainPanel(checkboxGroupInput("measurements3",
                                       label="Evaluation measurement(s)",
                                       choices = list("Sensitivity","Precision"),
                                       selected = "Sensitivity",inline=TRUE),
                    radioButtons("X_axis3",
                                 label="Evaluation across",
                                 choices = c("Tumor purity/VAF" = "VAF","Tumor coverage" = "T_coverage","Normal coverage" = "N_coverage","Breakpoint precision threshold" = "BND_threshold"),
                                 selected = "VAF",inline=TRUE),
                    textOutput("txtOutput3"),
                    plotOutput("Plot3"),#downloadButton("report", "Generate report"),
                    width=9))
    ),
  tabPanel(title = "Installation", value = "installation",
           h5("Shiny-SoSV is hosted on shinyapp.io by RStudio with source code hosted on the GitHub repository tgong1/Shiny-SoSV."),
           strong("Direct Web Access"),
           p("The easiest way to access Shiny-SoSV is via the direct URL,", em("https://hcpcg.shinyapps.io/Shiny-SoSV/.")),
           #br(),
           strong("Launching from the GitHub repository"),
           p("Shiny-SoSV can also be launched from within a R session with command", 
             code("runGitHub('Shiny-SoSV', 'tgong1')"),".", 
             "You will need to install R package Shiny, Shinyjs, gridExtra, ggplot2 and ggsci."),
           strong("GitHub"),
           p("Anyone wishing a local copy of, or contribute to Shiny-SoSV, can obtain it via GitHub. To copy the repo to your local machine, use the command:",
             code("git clone https://github.com/tgong1/Shiny-SoSV.git"),".",
             "The data folder of the repo contains all evaluation results presented underlying the web app."),
           br(),
           p("Note to SV caller developers:",
             "If you would like to include your SV caller in Shiny-SoSV, please get in touch. We would be able to provide you a copy of the simulated aligned bam files to run your software on!")
           
  ),
  tabPanel(title = "User Guide", value = "userguide",
           h5("Shiny-SoSV provides predictions of the impact of common variables (SV caller, sequencing coverage, tumour allele frequencies, tolerance of breakpoint precision) on the sensitivity and precision of somatic SV calls through interactive plots."),
           img(src = "UserGuide.png",height=509, width=1060),
           #img(src = "UserGuide.png",height=764, width=1590),
           p("[1] Evaluation measurement (s): Either or both performance measures (sensitivity and precision) can be evaluated. These values are shown on the y-axes, with 0 indicating poor performance and 1 for perfect performance. Confidence intervals of the predictions are shown by flanking grey lines."),
           h5("One of four predictors can be plotted on the x-axis using the “Evaluation across” radio button, including:"),
           p("[2] “Tumour purity/VAF”: the proportion of variant allele in the tumour sample."),
           p("[3] “Tumour coverage”: the depth of sequencing coverages of the tumour sample."),
           p("[4] “Normal coverage”: the depth of sequencing coverages of the normal sample."),
           p("[5] “Breakpoint precision threshold”: the precision of the breakpoint calls in nucleotide units."),
           h5("Additional predictor variables can be examined and visualized using sidebar. In general, checkboxes allow additional prediction lines to be added/removed from the plots, while slider bars allow different values of the corresponding variable to be altered in the prediction. In particular:"),
           p("[6] SV Caller(s): One or more, and combinations of, SV callers can be examined (distinguished by line colour in the plots). Combination of SV callers can be examined as “Union” or “Interaction” sets, where A∪B sets are SV calls identified by either caller A or caller B, while A∩B sets are SV calls identified by BOTH A and B."),
           p("[7] Tumour purity/VAF: Up to three values of this predictor can be visualized on the plots (distinguished by line type on the plots) with “Evaluation across” predictor as “Tumour coverage”, using the slider bar and checkboxing one or both pre-set values of 0.2 & 0.8, which were designed to be the lower and upper bounds of histopathology tumour purity values. If “Evaluate Across” “Tumour Purity/VAF” [2] is selected, changes to this is greyed out (disabled)."),
           p("[8] Tumour Coverage: As with “Evaluation across” predictor as “Tumour purity/VAF”, “Normal coverage” or “Breakpoint precision threshold”, the two preset values are 30x and 60x, reflecting the typical depth of sequencing coverage for tumour samples. If “Evaluate Across” “Tumour Coverage” [3] is selected, changes to this is greyed out (disabled)."),
           p("[9] Normal Coverage: Only one value can be evaluated at any one time. If “Evaluate Across” “Normal Coverage” [4] is selected, changes to this is greyed out (disabled)."),
           p("[10] Breakpoint Precision: Only one value can be evaluated at any one time. If “Evaluate Across” “Breakpoint Precision” [4] is selected, changes to this is greyed out (disabled)."),
           strong("A note on the parameters:"),
           p("It should be noted histopathological estimates of tumour purity is typically the upper bound of observed VAF in genomics studies in part due to the likelihood of the presence of sub-clonality."),
           p("Breakpoint precision threshold is the maximum allowed difference of called breakpoint to the simulated breakpoints, to be considering as true positive call.")
           ),
  tabPanel(title = "Example Use Cases", value = "usecase",
           h5("Hypothetical scenarios of highlighting the Shiny-SoSV’s utility:"),
           fluidRow(column(11,h5("Scenario 1: I have a cohort of matched-normal cancer samples, each with different histopathology-estimates of tumor purity. Our WGS (or bioinformatics) pipeline uses Lumpy, which could potentially be changed, though we’d rather not have to. How much coverage would I need to have at least 80% sensitivity and >90% precision on somatic SV calls.")),
                    column(1, 
                           actionButton("S1", label = "", icon = icon("angle-down"), class = "btn btn-info")
                    )),
           hidden(imageOutput(outputId = "image_S1")),
           hidden(textOutput(outputId = "text_S1")),
           
           fluidRow(column(11,h5("Scenario 2: I have a handful of samples, about 5-10, my boss is wondering if we can generate low pass coverage on the matched normal and still obtain >80% sensitivity and precision. We haven’t decided on which SV caller to use yet.")),
                    column(1, 
                           actionButton("S2", label = "", icon = icon("angle-down"), class = "btn btn-info")
                    )),
           hidden(imageOutput(outputId = "image_S2")),
           #hidden(textOutput(outputId = "text_S2.1")),
           #tags$head(tags$style("#text_S2.1{color: red;font-size: 20px;font-style: italic}")),
           hidden(textOutput(outputId = "text_S2")),
           
           fluidRow(column(11,h5("Scenario 3: I have a cancer sample with estimated tumor purity estimated of about 20%. It is a precious sample, and we want to call confident SVs. How much should we sequence to get >90% sensitivity and precision. Is it even possible?")),
                    column(1, 
                           actionButton("S3", label = "", icon = icon("angle-down"), class = "btn btn-info")
                    )),
           hidden(imageOutput(outputId = "image_S3")),
           hidden(textOutput(outputId = "text_S3")),
           
           
           fluidRow(column(11,h5("Scenario 4: I am a clinical geneticist and I have a patient whom we cannot find a pathological SNV/indel from a 30x WGS. We think the driver mutation may be a structural variant. We don’t know much about SVs or how to call them. Which SV caller should we use? Our NGS data suggested the tumor purity is about 50%. Do we need to sequence more? We care about both false positives and false negatives, but would prefer to be confident in what we find than finding more of something that may be false.")),
                    column(1, 
                           actionButton("S4", label = "", icon = icon("angle-down"), class = "btn btn-info")
                    )),
           hidden(imageOutput(outputId = "image_S4")),
           hidden(textOutput(outputId = "text_S4")),
           
           
           p()
           )
                          
                
)

server <- function(input, output, session)({
  observeEvent(input$jumpToInstallation, {
    updateTabsetPanel(session, "Shiny-SoSV",
                      selected = "installation")
  })
  
  observeEvent(input$jumpToUserGuide, {
    updateTabsetPanel(session, "Shiny-SoSV",
                      selected = "userguide")
  })
  
  observeEvent(input$jumpToUseCase, {
    updateTabsetPanel(session, "Shiny-SoSV",
                      selected = "usecase")
  })
  
  observeEvent(input$jumpToEvaluation, {
    updateTabsetPanel(session, "Shiny-SoSV",
                      selected = "evaluation")
  })
  
  observeEvent(input$S1, {
     toggle('image_S1')
     output$image_S1 <- renderImage({
       filename <- normalizePath(file.path('./images',paste('FigureS1.1', '.png', sep='')))
       list(src = filename, height=382, width=795, alt="Figure 1")
       #list(src = filename, alt="Figure 1")
     }, deleteFile = FALSE)
  })
  
  observeEvent(input$S1, {
    toggle('text_S1')
    #output$text_S1 <- renderText({"ahh you pressed it"})
    output$text_S1 <- renderText({c("To address this using Shiny-SoSV, as demonstrated in Figure 1, the user would select both “Sensitivity” and “Precision” as evaluation measurements and “Tumor purity/VAF” as the “Evaluation across” parameter. On the sidebar, choose all SV callers and different tumor coverages, e.g. 30x, 60x and 90x, for comparison, while fixing all other parameters as default.",
       "From this, it should be immediately obvious that tumor purity/VAF has a great impact on sensitivity, while little impact on precision, particularly for VAF within 5% and 30%. With VAF > 30%, improvements in sensitivity notably slows for all SV callers, with Manta showing relatively larger improvements until it reaches the limit at this setting.", 
       "Considering the objective (at least 80% sensitivity and > 90% precision level), the user could choose to keep Lumpy in their pipeline and consider excluding all samples with tumor purity < 30%, allowing them to use any extra funds to sequence the remaining tumor samples to 90x depth of coverage. Alternatively, the user could switch to using Manta, sequencing at 60x, and including all samples.")})
  })
  
  observeEvent(input$S2, {
    toggle('image_S2')
    output$image_S2 <- renderImage({
      filename <- normalizePath(file.path('./images',paste('FigureS2.1', '.png', sep='')))
      list(src = filename, height=382, width=795, alt="Figure 2")
    }, deleteFile = FALSE)
  })
  
  observeEvent(input$S2, {
    toggle('text_S2')
    #toggle('text_S2.1')
    #output$text_S2.1 <- renderText({c("Figure2")})
    output$text_S2 <- renderText({c("To address this using Shiny-SoSV, as demonstrated in Figure 2, the user would select both “Sensitivity” and “Precision” as evaluation measurements and “Tumor purity/VAF” as the “Evaluation across” parameter. On the sidebar, choose all SV callers (Manta, Lumpy and GRIDSS) and different tumour coverages, e.g. 30x, 60x and 90x, for comparison and set a low normal coverage e.g. 15x.",
                                    "From this, it should be immediately obvious that tumor purity/VAF has a great impact on sensitivity, while little impact on precision, particularly for VAF within 5% and 30%. With VAF > 30%, improvements in sensitivity notably slow for all SV callers, with Manta showing relatively larger improvements until it reaches the limit at this setting. The precision level is relatively high (around 90%) for all VAF, while it would not be possible to obtain > 80% sensitivity for samples with tumor purity < 20%.", 
                                    "Any SV caller to use would obtain sensitivity greater than 80% at high tumor coverage of 90x, while Manta would have the highest sensitivity. Therefore, the user can choose to use Manta for all samples, sequencing at 90x tumor coverage. In case of budgetary constraints, another option is to use only samples with tumor purity > 40%, sequencing to 60x, and use Manta for SV calls in order to obtain > 80% sensitivity.")})
  })
  
  observeEvent(input$S3, {
    toggle('image_S3')
    output$image_S3 <- renderImage({
      filename <- normalizePath(file.path('./images',paste('FigureS3.1', '.png', sep='')))
      list(src = filename, height=382, width=795, alt="Figure 3")
    }, deleteFile = FALSE)
  })
  
  observeEvent(input$S3, {
    toggle('text_S3')
    output$text_S3 <- renderText({c("To address this using Shiny-SoSV, as demonstrated in Figure 3, the user would select both “Sensitivity” and “Precision” as evaluation measurements and “Tumor coverage” as the “Evaluation across” parameter. On the sidebar, the user can choose all SV callers and their union sets aiming to increase sensitivity and set tumour purity/VAF to 0.2, while fixing all other parameters as default.",
                                    "From the evaluation plots, we can see a dramatic increase (from 50% to 80% by individual SV callers and from 55% to 85% by union call sets) on sensitivity when increasing tumor coverage, while precision level remains high >90%. Moving the slider bar from 30x (default) to 90x to increasing matched normal coverage has little improvement on sensitivity.", 
                                    "Therefore, while >90% precision can easily be reached (regardless of SV caller, depth of sequencing coverage, or breakpoint precision threshold), it is far more difficult to attain sensitivity >85% with such a low tumour purity (20%). If we are to extrapolate on the plot shown, it may be possible to attain > 90% sensitivity with > 120x coverage on the tumour.")})
  })
  
  observeEvent(input$S4, {
    toggle('image_S4')
    output$image_S4 <- renderImage({
      filename <- normalizePath(file.path('./images',paste('FigureS4.1', '.png', sep='')))
      list(src = filename, height=382, width=795, alt="Figure 4")
    }, deleteFile = FALSE)
  })
  
  observeEvent(input$S4, {
    toggle('text_S4')
    output$text_S4 <- renderText({c("To address this using Shiny-SoSV, as demonstrated in Figure 4, the user would select both “Sensitivity” and “Precision” as evaluation measurements and “Tumor coverage” as the “Evaluation across” parameter. On the sidebar, the user can choose all SV callers (Manta, Lumpy and GRIDSS) for comparison and set tumor purity/VAF value as 0.5, while fixing all other parameters as default.",
                                    "From the evaluation in 30x tumor and normal coverage setting, the suggested SV caller with highest sensitivity is Manta, with 5% and 10% higher sensitivity than GRIDSS and Lumpy respectively. All SV callers can reach >90% precision. Considering the objective of confidence SV calling (high precision), the user can choose intersection sets of callers.", 
                                    "Here, for example, using intersection set of Manta and GRIDSS would increase the precision by around 3%, however, result in sensitivity dropping to 60%. Evaluation of tumor coverage suggests deeper sequencing would improve sensitivity, with around 18% increased sensitivity when increasing tumor coverage from 30x to 90x.")})
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  MeasureInput3 <- reactive({c("Sensitivity","Precision") %in% input$measurements3})
  
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
    disable("VAF3.1")
    enable("T_coverage3")
    enable("T_coverage3.1")
    enable("N_coverage3")
    disable("BND_threshold3")
  }})
  
  dataInput3 <- reactive({
    if(input$X_axis3 == "VAF"){
      T_coverage_label = paste0(c(input$T_coverage3,60,30),"x")
      line_type <- c("solid","dashed","dotted")
      names(line_type) <- T_coverage_label
      
      newdata <- data.frame(Caller = SV_caller[1:3],
                            T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller[1:3])),
                            N_coverage = input$N_coverage3,
                            VAF = rep(seq(0.05,1,0.01),each=length(T_coverage_label)*length(SV_caller[1:3])),
                            BND_threshold = input$BND_threshold3)
      newdataUnion <- data.frame(Caller = SV_caller[c(4,6,8,10,12,14)],
                                 T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller[c(4,6,8,10,12,14)])),
                                 N_coverage = input$N_coverage3,
                                 VAF = rep(seq(0.05,1,0.01),each=length(T_coverage_label)*length(SV_caller[c(4,6,8,10,12,14)])),
                                 BND_threshold = input$BND_threshold3)
      newdataIntersect <- data.frame(Caller = SV_caller[c(5,7,9,11,13,15)],
                                     T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller[c(5,7,9,11,13,15)])),
                                     N_coverage = input$N_coverage3,
                                     VAF = rep(seq(0.05,1,0.01),each=length(T_coverage_label)*length(SV_caller[c(5,7,9,11,13,15)])),
                                     BND_threshold = input$BND_threshold3)
      
      xlabel <- "Tumor purity/VAF"
      Tcov_var <- c(paste0(input$T_coverage3,"x"),input$T_coverage3.1)
      Ncov_var <- paste0(input$N_coverage3,"x")
      VAF_var <- paste0(seq(0.05,1,0.01))
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
      newdataUnion <- data.frame(Caller = SV_caller[c(4,6,8,10,12,14)],
                                 T_coverage = rep(c(20:90),each = length(VAF_label)*length(SV_caller[c(4,6,8,10,12,14)])),
                                 N_coverage = input$N_coverage3,
                                 VAF = rep(c(input$VAF3,0.2,0.8), each=length(SV_caller[c(4,6,8,10,12,14)])),
                                 BND_threshold = input$BND_threshold3)
      newdataIntersect <- data.frame(Caller = SV_caller[c(5,7,9,11,13,15)],
                                     T_coverage = rep(c(20:90),each = length(VAF_label)*length(SV_caller[c(5,7,9,11,13,15)])),
                                     N_coverage = input$N_coverage3,
                                     VAF = rep(c(input$VAF3,0.2,0.8), each=length(SV_caller[c(5,7,9,11,13,15)])),
                                     BND_threshold = input$BND_threshold3)
      
      xlabel <- "Tumor coverage (x)"
      Tcov_var <- paste0(c(20:90),"x")
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
      newdataUnion <- data.frame(Caller = SV_caller[c(4,6,8,10,12,14)],
                                 T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller[c(4,6,8,10,12,14)])),
                                 N_coverage = rep(c(15:90), each = length(T_coverage_label)*length(SV_caller[c(4,6,8,10,12,14)])),
                                 VAF = input$VAF3,
                                 BND_threshold = input$BND_threshold3)
      newdataIntersect <- data.frame(Caller = SV_caller[c(5,7,9,11,13,15)],
                                     T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller[c(5,7,9,11,13,15)])),
                                     N_coverage = rep(c(15:90), each = length(T_coverage_label)*length(SV_caller[c(5,7,9,11,13,15)])),
                                     VAF = input$VAF3,
                                     BND_threshold = input$BND_threshold3)
      xlabel <- "Normal coverage (x)"
      Tcov_var <- c(paste0(input$T_coverage3,"x"),input$T_coverage3.1)
      Ncov_var <- paste0(c(15:90),"x")
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
      newdataUnion <- data.frame(Caller = SV_caller[c(4,6,8,10,12,14)],
                                 T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller[c(4,6,8,10,12,14)])),
                                 N_coverage = input$N_coverage3,
                                 VAF = input$VAF3,
                                 BND_threshold = rep(c(2:200), each = length(T_coverage_label)*length(SV_caller[c(4,6,8,10,12,14)])))
      newdataIntersect <- data.frame(Caller = SV_caller[c(5,7,9,11,13,15)],
                                     T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller[c(5,7,9,11,13,15)])),
                                     N_coverage = input$N_coverage3,
                                     VAF = input$VAF3,
                                     BND_threshold = rep(c(2:200), each = length(T_coverage_label)*length(SV_caller[c(5,7,9,11,13,15)])))
      xlabel <- "Breakpoint precision threshold (bp)"
      Tcov_var <- c(paste0(input$T_coverage3,"x"),input$T_coverage3.1)
      Ncov_var <- paste0(input$N_coverage3,"x")
      VAF_var <- paste0(input$VAF3)
      BND_var <- paste0(c(2:200))
      index <- 8
      legend_label <- "Tumor coverage"
      x_min <- 0
      x_max <- 200
      x_by <- 20
    }
    
    df.Sensitivity_caller <- data.frame(newdata,
                                        predict(gamsen,newdata,type = "response",se.fit = T,unconditional = TRUE),
                                        T_coverage_label = paste0(newdata$T_coverage,"x"),
                                        N_coverage_label = paste0(newdata$N_coverage,"x"),
                                        VAF_label = paste0(newdata$VAF),
                                        BND_label = paste0(newdata$BND_threshold),
                                        row.names = c(1:(nrow(newdata))))
    df.Precision_caller <- data.frame(newdata,
                                      predict(gampre_off,newdata,type = "response",se.fit = T, unconditional = TRUE),
                                      T_coverage_label = paste0(newdata$T_coverage,"x"),
                                      N_coverage_label = paste0(newdata$N_coverage,"x"),
                                      VAF_label = paste0(newdata$VAF),
                                      BND_label = paste0(newdata$BND_threshold),
                                      row.names = c(1:(nrow(newdata))))
    
    df.Sensitivity_Union <- data.frame(newdataUnion,
                                       predict(gamsenUnion,newdataUnion,type = "response",se.fit = T,unconditional = TRUE),
                                       T_coverage_label = paste0(newdataUnion$T_coverage,"x"),
                                       N_coverage_label = paste0(newdataUnion$N_coverage,"x"),
                                       VAF_label = paste0(newdataUnion$VAF),
                                       BND_label = paste0(newdataUnion$BND_threshold),
                                       row.names = c(1:(nrow(newdataUnion))))
    
    df.Precision_Union <- data.frame(newdataUnion,
                                     predict(gampre_offUnion,newdataUnion,type = "response",se.fit = T,unconditional = TRUE),
                                     T_coverage_label = paste0(newdataUnion$T_coverage,"x"),
                                     N_coverage_label = paste0(newdataUnion$N_coverage,"x"),
                                     VAF_label = paste0(newdataUnion$VAF),
                                     BND_label = paste0(newdataUnion$BND_threshold),
                                     row.names = c(1:(nrow(newdataUnion))))
    
    df.Sensitivity_Intersect <- data.frame(newdataIntersect,
                                           predict(gamsenIntersect,newdataIntersect,type = "response",se.fit = T,unconditional = TRUE),
                                           T_coverage_label = paste0(newdataIntersect$T_coverage,"x"),
                                           N_coverage_label = paste0(newdataIntersect$N_coverage,"x"),
                                           VAF_label = paste0(newdataIntersect$VAF),
                                           BND_label = paste0(newdataIntersect$BND_threshold),
                                           row.names = c(1:(nrow(newdataIntersect))))
    
    df.Precision_Intersect <- data.frame(newdataIntersect,
                                         predict(gampre_offIntersect,newdataIntersect,type = "response",se.fit = T,unconditional = TRUE),
                                         T_coverage_label = paste0(newdataIntersect$T_coverage,"x"),
                                         N_coverage_label = paste0(newdataIntersect$N_coverage,"x"),
                                         VAF_label = paste0(newdataIntersect$VAF),
                                         BND_label = paste0(newdataIntersect$BND_threshold),
                                         row.names = c(1:(nrow(newdataIntersect))))
    
    
    df.Sensitivity <- rbind(df.Sensitivity_caller, df.Sensitivity_Union, df.Sensitivity_Intersect)
    df.Precision <- rbind(df.Precision_caller, df.Precision_Union, df.Precision_Intersect)
    
    df.Sensitivity$Caller <- factor(df.Sensitivity$Caller, levels = SV_caller)
    df.Precision$Caller <- factor(df.Precision$Caller, levels = SV_caller)
    
    return(list(df.Sensitivity,df.Precision,
                xlabel,line_type,Tcov_var,Ncov_var,VAF_var,BND_var,index,legend_label,x_min,x_max,x_by))
  })
  
  #output$txtOutput3 <- renderText({
  #  paste("Show the",#c(input$SVCaller3,input$SVCaller3.1,input$SVCaller3.2,input$SVCaller3.3,input$SVCaller3.4),
        #df$SV_caller_label
   #       input$measurements3,MeasureInput3()#[1],MeasureInput3()[2]
    #      "of",input$SVCaller3
    #         'across',input$X_axis3 == "VAF"
    #,dataInput3()[[3]],", with"
          #" VAF=",input$VAF3,
          # " Tumor coverage =",input$T_coverage3,
          # " Normal coverage=",input$N_coverage3,
          # " breakend resolution=",input$BND_threshold3,
          #paste(dataInput3()[[1]]$T_coverage_label,collapse = ","),
          #paste(input$T_coverage3.1,collapse = ","),
          #paste(colnames(dataInput3()[[1]]),collapse = ",")
          #dataInput3()[[4]]
          #head(dataInput3()[[1]])
    #)
  #})
  
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
    
    df = dataInput3()[[1]][(dataInput3()[[1]]$Caller %in% c(input$SVCaller3,input$SVCaller3.1,input$SVCaller3.2,input$SVCaller3.3,input$SVCaller3.4)) & 
                             (dataInput3()[[1]]$T_coverage_label %in% Tcov_var) &
                             (dataInput3()[[1]]$N_coverage_label %in% Ncov_var) &
                             (dataInput3()[[1]]$VAF_label %in% VAF_var) &
                             (dataInput3()[[1]]$BND_label %in% BND_var),]
    ggplot(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit, group = interaction(Caller,eval(parse(text = colnames(df)[index]))))) +
      geom_ribbon(aes(ymin = fit-1.96*se.fit, ymax = fit+1.96*se.fit), fill = "grey70")+
      geom_line(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit,
                               group = interaction(Caller,eval(parse(text = colnames(df)[index]))), 
                               color = Caller, linetype = eval(parse(text = colnames(df)[index]))))+
      ggtitle(paste("Sensitivity", "across", xlabel))+
      scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
      scale_x_continuous(breaks = seq(x_min, x_max, by = x_by), limits = c(x_min,x_max))+
      scale_color_manual(values = color_SVCaller[names(color_SVCaller) %in% df$Caller], name = "SV Caller",labels = SV_caller_label[SV_caller %in% df$Caller])+
      scale_linetype_manual(values = line_type[names(line_type) %in% unique(df[,index])])+
      labs(y = "Sensitivity", x = xlabel, linetype = legend_label)+
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
    df = df.Precision[(df.Precision$Caller %in% c(input$SVCaller3,input$SVCaller3.1,input$SVCaller3.2,input$SVCaller3.3,input$SVCaller3.4)) & 
                        (df.Precision$T_coverage_label %in% Tcov_var) &
                        (df.Precision$N_coverage_label %in% Ncov_var) &
                        (df.Precision$VAF_label %in% VAF_var)&
                        (df.Precision$BND_label %in% BND_var),]
    ggplot(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit, group = interaction(Caller,eval(parse(text = colnames(df)[index]))))) +
      geom_ribbon(aes(ymin = fit-1.96*se.fit, ymax = fit+1.96*se.fit), fill = "grey70")+
      geom_line(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit,
                               group = interaction(Caller,eval(parse(text = colnames(df)[index]))), 
                               color = Caller, linetype = eval(parse(text = colnames(df)[index]))))+
      ggtitle(paste("Precision", "across", xlabel))+
      scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
      scale_x_continuous(breaks = seq(x_min, x_max, by = x_by), limits = c(x_min,x_max))+
      scale_color_manual(values = color_SVCaller[names(color_SVCaller) %in% df$Caller], name = "SV Caller",labels = SV_caller_label[SV_caller %in% df$Caller])+
      scale_linetype_manual(values = line_type[names(line_type) %in% unique(df[,index])])+
      labs(y = "Precision", x = xlabel, linetype = legend_label)+
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
  #rmarkdown::render("app.R", output_format = "all",output_file = "report"
                    # params = params,
                    #envir = new.env(parent = globalenv())
  #)
  #output$report <- downloadHandler(

    # For PDF output, change this to "report.pdf"
    #filename = "report.html",
    #content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      #tempReport <- file.path(tempdir(), "report.Rmd")
      #tempReport <- file.path( "./report.Rmd")
      #file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
     # params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      #rmarkdown::render(tempReport, output_format = "all",output_file = file,
                       # params = params,
                       #envir = new.env(parent = globalenv())
      #)
   # }
  #)
  
})

shinyApp(ui, server)