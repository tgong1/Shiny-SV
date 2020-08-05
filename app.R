library(shiny)
library(shinyjs)
library(gridExtra)

# Source helpers ----
source("helpers.R")

ui <- shinyUI(
  
  bootstrapPage( 
    shinyjs::useShinyjs(),
  title="Shiny-SoSV",
  
  #fluidPage(
  #list(tags$head(HTML('<link rel="icon", href="logo.png", type="image/png" />'))
  #),
  
  #tagList(
    #tags$head(tags$script(type="text/javascript", src = "code.js")),
    tags$style(HTML(".navbar-default .navbar-nav > li > a {color:black;}
                    .navbar-default .navbar-nav > .active > a,
                  .navbar-default .navbar-nav > .active > a:focus,
                  .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #337ab7; font-weight: bold;}
                  .navbar-default .navbar-nav > li > a:hover {color: white; background-color:#337ab7; font-weight: bold;}
                  ")),
  navbarPage(
    title=div(img(src ="logo_hcpcgcolors_capSV.png",height= "56px", width = "99px", align = "left",style = "margin:-10px 0px")),
    id = "Shiny-SoSV",
    
  tabPanel(title = "Home", 
           value = "introduction", 
           fluidRow(column(width=10,offset=1,
           img(src = "logo_hcpcgcolors_capSV.png",height="30%", width="30%",style = "display: block; margin-left: auto; margin-right: auto; margin-top: 0px; margin-bottom: -25px"), 
           #fluidRow(column(width = 5, offset = 2,
           #                img(src = "logo_hcpcgcolors_capSV.png",height="30%", width="30%",style = "display: block; margin-left: auto; margin-right: auto; margin-top: 0px; margin-bottom: -25px")),
           #         column(width = 2, offset = 8,
           #                img(src = "lablogo.png",height="30%", width="30%"))),
           #h1("Shiny-SoSV", align = "center"),
           h3("A web-based performance calculator for somatic structural variant detection", align = "center"),
           h5("Shiny-SoSV provides an interactive and visual platform to explore the impact of different parameters on the performance to detect somatic structural variants from short-read whole genome sequencing.", align = "center"),
           br(),
           p("Somatic structural variants are an important contributor to cancer development and evolution. ",
             "Accurate detection of these complex variants from whole genome sequencing data is influenced by a multitude of parameters. ",
             "However, there are currently no tools for guiding study design nor are there applications that could predict the performance of somatic structural variant detection. ",
             "To address this gap, we developed Shiny-SoSV, a user-friendly web-based calculator for determining the impact of common variables on the sensitivity, precision and F1 score of somatic structural variant detection, including choice of variant detection tool, sequencing depth of coverage, variant allele fraction, and variant breakpoint resolution. ", 
             "Using simulation studies, we determined singular and combinatoric effects of these variables, modelled the results using a generalised additive model, allowing structural variant detection performance to be predicted for any combination of predictors. ",
             "Shiny-SoSV provides an interactive and visual platform for users to easily compare individual and combined impact of different parameters. ",
             "It predicts the performance of a proposed study design, on somatic structural variant detection, prior to the commencement of benchwork. "),
           p("If you use this web app, please cite:"),
           p("Tingting Gong, Vanessa M Hayes, Eva KF Chan. Shiny-SoSV: A web app for interactive evaluation of somatic structural variant calls. BioRxiv 668723; doi:", 
             a("https://doi.org/10.1101/668723", href="https://doi.org/10.1101/668723")),
           br(),
           #h4("Shiny-SoSV can be access here"),
           fluidRow(column(10,offset=5,actionButton('jumpToEvaluation', 'Launch Shiny-SoSV', #class = "btn-warning"
                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold"))),
                                                    #style="color: #fff; background-color: #337ab7; border-color: #2e6da4; padding:4px;font-size:100%; font-weight: bold"))),
                                                    #style="color: #000; padding:4px;font-size:100%"))),
           br(),
           br(),
           br(),
           actionButton('jumpToEvaluation_svtype', 'Shiny-SoSV type', style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold"),
           p("Shiny-SoSV user interface for SV types."),
           br(),
           actionButton('jumpToInstallation', 'Installation', style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold"),
           p("Detailed description of how to obtain and install your own copy of Shiny-SoSV."),
           br(),
           actionButton('jumpToUserGuide', 'User Guide', style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold"),
           p("Explanation of Shiny-SoSV’s user interface."),
           br(),
           actionButton('jumpToUseCase', 'Example Use Cases', style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold"),
           p("Hypothetical scenarios of highlighting the Shiny-SoSV’s utility.")))
  ),
  
  tabPanel(shinyjs::useShinyjs(),title = "Shiny-SoSV", value = "evaluation",#tweaks,
           sidebarLayout(fluid=TRUE, 
                         sidebarPanel(checkboxGroupInput(inputId = "SVCaller3",
                                                      label = ("SV Caller(s)"),
                                                      choiceNames = list("Manta  ", "Lumpy ", "GRIDSS ", "SvABA ", "Delly "),
                                                      choiceValues = c("Manta", "Lumpy", "GRIDSS", "SvABA", "Delly"),
                                                      selected = "Manta", inline=TRUE),
                                   checkboxGroupInput(inputId = "SVCaller3.1",
                                                      label = ("SV Callers Union"),
                                                      choiceNames = list("Manta  ", "Lumpy ", "GRIDSS ", "SvABA ", "Delly "),
                                                      choiceValues = c("Manta", "Lumpy", "GRIDSS", "SvABA", "Delly"),
                                                      selected = c(), inline=TRUE),
                                   #selected = c("Manta","Lumpy"),inline=TRUE,width='400px'),
                                   checkboxGroupInput(inputId = "SVCaller3.2",
                                                      label = ("SV Caller Intersection"),
                                                      choiceNames = list("Manta  ", "Lumpy ", "GRIDSS ", "SvABA ", "Delly "),
                                                      choiceValues = c("Manta", "Lumpy", "GRIDSS", "SvABA", "Delly"),
                                                      selected = c(), inline=TRUE),
                                   
                                   sliderInput("VAF3", "Tumour purity/VAF:",
                                               min = 0.05, max = 1, value = 0.5, step= 0.01),
                                   checkboxGroupInput("VAF3.1",
                                                      label= NULL,
                                                      choices = c("0.2" = 0.2,"0.8"= 0.8),
                                                      selected = NULL),
                      
                                   
                                   sliderInput("T_coverage3", "Tumour coverage:", 
                                               min = 20, max = 100, value = 60, step = 0.1, post = "x"),
                                   checkboxGroupInput("T_coverage3.1",
                                                      label= NULL,
                                                      choices = c("30x","60x"),
                                                      selected = NULL),
                                   sliderInput("N_coverage3", "Normal coverage:", 
                                               min = 20, max = 100, value = 30, step = 0.1, post = "x"),
                                   sliderInput("BND_threshold3", "Breakpoint precision threshold:", 
                                               min = 2, max = 200, value = 100, step= 1, post = "bp"),
                                   width=3
                                    ),
                      mainPanel(checkboxGroupInput("measurements3",
                                        label="Evaluation measurement(s)",
                                        choices = list("Sensitivity","Precision","F1 score"),
                                        selected = "Sensitivity",inline=TRUE),
                                 radioButtons("X_axis3",
                                              label="Evaluation across",
                                              choices = c("Tumour purity/VAF" = "VAF","Tumour coverage" = "T_coverage","Normal coverage" = "N_coverage","Breakpoint precision threshold" = "BND_threshold"),
                                              selected = "VAF",inline=TRUE),
                                 textOutput("txtOutput3"),
                                 radioButtons("type3",
                                              label="SV type",
                                              choices = c("Total" = "","Deletion" = "DEL","Duplication" = "DUP","Domestic insertion" = "DINS","Foreign insertion" = "FINS","Inversion" = "INV","Translocation" = "TRA"),
                                              selected = "",inline=TRUE),
                                 fluidRow(column(4,numericInput("objective3.1", label= "Your desired sensitivity", value = 0.7, min=0, max = 1, step = 0.1)),
                                          column(4,numericInput("objective3.2", label= "Your desired precision", value = 0.7, min=0, max = 1, step = 0.1)),
                                          column(4,numericInput("objective3.3", label= "Your desired F1 score", value = 0.7, min=0, max = 1, step = 0.1))),
                                 plotOutput("Plot3.1"),
                                 br(),
                                 fluidRow(column(6,tableOutput('table3.1')), 
                                          column(6,tableOutput('table3.2'))),
                                 plotOutput("Plot3.2"),
                                 br(),
                                 fluidRow(column(6,tableOutput('table3.3'))),
                                 width=9))
        ),
  tabPanel(shinyjs::useShinyjs(),title = "Shiny-SoSV type", value = "evaluation_svtype", #tweaks,
           sidebarLayout(fluid=TRUE, sidebarPanel(
                           # radioButtons(inputId = "SVCaller2",
                           #              label = ("SV Caller"),
                           #              choiceNames = as.character(list("Manta  ", "Lumpy ", "GRIDSS ", "SvABA ", "Delly ")),
                           #              choiceValues = c("Manta", "Lumpy", "GRIDSS", "SvABA", "Delly"),
                           #              selected = "Manta", inline=TRUE),
                           radioButtons(inputId = "SVCallset",
                                        label = ("Choose the SV call set"),
                                        choiceNames = as.character(list("Individual  ", "Union ", "Intersection ")),
                                        choiceValues = c("", "Union", "Intersect"),
                                        
                                        selected = "", inline=TRUE),
                           
                           radioButtons(inputId = "SVCaller2.1",
                                             label = ("SV caller"),
                                             choiceNames = list("Manta  ", "Lumpy ", "GRIDSS ", "SvABA ", "Delly "),
                                             choiceValues = c("Manta", "Lumpy", "GRIDSS", "SvABA", "Delly"),
                                             selected = "Manta", inline=TRUE),
                           radioButtons(inputId = "SVCaller2.2",
                                             label = ("Another SV caller to combine"),
                                             choiceNames = list("Manta  ", "Lumpy ", "GRIDSS ", "SvABA ", "Delly "),
                                             choiceValues = c("Manta", "Lumpy", "GRIDSS", "SvABA", "Delly"),
                                             selected =  "Manta", inline=TRUE),
                           
                           textOutput("txtOutput2"),
                           #checkboxGroupInput(inputId = "SVCaller2",
                           #                   label = ("SV Caller(s)"),
                           #                   choiceNames = list("Manta  ", "Lumpy ", "GRIDSS ", "SvABA ", "Delly "),
                           #                   choiceValues = c("Manta", "Lumpy", "GRIDSS", "SvABA", "Delly"),
                           #                   selected = "Manta", inline=TRUE),
                           #checkboxGroupInput(inputId = "SVCaller2.1",
                           #                   label = ("SV Callers Union"),
                           #                   choiceNames = list("Manta  ", "Lumpy ", "GRIDSS ", "SvABA ", "Delly "),
                           #                   choiceValues = c("Manta", "Lumpy", "GRIDSS", "SvABA", "Delly"),
                           #                   selected = c(), inline=TRUE),
                           #selected = c("Manta","Lumpy"),inline=TRUE,width='400px'),
                           # checkboxGroupInput(inputId = "SVCaller2.2",
                           #                    label = ("SV Caller Intersection"),
                           #                    choiceNames = list("Manta  ", "Lumpy ", "GRIDSS ", "SvABA ", "Delly "),
                           #                    choiceValues = c("Manta", "Lumpy", "GRIDSS", "SvABA", "Delly"),
                           #                    selected = c(), inline=TRUE),
                           sliderInput("VAF2", "Tumour purity/VAF:",
                                       min = 0.2, max = 1, value = 0.5, step= 0.01),
                           checkboxGroupInput("VAF2.1",
                                              label= NULL,
                                              choices = c("0.2" = 0.2,"0.8"= 0.8),
                                              selected = NULL),
                           sliderInput("T_coverage2", "Tumour coverage:",
                                       min = 30, max = 100, value = 60, step = 0.1, post = "x"),
                           checkboxGroupInput("T_coverage2.1",
                                              label= NULL,
                                              choices = c("30x","60x"),
                                              selected = NULL),
                           sliderInput("N_coverage2", "Normal coverage:",
                                       min = 20, max = 100, value = 30, step = 0.1, post = "x"),
                           sliderInput("BND_threshold2", "Breakpoint precision threshold:",
                                       min = 2, max = 200, value = 100, step= 1, post = "bp"),
                           width=3
                         ),
           mainPanel(checkboxGroupInput("type2",
                                  label="SV type(s)",
                                  choices = c("Deletion" = "DEL","Duplication" = "DUP","Domestic insertion" = "DINS","Foreign insertion" = "FINS","Inversion" = "INV","Translocation" = "TRA"),
                                  selected = "DEL",inline=TRUE),
                     checkboxGroupInput("measurements2",
                                        label = "Evaluation measurement(s)",
                                        choices = list("Sensitivity","Precision","F1 score"),
                                        selected = "Sensitivity",inline=TRUE),
                     radioButtons("X_axis2",
                                  label="Evaluation across",
                                  choices = c("Tumour purity/VAF" = "VAF","Tumour coverage" = "T_coverage","Normal coverage" = "N_coverage","Breakpoint precision threshold" = "BND_threshold"),
                                  selected = "VAF",inline=TRUE),
                     fluidRow(column(4,numericInput("objective2.1", label= "Your desired sensitivity", value = 0.7, min=0, max = 1, step = 0.1)),
                              column(4,numericInput("objective2.2", label= "Your desired precision", value = 0.7, min=0, max = 1, step = 0.1)),
                              column(4,numericInput("objective2.3", label= "Your desired F1 score", value = 0.7, min=0, max = 1, step = 0.1))),
                     
                     plotOutput("Plot2.1"),
                     br(),
                     fluidRow(column(6,tableOutput('table2.1')), 
                              column(6,tableOutput('table2.2'))),
                     plotOutput("Plot2.2"),
                     br(),
                     fluidRow(column(6,tableOutput('table2.3'))),
                     width=9)
           )
  ),
  tabPanel(title = "Installation", value = "installation", fluidRow(column(width=10,offset=1,
           h4("Direct Web Access"),
           p("Shiny-SoSV is hosted on shinyapp.io. The easiest way to access Shiny-SoSV is via the direct URL,", em("https://hcpcg.shinyapps.io/Shiny-SoSV/.")),
           #br(),
           h4("Launching from the GitHub repository"),
           p("Download R or RStudio and run the following commands once to set up the environment:"), 
           code("install.packages(c('shiny','shinyjs','ggplot2', 'gridExtra', 'ggsci'))"),
           br(),
           br(),
           p("Run the shiny app with command in R:"),
           code("library(shiny)"),
           br(),
           code("runGitHub('Shiny-SoSV', 'tgong1')"),
           br(),
           br(),
           h4("GitHub"),
           p("Souce code is available at the GitHub repository tgong1/Shiny-SoSV."),
           p("To copy the repo to your local machine, use the command:"),
           code("git clone https://github.com/tgong1/Shiny-SoSV.git"),
           br(),
           br(),
           p("The data_SV and scripts_R folders of the repo contains all evaluation results and R script of prediction model fitting respectively underlying the web app."),
           br(),
           strong("Note to SV caller developers:"),
           p("If you would like to include your SV caller in Shiny-SoSV, please get in touch. We would be able to provide you a copy of the simulated aligned bam files to run your software on!")))
           
  ),
  tabPanel(title = "User Guide", value = "userguide",fluidRow(column(width=10,offset=1,
           h4("Shiny-SoSV provides predictions of the impact of common variables (SV caller, sequencing coverage, tumour allele frequencies, tolerance of breakpoint precision) on the performance (sensitivity, precision and F1 score) of somatic SV calls through interactive plots. Please click on “Launch Shiny-SoSV” in Home page or “Shiny-SoSV” tab on top navigation panel for the main interface below:"),
           #img(src = "UserGuide.png",height=925, width=1914),
           #img(src = "UserGuide.png"),
           img(src = "UserGuide.png",height="100%", width="100%"),
           #img(src = "UserGuide.pdf",height="100%", width="100%"),
           h5("Predictor variables to be examined and visualised: "),
           h5("Checkboxes allow additional prediction lines to be added/removed from the plots."),
           p("[1] SV Caller(s): One or more SV callers can be examined (distinguished by line colour in the plots). "),
           p("[2] SV Callers Union: Union set of SV callers can be examined (distinguished by line colour in the plots). Union sets (A∪B sets) are SV calls identified by either caller A or caller B. Caller A is the “dominant caller” such that any overlapping calls in the final callset is taken from the output from caller A (including coordinates and SV types). "),
           p("[3] SV Callers Intersection: Intersection set of SV callers can be examined (distinguished by line colour in the plots). Interaction sets (A∩B sets) are SV calls identified by BOTH A and B. Caller A is the “dominant caller” such that any overlapping calls in the final callset is taken from the output from caller A (including coordinates and SV types). "),
           h5("Slider bars allow different values of the corresponding variable to be altered in the prediction. "),
           p("[4] Tumour purity/VAF: Up to three values of this predictor can be visualised on the plots (distinguished by line type on the plots) evaluating across “Tumour coverage” [10]. Two of the values are preset to 0.2 and 0.8 (as checkboxes). The third can be any value between 0.05 and 1 (adjustable via the sliderbar) when “Total” [13] is selected, or between 0.2 and 1 when individual SV type is selected. When evaluating across “Normal coverage” [11] or “Breakpoint precision threshold” [12], only one VAF values can be elected, and this can be done via the sliderbar. When evaluating across “Tumour purity/VAF” [9], this option is disabled (greyed out). "),
           p("[5] Tumour coverage: When evaluating across “Tumour purity/VAF” [9], “Normal coverage” [11] or “Breakpoint precision threshold” [12], three Tumour coverage values can be examined simultaneously, including two preset values of 30x and 60x and a third definable by the user via the slider bar between 20x to 100x when “Total” [13] selected or between 30x to 100x when individual SV type selected. When evaluating across “Tumour Coverage” [10], this option is disabled (greyed out)."),
           p("[6] Normal coverage: Only one value can be evaluated at any one time. When evaluating across “Normal coverage” [11], this option is disabled."),
           p("[7] Breakpoint precision threshold: Only one value can be evaluated at any one time. When evaluating across “Breakpoint precision threshold” [12], this option is disabled."),
           br(),
           h5("One of four predictors to be plotted on the x-axis using the “Evaluation across” radio button, including:"),
           p("[9] “Tumour purity/VAF”: proportion of sequencing reads supporting the variant in the tumour sample. The minimum value available for overall and individual SV type performance estimation is 0 and 0.2 respectively."),
           p("[10] “Tumour coverage”: the depth of sequencing coverage of the tumour sample. The minimum value available for overall and individual SV type performance estimation is 20x and 30x respectively. "),
           p("[11] “Normal coverage”: the depth of sequencing coverage of the normal sample."),
           p("[12] “Breakpoint precision threshold”: the precision of the breakpoint calls in nucleotide units."),
           h5("SVs are broadly classified into six types. Performance measures can be estimated for:"),
           p("[13] “Total”: all SV types aggregated in one call set."),
           p("[14] “Deletion”: the SV event of a DNA segment removal from the genome."),
           p("[15] “Duplication”: also known as tandem duplication, is the event of copying a DNA segment and inserting it beside the original copy. The precision and F1 score for duplication of SvABA can also for insertion, as these two SV types are not distinguishable by SvABA. "),
           p("[16] “Domestic insertion”: the addition of a DNA segment copied from a distant site of the same genome (i.e. “copy-and-paste”). "),
           p("[17] “Foreign insertion”: the addition of a novel sequence, not known to be present in the sample genome. The precision and F1 score only estimated for a general insertion if this SV type or “Domestic insertion” [16] selected. Due to the limitations of callers in detecting insertion, the sensitivity estimation for this SV type by Lumpy and SvABA and precision and F1 score estimation by Lumpy, SvABA and GRIDSS are not available."),
           p("[18] “Inversion”: the inversion of a DNA segment at the same locus."),
           p("[19] “Translocation”: involves the deletion of a DNA segment from one locus and its reinsertion at another locus (i.e. “cut-and-paste”). Callers report breakend (BND) of fusion junctions for inter-chromosomal events, including domestic insertion and translocation, so the precision and F1 score of BND for will be shown for this SV type."),
           h5("To visualise your desired sensitivity, precision and F1 score lines on the plot and make decisions on variables based on the objective:"),
           p("[20] Enter your desired sensitivity, precision and F1 score."),
           p("[21] Tables show the lower bound of variables to achieve desired sensitivity, precision and F1 score under the visualisation."),
           br(),
           h4("Shiny-SoSV type interface provides predictions of the impact of common variables (sequencing coverage, tumour allele frequencies, tolerance of breakpoint precision) on the performance (sensitivity, precision and F1 score) of different SV type calling for each call set through interactive plots. Please click on “Shiny-SoSV type” tab on top navigation panel for the interface:"),
           img(src = "UserGuide_type.png",height="100%", width="100%"),
           h5("Predictor variables can be altered and visualized in the same way as the main Shiny-SoSV interface, with the differences of: "),
           p("[22] Choose the SV call set: Selection of the individual, union or intersection call set. "),
           p("[23] SV caller: One individual SV caller or the first caller for union or intersection call sets. Any overlapping calls in the combined call set is taken from the output from the first caller. "),
           p("[24] Another SV caller to combine: The second SV caller for union or intersection call set. When “individual” in [22] selected, this option is disabled. "),
           p("[25] SV type(s): One or more SV types can be examined (distinguished by line colour in the plots)."),
           
           tags$hr(),
           strong("A note on the parameters:"),
           tags$ul(
             tags$li("Histopathological estimates of tumour purity or percentage tumour is typically the upper bound of observed VAF in genomics studies in part due to the likelihood of the presence of sub-clonality."), 
             tags$li("Breakpoint precision threshold is the maximum difference (in bp) between what was reported by the corresponding SV caller and what was simulated.")
           )
  ))),
  tabPanel(title = "Example Use Cases", value = "usecase",fluidRow(column(width=10,offset=1,
           h5("Hypothetical scenarios of highlighting the Shiny-SoSV’s utility:"),
           fluidRow(column(width = 11, h5("Scenario 1: I have a cohort of matched-normal cancer samples, each with different histopathology-estimates of tumour purity.", 
                                 "Our WGS (or bioinformatics) pipeline uses Lumpy, which could potentially be changed, though we’d rather not have to.", 
                                 "How much coverage would I need to achieve at least 80% sensitivity and >90% precision on somatic SV calls.")),
                    column(1, 
                           actionButton("S1", label = "", icon = icon("angle-down"), class = "btn btn-info")
                    )),
           fluidRow(column(width = 11,hidden(htmlOutput(outputId = "text_S1")))),
           hidden(imageOutput(outputId = "image_S1", width = "100%", height = "100%")),
           #hidden(imageOutput(outputId = "image_S1", width = "100%", height = "500px")),
           #fluidRow(column(3,br(),br())),
        
           fluidRow(column(11,h5("Scenario 2: I have a handful of tumour samples sequenced to 60x,", 
                                 "I am wondering if I can generate low pass coverage on the matched normal and still obtain good sensitivity and precision.", 
                                 "We haven’t decided on which SV caller to use yet.")),
                    column(1, 
                           actionButton("S2", label = "", icon = icon("angle-down"), class = "btn btn-info")
                    )),
    
           fluidRow(column(width = 11,hidden(htmlOutput(outputId = "text_S2")))),
           hidden(imageOutput(outputId = "image_S2a", width = "100%", height = "100%")),
           fluidRow(column(width = 11,hidden(htmlOutput(outputId = "text_S2.1")))),
           hidden(imageOutput(outputId = "image_S2b", width = "100%", height = "100%")),
           hidden(imageOutput(outputId = "image_S2c", width = "100%", height = "100%")),
           
           fluidRow(column(11,h5("Scenario 3: I have a cancer sample with estimated tumour purity estimated of about 20%.", 
                                 "It is a precious sample, and we want to call confident SVs.", 
                                 "How much should we sequence to get >90% sensitivity and precision. Is it even possible?")),
                    column(1, 
                           actionButton("S3", label = "", icon = icon("angle-down"), class = "btn btn-info")
                    )),
           fluidRow(column(width = 11,hidden(htmlOutput(outputId = "text_S3")))),
           hidden(imageOutput(outputId = "image_S3", width = "100%", height = "100%")),
           
           fluidRow(column(11,h5("Scenario 4: I have a patient whom we cannot find a pathological SNV/indel from a 30x WGS.",
                                 " We think the driver mutation may be a structural variant. We don’t know much about SVs or how to call them.", 
                                 "Which SV caller should we use? Our NGS data suggested the tumour purity is about 50%.", 
                                 "Do we need to sequence more?", 
                                 "We care about both false positives and false negatives, but would prefer to be confident in what we find than finding more of something that may be false.")),
                    column(1, 
                           actionButton("S4", label = "", icon = icon("angle-down"), class = "btn btn-info")
                    )),
           fluidRow(column(width = 11,hidden(htmlOutput(outputId = "text_S4")))),
           hidden(imageOutput(outputId = "image_S4", width = "100%", height = "100%")),
           p()
  ))),
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
              Shiny.onInputChange(variableName, null);
              });
              ")

)))
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
  
  observeEvent(input$jumpToEvaluation_svtype, {
    updateTabsetPanel(session, "Shiny-SoSV",
                      selected = "evaluation_svtype")
  })
  
  observeEvent(input$S1, {
    toggle('text_S1')
    output$text_S1 <- renderText({paste("To address this using Shiny-SoSV, as demonstrated in the figure, the user would evaluate “Sensitivity” and “Precision” across “Tumour purity/VAF”. On the sidebar, choose any (combination of) SV caller(s), including Lumpy (the implemented caller in this scenario). Here, Lumpy, GRIDSS and the union set of them have been selected. Three tumour coverages have also been selected for comparison: presets 30x and 60x selected via the checkboxes and 100x via slider bar. All other parameters are left as default.",
                                  "<br>",
                                    "From this, it is immediately obvious that tumour purity has a great impact on sensitivity, while little impact on precision, particularly for tumour purity within 5% and 30%. In addition, the union set of Lumpy and GRIDSS has the higher sensitivity, but slightly lower precision. 
Entering the objectives (sensitivity > 80% and precision > 90%), the table shows that Lumpy cannot achieve 80% sensitivity even with tumour sequencing coverage at 100x. While, at 60x, the union set of Lumpy and GRIDSS can achieve > 80% sensitivity when tumour purity >30%. Therefore, the user could include GRIDSS into the pipeline and sequencing at 60x. 
",
                                   "</p>")})
  })
  
  observeEvent(input$S1, {
    toggle('image_S1')
    output$image_S1 <- renderImage({
      filename <- normalizePath(file.path('./images',paste0('FigureS1', '.png')))
      list(src = filename, height="100%", width="100%", alt="Figure 1")
      #list(src = filename, height=500, width=900, alt="Figure 1")
    }, deleteFile = FALSE)
  })
  
  
  observeEvent(input$S2, {
    toggle('text_S2')
    #output$text_S2 <- renderText({paste("There are several ways to address this. As we don’t know the expected tumour purity of these samples, we could evaluate across “Tumour Purity/VAF”, fixing tumour coverage at 60x and Normal coverage at the lowest option of 15x. Entering the desired sensitivity and precision, for example at least 80% sensitivity and precision, as shown in Figure 2a. Selecting all five SV callers shows that it is possible to achieve sensitivity above 80% with Manta but only if the tumour sample is at least 24% pure. Further selecting the union set of Manta and GRIDSS (the two best performing callers under this setting) suggests we might be able to reach > 80% sensitivity with tumour purity as low as 17%.",
    #                                    "</p>")})
    output$text_S2 <- renderText({paste("There are several ways to address this. Start by choosing F1 score, the harmonic mean of the sensitivity and precision, as the evaluation measurement. As we don’t know the expected tumour purity of these samples, we could evaluate across “Tumour Purity/VAF”, fixing Tumour coverage at 60x and Normal coverage at the lowest option of 20x. Entering the desired F1 score, for example at least 90% F1 score, as shown in Figure 2a. Selecting all five SV callers shows that it is possible to achieve F1 score above 90% with Manta but only if the tumour sample is at least 27% pure. Further selecting the union set of Manta and GRIDSS (the two best performing callers under this setting) suggests we might be able to reach > 90% F1 score with tumour purity as low as 21%.",
                                        "</p>")}) 
  })
  observeEvent(input$S2, {
    toggle('image_S2a')
    output$image_S2a <- renderImage({
      filename <- normalizePath(file.path('./images',paste('FigureS2a', '.png', sep='')))
      list(src = filename, height="100%", width="100%", alt="Figure 2")
    }, deleteFile = FALSE)
  })
  
  observeEvent(input$S2, {
    toggle('text_S2.1')
    #output$text_S2.1 <- renderText({paste("Another way to address this question would be to evaluate across “Normal coverage” and setting Tumour coverage to 60x. Again, as we don’t know the purity of the tumour samples, we may need to explore a bit. For example, setting Tumour purity to 50% shown in Figure 2b, we see that the depth of coverage of the normal sample does not actually have very significant impact on the sensitivity or precision. Rather, it is the SV callers that have the biggest impact. Again, we see sensitivity exceeds 80% with Manta and union set of Manta and GRIDSS, providing tumour purity is > 50%. Lowering Tumour purity to 20% shown in Figure 2c, we note the need to use a combination of SV callers in order to achieve the desire sensitivity with low pass coverage of normal sample (15x).",
    #                                      "</p>")})
    output$text_S2.1 <- renderText({paste("Another way to address this question would be to evaluate across “Normal coverage” and setting Tumour coverage to 60x. Again, as we don’t know the purity of the tumour samples, we may need to explore a bit. For example, setting Tumour purity to 80% shown in Figure 2b, we see that the depth of coverage of the normal sample does not actually have very significant impact on the F1 score. Rather, it is the SV callers that have the biggest impact. Again, we see F1 score exceeds 90% with Manta and union set of Manta and GRIDSS, providing tumour purity is > 80%. Lowering Tumour purity to 50% shown in Figure 2c, we note the need to use a combination of SV callers in order to achieve the desired F1 score with low pass coverage of normal sample (20x).",
                                          "</p>")})
  })
  
  observeEvent(input$S2, {
    toggle('image_S2b')
    output$image_S2b <- renderImage({
      filename <- normalizePath(file.path('./images',paste('FigureS2b', '.png', sep='')))
      list(src = filename, height="100%", width="100%", alt="Figure 2")
    }, deleteFile = FALSE)
  })
  
  observeEvent(input$S2, {
    toggle('image_S2c')
    output$image_S2c <- renderImage({
      filename <- normalizePath(file.path('./images',paste('FigureS2c', '.png', sep='')))
      list(src = filename, height="100%", width="100%", alt="Figure 2")
    }, deleteFile = FALSE)
  })
  
  observeEvent(input$S3, {
    toggle('text_S3')
    #output$text_S3 <- renderText({paste("To address this using Shiny-SoSV, as demonstrated in the figure below, the user can evaluate both “Sensitivity” and “Precision” across “Tumour coverage”. On the sidebar, the user can choose all SV callers and their union sets aiming to increase sensitivity and set tumour purity/VAF to 0.2, while fixing all other parameters as default. ",
    #                                    "<br>","Entering the desired sensitivity and precision (90%) can help the user quickly calculate how much to sequence and which SV caller to choose. From the table and the plots, we can see it is possible to attain > 90% sensitivity with both Manta and GRIDSS, sequencing at least 77x coverage on the tumour. Moving the slider bar from 30x (default) to 90x to increase matched normal coverage has little improvement on sensitivity. While >90% precision can easily be reached (regardless of SV caller, depth of sequencing coverage, or breakpoint precision threshold). ",
    #                                    "</p>")})
    output$text_S3 <- renderText({paste("To address this using Shiny-SoSV, as demonstrated in the figure below, the user can evaluate both “Sensitivity” and “Precision” across “Tumour coverage”. On the sidebar, the user can choose all SV callers and their union sets aiming to increase sensitivity and set tumour purity/VAF to 0.2. As the user want to call confident SVs, a higher breakpoint resolution can be required by setting breakpoint precision threshold to a lower value (e.g. 5bp), while fixing all other parameters as default. ",
                                        "<br>","Entering the desired sensitivity and precision (90%) can help the user quickly calculate how much to sequence and which SV caller to choose. From the table and the plots, we can see that while >90% precision can easily be reached (regardless of SV caller, depth of sequencing coverage, or breakpoint precision threshold), the highest sensitivity can be attained is around 85% with such low tumour purity (20%). In addition, moving the slider bar from 30x (default) to 100x to increase matched normal coverage has little improvement on sensitivity. If we are to extrapolate on the plot shown, it may be possible to attain > 90% sensitivity with > 120x coverage on the tumour.",
                                        "</p>")})
  })
  
  observeEvent(input$S3, {
    toggle('image_S3')
    output$image_S3 <- renderImage({
      filename <- normalizePath(file.path('./images',paste('FigureS3', '.png', sep='')))
      list(src = filename, height="100%", width="100%", alt="Figure 3")
    }, deleteFile = FALSE)
  })
  
  
  
  observeEvent(input$S4, {
    toggle('text_S4')
    #output$text_S4 <- renderText({paste("To address this using Shiny-SoSV, as demonstrated in Figure, the user would select to evaluate both “Sensitivity” and “Precision” across “Tumour coverage”. On the sidebar, the user can choose all SV callers (Manta, Lumpy, GRIDSS, SvABA and Delly) for comparison in the first instance, setting Tumour purity to 0.5 and fixing all other parameters as default. ",
    #                                    "<br>","With these settings, Manta has the highest sensitivity, with 10% and 15% higher sensitivity than GRIDSS and Lumpy respectively. All SV callers can reach >90% precision, except SvABA. Considering the user’s preference to miss calls rather than make false calls, they may want to consider using two different SV callers and taking the intersection callset. From the table, the intersection callset from any two SV callers would increase the precision by around 1-18%, however, it is worth noting that sensitivity does drop. To explore how much more sequencing is needed to compensate the dropping sensitivity, the user can view the lower bound of variables required for the desired sensitivity. If the desired sensitivity is 70%, as shown in table, using the intersection set of Manta and GRIDSS would need another 14x depth of sequencing coverage on tumour, comparing to using Manta only.",
    #                                    "<br>","Therefore, the plots and table provide sufficient information for users to make educated decisions tailored for the situation. ",
    #                                    "</p>")})
    output$text_S4 <- renderText({paste("To address this using Shiny-SoSV, as demonstrated in Figure, the user would select to evaluate both “Sensitivity” and “Precision” across “Tumour coverage”. On the sidebar, the user can choose all SV callers (Manta, Lumpy, GRIDSS, SvABA and Delly) for comparison in the first instance, setting tumour purity to 0.5, setting breakpoint precision threshold to low (e.g. 5bp) aiming to have a high breakpoint resolution and fixing all other parameters as default. ",
                                        "<br>","With these settings, Manta has the highest sensitivity, with 10% and 15% higher sensitivity than GRIDSS and Lumpy respectively. All SV callers can reach >90% precision, except SvABA. Considering the user’s preference to miss calls rather than make false calls, they may want to consider using two different SV callers and taking the intersection callset. From the table, the intersection callset from any two SV callers would increase the precision by around 1-5%, however, it is worth noting that sensitivity does drop. To explore how much more sequencing is needed to compensate the dropping sensitivity, the user can view the lower bound of variables required for the desired sensitivity. If the desired sensitivity is 65%, as shown in table, using the intersection set of Manta and GRIDSS would need another 12x depth of sequencing coverage on tumour, while still have 8% lower sensitivity comparing to using Manta only.",
                                        "<br>","Therefore, the plots and table provide sufficient information for users to make educated decisions tailored for the situation. ",
                                        "</p>")})
                                        
  })
  
  observeEvent(input$S4, {
    toggle('image_S4')
    output$image_S4 <- renderImage({
      filename <- normalizePath(file.path('./images',paste('FigureS4', '.png', sep='')))
      list(src = filename, height="100%", width="100%", alt="Figure 4")
    }, deleteFile = FALSE)
  })
  
  # disable("SVCaller3.1")
  # disable("SVCaller3.2")
  # disable("SVCaller2.2")
  MeasureInput2 <- reactive({c("Sensitivity","Precision","F1 score") %in% input$measurements2})
  MeasureInput3 <- reactive({c("Sensitivity","Precision","F1 score") %in% input$measurements3})

  
  observe({if(input$type3 != ''){
    updateSliderInput(session, "VAF3", min = 0.2)
  }
  })
  
  observe({if(input$type3 != ''){
    updateSliderInput(session, "T_coverage3", min = 30)
  }
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
    disable("VAF3.1")
    enable("T_coverage3")
    enable("T_coverage3.1")
    enable("N_coverage3")
    disable("BND_threshold3")
  }})
  
  observe({if(input$X_axis2 == "VAF"){
    disable("VAF2")
    disable("VAF2.1")
    enable("T_coverage2")
    enable("T_coverage2.1")
    enable("N_coverage2")
    enable("BND_threshold2")
  }else if(input$X_axis2 == "T_coverage"){
    enable("VAF2")
    enable("VAF2.1")
    disable("T_coverage2")
    disable("T_coverage2.1")
    enable("N_coverage2")
    enable("BND_threshold2")
  }else if(input$X_axis2 == "N_coverage"){
    enable("VAF2")
    disable("VAF2.1")
    enable("T_coverage2")
    enable("T_coverage2.1")
    disable("N_coverage2")
    enable("BND_threshold2")
  }else if(input$X_axis2 == "BND_threshold"){
    disable("VAF2.1")
    enable("T_coverage2")
    enable("T_coverage2.1")
    enable("N_coverage2")
    disable("BND_threshold2")
  }})
  observe({if(!MeasureInput3()[1]){ disable("objective3.1") }})
  observe({if(MeasureInput3()[1]){ enable("objective3.1") }}) 
  observe({if(!MeasureInput3()[2]){ disable("objective3.2") }})
  observe({if(MeasureInput3()[2]){ enable("objective3.2") }})
  observe({if(!MeasureInput3()[3]){ disable("objective3.3") }})
  observe({if(MeasureInput3()[3]){ enable("objective3.3") }})
  
  observe({if(!MeasureInput2()[1]){ disable("objective2.1") }})
  observe({if(MeasureInput2()[1]){ enable("objective2.1") }}) 
  observe({if(!MeasureInput2()[2]){ disable("objective2.2") }})
  observe({if(MeasureInput2()[2]){ enable("objective2.2") }})
  observe({if(!MeasureInput2()[3]){ disable("objective2.3") }})
  observe({if(MeasureInput2()[3]){ enable("objective2.3") }})
  
  observe({if(input$SVCallset == ""){
    disable("SVCaller2.2")
  }else{
    enable("SVCaller2.2")
  }
  })
  
  SVCaller_name_input <- reactive({
   if(input$SVCallset == ""){
     SVCaller_name <- input$SVCaller2.1
     SVCaller_name_label <- input$SVCaller2.1
   }else if (input$SVCallset == "Union"){
     if(input$SVCaller2.1 != input$SVCaller2.2){
      SVCaller_name <- paste0(input$SVCaller2.1,input$SVCaller2.2,"Union")
      SVCaller_name_label <- paste0("Union set of ", input$SVCaller2.1,input$SVCaller2.2)
     }else{
       SVCaller_name <- input$SVCaller2.1
       SVCaller_name_label <- input$SVCaller2.1
     }
   }else if (input$SVCallset == "Intersect"){
     if(input$SVCaller2.1 != input$SVCaller2.2){
     SVCaller_name <- paste0(input$SVCaller2.1,input$SVCaller2.2,"Intersect")
     SVCaller_name_label <- paste0("Intersection set of ", input$SVCaller2.1,input$SVCaller2.2)
     }else{
       SVCaller_name <- input$SVCaller2.1
       SVCaller_name_label <- input$SVCaller2.1
     }
   }
    return(list(SVCaller_name, SVCaller_name_label))
  })
  
  # SVCaller_name_input <- reactive({
  #     SVCaller_name <- input$SVCaller2.1
  #     SVCaller_name_label <- input$SVCaller2.1
  #  
  #   return(list(SVCaller_name, SVCaller_name_label))
  # })
  
  #output$txtOutput2 <- renderText({ paste(input$SVCallset, input$SVCaller2.1, input$SVCaller2.2, "----", SVCaller_name_input())})
  
  newdata2 <- reactive({
    SVCaller_name <- SVCaller_name_input()[[1]]
    if(input$X_axis2 == "VAF"){
      T_coverage_label = paste0(c(input$T_coverage2,30,60),"x")
      VAF_label = c()
      line_type <- c("solid","twodash","dotted")
      names(line_type) <- T_coverage_label
      
      data <- data.frame(Caller = SVCaller_name,
                         T_coverage = rep(c(input$T_coverage2,30,60)),
                         N_coverage = input$N_coverage2,
                         VAF = rep(seq(0.2,1,0.01),each=length(T_coverage_label)),
                         BND_threshold = input$BND_threshold2)
      xlabel <- "Tumour purity/VAF"
      Tcov_var <- c(paste0(input$T_coverage2,"x"),input$T_coverage2.1)
      Ncov_var <- paste0(input$N_coverage2,"x")
      VAF_var <- paste0(seq(0.2,1,0.01))
      BND_var <- paste0(input$BND_threshold2)
      index <- 8
      legend_label <- "Tumour coverage"
      x_min <- 0.2
      x_max <- 1
      x_by <- 0.1
    }else if(input$X_axis2 == "T_coverage"){
      T_coverage_label = c()
      VAF_label = paste0(c(input$VAF2,0.2,0.8))
      line_type <- c("solid","twodash","dotted")
      names(line_type) <- VAF_label
      
      data <- data.frame(Caller = SVCaller_name,
                         T_coverage = rep(c(30:100),each = length(VAF_label)),
                         N_coverage = input$N_coverage2,
                         VAF = rep(c(input$VAF2,0.2,0.8)),
                         BND_threshold = input$BND_threshold2)
      xlabel <- "Tumour coverage (x)"
      Tcov_var <- paste0(c(30:100),"x")
      Ncov_var <- paste0(input$N_coverage2,"x")
      VAF_var <- c(paste0(input$VAF2),input$VAF2.1)
      BND_var <- paste0(input$BND_threshold2)
      index <- 10
      legend_label <- "Tumour purity/VAF"
      x_min <- 30
      x_max <- 100
      x_by <- 10
    }else if(input$X_axis2 == "N_coverage"){
      T_coverage_label = paste0(c(input$T_coverage2,30,60),"x")
      line_type <- c("solid","twodash","dotted")
      names(line_type) <- T_coverage_label
      VAF_label = c()
      data <- data.frame(Caller = SVCaller_name,
                         T_coverage = rep(c(input$T_coverage2,30,60)),
                         N_coverage = rep(c(20:100), each = length(T_coverage_label)),
                         VAF = input$VAF2,
                         BND_threshold = input$BND_threshold2)
      xlabel <- "Normal coverage (x)"
      Tcov_var <- c(paste0(input$T_coverage2,"x"),input$T_coverage2.1)
      Ncov_var <- paste0(c(20:100),"x")
      VAF_var <- paste0(input$VAF2)
      BND_var <- paste0(input$BND_threshold2)
      index <- 8
      legend_label <- "Tumour coverage"
      x_min <- 20
      x_max <- 100
      x_by <- 10
    }else if(input$X_axis2 == "BND_threshold"){
      T_coverage_label = paste0(c(input$T_coverage2,30,60),"x")
      line_type <- c("solid","twodash","dotted")
      names(line_type) <- T_coverage_label
      VAF_label = c()
      data <- data.frame(Caller = SVCaller_name,
                         T_coverage = rep(c(input$T_coverage2,30,60)),
                         N_coverage = input$N_coverage2,
                         VAF = input$VAF2,
                         BND_threshold = rep(c(2:200), each = length(T_coverage_label)))
      xlabel <- "Breakpoint precision threshold (bp)"
      Tcov_var <- c(paste0(input$T_coverage2,"x"),input$T_coverage2.1)
      Ncov_var <- paste0(input$N_coverage2,"x")
      VAF_var <- paste0(input$VAF2)
      BND_var <- paste0(c(2:200))
      index <- 8
      legend_label <- "Tumour coverage"
      x_min <- 0
      x_max <- 200
      x_by <- 20
    }
    return(list(data, T_coverage_label,VAF_label,Tcov_var, Ncov_var, VAF_var, BND_var,
                xlabel,line_type,index,legend_label,x_min,x_max,x_by))
  })
  
  newdata3 <- reactive({
    if(input$X_axis3 == "VAF"){
      T_coverage_label = paste0(c(input$T_coverage3,60,30),"x")
      line_type <- c("solid","twodash","dotted")
      names(line_type) <- T_coverage_label
      VAF_label =  c()
      data <- data.frame(Caller = SV_caller,
                         T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller)),
                         N_coverage = input$N_coverage3,
                         VAF = rep(seq(0.05,1,0.01),each=length(T_coverage_label)*length(SV_caller)),
                         BND_threshold = input$BND_threshold3)
      xlabel <- "Tumour purity/VAF"
      Tcov_var <- c(paste0(input$T_coverage3,"x"),input$T_coverage3.1)
      Ncov_var <- paste0(input$N_coverage3,"x")
      VAF_var <- paste0(seq(0.05,1,0.01))
      BND_var <- paste0(input$BND_threshold3)
      index <- 8
      legend_label <- "Tumour coverage"
      if(input$type3 == ""){x_min <- 0}else{x_min <- 0.2}
      x_max <- 1
      x_by <- 0.1
    }else if(input$X_axis3 == "T_coverage"){
      VAF_label = paste0(c(input$VAF3,0.2,0.8))
      line_type <- c("solid","twodash","dotted")
      names(line_type) <- VAF_label
      T_coverage_label = c()
      data <- data.frame(Caller = SV_caller,
                         T_coverage = rep(c(20:100),each = length(VAF_label)*length(SV_caller)),
                         N_coverage = input$N_coverage3,
                         VAF = rep(c(input$VAF3,0.2,0.8), each=length(SV_caller)),
                         BND_threshold = input$BND_threshold3)
      xlabel <- "Tumour coverage (x)"
      Tcov_var <- paste0(c(20:100),"x")
      Ncov_var <- paste0(input$N_coverage3,"x")
      VAF_var <- c(paste0(input$VAF3),input$VAF3.1)
      BND_var <- paste0(input$BND_threshold3)
      index <- 10
      legend_label <- "Tumour purity/VAF"
      if(input$type3 == ""){x_min <- 20}else{x_min <- 30}
      x_max <- 100
      x_by <- 10
    }else if(input$X_axis3 == "N_coverage"){
      T_coverage_label = paste0(c(input$T_coverage3,60,30),"x")
      line_type <- c("solid","twodash","dotted")
      names(line_type) <- T_coverage_label
      VAF_label =  c()
      data <- data.frame(Caller = SV_caller,
                         T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller)),
                         N_coverage = rep(c(20:100), each = length(T_coverage_label)*length(SV_caller)),
                         VAF = input$VAF3,
                         BND_threshold = input$BND_threshold3)
      xlabel <- "Normal coverage (x)"
      Tcov_var <- c(paste0(input$T_coverage3,"x"),input$T_coverage3.1)
      Ncov_var <- paste0(c(20:100),"x")
      VAF_var <- paste0(input$VAF3)
      BND_var <- paste0(input$BND_threshold3)
      index <- 8
      legend_label <- "Tumour coverage"
      x_min <- 20
      x_max <- 100
      x_by <- 10
    }else if(input$X_axis3 == "BND_threshold"){
      T_coverage_label = paste0(c(input$T_coverage3,60,30),"x")
      line_type <- c("solid","twodash","dotted")
      names(line_type) <- T_coverage_label
      VAF_label =  c()
      data <- data.frame(Caller = SV_caller,
                         T_coverage = rep(c(input$T_coverage3,60,30), each=length(SV_caller)),
                         N_coverage = input$N_coverage3,
                         VAF = input$VAF3,
                         BND_threshold = rep(c(2:200), each = length(T_coverage_label)*length(SV_caller)))
      xlabel <- "Breakpoint precision threshold (bp)"
      Tcov_var <- c(paste0(input$T_coverage3,"x"),input$T_coverage3.1)
      Ncov_var <- paste0(input$N_coverage3,"x")
      VAF_var <- paste0(input$VAF3)
      BND_var <- paste0(c(2:200))
      index <- 8
      legend_label <- "Tumour coverage"
      x_min <- 0
      x_max <- 200
      x_by <- 20
    }
    return(list(data, T_coverage_label,VAF_label,Tcov_var, Ncov_var, VAF_var, BND_var,
                xlabel,line_type,index,legend_label,x_min,x_max,x_by))
  })
  ########################################################################################################################  
  dataInput2.1 <- reactive({
    input_type <- input$type2
    SVCaller_name <- SVCaller_name_input()[[1]]
    data <- newdata2()[[1]]
    T_coverage_label <- newdata2()[[2]]
    VAF_label <- newdata2()[[3]]
    Tcov_var <- newdata2()[[4]]
    Ncov_var <- newdata2()[[5]]
    VAF_var <- newdata2()[[6]]
    BND_var <- newdata2()[[7]]
    
    df.Sensitivity <- c()
    for (SVTYPE in SVTYPE_all){
      if(!((SVCaller_name == "Lumpy" & SVTYPE == "FINS")|
           (SVCaller_name == "SvABA" & SVTYPE == "FINS")|
           (grepl("Lumpy",SVCaller_name) & grepl("Intersect", SVCaller_name) & SVTYPE == "FINS") | 
           (grepl("SvABA",SVCaller_name) & grepl("Intersect", SVCaller_name) & SVTYPE == "FINS") |
           (grepl("LumpySvABAUnion", SVCaller_name) & SVTYPE == "FINS") | 
           (grepl("SvABALumpyUnion", SVCaller_name) & SVTYPE == "FINS"))){
      newdata <- data
      assign("df.Sensitivity_caller",  data.frame(newdata,
                                                  #predict(eval(parse(text=paste0("gamsen_", SVCaller_name))),newdata,type = "response",se.fit = T,unconditional = TRUE),
                                                  predict(eval(parse(text=paste0("gamsen_",SVCaller_name, SVTYPE))),newdata,type = "response",se.fit = T,unconditional = TRUE),
                                                  T_coverage_label = paste0(newdata$T_coverage,"x"),
                                                  N_coverage_label = paste0(newdata$N_coverage,"x"),
                                                  VAF_label = paste0(newdata$VAF),
                                                  BND_label = paste0(newdata$BND_threshold),
                                                  SVTYPE = SVTYPE,
                                                  row.names = c(1:(nrow(newdata)))))
      df.Sensitivity <- rbind(df.Sensitivity, df.Sensitivity_caller)
      }
    }
    tmp1 <- df.Sensitivity
    obj_row <- c()
    if(input$X_axis2 %in% c("VAF","N_coverage","BND_threshold")){
      for(i in 1: length(SVTYPE_all)){
        for(j in 1: length(T_coverage_label)){
          tmp = tmp1[(tmp1$SVTYPE %in% SVTYPE_all[i]) &
                       (tmp1$T_coverage_label %in% T_coverage_label[j]) &
                       (tmp1$N_coverage_label %in% Ncov_var) &
                       (tmp1$VAF_label %in% VAF_var) &
                       (tmp1$BND_threshold %in% BND_var),]
          obj_row <- c(obj_row, rownames(tmp[tmp$fit>input$objective2.1,][1,]))
        }
      }
    }else if(input$X_axis2 == "T_coverage"){
      for(i in 1: length(SVTYPE_all)){
        for(k in 1: length(VAF_label)){
          tmp = tmp1[(tmp1$SVTYPE %in% SVTYPE_all[i]) &
                       (tmp1$T_coverage_label %in% Tcov_var) &
                       (tmp1$N_coverage_label %in% Ncov_var) &
                       (tmp1$VAF_label %in% VAF_label[k]) &
                       (tmp1$BND_threshold %in% BND_var),]
          obj_row <- c(obj_row, rownames(tmp[tmp$fit>input$objective2.1,][1,]))
        }
      }
    }
    obj <- rep(FALSE,nrow(df.Sensitivity))
    obj[as.numeric(obj_row[obj_row!="NA"])] <- TRUE
    df.Sensitivity <- cbind(df.Sensitivity,obj)
    
    obj.y <- rep(NA,nrow(df.Sensitivity))
    obj.y[df.Sensitivity$obj] <- df.Sensitivity$fit[df.Sensitivity$obj]
    df.Sensitivity <- cbind(df.Sensitivity, obj.y)
    
    df.Sensitivity$Caller <- factor(df.Sensitivity$Caller, levels = SV_caller)
    
    df = df.Sensitivity[(df.Sensitivity$SVTYPE %in% input_type) & 
                          (df.Sensitivity$T_coverage_label %in% Tcov_var) &
                          (df.Sensitivity$N_coverage_label %in% Ncov_var) &
                          (df.Sensitivity$VAF_label %in% VAF_var) &
                          (df.Sensitivity$BND_label %in% BND_var),]
    
    tmp <- cbind(df, SV_caller_label1[match(df$Caller,SV_caller)], SVTYPE_all_label[match(df$SVTYPE, SVTYPE_all)])
    table <- cbind(tmp[tmp$obj, c(ncol(tmp)-1,ncol(tmp), 2:6)])
    colnames(table) <- c("SV caller","SV type","Tumour coverage","Normal coverage","Tumour purity/VAF","Breakpoint precision threshold","Sensitivity")
    #return(list(xlabel,line_type,index,legend_label,x_min,x_max,x_by,df.Sensitivity,df,table))
    return(list(df.Sensitivity,df,table))
  })
  ########################################################################################################################
  dataInput2.2 <- reactive({
    input_type <- input$type2
    input_type[input_type %in% c("DINS","FINS")] <- "INS"
    input_type[input_type %in% c("TRA")] <- "BND"
    SVCaller_name <- SVCaller_name_input()[[1]]
    data <- newdata2()[[1]]
    T_coverage_label <- newdata2()[[2]]
    VAF_label <- newdata2()[[3]]
    Tcov_var <- newdata2()[[4]]
    Ncov_var <- newdata2()[[5]]
    VAF_var <- newdata2()[[6]]
    BND_var <- newdata2()[[7]]
    
    df.Precision <- c()
    for (SVTYPE in SVTYPE_all2){
      #if(SVTYPE == "DINS"|SVTYPE == "FINS"){SVTYPE_tmp <- "INS"}
      #if(SVTYPE == "TRA"){SVTYPE_tmp <- "BND"}
      #if(!((SVCaller_name == "Lumpy" & SVTYPE_tmp == "INS")|
      #     (SVCaller_name == "SvABA" & SVTYPE_tmp == "INS")|
      #     (SVCaller_name == "GRIDSS" & SVTYPE_tmp == "INS"))){
        
      if(!((SVCaller_name == "Lumpy" & SVTYPE == "INS")|
           (SVCaller_name == "SvABA" & SVTYPE == "INS")|
           (SVCaller_name == "GRIDSS" & SVTYPE == "INS") |
           (grepl("LumpySvABAUnion", SVCaller_name) & SVTYPE == "INS") | 
           (grepl("LumpyGRIDSSUnion", SVCaller_name) & SVTYPE == "INS") | 
           (grepl("SvABALumpyUnion", SVCaller_name) & SVTYPE == "INS") |
           (grepl("SvABAGRIDSSUnion", SVCaller_name) & SVTYPE == "INS") |
           (grepl("GRIDSSLumpyUnion", SVCaller_name) & SVTYPE == "INS") |
           (grepl("GRIDSSSvABAUnion", SVCaller_name) & SVTYPE == "INS") |
           (grepl("Lumpy", SVCaller_name) & grepl("Intersect", SVCaller_name) & SVTYPE == "INS") | 
           (grepl("SvABA", SVCaller_name) & grepl("Intersect", SVCaller_name) & SVTYPE == "INS") | 
           (grepl("GRIDSS", SVCaller_name) & grepl("Intersect", SVCaller_name) & SVTYPE == "INS"))){
      newdata <- data
      assign("df.Precision_caller",  data.frame(newdata,
                                                predict(eval(parse(text=paste0("gampre_off_", SVCaller_name, SVTYPE))),newdata,type = "response",se.fit = T,unconditional = TRUE),
                                                T_coverage_label = paste0(newdata$T_coverage,"x"),
                                                N_coverage_label = paste0(newdata$N_coverage,"x"),
                                                VAF_label = paste0(newdata$VAF),
                                                BND_label = paste0(newdata$BND_threshold),
                                                SVTYPE = SVTYPE,
                                                row.names = c(1:(nrow(newdata)))))
      df.Precision <- rbind(df.Precision, df.Precision_caller)
      }
    }
    tmp1 <- df.Precision
    obj_row <- c()
    if(input$X_axis2 %in% c("VAF","N_coverage","BND_threshold")){
      for(i in 1: length(SVTYPE_all2)){
        for(j in 1: length(T_coverage_label)){
          tmp = tmp1[(tmp1$SVTYPE %in% SVTYPE_all2[i]) &
                       (tmp1$T_coverage_label %in% T_coverage_label[j]) &
                       (tmp1$N_coverage_label %in% Ncov_var) &
                       (tmp1$VAF_label %in% VAF_var) &
                       (tmp1$BND_threshold %in% BND_var),]
          obj_row <- c(obj_row,rownames(tmp[tmp$fit>input$objective2.2,][1,]))
        }
      }
    }else if(input$X_axis2 == "T_coverage"){
      for(i in 1: length(SVTYPE_all2)){
        for(k in 1: length(VAF_label)){
          tmp = tmp1[(tmp1$SVTYPE %in% SVTYPE_all2[i]) &
                       (tmp1$T_coverage_label %in% Tcov_var) &
                       (tmp1$N_coverage_label %in% Ncov_var) &
                       (tmp1$VAF_label %in% VAF_label[k]) &
                       (tmp1$BND_threshold %in% BND_var),]
          obj_row <- c(obj_row,rownames(tmp[tmp$fit>input$objective2.2,][1,]))
        }
      }
    }
    obj <- rep(FALSE,nrow(df.Precision))
    obj[as.numeric(obj_row[obj_row!="NA"])] <- TRUE
    df.Precision <- cbind(df.Precision,obj)
    
    obj.y <- rep(NA,nrow(df.Precision))
    obj.y[df.Precision$obj] <- df.Precision$fit[df.Precision$obj]
    df.Precision <- cbind(df.Precision,obj.y)
    
    df.Precision$Caller <- factor(df.Precision$Caller, levels = SV_caller)
    
    df = df.Precision[(df.Precision$SVTYPE %in% input_type) & 
                        (df.Precision$T_coverage_label %in% Tcov_var) &
                        (df.Precision$N_coverage_label %in% Ncov_var) &
                        (df.Precision$VAF_label %in% VAF_var) &
                        (df.Precision$BND_label %in% BND_var),]
    
    tmp <- cbind(df,SV_caller_label1[match(df$Caller,SV_caller)], SVTYPE_all_label2[match(df$SVTYPE, SVTYPE_all2)])
    table <- cbind(tmp[tmp$obj, c(ncol(tmp)-1,ncol(tmp), 2:6)])
    #table <- unique(table)
    colnames(table) <- c("SV caller","SV type","Tumour coverage","Normal coverage","Tumour purity/VAF","Breakpoint precision threshold","Precision")
    #return(list(xlabel,line_type,index,legend_label,x_min,x_max,x_by,df.Precision,df,table))
    return(list(df.Precision,df,table))
  })
  
  ########################################################################################################################
  dataInput2.3 <- reactive({
    input_type <- input$type2
    input_type[input_type %in% c("DINS","FINS")] <- "INS"
    input_type[input_type %in% c("TRA")] <- "BND"
    SVCaller_name <- SVCaller_name_input()[[1]]
    data <- newdata2()[[1]]
    T_coverage_label <- newdata2()[[2]]
    VAF_label <- newdata2()[[3]]
    Tcov_var <- newdata2()[[4]]
    Ncov_var <- newdata2()[[5]]
    VAF_var <- newdata2()[[6]]
    BND_var <- newdata2()[[7]]

    df.F1_score <- c()
    for (SVTYPE in SVTYPE_all2){
      if(!((SVCaller_name == "Lumpy" & SVTYPE == "INS")|
           (SVCaller_name == "SvABA" & SVTYPE == "INS")|
           (SVCaller_name == "GRIDSS" & SVTYPE == "INS") |
           (grepl("LumpySvABAUnion", SVCaller_name) & SVTYPE == "INS") | 
           (grepl("LumpyGRIDSSUnion", SVCaller_name) & SVTYPE == "INS") | 
           (grepl("SvABALumpyUnion", SVCaller_name) & SVTYPE == "INS") |
           (grepl("SvABAGRIDSSUnion", SVCaller_name) & SVTYPE == "INS") |
           (grepl("GRIDSSLumpyUnion", SVCaller_name) & SVTYPE == "INS") |
           (grepl("GRIDSSSvABAUnion", SVCaller_name) & SVTYPE == "INS") |
           (grepl("Lumpy", SVCaller_name) & grepl("Intersect", SVCaller_name) & SVTYPE == "INS") | 
           (grepl("SvABA", SVCaller_name) & grepl("Intersect", SVCaller_name) & SVTYPE == "INS") | 
           (grepl("GRIDSS", SVCaller_name) & grepl("Intersect", SVCaller_name) & SVTYPE == "INS"))){
      newdata <- data
      assign("df.F1_score_caller",  data.frame(newdata,
                                               predict(eval(parse(text=paste0("gamF1_score_", SVCaller_name, SVTYPE))),newdata,type = "response",se.fit = T,unconditional = TRUE),
                                               T_coverage_label = paste0(newdata$T_coverage,"x"),
                                               N_coverage_label = paste0(newdata$N_coverage,"x"),
                                               VAF_label = paste0(newdata$VAF),
                                               BND_label = paste0(newdata$BND_threshold),
                                               SVTYPE = SVTYPE,
                                               row.names = c(1:(nrow(newdata)))))
      df.F1_score <- rbind(df.F1_score, df.F1_score_caller)
      }
    }
    tmp1 <- df.F1_score
    obj_row <- c()
    if(input$X_axis2 %in% c("VAF","N_coverage","BND_threshold")){
      for(i in 1: length(SVTYPE_all2)){
        for(j in 1: length(T_coverage_label)){
          tmp = tmp1[(tmp1$SVTYPE %in% SVTYPE_all2[i]) &
                       (tmp1$T_coverage_label %in% T_coverage_label[j]) &
                       (tmp1$N_coverage_label %in% Ncov_var) &
                       (tmp1$VAF_label %in% VAF_var) &
                       (tmp1$BND_threshold %in% BND_var),]
          obj_row <- c(obj_row,rownames(tmp[tmp$fit>input$objective2.3,][1,]))
        }
      }
    }else if(input$X_axis2 == "T_coverage"){
      for(i in 1: length(SVTYPE_all2)){
        for(k in 1: length(VAF_label)){
          tmp = tmp1[(tmp1$SVTYPE %in% SVTYPE_all2[i]) &
                       (tmp1$T_coverage_label %in% Tcov_var) &
                       (tmp1$N_coverage_label %in% Ncov_var) &
                       (tmp1$VAF_label %in% VAF_label[k]) &
                       (tmp1$BND_threshold %in% BND_var),]
          obj_row <- c(obj_row,rownames(tmp[tmp$fit>input$objective2.3,][1,]))
        }
      }
    }
    obj <- rep(FALSE,nrow(df.F1_score))
    obj[as.numeric(obj_row[obj_row!="NA"])] <- TRUE
    df.F1_score <- cbind(df.F1_score,obj)
    
    obj.y <- rep(NA,nrow(df.F1_score))
    obj.y[df.F1_score$obj] <- df.F1_score$fit[df.F1_score$obj]
    df.F1_score <- cbind(df.F1_score,obj.y)
    
    df.F1_score$Caller <- factor(df.F1_score$Caller, levels = SV_caller)
    
    df = df.F1_score[(df.F1_score$SVTYPE %in% input_type) & 
                       (df.F1_score$T_coverage_label %in% Tcov_var) &
                       (df.F1_score$N_coverage_label %in% Ncov_var) &
                       (df.F1_score$VAF_label %in% VAF_var) &
                       (df.F1_score$BND_label %in% BND_var),]
    
    tmp <- cbind(df,SV_caller_label1[match(df$Caller,SV_caller)], SVTYPE_all_label2[match(df$SVTYPE, SVTYPE_all2)])
    table <- cbind(tmp[tmp$obj, c(ncol(tmp)-1,ncol(tmp), 2:6)])
    colnames(table) <- c("SV caller","SV type","Tumour coverage","Normal coverage","Tumour purity/VAF","Breakpoint precision threshold","F1 score")
    #return(list(xlabel,line_type,index,legend_label,x_min,x_max,x_by,df.F1_score,df,table))
    return(list(df.F1_score,df,table))
  })
  
  input_svtype <- reactive({
    if(input$type3 == "DEL"){
      input_type_title <- "Deletion"
    }else if(input$type3 == "DUP"){
      input_type_title <- "Duplication"
    }else if(input$type3 == "INV"){
      input_type_title <- "Inversion"
    }else if(input$type3 == "DINS"){
      input_type_title <- "Insertion"
    }else if(input$type3 == "FINS"){
      input_type_title <- "Insertion"
    }else if(input$type3 == "TRA"){
      input_type_title <- "Translocation"
    }else if(input$type3 == ""){
      input_type_title <- "Overall"
    }
    return(input_type_title)
  })
  
  
  ########################################################################################################################
  dataInput3.1 <- reactive({
    input_type <- input$type3

    data <- newdata3()[[1]]
    T_coverage_label <- newdata3()[[2]]
    VAF_label <- newdata3()[[3]]
    Tcov_var <- newdata3()[[4]]
    Ncov_var <- newdata3()[[5]]
    VAF_var <- newdata3()[[6]]
    BND_var <- newdata3()[[7]]
    
    df.Sensitivity <- c()
    for (SVCaller_name in SV_caller){
      
      if(!((SVCaller_name == "Lumpy" & input_type == "FINS")|
           (SVCaller_name == "SvABA" & input_type == "FINS")|
           (grepl("Lumpy",SVCaller_name) & grepl("Intersect", SVCaller_name) & input_type == "FINS") | 
           (grepl("SvABA",SVCaller_name) & grepl("Intersect", SVCaller_name) & input_type == "FINS") |
           (grepl("LumpySvABAUnion", SVCaller_name) & input_type == "FINS") | 
           (grepl("SvABALumpyUnion", SVCaller_name) & input_type == "FINS"))){
      newdata <- data[data$Caller == SVCaller_name,]
      assign("df.Sensitivity_caller",  data.frame(newdata,
                                                  predict(eval(parse(text=paste0("gamsen_", SVCaller_name,input_type))),newdata,type = "response",se.fit = T,unconditional = TRUE),
                                                  T_coverage_label = paste0(newdata$T_coverage,"x"),
                                                  N_coverage_label = paste0(newdata$N_coverage,"x"),
                                                  VAF_label = paste0(newdata$VAF),
                                                  BND_label = paste0(newdata$BND_threshold),
                                                  row.names = c(1:(nrow(newdata)))))
      df.Sensitivity <- rbind(df.Sensitivity, df.Sensitivity_caller)
      }
    }
    tmp1 <- df.Sensitivity
    obj_row <- c()
    if(input$X_axis3 %in% c("VAF","N_coverage","BND_threshold")){
      for(i in 1: length(SV_caller)){
        for(j in 1: length(T_coverage_label)){
          tmp = tmp1[(tmp1$Caller %in% SV_caller[i]) &
                       (tmp1$T_coverage_label %in% T_coverage_label[j]) &
                       (tmp1$N_coverage_label %in% Ncov_var) &
                       (tmp1$VAF_label %in% VAF_var) &
                       (tmp1$BND_threshold %in% BND_var),]
          obj_row <- c(obj_row,rownames(tmp[tmp$fit>input$objective3.1,][1,]))
        }
      }
    }else if(input$X_axis3 == "T_coverage"){
      for(i in 1: length(SV_caller)){
        for(k in 1: length(VAF_label)){
          tmp = tmp1[(tmp1$Caller %in% SV_caller[i]) &
                       (tmp1$T_coverage_label %in% Tcov_var) &
                       (tmp1$N_coverage_label %in% Ncov_var) &
                       (tmp1$VAF_label %in% VAF_label[k]) &
                       (tmp1$BND_threshold %in% BND_var),]
          obj_row <- c(obj_row,rownames(tmp[tmp$fit>input$objective3.1,][1,]))
        }
      }
    }
    obj <- rep(FALSE,nrow(df.Sensitivity))
    obj[as.numeric(obj_row[obj_row!="NA"])] <- TRUE
    df.Sensitivity <- cbind(df.Sensitivity,obj)
    
    obj.y <- rep(NA,nrow(df.Sensitivity))
    obj.y[df.Sensitivity$obj] <- df.Sensitivity$fit[df.Sensitivity$obj]
    df.Sensitivity <- cbind(df.Sensitivity,obj.y)
    
    df.Sensitivity$Caller <- factor(df.Sensitivity$Caller, levels = SV_caller)
    
    input_union <- c()
    if(length(input$SVCaller3.1)==1){
      input_union <- input$SVCaller3.1
    }else{
      for(i in 1: length(input$SVCaller3.1)){
        for(j in 1: length(input$SVCaller3.1)){
          if(j!=i){
            input_union <- c(input_union,paste0(input$SVCaller3.1[i],input$SVCaller3.1[j],"Union"))
          }
        }
      }
    }
    
    input_intersect <- c()
    if(length(input$SVCaller3.2)==1){
      input_intersect <- input$SVCaller3.2
    }else{
      for(i in 1: length(input$SVCaller3.2)){
        for(j in 1: length(input$SVCaller3.2)){
          if(j!=i){
            input_intersect <- c(input_intersect,paste0(input$SVCaller3.2[i],input$SVCaller3.2[j],"Intersect"))
          }
        }
      }
    }
    
    df = df.Sensitivity[(df.Sensitivity$Caller %in% c(input$SVCaller3,input_union,input_intersect)) & 
                          (df.Sensitivity$T_coverage_label %in% Tcov_var) &
                          (df.Sensitivity$N_coverage_label %in% Ncov_var) &
                          (df.Sensitivity$VAF_label %in% VAF_var) &
                          (df.Sensitivity$BND_label %in% BND_var),]
    
    tmp <- cbind(df,SV_caller_label1[match(df$Caller,SV_caller)])
    table <- cbind(tmp[tmp$obj,c(ncol(tmp),2:6)])
    colnames(table) <- c("SV caller","Tumour coverage","Normal coverage","Tumour purity/VAF","Breakpoint precision threshold","Sensitivity")
    #return(list(xlabel,line_type,index,legend_label,x_min,x_max,x_by,df.Sensitivity,df,table,input_type_title))
    return(list(df.Sensitivity,df,table))
  })
  ########################################################################################################################
  dataInput3.2 <- reactive({
    input_type <- input$type3
    data <- newdata3()[[1]]
    T_coverage_label <- newdata3()[[2]]
    VAF_label <- newdata3()[[3]]
    Tcov_var <- newdata3()[[4]]
    Ncov_var <- newdata3()[[5]]
    VAF_var <- newdata3()[[6]]
    BND_var <- newdata3()[[7]]
   
    
    df.Precision <- c()
    for (SVCaller_name in SV_caller){
      
      if(input_type == "DINS"|input_type == "FINS"){input_type <- "INS"}
      if(input_type == "TRA"){input_type <- "BND"}
      
      if(!((SVCaller_name == "Lumpy" & input_type == "INS")|
           (SVCaller_name == "SvABA" & input_type == "INS")|
           (SVCaller_name == "GRIDSS" & input_type=="INS") |
           (grepl("LumpySvABAUnion", SVCaller_name) & input_type == "INS") | 
           (grepl("LumpyGRIDSSUnion", SVCaller_name) & input_type == "INS") | 
           (grepl("SvABALumpyUnion", SVCaller_name) & input_type == "INS") |
           (grepl("SvABAGRIDSSUnion", SVCaller_name) & input_type == "INS") |
           (grepl("GRIDSSLumpyUnion", SVCaller_name) & input_type == "INS") |
           (grepl("GRIDSSSvABAUnion", SVCaller_name) & input_type == "INS") |
           (grepl("Lumpy", SVCaller_name) & grepl("Intersect", SVCaller_name) & input_type == "INS") | 
           (grepl("SvABA", SVCaller_name) & grepl("Intersect", SVCaller_name) & input_type == "INS") | 
           (grepl("GRIDSS", SVCaller_name) & grepl("Intersect", SVCaller_name) & input_type == "INS"))){
      newdata <- data[data$Caller == SVCaller_name,]
      assign("df.Precision_caller",  data.frame(newdata,
                                                  predict(eval(parse(text=paste0("gampre_off_", SVCaller_name, input_type))),newdata,type = "response",se.fit = T,unconditional = TRUE),
                                                  T_coverage_label = paste0(newdata$T_coverage,"x"),
                                                  N_coverage_label = paste0(newdata$N_coverage,"x"),
                                                  VAF_label = paste0(newdata$VAF),
                                                  BND_label = paste0(newdata$BND_threshold),
                                                  row.names = c(1:(nrow(newdata)))))
      df.Precision <- rbind(df.Precision, df.Precision_caller)
      }
    }
    tmp1 <- df.Precision
    obj_row <- c()
    if(input$X_axis3 %in% c("VAF","N_coverage","BND_threshold")){
      for(i in 1: length(SV_caller)){
        for(j in 1: length(T_coverage_label)){
          tmp = tmp1[(tmp1$Caller %in% SV_caller[i]) &
                       (tmp1$T_coverage_label %in% T_coverage_label[j]) &
                       (tmp1$N_coverage_label %in% Ncov_var) &
                       (tmp1$VAF_label %in% VAF_var) &
                       (tmp1$BND_threshold %in% BND_var),]
          obj_row <- c(obj_row,rownames(tmp[tmp$fit>input$objective3.2,][1,]))
        }
      }
    }else if(input$X_axis3 == "T_coverage"){
      for(i in 1: length(SV_caller)){
        for(k in 1: length(VAF_label)){
          tmp = tmp1[(tmp1$Caller %in% SV_caller[i]) &
                       (tmp1$T_coverage_label %in% Tcov_var) &
                       (tmp1$N_coverage_label %in% Ncov_var) &
                       (tmp1$VAF_label %in% VAF_label[k]) &
                       (tmp1$BND_threshold %in% BND_var),]
          obj_row <- c(obj_row,rownames(tmp[tmp$fit>input$objective3.2,][1,]))
        }
      }
    }
    obj <- rep(FALSE,nrow(df.Precision))
    obj[as.numeric(obj_row[obj_row!="NA"])] <- TRUE
    df.Precision <- cbind(df.Precision,obj)
    
    obj.y <- rep(NA,nrow(df.Precision))
    obj.y[df.Precision$obj] <- df.Precision$fit[df.Precision$obj]
    df.Precision <- cbind(df.Precision,obj.y)
    
    df.Precision$Caller <- factor(df.Precision$Caller, levels = SV_caller)
    
    input_union <- c()
    if(length(input$SVCaller3.1)==1){
      input_union <- input$SVCaller3.1
    }else{
      for(i in 1: length(input$SVCaller3.1)){
        for(j in 1: length(input$SVCaller3.1)){
          if(j!=i){
            input_union <- c(input_union,paste0(input$SVCaller3.1[i],input$SVCaller3.1[j],"Union"))
          }
        }
      }
    }
    
    input_intersect <- c()
    if(length(input$SVCaller3.2)==1){
      input_intersect <- input$SVCaller3.2
    }else{
      for(i in 1: length(input$SVCaller3.2)){
        for(j in 1: length(input$SVCaller3.2)){
          if(j!=i){
            input_intersect <- c(input_intersect,paste0(input$SVCaller3.2[i],input$SVCaller3.2[j],"Intersect"))
          }
        }
      }
    }
    
    df = df.Precision[(df.Precision$Caller %in% c(input$SVCaller3,input_union,input_intersect)) & 
                          (df.Precision$T_coverage_label %in% Tcov_var) &
                          (df.Precision$N_coverage_label %in% Ncov_var) &
                          (df.Precision$VAF_label %in% VAF_var) &
                          (df.Precision$BND_label %in% BND_var),]
    
    tmp <- cbind(df,SV_caller_label1[match(df$Caller,SV_caller)])
    table <- cbind(tmp[tmp$obj,c(ncol(tmp),2:6)])
    colnames(table) <- c("SV caller","Tumour coverage","Normal coverage","Tumour purity/VAF","Breakpoint precision threshold","Precision")
    #return(list(xlabel,line_type,index,legend_label,x_min,x_max,x_by,df.Precision,df,table))
    return(list(df.Precision,df,table))
  })
  ########################################################################################################################
  dataInput3.3 <- reactive({
    input_type <- input$type3
    data <- newdata3()[[1]]
    T_coverage_label <- newdata3()[[2]]
    VAF_label <- newdata3()[[3]]
    Tcov_var <- newdata3()[[4]]
    Ncov_var <- newdata3()[[5]]
    VAF_var <- newdata3()[[6]]
    BND_var <- newdata3()[[7]]
   
    
    df.F1_score <- c()
    for (SVCaller_name in SV_caller){
      if(input_type == "DINS"|input_type == "FINS"){input_type <- "INS"}
      if(input_type == "TRA"){input_type <- "BND"}
      
      if(!((SVCaller_name == "Lumpy" & input_type == "INS")|
           (SVCaller_name == "SvABA" & input_type == "INS")|
           (SVCaller_name == "GRIDSS" & input_type=="INS") |
           (grepl("LumpySvABAUnion", SVCaller_name) & input_type == "INS") | 
           (grepl("LumpyGRIDSSUnion", SVCaller_name) & input_type == "INS") | 
           (grepl("SvABALumpyUnion", SVCaller_name) & input_type == "INS") |
           (grepl("SvABAGRIDSSUnion", SVCaller_name) & input_type == "INS") |
           (grepl("GRIDSSLumpyUnion", SVCaller_name) & input_type == "INS") |
           (grepl("GRIDSSSvABAUnion", SVCaller_name) & input_type == "INS") |
           (grepl("Lumpy", SVCaller_name) & grepl("Intersect", SVCaller_name) & input_type == "INS") | 
           (grepl("SvABA", SVCaller_name) & grepl("Intersect", SVCaller_name) & input_type == "INS") | 
           (grepl("GRIDSS", SVCaller_name) & grepl("Intersect", SVCaller_name) & input_type == "INS"))){
      newdata <- data[data$Caller == SVCaller_name,]
      assign("df.F1_score_caller",  data.frame(newdata,
                                                  predict(eval(parse(text=paste0("gamF1_score_", SVCaller_name, input_type))),newdata,type = "response",se.fit = T,unconditional = TRUE),
                                                  T_coverage_label = paste0(newdata$T_coverage,"x"),
                                                  N_coverage_label = paste0(newdata$N_coverage,"x"),
                                                  VAF_label = paste0(newdata$VAF),
                                                  BND_label = paste0(newdata$BND_threshold),
                                                  row.names = c(1:(nrow(newdata)))))
      df.F1_score <- rbind(df.F1_score, df.F1_score_caller)
      }
    }
    tmp1 <- df.F1_score
    obj_row <- c()
    if(input$X_axis3 %in% c("VAF","N_coverage","BND_threshold")){
      for(i in 1: length(SV_caller)){
        for(j in 1: length(T_coverage_label)){
          tmp = tmp1[(tmp1$Caller %in% SV_caller[i]) &
                       (tmp1$T_coverage_label %in% T_coverage_label[j]) &
                       (tmp1$N_coverage_label %in% Ncov_var) &
                       (tmp1$VAF_label %in% VAF_var) &
                       (tmp1$BND_threshold %in% BND_var),]
          obj_row <- c(obj_row,rownames(tmp[tmp$fit>input$objective3.3,][1,]))
        }
      }
    }else if(input$X_axis3 == "T_coverage"){
      for(i in 1: length(SV_caller)){
        for(k in 1: length(VAF_label)){
          tmp = tmp1[(tmp1$Caller %in% SV_caller[i]) &
                       (tmp1$T_coverage_label %in% Tcov_var) &
                       (tmp1$N_coverage_label %in% Ncov_var) &
                       (tmp1$VAF_label %in% VAF_label[k]) &
                       (tmp1$BND_threshold %in% BND_var),]
          obj_row <- c(obj_row,rownames(tmp[tmp$fit>input$objective3.3,][1,]))
        }
      }
    }
    obj <- rep(FALSE,nrow(df.F1_score))
    obj[as.numeric(obj_row[obj_row!="NA"])] <- TRUE
    df.F1_score <- cbind(df.F1_score,obj)
    
    obj.y <- rep(NA,nrow(df.F1_score))
    obj.y[df.F1_score$obj] <- df.F1_score$fit[df.F1_score$obj]
    df.F1_score <- cbind(df.F1_score,obj.y)
    
    df.F1_score$Caller <- factor(df.F1_score$Caller, levels = SV_caller)
    
    input_union <- c()
    if(length(input$SVCaller3.1)==1){
      input_union <- input$SVCaller3.1
    }else{
      for(i in 1: length(input$SVCaller3.1)){
        for(j in 1: length(input$SVCaller3.1)){
          if(j!=i){
            input_union <- c(input_union,paste0(input$SVCaller3.1[i],input$SVCaller3.1[j],"Union"))
          }
        }
      }
    }
    
    input_intersect <- c()
    if(length(input$SVCaller3.2)==1){
      input_intersect <- input$SVCaller3.2
    }else{
      for(i in 1: length(input$SVCaller3.2)){
        for(j in 1: length(input$SVCaller3.2)){
          if(j!=i){
            input_intersect <- c(input_intersect,paste0(input$SVCaller3.2[i],input$SVCaller3.2[j],"Intersect"))
          }
        }
      }
    }
    
    df = df.F1_score[(df.F1_score$Caller %in% c(input$SVCaller3,input_union,input_intersect)) & 
                          (df.F1_score$T_coverage_label %in% Tcov_var) &
                          (df.F1_score$N_coverage_label %in% Ncov_var) &
                          (df.F1_score$VAF_label %in% VAF_var) &
                          (df.F1_score$BND_label %in% BND_var),]
    
    tmp <- cbind(df,SV_caller_label1[match(df$Caller,SV_caller)])
    table <- cbind(tmp[tmp$obj,c(ncol(tmp),2:6)])
    colnames(table) <- c("SV caller","Tumour coverage","Normal coverage","Tumour purity/VAF","Breakpoint precision threshold","F1 score")
    #return(list(xlabel,line_type,index,legend_label,x_min,x_max,x_by,df.F1_score,df,table))
    return(list(df.F1_score,df,table))
  })
  
  #output$txtOutput3 <- renderText({
    #paste(head(dataInput3.2()[[8]]))
    #paste(dataInput3.2()[[8]]$T_coverage_label)
  #  paste(nrow(dataInput3.1()[[8]]), nrow(dataInput3.1()[[9]]))
  #})
  ########################################################################################################################
 
  Sensitivity_plot2 <- reactive({
    if(!MeasureInput2()[1]) return(NULL)

    xlabel <- newdata2()[[8]]
    line_type <- newdata2()[[9]]
    index <- newdata2()[[10]]
    legend_label <- newdata2()[[11]]
    x_min <- newdata2()[[12]]
    x_max <- newdata2()[[13]]
    x_by <- newdata2()[[14]]
    
    df.Sensitivity <- dataInput2.1()[[1]]
    df <- dataInput2.1()[[2]]
    
    
    SVCaller_name_label <- SVCaller_name_input()[[2]]
    ggplot(data = df, aes(x = eval(parse(text = input$X_axis2)), y = fit, group = interaction(SVTYPE,eval(parse(text = colnames(df)[index]))))) +
      geom_ribbon(aes(ymin = fit-1.96*se.fit, ymax = fit+1.96*se.fit), fill = "grey70")+
      geom_line(data = df, aes(x = eval(parse(text = input$X_axis2)), y = fit,
                               group = interaction(SVTYPE,eval(parse(text = colnames(df)[index]))), 
                               color = SVTYPE, linetype = eval(parse(text = colnames(df)[index]))), size = 1.5)+
      geom_hline(yintercept = input$objective2.1)+
      geom_point(data = df,aes(x = eval(parse(text = input$X_axis2)), y = obj.y))+ 
      # geom_text(aes(x=eval(parse(text = input$X_axis3)), y = cutoff.y+0.05, label=round(cutoff.y,2)))+
      ggtitle(paste(SVCaller_name_label,"sensitivity", "across", xlabel))+
      scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
      scale_x_continuous(breaks = seq(x_min, x_max, by = x_by), limits = c(x_min,x_max))+
      scale_color_manual(values = color_SVTYPE[names(color_SVTYPE) %in% df$SVTYPE], name = "SV TYPE",labels = SVTYPE_all_label[SVTYPE_all %in% df$SVTYPE])+
      scale_linetype_manual(values = line_type[names(line_type) %in% unique(df[,index])])+
      labs(y = "Sensitivity", x = xlabel, linetype = legend_label)+
      theme
  })
  
  Precision_plot2 <- reactive({
    if(!MeasureInput2()[2]) return(NULL)
    xlabel <- newdata2()[[8]]
    line_type <- newdata2()[[9]]
    index <- newdata2()[[10]]
    legend_label <- newdata2()[[11]]
    x_min <- newdata2()[[12]]
    x_max <- newdata2()[[13]]
    x_by <- newdata2()[[14]]
    
    df.Precision <- dataInput2.2()[[1]]
    df <- dataInput2.2()[[2]]
    
    
    SVCaller_name_label <- SVCaller_name_input()[[2]]
    ggplot(data = df, aes(x = eval(parse(text = input$X_axis2)), y = fit, group = interaction(SVTYPE,eval(parse(text = colnames(df)[index]))))) +
      geom_ribbon(aes(ymin = fit-1.96*se.fit, ymax = fit+1.96*se.fit), fill = "grey70")+
      geom_line(data = df, aes(x = eval(parse(text = input$X_axis2)), y = fit,
                               group = interaction(SVTYPE,eval(parse(text = colnames(df)[index]))), 
                               color = SVTYPE, linetype = eval(parse(text = colnames(df)[index]))), size = 1.5)+
      geom_hline(yintercept=input$objective2.2)+
      geom_point(data=df,aes(x=eval(parse(text = input$X_axis2)), y = obj.y))+ 
      ggtitle(paste(SVCaller_name_label,"precision", "across", xlabel))+
      scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
      scale_x_continuous(breaks = seq(x_min, x_max, by = x_by), limits = c(x_min,x_max))+
      scale_color_manual(values = color_SVTYPE2[names(color_SVTYPE2) %in% df$SVTYPE], name = "SV TYPE",labels = SVTYPE_all_label2[SVTYPE_all2 %in% df$SVTYPE])+
      scale_linetype_manual(values = line_type[names(line_type) %in% unique(df[,index])])+
      labs(y = "Precision", x = xlabel, linetype = legend_label)+
      theme
  })
  
  F1_score_plot2 <- reactive({
    if(!MeasureInput2()[3]) return(NULL)
    xlabel <- newdata2()[[8]]
    line_type <- newdata2()[[9]]
    index <- newdata2()[[10]]
    legend_label <- newdata2()[[11]]
    x_min <- newdata2()[[12]]
    x_max <- newdata2()[[13]]
    x_by <- newdata2()[[14]]
    
    df.F1_score <- dataInput2.3()[[1]]
    df <- dataInput2.3()[[2]]
    
    SVCaller_name_label <- SVCaller_name_input()[[2]]
    
    ggplot(data = df, aes(x = eval(parse(text = input$X_axis2)), y = fit, group = interaction(SVTYPE,eval(parse(text = colnames(df)[index]))))) +
      geom_ribbon(aes(ymin = fit-1.96*se.fit, ymax = fit+1.96*se.fit), fill = "grey70")+
      geom_line(data = df, aes(x = eval(parse(text = input$X_axis2)), y = fit,
                               group = interaction(SVTYPE,eval(parse(text = colnames(df)[index]))), 
                               color = SVTYPE, linetype = eval(parse(text = colnames(df)[index]))), size = 1.5)+
      geom_hline(yintercept=input$objective2.3)+
      geom_point(data=df,aes(x=eval(parse(text = input$X_axis2)), y = obj.y))+ 
      # geom_text(aes(x=eval(parse(text = input$X_axis3)), y = cutoff.y+0.05, label=round(cutoff.y,2)))+
      ggtitle(paste(SVCaller_name_label,"F1 score", "across", xlabel))+
      scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
      scale_x_continuous(breaks = seq(x_min, x_max, by = x_by), limits = c(x_min,x_max))+
      scale_color_manual(values = color_SVTYPE2[names(color_SVTYPE2) %in% df$SVTYPE], name = "SV TYPE",labels = SVTYPE_all_label2[SVTYPE_all2 %in% df$SVTYPE])+
      scale_linetype_manual(values = line_type[names(line_type) %in% unique(df[,index])])+
      labs(y = "F1 score", x = xlabel, linetype = legend_label)+
      theme
  })
  
  ########################################################################################################################
  Sensitivity_plot3 <- reactive({
    if(!MeasureInput3()[1]) return(NULL)
    
    xlabel <- newdata3()[[8]]
    line_type <- newdata3()[[9]]
    index <- newdata3()[[10]]
    legend_label <- newdata3()[[11]]
    x_min <- newdata3()[[12]]
    x_max <- newdata3()[[13]]
    x_by <- newdata3()[[14]]
    
    df.Sensitivity <- dataInput3.1()[[1]]
    df <- dataInput3.1()[[2]]
    input_type_title <- input_svtype()
    
    ggplot(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit, group = interaction(Caller,eval(parse(text = colnames(df)[index]))))) +
      geom_ribbon(aes(ymin = fit-1.96*se.fit, ymax = fit+1.96*se.fit), fill = "grey70")+
      geom_line(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit,
                               group = interaction(Caller,eval(parse(text = colnames(df)[index]))), 
                               color = Caller, linetype = eval(parse(text = colnames(df)[index]))), size = 1.5)+
      geom_hline(yintercept=input$objective3.1)+
      geom_point(data=df,aes(x=eval(parse(text = input$X_axis3)), y = obj.y))+ 
      # geom_text(aes(x=eval(parse(text = input$X_axis3)), y = cutoff.y+0.05, label=round(cutoff.y,2)))+
      ggtitle(paste(input_type_title, "sensitivity", "across", xlabel))+
      scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
      scale_x_continuous(breaks = seq(x_min, x_max, by = x_by), limits = c(x_min,x_max))+
      scale_color_manual(values = color_SVCaller[names(color_SVCaller) %in% df$Caller], name = "SV Caller",labels = SV_caller_label[SV_caller %in% df$Caller])+
      scale_linetype_manual(values = line_type[names(line_type) %in% unique(df[,index])])+
      labs(y = "Sensitivity", x = xlabel, linetype = legend_label)+
      theme
  })
  
  Precision_plot3 <- reactive({
    if(!MeasureInput3()[2]) return(NULL)
    xlabel <- newdata3()[[8]]
    line_type <- newdata3()[[9]]
    index <- newdata3()[[10]]
    legend_label <- newdata3()[[11]]
    x_min <- newdata3()[[12]]
    x_max <- newdata3()[[13]]
    x_by <- newdata3()[[14]]
    
    df.Precision <- dataInput3.2()[[1]]
    df <- dataInput3.2()[[2]]
    input_type_title <- input_svtype()
    ggplot(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit, group = interaction(Caller,eval(parse(text = colnames(df)[index]))))) +
      geom_ribbon(aes(ymin = fit-1.96*se.fit, ymax = fit+1.96*se.fit), fill = "grey70")+
      geom_line(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit,
                               group = interaction(Caller,eval(parse(text = colnames(df)[index]))), 
                               color = Caller, linetype = eval(parse(text = colnames(df)[index]))), size = 1.5)+
      geom_hline(yintercept=input$objective3.2)+
      geom_point(data=df,aes(x=eval(parse(text = input$X_axis3)), y = obj.y))+ 
      ggtitle(paste(input_type_title, "precision", "across", xlabel))+
      scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
      scale_x_continuous(breaks = seq(x_min, x_max, by = x_by), limits = c(x_min,x_max))+
      scale_color_manual(values = color_SVCaller[names(color_SVCaller) %in% df$Caller], name = "SV Caller",labels = SV_caller_label[SV_caller %in% df$Caller])+
      scale_linetype_manual(values = line_type[names(line_type) %in% unique(df[,index])])+
      labs(y = "Precision", x = xlabel, linetype = legend_label)+
      theme
  })
  
  F1_score_plot3 <- reactive({
    if(!MeasureInput3()[3]) return(NULL)
    xlabel <- newdata3()[[8]]
    line_type <- newdata3()[[9]]
    index <- newdata3()[[10]]
    legend_label <- newdata3()[[11]]
    x_min <- newdata3()[[12]]
    x_max <- newdata3()[[13]]
    x_by <- newdata3()[[14]]
    
    df.F1_score <- dataInput3.3()[[1]]
    df <- dataInput3.3()[[2]]
    input_type_title <- input_svtype()
    
    ggplot(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit, group = interaction(Caller,eval(parse(text = colnames(df)[index]))))) +
      geom_ribbon(aes(ymin = fit-1.96*se.fit, ymax = fit+1.96*se.fit), fill = "grey70")+
      geom_line(data = df, aes(x = eval(parse(text = input$X_axis3)), y = fit,
                               group = interaction(Caller,eval(parse(text = colnames(df)[index]))), 
                               color = Caller, linetype = eval(parse(text = colnames(df)[index]))), size = 1.5)+
      geom_hline(yintercept=input$objective3.3)+
      geom_point(data=df,aes(x=eval(parse(text = input$X_axis3)), y = obj.y))+ 
      # geom_text(aes(x=eval(parse(text = input$X_axis3)), y = cutoff.y+0.05, label=round(cutoff.y,2)))+
      ggtitle(paste(input_type_title,"F1 score", "across", xlabel))+
      scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
      scale_x_continuous(breaks = seq(x_min, x_max, by = x_by), limits = c(x_min,x_max))+
      scale_color_manual(values = color_SVCaller[names(color_SVCaller) %in% df$Caller], name = "SV Caller",labels = SV_caller_label[SV_caller %in% df$Caller])+
      scale_linetype_manual(values = line_type[names(line_type) %in% unique(df[,index])])+
      labs(y = "F1 score", x = xlabel, linetype = legend_label)+
      theme
  })
  
  ########################################################################################################################
  
  output$Plot2.1 <- renderPlot({
    ptlist <- list(Sensitivity_plot2(), Precision_plot2())
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    grid.arrange(grobs=ptlist,ncol=length(ptlist))
  })
  output$Plot2.2 <- renderPlot({
    ptlist <- list(F1_score_plot2())
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    grid.arrange(grobs=ptlist,ncol=length(ptlist))
  })
  ########################################################################################################################
  output$Plot3.1 <- renderPlot({
    ptlist <- list(Sensitivity_plot3(), Precision_plot3())
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    grid.arrange(grobs=ptlist,ncol=length(ptlist))
  })

  output$Plot3.2 <- renderPlot({
    ptlist <- list(F1_score_plot3())
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    grid.arrange(grobs=ptlist,ncol=length(ptlist))
  })
  # output$Plot3 <- renderUI({
  #   if(sum(MeasureInput3())>1 & (MeasureInput3()[3]))
  #     fluidPage(plotOutput("Plot3.1"), plotOutput("Plot3.2"))
  #   else if(sum(MeasureInput3())==1 & (MeasureInput3()[3]))
  #     plotOutput("Plot3.2")
  #   else
  #     plotOutput("Plot3.1")
  #   
  # })
  ########################################################################################################################
  #output$table3.1 <- renderTable(if(!MeasureInput3()[1]){NULL}else{dataInput3.1()[[10]]}, sanitize.text.function=identity)
  output$table3.1 <- renderTable(if(!MeasureInput3()[1]){NULL}else{dataInput3.1()[[3]]}, sanitize.text.function=identity)
  output$table3.2 <- renderTable(if(!MeasureInput3()[2]){NULL}else{dataInput3.2()[[3]]}, sanitize.text.function=identity)
  output$table3.3 <- renderTable(if(!MeasureInput3()[3]){NULL}else{dataInput3.3()[[3]]}, sanitize.text.function=identity)
  
  #output$table2.1 <- renderTable(if(!MeasureInput2()[1]){NULL}else{dataInput2.1()[[10]]}, sanitize.text.function=identity)
  #output$table2.2 <- renderTable(if(!MeasureInput2()[2]){NULL}else{dataInput2.2()[[10]]}, sanitize.text.function=identity)
  #output$table2.3 <- renderTable(if(!MeasureInput2()[3]){NULL}else{dataInput2.3()[[10]]}, sanitize.text.function=identity)
  output$table2.1 <- renderTable(if(!MeasureInput2()[1]){NULL}else{dataInput2.1()[[3]]}, sanitize.text.function=identity)
  output$table2.2 <- renderTable(if(!MeasureInput2()[2]){NULL}else{dataInput2.2()[[3]]}, sanitize.text.function=identity)
  output$table2.3 <- renderTable(if(!MeasureInput2()[3]){NULL}else{dataInput2.3()[[3]]}, sanitize.text.function=identity)
  ########################################################################################################################

})

shinyApp(ui, server)
