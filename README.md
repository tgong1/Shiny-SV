# Shiny-SoSV

Shiny-SoSV provides an interactive and visual platform for users to easily explore the impact of different parameters on the power to detect somatic structural variants derived from short-read shole genome sequencing. Therefore, it enables users to rapidly address essential questions related to their study design. 

If you use the Shiny-SoSV, please cite:</br>
Tingting Gong, Vanessa M Hayes, Eva KF Chan. Shiny-SoSV: A web app for interactive evaluation of somatic structural variant calls. BioRxiv 668723; doi: https://doi.org/10.1101/668723

# Installation
The app is hosted on Shinyapps.so: https://hcpcg.shinyapps.io/Shiny-SoSV/.

To run this app locally on your machine, download R or RStudio and run the following commands once to set up the environment:
```
install.packages(c("shiny", "shinyjs", "ggplot2", "gridExtra", "ggsci"))
```
You may now run the shiny app with command in R:
```
library(shiny)
runGitHub("Shiny-SoSV", "tgong1")
```

# Usage
See [Shiny-SoSV](https://hcpcg.shinyapps.io/Shiny-SoSV/) User Guide and Example Use Cases pages for further details regarding the usage of Shiny-SoSV. 
