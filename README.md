
# shinyEndomineR

Endoscopy performance needs to be measurable. EndoMineR-Shiny is the
interface for the [package
EndoMineR](https://github.com/ropensci/EndoMineR). The package
aims to extract information from endoscopic and associated pathology
reports. The extracted information is used to automatically derive
performance metrics for endoscopists, follow-up timings for patients and
a number of other graphical outcomes. The interface hopes to make the
whole process of automated metrics user friendly and graphical.

This application is in the very early stages of development.

Install and run as follows:

``` r
# devtools::install_github("CDU-data-science-team/shinyEndomineR")

library(shinyEndomineR)

run_app()
```

[Link to hosted
version](https://feedbackmatters.uk/rsconnect/shiny_endominer/)
