# Topic
Measure of biodiversity

## Lecturer
Jon, Alban

## Schedule
- Morning (from 10:00): Theoretical lesson
- Afternoon: Practical lesson using MOBsim

## Content
 - MOBsim Shiny app
    + Different SAD simulation processes
    + Spatial distribution of individuals
      - Clustering type
      - Aggregation coefficient
    + Sampling design
    + Measure of Diversity
      - Accumulation / Rarefaction curves
      - Alpha -> gamma diversity metrics
  
  - MOBsim R programm
  - Overlook
  - Exercises
  
## Material
You can open the app online here: https://sagouis.shinyapps.io/mobsim_app/

You can download the app folder from my GitHub here: https://github.com/AlbanSagouis/mobsim_app unzip it and run `shiny::runApp("path")` in R (there should be an error if you run it in RStudio). You need to install Shiny `install.packages("shiny")`

You can install the MOBsim package directly from R with the devtools packages installed.
`devtools::install_github("AlbanSagouis/mobsim", dependencies=TRUE)` # for the newest version
`devtools::install_github("MoBiodiv/mobsim", dependencies=FALSE)`	# install github version of the package
