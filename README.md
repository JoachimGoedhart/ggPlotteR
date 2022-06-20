# ggPlotteR
A Shiny App that demonstrates how plots are encoded with R/ggplot2


### Running the App

The web-tool runs from a shiny server, and can be accessed at: [https://huygens.science.uva.nl/ggPlotteR/](https://huygens.science.uva.nl/ggPlotteR/)

Alternatively, the app can run from R/Rstudio.

#### Preparations
The app depends on the R packages 'shiny' and 'ggplot2'. 
Run this command in R/Rstudio to download and install the packages (only needs to be done once):
```
install.packages("shiny", "ggplot2")
```
o The first option is running it directly from Github. In the command line (in R or Rstudio) type:
```
shiny::runGitHub('ggPlotteR', 'JoachimGoedhart')
```
o The second option is download the app and to use it offline:

-download the `app.R` and csv file (`gapminder-FiveYearData.csv`) with example data.

-Run RStudio and load `app.R`

-Select 'Run All' (shortcut is command-option-R on a Mac) or click on "Run App" (upper right button on the window)

This should launch a web browser with the Shiny app.


### Credits

The code for the shiny app is partially derived from [ggplotGUI](https://github.com/gertstulp/ggplotgui) by [Gert Stulp](https://www.gertstulp.com)  
The idea of line-by-line encoding with ggplot2 is taken from [Evangeline Reynolds](https://evangelinereynolds.netlify.com) and elegantly explained in [this flipbook](https://evamaerey.github.io/ggplot2_grammar_guide/ggplot2_grammar_guide#1) (it may take a moment to load the flipbook completely)

ggPlotteR is created and maintained by Joachim Goedhart ([@joachimgoedhart](https://twitter.com/joachimgoedhart))

### Example output

![alt text](https://github.com/JoachimGoedhart/ggPlotteR/blob/master/ggPlotteR-output.png "Output")
