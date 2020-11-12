# hw06

## Gustavo Arruda

This repository is part of a University of Chicago course called "Computation for the Social Sciences" taught in the fall of 2020. It contains 2013-2018 data from the [Latinobarometro survey](https://www.latinobarometro.org/), which measures variations in Latin American political sentiments. It also contains an exploratory analysis of the relationship between religious and democracy support variables in Brazil.

 - Running the [data_cleaning.R](data_cleaning.R) script loads, standardizes and combines the datasets into an analyzable output.
 - The [data_analysis.R](data_analysis.R) script takes the outputs of data_cleaning.R, then produces its own outputs describing the relations among the variables. They are necessary to produce the graphs from Brazilian_Religion_Democracy.Rmd.
 - [Brazilian_Religion_Democracy.Rmd](Brazilian_Religion_Democracy.Rmd) is a Markdown file that renders a written analysis of the data, including exploratory charts. It also automatically runs the two previous scripts.
  
Used Libraries:

- To run the code in this repository, the libraries used were:
  - library(readxl)
  
  Used to load the codebook, originally found in xlsx format.
 
  - library(sjlabelled)
 
  Used to strip labels preventing the combination of different datasets.
  
  - library(RColorBrewer)
 
  Used to improve visualization in the Markdown file.