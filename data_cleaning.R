library(dplyr)
library(readxl)
#install.packages("sjlabelled")
library(sjlabelled)
#https://rdrr.io/cran/sjlabelled/man/remove_all_labels.html
#The above package was helpful to deal with the labelled 2018 dataset, before
#combining all of them.


load("F00004532-Latinobarometro_2015_r/Latinobarometro_2015_Eng.rdata")
load("F00005906-Latinobarometro2016_r/Latinobarometro2016Eng_v20170205.rdata")
load("F00006501-Latinobarometro2017_r/Latinobarometro2017Eng_v20180117.rdata")
Latinobarometro_2018_Esp_R_v20190303 <- readRDS("F00008548-Latinobarometro_2018_Esp_R_v20190303/Latinobarometro_2018_Esp_R_v20190303.rds")
load("LAT_Latinobarometro2013_r/Latinobarometro2013Eng.rdata")
F00008653_SerieDeTiempo_1995_2018 <- read_xlsx("F00008653-SerieDeTiempo_1995_2018.xlsx")

codebook <- dplyr::select(F00008653_SerieDeTiempo_1995_2018, -('v1995':'v2011')) %>% #Dropping years that will not be analyzed.
  rename_with(~sub("v", "", .x), .cols = 9:13) #Standardizing year variables.

#Still standardizing year variables.
latinobarometro_2015 <- mutate(Latinobarometro_2015_Eng, numinves = 2015, .keep = "unused")
latinobarometro_2013 <- mutate(Latinobarometro2013Eng, numinves = 2013, .keep = "unused")

#Function that standardizes column names so it's possible to safely filter only data from Brazil.
brazil_filter <- function(data_year){
  upper_case_colnames <- rename_with(data_year, .fn = toupper)
  only_brazil <- filter(upper_case_colnames, IDENPA == 76)
  new_name <- paste('BRAZIL', as.character(data_year[1, 1]), sep = "_") #Creates new dataframe.
  assign(new_name, only_brazil, envir=.GlobalEnv)
  #Since each original dataframe only has data from a single year, it makes sense to extract names from the first year cell in each.
}

brazil_filter(latinobarometro_2013)
brazil_filter(latinobarometro_2015)
brazil_filter(Latinobarometro2016Eng_v20170205)
brazil_filter(Latinobarometro2017Eng_v20180117)
brazil_filter(Latinobarometro_2018_Esp_R_v20190303)

#I made a list of variables of interest, then filtered the codebook.

vars_interest <- c('A_001_001', 'A_003_031', 'H_002_101', 'H_002_111', 'H_002_161',
                   'I_001_001', 'S_700', 'S_701', 'X_002')

codebook_reduced <- filter(codebook, `Indice` %in% vars_interest) %>%
  rename('English' = 'Inglés')

#The following function finds out the year of each data set by selecting its first cell.
#Like previously, it works because the data sets are divided by year.
#The year selects the desirable variables through its column in the codebook_reduced.
#I finish standardizing the variables with a function to substitute each variable
#by its 'Indice' correspondent.

variable_selection <- function(whole_data, first = FALSE){
  year_function <- as.character(whole_data[1, 1])
  filtered_data <- dplyr::select(whole_data, codebook_reduced[[year_function]])
  for (i in vars_interest) {
    old_var <- as.character(codebook_reduced[codebook_reduced$'Indice' == i,][year_function])
    filtered_data <- rename_with(filtered_data, ~sub(paste(old_var), paste(i), .x))
  }
  if (first == TRUE){
    assign('combining_dataset', filtered_data, envir=.GlobalEnv)
    #Start a combined dataset, 
  }
  else{
    if (year_function == '2018'){
      filtered_data <- remove_all_labels(filtered_data)
      #Removes labels from 2018 dataset.
    }
    new_name <- paste('BRAZIL', year_function, 'FILTERED', sep = "_") #Creates new dataframe.
    assign(new_name, filtered_data, envir=.GlobalEnv)
  }
}

variable_selection(BRAZIL_2013, first = TRUE)
variable_selection(BRAZIL_2018)
variable_selection(BRAZIL_2017)
variable_selection(BRAZIL_2016)
variable_selection(BRAZIL_2015)

full_dataset <- bind_rows(combining_dataset, BRAZIL_2018_FILTERED, BRAZIL_2017_FILTERED,
                      BRAZIL_2016_FILTERED, BRAZIL_2015_FILTERED)

