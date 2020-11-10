library(dplyr)
library(readxl)

load("C:/Users/Gustavo/OneDrive/Documents/Computation Repo/hw02/hw06/F00004532-Latinobarometro_2015_r/Latinobarometro_2015_Eng.rdata")
load("C:/Users/Gustavo/OneDrive/Documents/Computation Repo/hw02/hw06/F00005906-Latinobarometro2016_r/Latinobarometro2016Eng_v20170205.rdata")
load("C:/Users/Gustavo/OneDrive/Documents/Computation Repo/hw02/hw06/F00006501-Latinobarometro2017_r/Latinobarometro2017Eng_v20180117.rdata")
Latinobarometro_2018_Esp_R_v20190303 <- readRDS("C:/Users/Gustavo/OneDrive/Documents/Computation Repo/hw02/hw06/F00008548-Latinobarometro_2018_Esp_R_v20190303/Latinobarometro_2018_Esp_R_v20190303.rds")
load("C:/Users/Gustavo/OneDrive/Documents/Computation Repo/hw02/hw06/LAT_Latinobarometro2013_r/Latinobarometro2013Eng.rdata")
F00008653_SerieDeTiempo_1995_2018 <- read_xlsx("F00008653-SerieDeTiempo_1995_2018.xlsx")

codebook <- select(F00008653_SerieDeTiempo_1995_2018, -('v1995':'v2011')) %>% #Dropping years that will not be analyzed.
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

#I had written down in my personal notes my variables of interest.
#I knew they spanned all the years I am investigating, but I only had the 2018 codes.
#To solve this issue, I made a list of 2018 variables of interest, and filtered the codebook.

vars_interest <- c('S5', 'S5A', 'P69ST.2', 'P15STGBSC.A', 'P15STGBSC.B',
                   'P15STGBSC.C', 'P12STGBS', 'P13STGBS.A', 'NUMINVES')

codebook_reduced <- filter(codebook, `2018` %in% vars_interest) %>%
  rename('English' = 'Inglés')

#The following function finds out the year of each data set by selecting its first cell.
#Like previously, it works because the data sets are divided by year.
#The year selects the desirable variables through its column in the codebook_reduced.
#To finish standardizing the variables, I continue to use 2018 as the reference year.
#If the data set is not from 2018, the function will substitute each variable by its correspondent.

variable_selection <- function(whole_data){
  year_function <- as.character(whole_data[1, 1])
  filtered_data <- select(whole_data, codebook_reduced[[year_function]])
  if (year_function == '2018') {
    assign('combined_dataset', filtered_data, envir=.GlobalEnv)
  }
  else{
    for (i in vars_interest) {
      old_var <- as.character(codebook_reduced[codebook_reduced$'2018' == i,][year_function])
      filtered_data <- rename_with(filtered_data, ~sub(paste(old_var), paste(i), .x))
    }
    bind_rows(combined_dataset, filtered_data) #Trying to iteratively combine the datasets.
  }
}

variable_selection(BRAZIL_2018)
variable_selection(BRAZIL_2017)
variable_selection(BRAZIL_2016)
variable_selection(BRAZIL_2015)
variable_selection(BRAZIL_2013)

#new_name <- paste('BRAZIL', year_function, 'FILTERED', sep = "_") #Creates new dataframe.
#assign(new_name, filtered_data, envir=.GlobalEnv)