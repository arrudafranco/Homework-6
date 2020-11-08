load("C:/Users/Gustavo/OneDrive/Documents/Computation Repo/hw02/hw06/F00004532-Latinobarometro_2015_r/Latinobarometro_2015_Eng.rdata")
load("C:/Users/Gustavo/OneDrive/Documents/Computation Repo/hw02/hw06/F00005906-Latinobarometro2016_r/Latinobarometro2016Eng_v20170205.rdata")
load("C:/Users/Gustavo/OneDrive/Documents/Computation Repo/hw02/hw06/F00006501-Latinobarometro2017_r/Latinobarometro2017Eng_v20180117.rdata")
Latinobarometro_2018_Esp_R_v20190303 <- readRDS("C:/Users/Gustavo/OneDrive/Documents/Computation Repo/hw02/hw06/F00008548-Latinobarometro_2018_Esp_R_v20190303/Latinobarometro_2018_Esp_R_v20190303.rds")
load("C:/Users/Gustavo/OneDrive/Documents/Computation Repo/hw02/hw06/LAT_Latinobarometro2013_r/Latinobarometro2013Eng.rdata")
F00008653_SerieDeTiempo_1995_2018 <- read_excel("F00008653-SerieDeTiempo_1995_2018.xlsx")

library(dplyr)

codebook <- select(F00008653_SerieDeTiempo_1995_2018, -('v1995':'v2011')) %>% #Dropping years that will not be analyzed.
  rename_with(~as.integer(sub("v", "", .x)), .cols = 9:13) #Standardizing year variables.

#Still standardizing year variables.
latinobarometro_2015 <- mutate(Latinobarometro_2015_Eng, numinves = 2015, .keep = "unused")
latinobarometro_2013 <- mutate(Latinobarometro2013Eng, numinves = 2013, .keep = "unused")

#Function that standardizes column names so it's possible to safely filter only data from Brazil.
brazil_filter <- function(data_year){
  upper_case_colnames <- rename_with(data_year, .fn = toupper)
  only_brazil <- filter(upper_case_colnames, IDENPA == 76)
  new.name <- paste('BRAZIL', as.character(data_year[1, 1]), sep = "_") #Creates new dataframe.
  assign(new.name, only_brazil, envir=.GlobalEnv)
  #Since each original dataframe only has data from a single year, it makes sense to extract names from the first year cell in each.
}

brazil_filter(latinobarometro_2013)
brazil_filter(latinobarometro_2015)
brazil_filter(Latinobarometro2016Eng_v20170205)
brazil_filter(Latinobarometro2017Eng_v20180117)
brazil_filter(Latinobarometro_2018_Esp_R_v20190303)

#Now I intend to rename all variables according to a single standard before selecting the ones I want to work with.
#There is a table relating them but I don't know how to use that yet in an efficient way.

variable_selection <- function(whole_data){
  select(whole_data, NUMINVES, )
}

codes_2013 <- data.frame("2013" = colnames(BRAZIL_2013))
rename(codes_2013, "X2013" = "2013") # I don't understand why this X showed up, so I have this solution for now.
codes_2013_int = left_join(codes_2013, codebook)
list_var <- as.vector(codes_2013_int['2015'], mode = 'character')
colnames(BRAZIL_2013) <- list_var
#I am not being able to neatly transfer the corresponding 2015 variable names to 2013.
#For some reason, when I try it all 2015 variable names end up in the first column of 2013.