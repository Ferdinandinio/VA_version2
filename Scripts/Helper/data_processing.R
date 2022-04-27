library(plyr)
library(tidyverse)



data <- readxl::read_excel("Data/IPBES_VA_Uptake_Corpus_06May20_GEONames_TS_16June20.xlsx", sheet = 1)
# Necessary datasets for the upcoming maps
harmonized_data <- read.csv("Outputs/Corpus/harmonized_data.csv")

# only keep relevant columns
relcol <- names(harmonized_data)[c(1,2,4)]
harmonized_data <- harmonized_data[relcol]
names(harmonized_data) <- c("ISO_Alpha_3", "DoSTotal", "DoOTotal")

# Declare Functions
countFunction <- function(data, x) {
                                      data %>% 
                                              mutate(x = strsplit(as.character(x), ", ")) %>% 
                                              unnest(x) %>% 
                                              count(x, sort = TRUE) %>% 
                                              drop_na() #%>%
                                              # mutate(n_log = log(n))
                                    }





detach("package:plyr", unload = TRUE)

# Prepare data
corpus <- data

# NAMES 1
# Group data temporally 
DoSb1990 <- corpus %>% 
              filter(PY < 1990) %>% 
              countFunction(.$CountryName_TI_AB_DE_ID) %>% 
              rename("ISO_Alpha_3" = x, "DoSb1990" = n)
            
DoSb2000 <- corpus %>% 
              filter(PY >= 1990 & PY < 2000) %>% 
              countFunction(.$CountryName_TI_AB_DE_ID) %>% 
              rename("ISO_Alpha_3" = x, "DoSb2000" = n)
            
DoSb2010 <- corpus %>% 
              filter(PY >= 2000 & PY < 2010) %>% 
              countFunction(.$CountryName_TI_AB_DE_ID) %>% 
              rename("ISO_Alpha_3" = x, "DoSb2010" = n)
            
DoSb2020 <- corpus %>% 
              filter(PY >= 2010) %>% 
              countFunction(.$CountryName_TI_AB_DE_ID) %>% 
              rename("ISO_Alpha_3" = x, "DoSb2020" = n)



# Names 2
# Group data temporally 
DoOb1990 <- corpus %>% 
              filter(PY < 1990) %>% 
              countFunction(.$CountryName_CI_FU_FX) %>% 
              rename("ISO_Alpha_3" = x, "DoOb1990" = n)

DoOb2000 <- corpus %>% 
              filter(PY >= 1990 & PY < 2000) %>% 
              countFunction(.$CountryName_CI_FU_FX) %>% 
              rename("ISO_Alpha_3" = x, "DoOb2000" = n)

DoOb2010 <- corpus %>% 
              filter(PY >= 2000 & PY < 2010) %>% 
              countFunction(.$CountryName_CI_FU_FX) %>% 
              rename("ISO_Alpha_3" = x, "DoOb2010" = n)

DoOb2020 <- corpus %>% 
              filter(PY >= 2010) %>% 
              countFunction(.$CountryName_CI_FU_FX) %>% 
              rename("ISO_Alpha_3" = x, "DoOb2020" = n)


alltfs <- mget(ls(pattern="^Do.*"))

listx <- append(list(harmonized_data), alltfs)

library(plyr)

joined <- join_all(listx, by = "ISO_Alpha_3", type="left") #%>% reduce(left_join, by = "ISO_Alpha_3")


#create Ratio
namesj <- names(joined)

namesDoS <- grep("DoS",namesj,value = T)
namesDoO <- grep("DoO",namesj,value = T)
namesRatio <- gsub("DoS", "Ratio", namesDoS)

nl <- length(namesDoS)

# loop divide
for(i in 1:nl){
  joined[namesRatio[i]] <- joined[namesDoS[i]] / joined[namesDoO[i]] 
  
}


namesneworder <- c("ISO_Alpha_3", namesDoS, namesDoO, namesRatio)

joined <- joined[namesneworder] #change order of names
joined

write.csv(joined, file="Data/processed_data_D.csv", row.names = F) #exports numbers
# x <- read.csv("Data/processed_data_D.csv")
# x
# joined 



