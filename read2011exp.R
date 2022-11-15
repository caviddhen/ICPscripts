library(readxlsb)
library(tidyverse)
library(magpie4)
library(mrland)
library(GDPuc)

setwd("C:/PIK/ICPdata")


### read 2017 sheet for hotel ratio
exp <- read_xlsb(path = "C:/PIK/ICPdata/ICP-Researcher-Data_Global_2017_Chen_0810-2022.xlsb",
                 sheet = "EXP", skip = 3)

#mapping of k to BH aggregate category to even broader cat
kBHmap <- read.csv("MappingKBH.csv")

colnames(exp)[1:5] <- exp[1, c(1:5)]
colnames(exp)[6] <- "ClassificationCode"

exp <- exp%>%
  filter(!row_number() == 1) %>%
  pivot_longer(cols = 7:ncol(exp), names_to = "iso3c", values_to = "LCU") %>%
  rename("BHCode" = `Item Code`, "BHName" = `Item Name`) %>%
  select(BHCode, BHName, iso3c, ClassificationCode, LCU)

source("readICPexchangeRates.R")

exp <- inner_join(exp, exchanges) %>%
  mutate(expPPP = (LCU/pppEX)/1e9, expMER = (LCU/merEX)/1e9) #in billions


### get ratio of resto to hotel
hrat <- filter(exp, BHName %in% c("RESTAURANTS AND HOTELS", "CATERING SERVICES")) %>%
         select(BHName, iso3c, expMER) %>%
         pivot_wider(names_from = BHName, values_from = expMER) %>%
        mutate(hrat = `CATERING SERVICES`/`RESTAURANTS AND HOTELS`)


unique(exp$BHName)
filter(exp, BHName  %in% c ( "CATERING SERVICES", "Catering services (Class)", "Catering services (BH)"),
            iso3c == "CAN")



exp2 <- read.csv("c4391ee3-4ebc-4ab0-a94b-bd47c059040e_Data.csv")

exp21 <- exp2 %>%
          filter(!is.na(Series.Code)) %>%
         select(Country.Code, Series.Name, X2011..YR2011.) %>%
        rename("iso3c" = Country.Code, "expMER" = X2011..YR2011.) %>%
       separate(Series.Name, into = c("BHCode", "BHName"), sep = ":") %>%
      filter(BHName %in% c( "Bread and cereals", "Meat", "Fish and seafood", "Milk, cheese and eggs",
                            "Oils and fats", "Fruit", "Vegetables", "Sugar, jam, honey, chocolate and confectionery",
                            "Food products n.e.c. (Class)", "RESTAURANTS AND HOTELS")) %>%
      filter(expMER != "..") %>%
      mutate(expMER = as.numeric(expMER))



cater <- filter(exp21, BHName == "RESTAURANTS AND HOTELS") %>%
       inner_join(hrat) %>%
        mutate("Catering services (Class)" = expMER * hrat) %>%
        select(iso3c,"Catering services (Class)")  %>%
        mutate(BHName = "Catering services (Class)", BHCode = "1111110") %>%
        rename("expMER" = "Catering services (Class)") %>%
        filter(!is.na(expMER))

exp21 <- rbind(exp21, cater) %>%
         filter(BHName != "RESTAURANTS AND HOTELS") %>%
  mutate(year = 2011)




