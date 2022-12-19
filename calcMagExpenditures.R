library(tidyverse)
library(magpie4)
library(mrcommons)
library(magpiesets)
setwd("C:/PIK/ICPdata/")

mag <- "./BAUfulldata.gdx"
pol <- "./POLfulldata.gdx"

kfo <- findset("kfo")

kBH <- read.csv("mapMAgPIELEM.csv") %>%
  rename("BHName" = prod)

h12 <- toolGetMapping("h12.csv", type = "regional") %>%
  rename("iso3c" = "CountryCode", "i" = "RegionCode") %>%
  select(iso3c, i)

t <- prices(mag, type = "consumer")
t["USA",c(2010,2015,2020),]


attr <- calcOutput("Attributes", aggregate = F)[,,"wm"] #convert prices to wm for markup
wm <- attr   %>% as.data.frame(rev = 2)  %>%
  rename("k" = "products", "wm" = ".value")  %>%
  select(k, wm)


prprmag <- prices(mag, type = "consumer")
prprmag <- add_dimension(prprmag, dim = 3.2, nm = "BAU")
prprPOL <- prices(pol, type = "consumer")
prprPOL <- add_dimension(prprPOL, dim = 3.2, nm = "POL")

prpr <- mbind(prprmag, prprPOL)

prpr <- collapseNames((prpr / attr[,,"wm"]))

prpr <- time_interpolate(prpr, interpolated_year = c(2010:2017), integrate_interpolated_years = TRUE)

prpr <- add_columns(prpr, addnm = "Vegetables", dim = 3.1, fill = NA)
prpr[,,"Vegetables"] <- prpr[,,"others"]

prpr <- toolAggregate(prpr, rel = h12,
                      from = "i", to = "iso3c",
                      weight = NULL)

### replace price for oils and sugars (which already processed) with weighted average price of
#primary products that go into the secondary products
ksd <- findset("ksd")

consmag <- demand(mag)
consmag <- gdxAggregate(gdx = mag, consmag, weight = 'Intake', to = "iso")[,,"food"]
consmag <- collapseNames(consmag)
consmag <- add_dimension(consmag, dim = 3.2, nm = "BAU")
conspol <- demand(pol)
conspol <- gdxAggregate(gdx = pol, conspol, weight = 'Intake', to = "iso")[,,"food"]
conspol <- collapseNames(conspol)
conspol <- add_dimension(conspol, dim = 3.2, nm = "POL")


cons <- mbind(consmag, conspol)

kfo <- c(findset("kfo"), "Vegetables")
#cons <- demand(mag)
#cons <- gdxAggregate(gdx = mag, cons, weight = 'Intake', to = "iso")[,,"food"]
cons <- add_columns(collapseNames(cons), addnm = "Vegetables")
#load input vegetable data to get a the latest split
load("consKmag.Rda")
VegShr <- consKmag[,,"Vegetables"]/collapseNames((consKmag[,,"others"]+consKmag[,,"Vegetables"]))
VegShr <- toolCountryFill(VegShr, fill = 0.48) #mean value
othShr <- (1-VegShr)
cons[,,"Vegetables"] <- setYears(VegShr[,2019,], NULL) * cons[,,"others"]
cons[,,"others"] <- setYears(othShr[,2019,], NULL) * cons[,,"others"]


attr <- add_columns(attr, addnm = "Vegetables", dim = 3.2)
attr[,,"Vegetables"] <- attr[,,"others"]
cons <- cons * attr[,,"wm"] %>%
  collapseNames()

load("proc_shr.Rda")
#proc_shr <- time_interpolate(proc_shr[getRegions(prpr),,], interpolated_year = getYears(prpr),
#                             integrate_interpolated_years = T)
proc_shr <- proc_shr[,getYears(cons1),]
proc_shr <- add_dimension(proc_shr, dim = 3.3, nm = c("BAU","POL"))
proc_shr[,,"POL"]  <- proc_shr[,,"BAU"]

load("cvn_fct.Rda")
#cvn_fct <- time_interpolate(cvn_fct, interpolated_year = getYears(prpr),
#                            integrate_interpolated_years = T)[,getYears(prpr),]
cvn_fct <- cvn_fct[,getYears(cons),]
cvn_fct <- add_dimension(cvn_fct, dim = 3.4, nm = c("BAU","POL"))
cvn_fct[,,"POL"]  <- cvn_fct[,,"BAU"]

proc <- cons[,,intersect(ksd, getNames(cons, dim = 1))][,,"alcohol", inv = T]
proc_oils <- collapseNames((proc[,,"oils"] /
                              dimSums(cvn_fct[,,c("milling", "extracting")][,,"oils"],dim=3.1) *
                              proc_shr[,,"oils"] ))
proc_oils[is.na(proc_oils)] <- 0
proc_oils <- time_interpolate(proc_oils, interpolated_year = getYears(prpr),
                               integrate_interpolated_years = T)[,getYears(prpr),]
proc_oils <- dimOrder(proc_oils, perm = c(2,1), dim =3)


proc_sugar <- collapseNames((proc[,,"sugar"] /
                               dimSums(cvn_fct[,,"refining"][,,list("ItemCodeItem" = "sugar")],dim=3.1) *
                               proc_shr[,,list("ItemCodeItem" = "sugar")] ))
proc_sugar[is.na(proc_sugar)] <- 0
proc_sugar <- time_interpolate(proc_sugar, interpolated_year = getYears(prpr),
                               integrate_interpolated_years = T)[,getYears(prpr),]
proc_sugar <- dimOrder(proc_sugar, perm = c(2,1), dim =3)

citems <- intersect(getNames(proc_sugar, dim =1), getNames(prpr, dim =1))

sugmap <- data.frame(sugar = rep("sugar", length(citems)), pr = citems)
prpr_sug <- toolAggregate(prpr[,,citems], rel = sugmap,
                   from = "pr", to = "sugar",
                   weight = proc_sugar[,,citems],
                   dim = 3.1)

citems <- intersect(getNames(proc_oils, dim =1), getNames(prpr, dim = 1))
oilmap <- data.frame(oils = rep("oils", length(citems)), pr = citems)
prpr_oils <- toolAggregate(prpr[,,citems], rel = oilmap,
                          from = "pr", to = "oils",
                          weight = proc_oils[,,citems],
                          dim = 3.1)

prpr[,,"sugar"] <- prpr_sug
prpr[,,"oils"] <- prpr_oils

prpr<- prpr[,,kfo]


prpr <- prpr %>%
  collapseNames() %>%
    as.data.frame(rev = 2) %>%
  rename("iso3c" = "i", "year" = t, "k" = kcr, "scen" = new, "value" = ".value") %>%
  inner_join(kBH) %>%
  GDPuc::convertGDP(unit_in = "constant 2005 US$MER",
                   unit_out = "constant 2017 US$MER",
                   replace_NAs = "no_conversion")


##### get markup regression coefs #####
load("CaterSplitNoLogModsNOW_DBUupdate.Rda")

coefsCater <- unlist(modsNoLogwCater$exps_coef)
coefsNoCater <-  unlist(modsNoLogNoCater$exps_coef)

coefsCater <- stack(coefsCater)  %>%
  separate(ind, sep = "[.]", into = c("prod", "value"))  %>%
  pivot_wider(names_from = value, values_from = values)

coefsNoCater <- stack(coefsNoCater)  %>%
  separate(ind, sep = "[.]", into = c("prod", "value"))  %>%
  pivot_wider(names_from = value, values_from = values)

# load("coefs_lmnoweight.Rda") # NO WEITH

#remove alcohol
prpr <- filter(prpr, k != "alcohol")

mappingLEM <- read.csv("./mapMAgPIELEM.csv")

magCoefsCater <- mappingLEM  %>%
  inner_join(coefsCater) %>%
  rename("BHName" = "prod")

magCoefsNoCater <- mappingLEM  %>%
  inner_join(coefsNoCater)%>%
  rename("BHName" = "prod")

load("gdppc_const2017MER.Rda")
gdppc <- gdppc %>%
  as.data.frame(rev = 2)  %>%
  rename("gdppc" = ".value") %>%
  select(iso3c, year, gdppc)


markupPrCater <- inner_join(prpr, gdppc)  %>%
  #rename("k" = kcr)  %>%
  inner_join(magCoefsCater)  %>%
mutate(markupCater = a*(b^log(gdppc, base = 10)),
         CaterPrice = value + markupCater)

markupPrNoCater <- inner_join(prpr, gdppc)  %>%
  #rename("k" = kcr)  %>%
  inner_join(magCoefsNoCater)  %>%
  mutate(markupNoCater = a*(b^log(gdppc, base = 10)),
         noCaterPrice = value + markupNoCater)

markups <- markupPrCater %>%
  select(iso3c, year, k, scen, value, markupCater, CaterPrice) %>%
  inner_join(select(markupPrNoCater, iso3c, year, k, scen, value, markupNoCater, noCaterPrice)) %>%
  rename("prodPrice" = value)

diff <- markups %>%
  mutate(comp = CaterPrice - noCaterPrice)

markups <-  markups %>%
  pivot_longer(cols = c(prodPrice, noCaterPrice,CaterPrice),
               names_to = "Price Type", values_to = "Price") %>%
  select(iso3c, year, k, scen,  `Price Type`, Price)


def2020 <- filter(markups,year == 2020) %>%
             dplyr::rename( "pr20" = "Price"  ) %>%
         select(iso3c, k, scen, pr20, `Price Type`)

markups <- inner_join(markups, def2020) %>%
         mutate(PriceIndex = Price/pr20) %>%
       select(!pr20)


markups$`Price Type` <- factor(markups$`Price Type`,
                               levels = c("prodPrice", "CaterPrice", "noCaterPrice"))


# reg <- "EUR"
#

### global price plot
iG <- gdppc %>% filter(year == 2020) %>%
  mutate(incomeG =  case_when(
    gdppc <= 1006 ~ "LIC",
    gdppc > 1006 & gdppc <= 3956 ~ "LMIC",
    gdppc > 3956 & gdppc <= 12235 ~ "UMIC",
    gdppc > 12235 ~ "HIC")) %>%
  select(iso3c, incomeG)


iG$incomeG <- factor(iG$incomeG, levels = c("HIC", "UMIC","LMIC", "LIC"))
markups$`Price Type` <- factor(markups$`Price Type`,
                                  levels = c("CaterPrice", "noCaterPrice", "prodPrice"))
kBH$BHName <- factor(kBH$BHName,
                               levels = c("Bread and cereals", "Meat", "Milk products", "Eggs",
                                          "Vegetables", "Fruit", "Processed"))
kBH <- mutate(kBH,
              t = case_when(
                BHName %in% c("Bread and cereals",
                               "Vegetables", "Fruit",
                                "Processed") ~ "Plant-Based",
                BHName %in% c("Meat", "Milk products", "Eggs") ~
                  "Livestock Products"
              ))

cons <- cons[,,kfo] %>%
  as.data.frame(rev=2)  %>%
  rename("foodD" = ".value", "iso3c" = "i",
         "k" = "kall", "year" = "t", "scen" = "new")


markupsGlo <- inner_join(markups, iG) %>%
   inner_join(cons) %>%
   inner_join(kBH) %>%
   group_by(year, incomeG, BHName, `Price Type`, scen) %>%
   summarise(Price = weighted.mean(Price, w = foodD))


markupsGloProd <- inner_join(markups, iG) %>%
  inner_join(cons) %>%
  inner_join(kBH) %>%
  group_by(year, `Price Type`, BHName, scen) %>%
  summarise(Price = weighted.mean(Price, w = foodD))

markupsGloG <- inner_join(markups, iG) %>%
  inner_join(cons) %>%
  inner_join(kBH) %>%
  group_by(year, `Price Type`, scen) %>%
  summarise(Price = weighted.mean(Price, w = foodD)) %>%
  mutate(t = "Total")

markupsGloIG <- inner_join(markups, iG) %>%
  inner_join(cons) %>%
  inner_join(kBH) %>%
  group_by(year, incomeG, `Price Type`, scen) %>%
  summarise(Price = weighted.mean(Price, w = foodD)) %>%
  mutate(t = "Total")

markupsGlo3 <- inner_join(markups, iG) %>%
  inner_join(cons) %>%
  inner_join(kBH) %>%
  group_by(year, `Price Type`, t, scen) %>%
  summarise(Price = weighted.mean(Price, w = foodD))

markupsGlo3 <- rbind(markupsGlo3, markupsGloG) %>%
             mutate(t = factor(t, levels = c("Plant-Based","Livestock Products",
                                              "Total")))

## BAU plot for all 3 aggregations

ggplot(filter(markupsGlo,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(incomeG~ BHName, scales = "free", nrow = 4) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 10)

ggplot(filter(markupsGloProd,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
 facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                    values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 14)

ggplot(filter(markupsGloIG,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~incomeG, scales = "free", nrow = 1) +
  ggtitle(paste("BAU")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 16)


ggplot(filter(markupsGlo3,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~ t, scales = "free", nrow = 1) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 14)


ggplot(filter(markupsGloG,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  #facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 18)



## POL plot for all 3 aggregations

ggplot(filter(markupsGlo,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(incomeG~ BHName, scales = "free", nrow = 4) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 10)

ggplot(filter(markupsGloProd,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 14)


ggplot(filter(markupsGloG,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  #facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 18)


### make relative to BAU

markupsRatioGlo <- markupsGlo %>%
         pivot_wider(names_from = scen, values_from = Price) %>%
         mutate(ratio = POL/BAU)

markupsRatioGloProd <- markupsGloProd %>%
  pivot_wider(names_from = scen, values_from = Price) %>%
  mutate(ratio = POL/BAU)

markupsRatioGloG <- markupsGloG %>%
  pivot_wider(names_from = scen, values_from = Price) %>%
  mutate(ratio = POL/BAU)

markupsRatioGloIG <- markupsGloIG %>%
  pivot_wider(names_from = scen, values_from = Price) %>%
  mutate(ratio = POL/BAU)

markupsRatioGlo3 <- markupsGlo3 %>%
  pivot_wider(names_from = scen, values_from = Price) %>%
  mutate(ratio = POL/BAU)


### plots comparing BAU and POL
ggplot(filter(markupsRatioGlo, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(incomeG ~ BHName, scales = "free", nrow = 4) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(labels = c("Prod Price", "Consumer Price FAH", "Consumer Price FAFH" ),
                     values = c( "#54D598", "#1E5B3E", "#348C62"))+
  theme_bw(base_size = 11)

ggplot(filter(markupsRatioGloProd, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap( ~ BHName, scales = "free", nrow =2) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(labels = c("Prod Price", "Consumer Price FAH", "Consumer Price FAFH" ),
                     values = c( "#54D598", "#1E5B3E", "#348C62"),
                     guide = guide_legend(reverse = TRUE) )+
  theme_bw(base_size = 11)

ggplot(filter(markupsRatioGloIG, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap( ~ incomeG, scales = "free", nrow = 2) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(labels = c( "Consumer Price FAFH", "Consumer Price FAH", "Prod Price"),
                     values = c(  "#1E5B3E","#348C62", "#54D598"))+
  theme_bw(base_size = 18)

ggplot(filter(markupsRatioGlo3, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap( ~ t, scales = "free", nrow = 2) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(labels = c( "Consumer Price FAFH", "Consumer Price FAH", "Prod Price"),
                     values = c(  "#1E5B3E","#348C62", "#54D598"))+
  theme_bw(base_size = 18)



ggplot(filter(markupsRatioGloG, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  #facet_wrap( ~ BHName, scales = "free", nrow = 4) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(labels = c( "Consumer Price FAFH", "Consumer Price FAH", "Prod Price"),
                     values = c(  "#1E5B3E","#348C62", "#54D598"))+
  theme_bw(base_size = 11)




#use FAO consumption

### plots comparing BAU and POL
ggplot(filter(markups, iso3c == iso,
              k %in% prods,
              scen %in% c("BAU", "POL"),
              `Price Type` == "noCaterPrice"),
       aes(x = year, y = Price, color = scen))+
  geom_line(lwd = 1.4)+
  facet_wrap(~k, scales = "free") +
  ggtitle(paste(iso, " Consumer FAH Prices")) +
  scale_color_manual(labels = c("BAU", "POL"),
                     values = c( "#5AD2D8", "#995AD8"),
                     guide = guide_legend(reverse = TRUE) ) +
  theme_bw(base_size = 18)

### plots comparing BAU and POL
ggplot(filter(markups, iso3c == iso,
              k %in% prods,
              scen %in% c("BAU", "POL"),
              `Price Type` == "CaterPrice"),
       aes(x = year, y = Price, color = scen))+
  geom_line(lwd = 1.4)+
  facet_wrap(~k, scales = "free") +
  ggtitle(paste(iso, " Consumer FAFH Prices")) +
  scale_color_manual(labels = c("BAU", "POL"),
                     values = c( "#5AD2D8", "#995AD8"),
                     guide = guide_legend(reverse = TRUE) ) +
  theme_bw(base_size = 18)


magExp <- inner_join(cons,
                     select(markups, !PriceIndex))  %>%
  inner_join(gdppc) %>%
  mutate(AFHshr =  (4.540e-06*gdppc + 6.611e-02)) %>%      ### from kcal_fafh
  pivot_wider(names_from =  c(`Price Type`),
              values_from = Price) %>%
  mutate(fahExp = foodD * (1-AFHshr) * (noCaterPrice),
         fafhExp = foodD * AFHshr * (CaterPrice),
         farmAHexp = foodD *(1-AFHshr) * prodPrice,
         farmAFHexp = foodD *AFHshr * prodPrice,
         farmAHshr = farmAHexp/fahExp,
         farmAFHshr = farmAFHexp/fafhExp)%>%
  select(iso3c, year, k, scen, foodD, gdppc, prodPrice, CaterPrice, noCaterPrice, fahExp, fafhExp, farmAHexp, farmAFHexp, farmAHshr, farmAFHshr)


magExpMeanK <- magExp %>%
  group_by(iso3c, year, scen) %>%
  summarise(fahExp = sum(fahExp),
            fafhExp = sum(fafhExp),
            farmAHexp = sum(farmAHexp),
            farmAFHexp = sum(farmAFHexp)) %>%
  mutate(totExp = fahExp + fafhExp,
         totfarmExp = farmAHexp + farmAFHexp,
         farmShrAH =  farmAHexp / fahExp,  # get farm shares
         farmShrAFH = farmAFHexp/fafhExp,
         farmShrTot = totfarmExp/totExp) %>%
  mutate(across(c(ends_with("Exp")),  ~ . / !!1e9 ),
         totalFoodExp = fahExp + fafhExp) # get total food exp in billions


yi4 <- readxl::read_xlsx("YiSourceFig4.xlsx", skip =1) %>%
  pivot_longer(cols = c(2:last_col()), names_to = "year", values_to = "YifarmAHshr") %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year)) %>%
  group_by(Country, year) %>%
  summarise(YifarmAHshr = mean(YifarmAHshr, na.rm =T)) %>%
  ungroup()
yi4$iso3c <- toolCountry2isocode(yi4$Country, mapping = c("Korea, Rep." = "KOR"))

compyi4 <-  select(magExpMeanK, iso3c, year,scen, farmShrAH) %>%
  inner_join( select(yi4, iso3c, year, YifarmAHshr)) %>%
  pivot_longer(cols = c(farmShrAH, YifarmAHshr),
               names_to = "source", values_to = "farmAHShr")

ggplot(filter(compyi4,
               scen == "BAU"),
       aes(x = year, y = farmAHShr, colour = source)) +
  geom_line()+
  facet_wrap(~iso3c) +
  #  ylim(c(0.15, 0.45)) +
  theme_bw(base_size = 20) +
  ggtitle("Farm Share of At Home Food Expenditures \n based on MAgPIE prices")


yi3 <- readxl::read_xlsx("YiSourceFig3.xlsx", skip = 4) %>%
  rename("year" = Year, "USDAfarmShrTot" = `Farm value share of expenditures`,
         "YifarmShrTot" = `New farm share series`,
  ) %>%
  select(year, USDAfarmShrTot, YifarmShrTot) %>%
  filter(!is.na(YifarmShrTot))
yi3$USDAfarmShrTot[is.na(yi3$USDAfarmShrTot)]  <- 0

yi3 <- yi3 %>%
  mutate(year = as.numeric(year),
         USDAfarmShrTot = as.numeric(USDAfarmShrTot),
         YifarmShrTot = YifarmShrTot/100,
         USDAfarmShrTot = USDAfarmShrTot/100)

yi3[which(yi3$USDAfarmShrTot==0),"USDAfarmShrTot"] <- NA


mkYi3 <- filter(magExpMeanK, iso3c == "USA") %>%
  select(year, farmShrTot) %>%
  as.magpie() %>%
  time_interpolate(interpolated_year = c(1990:2020), integrate_interpolated_years = TRUE) %>%
  as.data.frame(rev = 2) %>%
  pivot_wider(names_from = "data", values_from = ".value")

compyi3 <- mkYi3 %>%
  right_join(yi3) %>%
  pivot_longer(cols = c(farmShrTot, USDAfarmShrTot, YifarmShrTot),
               names_to = "source", values_to = "farmShr")

ggplot(compyi3, aes(x = year, y = farmShr, colour = source)) +
  geom_line() +
  #ylim(c(0.10, 0.30)) +
  theme_bw(base_size = 20) +
  ggtitle("Farm share of US food expenditures  \n based on MAgPIE prices")



