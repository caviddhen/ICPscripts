library(tidyverse)
library(magpie4)
library(mrcommons)
library(magpiesets)
setwd("C:/PIK/ICPdata/")

mag <- "./BAUfulldata.gdx"
pol <- "./POLfulldata.gdx"

kBH <- read.csv("mapMAgPIELEM.csv") %>%
  rename("BHName" = prod)

h12 <- toolGetMapping("h12.csv", type = "regional") %>%
  rename("iso3c" = "CountryCode", "i" = "RegionCode") %>%
  select(iso3c, i)

t <- prices(mag, type = "consumer")
t["USA",c(2010,2015,2020),]


attr <- calcOutput("Attributes", aggregate = F)[,,"wm"] #convert markup to dm for magpie
wm <- attr   %>% as.data.frame(rev = 2)  %>%
  rename("k" = "products", "wm" = ".value")  %>%
  select(k, wm)


prprmag <- prices(mag, type = "consumer")
prprmag <- add_dimension(prprmag, dim = 3.2, nm = "BAU")
prprPOL <- prices(pol, type = "consumer")
prprPOL <- add_dimension(prprPOL, dim = 3.2, nm = "POL")

prpr <- mbind(prprmag, prprPOL)

prpr <- (prpr / attr[,,"wm"])

prpr <- time_interpolate(prpr, interpolated_year = c(2010:2017), integrate_interpolated_years = TRUE)

prpr <- add_columns(prpr, addnm = "Vegetables", dim = 3.1, fill = NA)
prpr[,,"Vegetables"] <- prpr[,,"others"]


prpr <-     prpr %>%
  collapseNames() %>%
    as.data.frame(rev = 2) %>%
  rename("year" = t, "k" = kcr, "scen" = new, "value" = ".value") %>%
  inner_join(h12)   %>%
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
  inner_join(coefsCater)

magCoefsNoCater <- mappingLEM  %>%
  inner_join(coefsNoCater)

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
  select(i, year, k, scen, value, iso3c, markupCater, CaterPrice) %>%
  inner_join(select(markupPrNoCater, i, year, k, scen, value, iso3c, markupNoCater, noCaterPrice)) %>%
  rename("prodPrice" = value)

diff <- markups %>%
  mutate(comp = CaterPrice - noCaterPrice)

markups <-  markups %>%
  pivot_longer(cols = c(prodPrice, noCaterPrice,CaterPrice),
               names_to = "Price Type", values_to = "Price") %>%
  select(i, iso3c, year, k, scen,  `Price Type`, Price)


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
prods <- c("tece", "maiz","rice_pro",  "livst_rum",
        "Vegetables", "livst_pig", "livst_chick")
iso <- "CHN"

## BAU plot for all 3 prices
ggplot(filter(markups, iso3c == iso,
              k %in% prods ,
              scen %in% c("BAU")),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~k, scales = "free") +
  ggtitle(paste(iso, " BAU")) +
  scale_color_manual(labels = c("Prod Price", "Consumer Price FAH", "Consumer Price FAFH" ),
                    values = c( "#54D598", "#1E5B3E", "#348C62"),
                      guide = guide_legend(reverse = TRUE) ) +
  theme_bw(base_size = 18)


## POL plot for all 3 prices
ggplot(filter(markups, iso3c == iso,
              k %in% prods,
              scen %in% c("POL")),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~k, scales = "free") +
  ggtitle(paste(iso, " POL")) +
  scale_color_manual(labels = c("Prod Price", "Consumer Price FAH", "Consumer Price FAFH" ),
                     values = c( "#54D598", "#1E5B3E", "#348C62"),
                     guide = guide_legend(reverse = TRUE) ) +
  theme_bw(base_size = 18)

### plots comparing BAU and POL
ggplot(filter(markups, iso3c == iso,
              k %in% prods,
              scen %in% c("BAU", "POL"),
              `Price Type` == "prodPrice"),
       aes(x = year, y = Price, color = scen))+
  geom_line(lwd = 1.4)+
  facet_wrap(~k, scales = "free") +
  ggtitle(paste(iso, " Producer Prices")) +
  scale_color_manual(labels = c("BAU", "POL"),
                     values = c( "#5AD2D8", "#995AD8"),
                     guide = guide_legend(reverse = TRUE) ) +
  theme_bw(base_size = 18)
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

kfo <- c(findset("kfo"), "Vegetables")
cons <- demand(mag)
cons <- gdxAggregate(gdx = mag, cons, weight = 'Intake', to = "iso")[,,"food"]
cons <- add_columns(collapseNames(cons), addnm = "Vegetables")
#load input vegetable data to get a the latest split
load("consKmag.Rda")
VegShr <- consKmag[,,"Vegetables"]/collapseNames((consKmag[,,"others"]+consKmag[,,"Vegetables"]))
VegShr <- toolCountryFill(VegShr, fill = 0.48) #mean value
othShr <- (1-VegShr)
cons[,,"Vegetables"] <- setYears(VegShr[,2019,], NULL) * setNames(cons[,,"others"],NULL)
cons[,,"others"] <- setYears(othShr[,2019,], NULL) * setNames(cons[,,"others"],NULL)

cons <- cons[,,kfo] %>%
  as.data.frame(rev=2)  %>%
  rename("foodD" = ".value", "iso3c" = "i",
         "k" = "kall", "year" = "t")

magExp <- inner_join(cons,
                     select(markups, !PriceIndex))  %>%
  inner_join(gdppc) %>%
  mutate(AFHshr =  (4.540e-06*gdppc + 6.611e-02)) %>%      ### from kcal_fafh
  pivot_wider(names_from = `Price Type`,
              values_from = Price) %>%
  mutate(fahExp = foodD * (1-AFHshr) * (noCaterPrice),
         fafhExp = foodD * AFHshr * (CaterPrice),
         farmAHexp = foodD *(1-AFHshr) * prodPrice,
         farmAFHexp = foodD *AFHshr * prodPrice,
         farmAHshr = farmAHexp/fahExp,
         farmAFHshr = farmAFHexp/fafhExp)%>%
  select(iso3c, year, k, foodD, gdppc, fahExp, fafhExp, farmAHexp, farmAFHexp, farmAHshr, farmAFHshr)


magExpMeanK <- magExp %>%
  group_by(iso3c, year) %>%
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


magExpMeanKvalid <- magExpMeanK %>%
  filter(iso3c %in% c("CHN", "USA", "IND", "BRA")) %>%
  relocate(farmAHexp, .after = fahExp) %>%
  relocate(farmShrAH, .after = farmAHexp)  %>%
  relocate(farmAFHexp, .after = fafhExp) %>%
  relocate(farmShrAFH, .after = farmAFHexp) %>%
  select(-totalFoodExp)

magExpMeanKvalid[c(3:ncol(magExpMeanKvalid))] <- round(magExpMeanKvalid[c(3:ncol(magExpMeanKvalid))], digits = 3)




yi4 <- readxl::read_xlsx("YiSourceFig4.xlsx", skip =1) %>%
  pivot_longer(cols = c(2:last_col()), names_to = "year", values_to = "YifarmAHshr") %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year)) %>%
  group_by(Country, year) %>%
  summarise(YifarmAHshr = mean(YifarmAHshr, na.rm =T)) %>%
  ungroup()
yi4$iso3c <- toolCountry2isocode(yi4$Country, mapping = list(c("Korea, Rep." = "KOR")) )

compyi4 <-  select(magExpMeanK, iso3c, year, farmShrAH) %>%
  inner_join( select(yi4, iso3c, year, YifarmAHshr)) %>%
  pivot_longer(cols = c(farmShrAH, YifarmAHshr),
               names_to = "source", values_to = "farmAHShr")

ggplot(compyi4, aes(x = year, y = farmAHShr, colour = source)) +
  geom_line()+
  facet_wrap(~iso3c) +
  #  ylim(c(0.15, 0.45)) +
  theme_bw(base_size = 20) +
  ggtitle("Farm Share of At Home Food Expenditures")


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
  ggtitle("Farm share of US food expenditures")



