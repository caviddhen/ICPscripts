library(tidyverse)
library(magpie4)
library(mrcommons)
setwd("C:/PIK/ICPdata/")

kBH <- read.csv("mapMAgPIELEM.csv") %>%
  rename("BHName" = prod)

h12 <- toolGetMapping("h12.csv", type = "regional") %>%
  rename("iso3c" = "CountryCode", "i" = "RegionCode") %>%
  select(iso3c, i)
#
# prpr <- prices(mag, type = "consumer") %>%
#   time_interpolate(interpolated_year =  c(2010:2017), integrate_interpolated_years = TRUE) %>%
#   as.data.frame(rev = 2) %>%
#   rename("year" = t, "k" = kcr, "value" = ".value") %>%
#   inner_join(h12)   %>%
#   inner_join(kBH) %>%
#   #GDPuc::convertGDP(unit_in = "constant 2005 US$MER",
#    #                 unit_out = "constant 2017 US$MER",
#    #                 replace_NAs = "no_conversion") %>%
#   mutate(value = value * 1.23) %>% #USD inflation rate
#   rename("magPrice" = "value")

#FAOexpenditures_newProc_CaterSplit on cluster

load("FAOpmag.Rda")
FAOp <- FAOpmag
FAOp[FAOp == 0] <- NA

iso <- getItems(FAOp, dim = 1)
items <- getItems(FAOp, dim = 3)
avg <- new.magpie(cells_and_regions = iso,
                  years = NULL,
                  names = items)
for (i in iso ){
  for (t in items){
    avg[i,,t] <- dimSums(FAOp[i,,t], dim = 2, na.rm = T)/length(which(!is.na(FAOp[i,,t])))

    FAOp[i,,t] <- ifelse(is.na(FAOp[i,,t]), avg[i,,t], FAOp[i,,t])

}
}


#then fill with regional average
for (i in getNames(FAOp)){
FAOp[,,i] <- toolFillWithRegionAvg(FAOp[,,i])
}

#then fill with global FAOini price
load("pinit.Rda")
pinit <- add_columns(pinit, addnm = "Vegetables", dim = 3.1, fill = 0)
FAOp <- FAOp[,,"remaining", inv=T]
for (i in getNames(FAOp) ) {
FAOp[,,i] <- ifelse(is.na(FAOp[,,i]), pinit[,,i], FAOp[,,i])
  }

prpr <- FAOp %>%  as.data.frame(rev = 2) %>%
    rename( "iso3c"= ISO, "year" = Year, "k" = ItemCodeItem, "value" = ".value") %>%
    inner_join(h12)   %>%
    inner_join(kBH) %>%
    GDPuc::convertGDP(unit_in = "constant 2005 US$MER",
                     unit_out = "constant 2017 US$MER",
                     replace_NAs = "no_conversion") %>%
    #mutate(value = value * 1.23) %>% #USD inflation rate
    rename("magPrice" = "value")




chn <- data.frame(iso3c = "CHN", year = 2005, "value" = 1) %>%
  GDPuc::convertGDP(unit_in = "constant 2005 US$MER",
                    unit_out = "constant 2017 US$MER" )



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

# gdppc <- calcOutput("GDPpc", aggregate = F)[,,"SSP2"] %>%
#   as.data.frame(rev = 2)  %>%
#   rename("gdppc" = ".value",
#          "iso3c" = region)  %>%
#   select(iso3c, year, gdppc)
load("gdppc_const2017MER.Rda")
gdppc <- gdppc %>%
  as.data.frame(rev = 2)  %>%
  rename("gdppc" = ".value") %>%
  select(iso3c, year, gdppc)


#attr <- calcOutput("Attributes", aggregate = F)[,,"wm"] #convert markup to dm for magpie
#wm <- attr   %>% as.data.frame(rev = 2)  %>%
#  rename("k" = "products", "wm" = ".value")  %>%
#  select(k, wm)


markupPrCater <- inner_join(prpr, gdppc)  %>%
  #rename("k" = kcr)  %>%
  inner_join(magCoefsCater)  %>%
 # inner_join(wm) %>%
  mutate(markupCater = a*(b^log(gdppc, base = 10)),
        # markupCater = markupCater * wm,  #convert to dry matter
         CaterPrice = magPrice + markupCater)


markupPrNoCater <- inner_join(prpr, gdppc)  %>%
  #rename("k" = kcr)  %>%
  inner_join(magCoefsNoCater)  %>%
  #inner_join(wm) %>%
  mutate(markupNoCater = a*(b^log(gdppc, base = 10)),
         #markupNoCater = markupNoCater * wm,  #convert to dry matter
         NoCaterPrice = magPrice + markupNoCater)


markups <- markupPrCater %>%
  select(i, year, k, magPrice, iso3c, markupCater, CaterPrice) %>%
  inner_join(select(markupPrNoCater, i, year, k, magPrice, iso3c, markupNoCater, NoCaterPrice))
diff <- markups %>%
  mutate(comp = CaterPrice - NoCaterPrice)
markups <-  markups %>%
  pivot_longer(cols = c(magPrice, NoCaterPrice,CaterPrice), names_to = "Price Type", values_to = "Price")



# reg <- "EUR"
#
pr <- "others"
iso <- "USA"
ggplot(filter(markups, iso3c == iso, k %in% c("tece", "livst_rum","oils","sugar","Vegetables", "others", "livst_pig", "livst_chick")),
       aes(x = year, y = Price, colour = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~k) +
  ggtitle(paste(iso))



#use FAO consumption


load("consKmag.Rda")

cons <- consKmag %>%
  as.data.frame(rev=2)  %>%
  rename("foodD" = ".value", "iso3c" = "Area",
         "k" = "prod", "year" = "Year")

magExp <- inner_join(cons, markups)  %>%
          inner_join(gdppc) %>%
  mutate(AFHshr =  (4.540e-06*gdppc + 6.611e-02)) %>%      ### from kcal_fafh
  pivot_wider(names_from = `Price Type`,
              values_from = Price) %>%
  mutate(fahExp = foodD * (1-AFHshr) * (NoCaterPrice),
         fafhExp = foodD * AFHshr * (CaterPrice),
         farmAHexp = foodD *(1-AFHshr) * magPrice,
         farmAFHexp = foodD *AFHshr * magPrice,
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
  filter(year == "2015", iso3c %in% c("CHN", "USA", "IND", "BRA")) %>%
  relocate(farmAHexp, .after = fahExp) %>%
  relocate(farmShrAH, .after = farmAHexp)  %>%
  relocate(farmAFHexp, .after = fafhExp) %>%
  relocate(farmShrAFH, .after = farmAFHexp) %>%
  select(-totalFoodExp)

magExpMeanKvalid[c(3:ncol(magExpMeanKvalid))] <- round(magExpMeanKvalid[c(3:ncol(magExpMeanKvalid))], digits = 3)

write.csv(magExpMeanKvalid, file="noweightReg_GDPconv.csv")



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

compyi3 <- filter(magExpMeanK, iso3c == "USA") %>%
  select(year, farmShrTot) %>%
  right_join(yi3) %>%
  pivot_longer(cols = c(farmShrTot, USDAfarmShrTot, YifarmShrTot),
               names_to = "source", values_to = "farmShr")

ggplot(compyi3, aes(x = year, y = farmShr, colour = source)) +
  geom_line() +
 #ylim(c(0.10, 0.30)) +
  theme_bw(base_size = 20) +
  ggtitle("Farm share of US food expenditures")



