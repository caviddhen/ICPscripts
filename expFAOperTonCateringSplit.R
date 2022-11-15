library(readxlsb)
library(tidyverse)
library(magpie4)
library(mrland)
library(GDPuc)

  setwd("C:/PIK/ICPdata")
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
    mutate(expPPP = (LCU/pppEX)/1e9, expMER = (LCU/merEX)/1e9)

  #in billions


  expProdAgg <- filter(exp, ClassificationCode == "e_Class",
                       !str_detect(BHName, "Spirits|Wine|Beer|Tobacco|Narcotics|Coffee|Mineral|cheese and eggs")) %>%
    group_by(BHName, iso3c) %>%
    select(iso3c, BHName, expPPP, expMER) %>%
    mutate(year = 2017) %>%
    select(iso3c, BHName, expMER, year) #only MER for now

  #split milk and eggs for better matching with FAO where countries only report one or the other

expMilk <- filter(exp, BHName %in% c("Fresh milk","Preserved milk and other milk products",
                                       "Cheese and curd")) %>%
          group_by(iso3c) %>%
        summarise(expMER = sum(expMER, na.rm=T)) %>%
       mutate(year = 2017, BHName = "Milk products")


expEggs <- filter(exp, BHName =="Eggs and egg-based products" ) %>%
  group_by(iso3c) %>%
  summarise(expMER = sum(expMER, na.rm=T)) %>%
  mutate(year = 2017, BHName = "Eggs")

expProdAgg <- rbind(expProdAgg, expMilk, expEggs)

############# ADD 2011 values###################
################################

### get ratio of resto to hotel
hrat <- filter(exp, BHName %in% c("RESTAURANTS AND HOTELS", "CATERING SERVICES")) %>%
  select(BHName, iso3c, expMER) %>%
  pivot_wider(names_from = BHName, values_from = expMER) %>%
  mutate(hrat = `CATERING SERVICES`/`RESTAURANTS AND HOTELS`)

eggrat <- filter(exp, BHName %in% c("Milk, cheese and eggs", "Eggs and egg-based products")) %>%
  select(BHName, iso3c, expMER) %>%
  pivot_wider(names_from = BHName, values_from = expMER) %>%
  mutate(eggrat  = `Eggs and egg-based products`/`Milk, cheese and eggs`)



exp2 <- read.csv("c4391ee3-4ebc-4ab0-a94b-bd47c059040e_Data.csv")

exp21 <- exp2 %>%
  filter(!is.na(Series.Code)) %>%
  select(Country.Code, Series.Name, X2011..YR2011.) %>%
  rename("iso3c" = Country.Code, "expMER" = X2011..YR2011.) %>%
  separate(Series.Name, into = c("BHCode", "BHName"), sep = ":") %>%
  select(!BHCode) %>%
  filter(BHName %in% c( "Bread and cereals", "Meat", "Fish and seafood", "Milk, cheese and eggs",
                        "Oils and fats", "Fruit", "Vegetables", "Sugar, jam, honey, chocolate and confectionery",
                        "Food products n.e.c. (Class)", "RESTAURANTS AND HOTELS")) %>%
  filter(expMER != "..") %>%
  mutate(expMER = as.numeric(expMER))



cater <- filter(exp21, BHName == "RESTAURANTS AND HOTELS") %>%
  inner_join(hrat) %>%
  mutate("Catering services (Class)" = expMER * hrat) %>%
  select(iso3c,"Catering services (Class)")  %>%
  mutate(BHName = "Catering services (Class)") %>%
  rename("expMER" = "Catering services (Class)") %>%
  filter(!is.na(expMER))

eggMilk <- filter(exp21, BHName == "Milk, cheese and eggs") %>%
  inner_join(eggrat) %>%
  mutate( Eggs = expMER * eggrat,
          "Milk products" = expMER * (1-eggrat)) %>%
  select(iso3c,"Eggs", "Milk products")  %>%
  pivot_longer(cols = c("Eggs", "Milk products"), names_to = "BHName", values_to = "expMER") %>%
  filter(!is.na(expMER))

exp21 <- rbind(exp21, cater) %>%
  filter(BHName != "RESTAURANTS AND HOTELS") %>%
  mutate(year = 2011)

############################
################


  exp21 <- exp21  %>%
    rename("value" = expMER) %>%
    GDPuc::convertGDP(unit_in = "constant 2011 US$MER",
                      unit_out = "constant 2017 US$MER" ) %>%
    rename("expMER" = value)

  expProdAgg <- rbind(expProdAgg, exp21)

  ##########

  redistribute <- "nec"

  if (redistribute %in% c("both", "nec", "catering")){

    if(redistribute == "both"){
      redist <- c("Food products n.e.c. (Class)", "Catering services (Class)")} else if (redistribute == "nec") {
        redist <- "Food products n.e.c. (Class)" } else if(redistribute == "catering"){
          redist <-  "Catering services (Class)" }

    #############
    ### add food n.e.c.,  sugar, and catering, proportionally based on expenditure to other categories
    ##############

    expRatios <- expProdAgg %>%
      filter(!BHName %in%  c("Food products n.e.c. (Class)", "Catering services (Class)")) %>%
      group_by(iso3c, year) %>%
      summarise( sumMER = sum(expMER)) %>%
      #sumPPP = sum(expPPP)) %>%
      inner_join(expProdAgg) %>%
      filter(!BHName %in%  c("Food products n.e.c. (Class)", "Catering services (Class)")) %>%
      mutate(ratioMER = expMER/sumMER) %>%
      #ratioPPP = expPPP/sumPPP,
      select(iso3c, year, BHName, ratioMER) %>%
      #ratioPPP, ratioMER) %>%
      pivot_wider(names_from = BHName, values_from = ratioMER)
    # c(ratioPPP, ratioMER))


    colnames(expRatios)[3:ncol(expRatios)] <- paste0("ratioMER_",colnames(expRatios)[3:ncol(expRatios)] )


    others <- filter(expProdAgg, BHName %in% redist ) %>%
      inner_join(expRatios) %>%
      mutate(across(starts_with("ratioMER"), ~ .x * expMER, .names = "Add_{.col}")) %>%
      #across(starts_with("ratioPPP"), ~ .x * expPPP, .names = "Add_{.col}"),
      select(c(iso3c,year, BHName, starts_with("Add"))) %>%
      group_by(iso3c, year) %>%
      summarise(across(starts_with("Add_"), sum, na.rm= T)) %>%
      rename_with(~ str_remove(pattern = "^.*?(_)", .x)) %>%
      pivot_longer(cols = c(3:last_col()), values_to = "exp"  ,
                   names_to = c("curr", "BHName"), names_sep = "_") %>%
      pivot_wider(names_from = "curr", values_from = "exp") %>%
      rename("expMER" = ratioMER)
    # "expPPP" = ratioPPP,


    ### only when redist is set to catering!!!
    # prodCatering <-  rename(others, "Catering" = expMER) %>%
    #   inner_join(expProdAgg) %>%
    #   mutate(prodCat = Catering/(Catering + expMER)) %>%
    #   group_by(iso3c) %>%
    #   summarise(prodCat = mean(prodCat, na.rm=T)) %>%
    #   filter(!is.na(prodCat))
    #
    # save(prodCatering, file = "prodCatering.Rda")

    expProdAggNEC <- bind_rows(expProdAgg, others) %>%
      group_by(iso3c, year, BHName) %>%
      summarise(across(starts_with("exp"), sum, na.rm = TRUE)) %>%
      filter(!BHName %in% c("Food products n.e.c. (Class)",
                            #"Sugar, jam, honey, chocolate and confectionery",
                            "Catering services (Class)")) %>%
      rename("expMERnoCatering" = "expMER")

  }

  redistribute <- "catering"

  if (redistribute %in% c("both", "nec", "catering")){

    if(redistribute == "both"){

      redist <- c("Food products n.e.c. (Class)", "Catering services (Class)")} else if (redistribute == "nec") {
        redist <- "Food products n.e.c. (Class)" } else if(redistribute == "catering"){
          redist <-  "Catering services (Class)" }


    #############
    ### add food n.e.c.,  sugar, and catering, proportionally based on expenditure to other categories
    ##############

    expRatios <- expProdAgg %>%
      filter(!BHName %in%  c("Food products n.e.c. (Class)", "Catering services (Class)")) %>%
      group_by(iso3c, year) %>%
      summarise( sumMER = sum(expMER)) %>%
      #sumPPP = sum(expPPP)) %>%
      inner_join(expProdAggNEC) %>%
      filter(!BHName %in%  c("Food products n.e.c. (Class)", "Catering services (Class)")) %>%
      mutate(ratioMER = expMERnoCatering/sumMER) %>%
      #ratioPPP = expPPP/sumPPP,
      select(iso3c, year, BHName, ratioMER) %>%
      #ratioPPP, ratioMER) %>%
      pivot_wider(names_from = BHName, values_from = ratioMER)
    # c(ratioPPP, ratioMER))
    colnames(expRatios)[3:ncol(expRatios)] <- paste0("ratioMER_",colnames(expRatios)[3:ncol(expRatios)] )


    others <- filter(expProdAgg, BHName %in% redist ) %>%
      inner_join(expRatios) %>%
      mutate(across(starts_with("ratioMER"), ~ .x * expMER, .names = "Add_{.col}")) %>%
      #across(starts_with("ratioPPP"), ~ .x * expPPP, .names = "Add_{.col}"),
      select(c(iso3c,year, BHName, starts_with("Add"))) %>%
      group_by(iso3c, year) %>%
      summarise(across(starts_with("Add_"), sum, na.rm= T)) %>%
      rename_with(~ str_remove(pattern = "^.*?(_)", .x)) %>%
      pivot_longer(cols = c(3:last_col()), values_to = "exp"  ,
                   names_to = c("curr", "BHName"), names_sep = "_") %>%
      pivot_wider(names_from = "curr", values_from = "exp") %>%
      rename("expMER" = ratioMER)
    # "expPPP" = ratioPPP,


    ### only when redist is set to catering!!!
    prodCatering <-  rename(others, "Catering" = expMER) %>%
      inner_join(expProdAgg) %>%
      mutate(prodCat = Catering/(Catering + expMER)) %>%
      group_by(iso3c) %>%
      summarise(prodCat = mean(prodCat, na.rm=T)) %>%
      filter(!is.na(prodCat))

    save(prodCatering, file = "prodCatering.Rda")


    expProdAggCatering <- bind_rows(expProdAgg, others) %>%
      group_by(iso3c, year, BHName) %>%
      summarise(across(starts_with("exp"), sum, na.rm = TRUE)) %>%
      filter(!BHName %in% c("Food products n.e.c. (Class)",
                            #"Sugar, jam, honey, chocolate and confectionery",
                            "Catering services (Class)")) %>%
      rename("expMERwCatering" = "expMER")

  }

  expProdAgg <- inner_join(expProdAggNEC, expProdAggCatering)

  #############
  ## end of redistribtuion
  #############

   load("gdppc_const2017MER.Rda")
  gdppc_iso <- as.data.frame(gdppc) %>%
    filter(Year %in% c(2011,2017)) %>%
    rename("gdp" = Value, "iso3c" = Region, "year" = Year) %>%
    select(iso3c, year, gdp) %>%
    mutate(year = as.numeric(as.character(year)))



  load("pop.Rda")
  pop <- rename(pop, "iso3c" = Region, "pop" = Value) %>%
    select(iso3c, pop)



  #merge sugars and fats
  expProdAggf <- filter(expProdAgg, BHName %in% c( "Sugar, jam, honey, chocolate and confectionery", "Oils and fats")) %>%
    group_by(iso3c, year) %>%
    summarise(across(c(expMERnoCatering,expMERwCatering), sum)) %>%  #expPPP
    mutate(BHName = "Processed")

  expProdAgg <- rbind(expProdAgg, expProdAggf) %>%
    filter(!BHName %in% c( "Sugar, jam, honey, chocolate and confectionery", "Oils and fats"),
           expMERnoCatering !=0 )



  ###################check against FAOmassbalance readSource prices and quantitites###############
  ### NOW WITH 2 YEARS#################
  ##############################
  load("C:/PIK/ICPdata/expLECconConv_newProc_caterSplit.Rds") #already currecny converted

  ###new
  expFAO <- mutate(expLEC, value = value/1e3,
                          FAOexpAFH = FAOexpAFH/1e3,
                           FAOexpAH = FAOexpAH/1e3) %>%  #in billions
    rename( "BHName" = prod, "FAOexpMER" = value)


  ##reconvert China's rate to 1.23 (USA)
  #expFAO1 <- expFAO
  #expFAO1[which(expFAO1$iso3c == "CHN"), "FAOexpMER"] <- expFAO1[which(expFAO1$iso3c == "CHN"), "FAOexpMER"]/1.83*1.23


  #### run this FOR WITH CATERING DIVIDE BY TOTAL Mt Consumption and total expenditure

  FAOmarkup <- inner_join(expFAO, expProdAgg) %>%
    mutate(FAOmarkupNoCater = (expMERnoCatering - FAOexpAH),
           FAOmarkupwCater = (expMERwCatering - FAOexpMER)) %>%
    filter(!(is.na(FAOmarkupNoCater) | is.infinite(FAOmarkupNoCater) | FAOexpMER ==0),
           !(is.na(FAOmarkupwCater) | is.infinite(FAOmarkupwCater) | FAOexpMER ==0)) ### remove where 0 expenditure

    load("consWnewFB.Rds")

  consW <- as.data.frame(consW, rev = 2) %>%
    rename("iso3c" = Area, "BHName" = prod, "year" = Year, "Consumption(Mt)" = ".value") %>%
    mutate("Consumption(Mt)" = `Consumption(Mt)`/1e6) %>%
    inner_join(gdppc_iso) %>%
    mutate(AFHshr =   (4.540e-06*gdp + 6.611e-02),
           AFHshr = case_when(AFHshr >= 0.85 ~ 0.85,
                              AFHshr < 0.85 ~ AFHshr),
           consAFH = `Consumption(Mt)` * AFHshr,
           consAH = `Consumption(Mt)`*(1-AFHshr)) %>% #add regressFAFH coefficients
    select(iso3c, year, BHName, `Consumption(Mt)`, AFHshr, consAFH, consAH)

  #replace korea for which we have information with its FAFH share (the others are not recent enough and also clse to regression)
   source("regressFAFH.R")
  consW[which(consW$iso3c == "KOR"),"consAFH"] <-  consW[which(consW$iso3c == "KOR"), "Consumption(Mt)"] *
                                                   catShr[which(catShr$country =="Korea"), "kcal_shr"]
  consW[which(consW$iso3c == "KOR"),"consAH"] <-  consW[which(consW$iso3c == "KOR"), "Consumption(Mt)"] *
   (1 - catShr[which(catShr$country =="Korea"), "kcal_shr"])




   FAOmarkup1 <- inner_join(FAOmarkup, consW) %>%
    mutate(FAOmarkupNoCater_perT = ((FAOmarkupNoCater/consAH))*1000,
           FAOmarkupwCater_perT = ((FAOmarkupwCater/`Consumption(Mt)`))*1000) %>%  #markup in billions, cons in milions
    inner_join(pop) %>%
    mutate(cons_pc = `Consumption(Mt)` /pop *1000) %>%  #kilograms consumption per capita
    filter(cons_pc > 3) #filter out less than 3 kg per year eating

### see if any expenditures are very low on ICP side
  FAOmarkup2 <- FAOmarkup1 %>% mutate(icpExpPc = expMERwCatering/pop * 1000,
                             ExpPerKG = icpExpPc/cons_pc,
                           incomeG =  case_when(
                                gdp <= 1006 ~ "LIC",
                                gdp > 1006 & gdp <= 3956 ~ "LMIC",
                                gdp > 3956 & gdp <= 12235 ~ "UMIC",
                                gdp > 12235 ~ "HIC"))  %>%

    arrange(ExpPerKG) %>%
    group_by(incomeG, BHName)

  checkAvg <- FAOmarkup2 %>%
    group_by(incomeG,BHName) %>%
    summarise(avg = mean(ExpPerKG))

  rm_20th <- checkAvg %>% mutate(rm= avg*0.1)

FAOmarkup3 <- inner_join(FAOmarkup2,rm_20th) %>%
              mutate(FAOmarkupNoCater_perT  =
                       case_when(ExpPerKG > rm ~ FAOmarkupNoCater_perT),
                     FAOmarkupwCater_perT  =
                       case_when(ExpPerKG > rm ~ FAOmarkupwCater_perT )) %>%
            filter(!is.na(FAOmarkupNoCater_perT),!is.na(FAOmarkupwCater_perT))


  ggplot(FAOmarkup3, aes(x=log(gdp,base=10), y = FAOmarkupNoCater_perT)) +
    geom_point(aes(size = pop))+
    #stat_smooth(method =  "lm") +
    facet_wrap(~BHName, scales = "free") +
    geom_line(y = 0) +
    ggrepel::geom_text_repel(aes(label = iso3c), max.overlaps = 15)  # facet_wrap(~Bhagg, scales = "free")


  ggplot(FAOmarkup3, aes(x= log(gdp, base=10), y = FAOmarkupwCater_perT)) +
    geom_point(aes(size = pop))+
    #stat_smooth(method =  "lm") +
    facet_wrap(~BHName, scales = "free") +
    geom_line(y = 0) +
    ggrepel::geom_text_repel(aes(label = iso3c), max.overlaps = 15)  # facet_wrap(~Bhagg, scales = "free")

  FAOmarkuppT <- FAOmarkup3
  #save(FAOmarkuppT, file = "./FAOmarkup_perTon_bugfix.Rds")
  #save(FAOmarkuppT, file = "./FAOmarkup_perTon_bugfix_noCater.Rds")
  save(FAOmarkuppT, file = "./FAOmarkup_perTon_DBU_update.Rds")


load("regPreds.Rda")

regPredsNoC <- mutate(regPredsNoCater,
                    label = case_when(
                      pop > 100 ~ iso3c),
                   BHName = factor(BHName, levels = c("Bread and cereals", "Fruit", "Vegetables",
                                                      "Meat",
                                                     "Milk products", "Eggs", "Processed")))

ggplot(regPredsNoC, aes(y=pred, x= loggdp)) +
  geom_point(aes(y=FAOmarkup_perT, x= loggdp, size = pop)) +
  geom_line(aes(size = 2), color = "darkgreen") +
  ggtitle("Consumer Price Markups At-Home") +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Markup (USD$05 / tWM)") + xlab("log10(GDPpc)")+
  facet_wrap(~ BHName) +
  ggrepel::geom_text_repel(aes(label = label), max.overlaps = 30) +
  theme_bw(base_size = 18)# facet_wrap(~Bhagg, scales = "free")
#label larger countries only

regPredswC <- mutate(regPredswCater,
                      label = case_when(
                        pop > 100 ~ iso3c),
                      BHName = factor(BHName, levels = c("Bread and cereals", "Fruit", "Vegetables",
                                                         "Meat",
                                                         "Milk products", "Eggs", "Processed")))
ggplot(regPredswC, aes(y=pred, x= loggdp)) +
  geom_point(aes(y=FAOmarkup_perT, x= loggdp, size = pop)) +
  geom_line(aes(size = 2), color = "darkgreen") +
  ggtitle("Consumer Price Markups Away-from-Home") +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Markup (USD$05 / tWM)") + xlab("log10(GDPpc)")+
  facet_wrap(~ BHName) +
  ggrepel::geom_text_repel(aes(label = label), max.overlaps = 30) +
  theme_bw(base_size = 18)# facet_wrap(~Bhagg, scales = "free")
#label larger countries only






  ######CHINA special

  filter(FAOmarkup, iso3c == "CHN")

  FAOmarkup[which(FAOmarkup$FAOmarkup == 0),]

  FAOmarkup[which(FAOmarkup$FAOmarkup < 0),] %>%
    mutate(FAOmarkup = round(FAOmarkup, 2)) %>% print(n=50)

  weightProd <- as.data.frame(prod) %>%
    filter(Year == 2010) %>%
    select(Region, Value, Data1) %>%
    rename("iso3c" = Region, "prod" = Value, "k" = Data1)

  pricesFAO1 <- as.data.frame(pricesFAO, rev = 2) %>%
    mutate(year = as.numeric(as.character(Year))) %>%
    filter(Year== 2010) %>%
    rename("iso3c" = ISO, "prices" = .value, "k" = ItemCodeItem) %>%
    inner_join(kBHmap) %>%
    inner_join(weightProd) %>%
    group_by(iso3c, BHName) %>%
    summarise(FAOprice = weighted.mean(prices, w = prod, na.rm = T))


  FAOmarkup2 <- inner_join(FAOmarkup1, pricesFAO1) %>%
    mutate(implICP = FAOprice * FAOmarkup)


  ggplot(filter(FAOmarkup2, implICP < 20000), aes(x= FAOprice, y = implICP)) +
    geom_point()+
    # stat_smooth(method =  "lm") +
    facet_wrap(~BHName, scales = "free") +
    geom_abline(intercept = 0, slope = 1)+
    ggrepel::geom_text_repel(aes(label = iso3c))  # facet_wrap(~Bhagg, scales = "free")



  FAOmarkup1 <- filter(FAOmarkup, FAOmarkup < 9)
  hist(FAOmarkup1$FAOmarkup)
  summary(FAOmarkup$FAOmarkup)
  FAOmarkup[which(FAOmarkup[,"FAOmarkup"]  < 1),] %>% print(n=100)
  FAOmarkup[which(FAOmarkup[,"FAOmarkup"] > 8),] %>% print(n=100)



  ### CHINA SPECIAL ###

  ICPC<-expProdAgg %>% filter(iso3c == "CHN") %>%
    select(BHName, expMER) %>%
    rename("Expenditure (Bn USD) ICP" = expMER)

  load("./consCavg.Rds")
  comp <- rename(consCavg, "BHName" = ItemCodeItem) %>%
    inner_join(ICPC) %>%
    mutate(diff = `Expenditure (Bn USD) ICP` -  `Expenditure (Bn USD) FAO` ) %>%
    relocate(diff, .after = BHName) %>%
    relocate( `Expenditure (Bn USD) ICP`, .after = diff )

  write.csv(comp, file = "./ICP_EXP_w_comparison.csv")
