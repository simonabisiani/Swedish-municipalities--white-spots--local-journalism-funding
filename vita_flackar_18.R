# libraries
library(readxl)
library(tidyverse)
library(lubridate)
library(fuzzyjoin)

# to load all this already in your environment
load(file = "municipalities,funding,white spots.RData")

#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

#--- Which newsrooms existed in Sweden in 2018? 

newsrooms <- read_csv("download.csv") # import kommundatabasen data from Mediestudier

newsrooms <- newsrooms %>% 
  mutate(sdate = ifelse(as.character(sdate) == "0", "1900-01-01", as.character(sdate)), #to avoid NAs when converting to date, I am putting a random date
         sdate = ymd(sdate, truncated = 3), # convert to date (recognising both obs with only year, ym, or ymd format)
         edate = ifelse(as.character(edate) == "9", "2021-01-01", as.character(edate)), #to avoid NAs when converting to date, I am putting a random date
         edate = ymd(edate, truncated = 3))
# (ignore error of 2 failed to parse, code works)

newsrooms_2018 <- newsrooms %>% 
  filter(sdate <= "2018-01-01", edate >= "2018-12-28")  # %>% subset for all newsrooms which existing during the entirety of 2018
  #group_by(municipality_name) %>% 
  #count() #how many did each municipality have?




#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

#--- Dataset of Swedish municipalities

kommuner <- read_excel("kommuner_codes.xlsx", col_names = c("Kommunkod","name_short", "municipality_name"))

kommuner <- kommuner %>% 
  mutate(municipality_name = gsub(" stad", " kommun", municipality_name))  # change "city" to "municipality" to merge with other dfs


komm2 <- read_excel("Översiktstabell och lista_Kommungruppsindelning 2017 reviderad.xlsx", col_names = TRUE)

kommuner_komplett <- komm2 %>% 
  left_join(kommuner, by = c("Kommunkod"))
  



#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

#--- Who received funding for local journalism? 

#--- In 2019
funding_19 <- read_excel("funding_2019.xlsx", col_names = c("newspaper", "amount", "name_short"))
funding_19 <- funding_19 %>% 
  filter(!is.na(newspaper) & !is.na(name_short)) # cleanup NAs
funding_19$year <- "2019" # add year column      

#--- In 2020
funding_20 <- read_excel("funding_2020.xlsx", col_names = c("newspaper", "amount", "name_short"))
funding_20 <- funding_20 %>% 
  filter(!is.na(newspaper) & !is.na(name_short)) # cleanup NAs
funding_20$year <- "2020" # add year column


#--- Merge and cleanup
funding <- bind_rows(list(funding_19, funding_20), .id = NULL)
funding[8,3] <- "Älvkarleby" 
funding[25,3] <- "Piteå, Älvsbyn, Luleå, Boden, Kalix, Överkalix, Arjeplog, Jokkmokk, Arvidsjaur"
funding[88,3] <- "Krokom"
funding <- funding[-167,]
funding[80,3] <- "Älvkarleby"
funding[150,3] <- "Tensta, Rinkeby"
funding[200,3] <- "Sundsvall"
funding[244,3] <- "Askersund, Laxå"
funding[247,3] <- "Motala"
funding <- funding %>% 
  separate_rows(name_short, sep = ", ") #%>% # where one outlet received funding to cover more than one municipality, split to get one observation for each municipality (careful as money totals won't match anymore)
  #group_by(name_short, year) %>%  #how many municipalities received funding in 2019 and 2020?
  #count()


#--- Which municipalities have received funding for two consecutive years?
fund_19_20 <- funding[funding$name_short %in% names(which(table(funding$name_short) > 1)), ]

fund_19_20 <- fund_19_20 %>% 
  left_join(kommuner_komplett, by = "name_short") # let's complete this with information from the other dataset

  
#--- This is a dataframe that only retains, within the funding, those observations that have a clear municipality targeted (removing for example "Västra Mälardalen")
funding_clear_munic <- funding %>% 
inner_join(kommuner, by = "name_short")

who <- anti_join(funding, kommuner) # which observations am I getting rid of? (helpful to see for example that Kil and Grums - 2 white spots in 2018, actually did receive funding in 2020)





#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

#--- Dataset of white spots in 2018 (municipalities with no newsroom)

white_spots_2018 <- anti_join(kommuner, newsrooms_2018, by = "municipality_name")
white_spots_2018$dummy <- 1

#--- Which white spots did not request any funding in both 2019 and 2020?
white_spots_18_no_fund_both_years <- anti_join(white_spots_2018, fund_19_20, by = "municipality_name") 





#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

#--- Dataframe to see all municipalities, outlets, amount and year of receiving funding

alla_kommuner_funding <- left_join(kommuner, funding_clear_munic, by = "municipality_name") %>% 
  select(-c(Kommunkod.y, name_short.y)) %>% 
  group_by(municipality_name) %>% mutate(id=row_number()) %>%
  pivot_longer(-c(municipality_name,id)) %>%
  mutate(name=paste0(name,'.',id)) %>% select(-id) %>%
  pivot_wider(names_from = name,values_from=value) 


#--- Adding a dummy to see if the municipality was a white spot in 2018 or not
complete_df <- left_join(alla_kommuner_funding, white_spots_2018, by ="municipality_name") %>% 
  select(-name_short) %>% 
  replace_na(list(dummy = 0, y = "0")) %>% 
  relocate(dummy)

#--- A dataset of municipalities which did not get funding, both white spots and not, in 2019 and/or 2020
no_funding_municipalities <- complete_df %>% 
  filter(is.na(newspaper.1))


save.image(file = "municipalities,funding,white spots.RData")
