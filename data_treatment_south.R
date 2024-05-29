# Data treatment for the southern waters

# erase
rm(list = ls(all = TRUE)) 

# packages
library(tidyverse)
library(here)
library(readxl)
library(sandwich)
library(gmm)
library(janitor)
library(fuzzyjoin)

# read 

# harvest and stock variables
df <- read_csv(here("data", "year_season_zone.csv")) %>% 
  filter(zone=="S") %>% 
  mutate(season_num=season_number) %>% 
  select(year, season_num, sum_landing)

stock <- read_excel(here("data", "per_anchovy_stock_estimate.xlsx")) %>% 
  filter(zone=="S") 

stock_df <- stock %>% 
  select(year, season_num, biomass)

stock_landing <- left_join(df, stock_df, by=c("year", "season_num")) %>% 
  mutate(landing=round(sum_landing/1000,2)) %>% 
  select(-sum_landing)
#######################################################################
# season dates
season <- read_excel(here("data", "season_dates.xlsx")) %>% 
  clean_names() %>% 
  filter(zona=="SOUTH") %>% 
  mutate(fecha_de_termino=excel_numeric_to_date(as.numeric(as.character(fecha_de_termino)), date_system = "modern")) 

season <- season %>%
  mutate(fecha_de_termino=if_else(is.na(fecha_de_termino) & ano == 2013,
                    ymd("2014-06-15"), fecha_de_termino))


#######################################################################
# weather variables
enso <- load(here("data", "enso_pdo.rda"))

nino1_2 <- read_excel(here("data", "nino1_2.xlsx")) %>% 
  gather(key="month", value="nino1_2", 2:13) %>% 
  clean_names()

nino1_2_df <- nino1_2 %>% 
  filter(year>2005) %>% 
  filter(year<2014) %>% 
  mutate(month=recode(month,
                     January = 1,
                     February = 2,
                     March = 3,
                     April = 4,
                     May = 5,
                     June = 6,
                     July = 7,
                     August = 8,
                     September = 9,
                     October = 10,
                     November = 11,
                     December = 12)) %>% 
  mutate(Date=0)

for(i in 1:nrow(nino1_2_df)){
  if(nchar(nino1_2_df$month[i])==1){
    nino1_2_df$Date[i] <- paste(nino1_2_df$year[i],nino1_2_df$month[i],sep="-0")
  }else{
    nino1_2_df$Date[i] <- paste(nino1_2_df$year[i],nino1_2_df$month[i], sep="-")
  }
}

nino1_2_df <- nino1_2_df %>% 
  mutate(Date= paste(Date, "01",  sep="-"))


# find average per season
nino_season <- fuzzy_left_join(season, nino1_2_df ,
                                by = c("fecha_de_inicio" = "Date", "fecha_de_termino" = "Date"),
                                match_fun = list(`<=`, `>=`)) %>%
  group_by(fecha_de_inicio, fecha_de_termino) %>%
  summarise(new_goal_column = mean(nino1_2))

nino_season_names <- left_join(season, nino_season, by=c("fecha_de_inicio", "fecha_de_termino")) %>% 
  select(ano, temporada, new_goal_column) %>% 
  mutate(season_num=case_when(temporada == "First" ~ 1,
                              temporada == "Second" ~ 2)) %>% 
  select(-temporada)

colnames(nino_season_names) <- c("year",  "nino", "season_num")


#######################################################################
# price variables
price <- read_excel(here("data", "fishmeal_price.xlsx")) %>% 
  select(-Change) %>% 
  mutate(year=year(Month),
         month=month(Month), 
         day=day(Month))

price_month <- price %>% 
  group_by(year, month) %>% 
  summarize(price=mean(Price))


#######################################################################
# filter season dates
price_season <- fuzzy_left_join(season, price,
                                by = c("fecha_de_inicio" = "Month", "fecha_de_termino" = "Month"),
                                match_fun = list(`<=`, `>=`)) %>%
  group_by(fecha_de_inicio, fecha_de_termino) %>%
  summarise(new_goal_column = mean(Price))

# add season names
price_season_names <- left_join(season, price_season, by=c("fecha_de_inicio", "fecha_de_termino")) %>% 
  select(ano, temporada, new_goal_column) %>% 
  mutate(season_num=case_when(temporada == "First" ~ 1,
                              temporada == "Second" ~ 2)) %>% 
  select(-temporada)

colnames(price_season_names) <- c("year",  "fishmeal_p", "season_num")

# join
stock_landing_prices <- left_join(stock_landing, price_season_names, by=c("year", "season_num")) %>% 
  filter(year<2014) 

# join nino 1+2

stock_landing_prices_nino <- left_join(stock_landing_prices, nino_season_names, by=c("year", "season_num"))%>% 
  filter(year<2014) %>% 
  drop_na()

# write
write.table(na.omit(stock_landing_prices_nino),here("data_gmm", "south_data"),
            row.names=FALSE, sep=",")
