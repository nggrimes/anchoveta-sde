#join files

# erase
rm(list = ls(all = TRUE)) 

# packages
library(tidyverse)
library(here)


# read 

north_df <- read_csv(here("data_gmm", "north_data.csv")) %>% 
  mutate(zone="CN")

south_df <- read_csv(here("data_gmm", "south_data.csv")) %>% 
  mutate(zone="S")

dataset_df <- bind_rows(north_df, south_df)

# write
write.table(na.omit(dataset_df),here("data_gmm", "final_data.csv"),
            row.names=FALSE, sep=",")