library(tidyverse)
library(lubridate)

#data clean  and organize R
#NFIP claims load
claims_raw <- read_csv("data_raw/ca_nfip_claims.csv", 
col_types = cols(countyCode = col_character()))

#oni el nino index v6 load
oni_raw <- read_csv("data_raw/oni.csv")

#FRED monthly cpi load
cpi_raw <- read_csv("data_raw/monthly_cpi.csv")

#check all
dim(claims_raw)
glimpse(claims_raw)
glimpse(oni_raw)
glimpse(cpi_raw)
head(cpi_raw)
tail(cpi_raw)
head(oni_raw)
tail(oni_raw)


#cleaning the data and keeping only the columns we want

claims <- claims_raw %>%
  select(dateOfLoss, countyCode, netBuildingPaymentAmount, netContentsPaymentAmount, policyCount) %>%
  mutate(dateOfLoss = as_date(dateOfLoss), 
         netBuildingPaymentAmount = replace_na(netBuildingPaymentAmount, 0), 
         netContentsPaymentAmount = replace_na(netContentsPaymentAmount, 0), 
                       total_paid = netBuildingPaymentAmount + netContentsPaymentAmount) %>%
filter(!is.na(countyCode))
#as_date simplifies date
#replace_na will return a blank payment as 0
#filtered out claims without any county assigned


#review total_paid
dim(claims)
summary(claims$total_paid)
sum(claims$total_paid) / 1e6

#add the winter year from oct-sep so we define the winter period
claims <- claims %>%
  mutate(winter_year = if_else(month(dateOfLoss) >= 10, year(dateOfLoss) + 1, year(dateOfLoss)))

#review where the winters cut off
claims %>% count(winter_year) %>% head(3)
claims %>% count(winter_year) %>% tail(3)

#lets do only complete winter years since 1978 is missing and 2026 is immature
claims <- claims %>%
  filter(winter_year >= 1979, winter_year <= 2025)

#quick check
dim(claims)
n_distinct(claims$countyCode)

#adjust for inflation (trend losses)
#annual avg cpi
#annual average CPI
cpi <- cpi_raw %>%
  rename(date = 1, cpi = 2) %>%
  mutate(date = as_date(date), cpi = suppressWarnings(as.numeric(cpi))) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(cpi = mean(cpi, na.rm = TRUE))
#rename by position, organize

base_cpi <- cpi %>%
  filter(year == 2025) %>%
pull(cpi)

#trend factor = 2025 cpi divided by cpi in the year of the loss:
claims <- claims %>%
   mutate(loss_year = year(dateOfLoss)) %>%
  left_join(cpi, by = c("loss_year" = "year")) %>%
mutate(trend_factor = base_cpi / cpi,total_paid_2025 = total_paid * trend_factor)

#sum should be 0
#sum should be total in millions
sum(is.na(claims$cpi))
sum(claims$total_paid_2025) / 1e6

#biggest winter claims in 2025 dollars (adjusted)
claims %>%
  group_by(winter_year) %>%
  summarise(n_claims = n(), paid_millions_2025 = sum(total_paid_2025) / 1e6) %>%
              arrange(desc(paid_millions_2025))

#el nino phase for each winter

#keep only rows where year is 4 digits
#remove repeated header rows, convert all to numeric, keep only rows where year is 4 digits
oni <- oni_raw %>%
  filter(str_detect(Year, "^\\d{4}$")) %>%
  mutate(across(everything(), as.numeric))

nrow(oni)

#djf = dec, jan, feb, where the djf value in the 1998 row covers dec 1997-feb 1998, lining up with winter_year 1998
#*la nina is the opposite of el nino
oni_winter <- oni %>%
  select(winter_year = Year, oni_djf = DJF) %>%
  filter(winter_year >= 1979, winter_year <= 2025) %>%
  mutate(enso_phase = case_when(oni_djf >=  1.5 ~ "Strong El Niño",
                                oni_djf >=  0.5 ~ "Weak/Moderate El Niño",
                                oni_djf <= -0.5 ~ "La Niña", TRUE ~ "Neutral"))

#strong el nino winters in the study period
oni_winter %>% filter(enso_phase == "Strong El Niño")
#number of winters in each phase
oni_winter %>% count(enso_phase)

#save cleaned data
dir.create("data_clean", showWarnings = FALSE)
write_csv(claims,     "data_clean/claims_clean.csv")
write_csv(oni_winter, "data_clean/oni_winter.csv")





