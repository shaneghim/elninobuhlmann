library(tidyverse)
library(actuar)
library(maps)
library(ggrepel)

#how much higher should a california flood insurer expect losses to be in a strong el nino winter like 2026–27?
#do counties differ in how they react to el nino, and how much should each county's own el nino history be trusted?

dir.create("output", showWarnings = FALSE)

#load all the cleaned data
claims <- read_csv("data_clean/claims_clean.csv", 
  col_types = cols(countyCode = col_character()))
oni_winter <- read_csv("data_clean/oni_winter.csv",
  col_types = cols(winter_year = col_double(),
                       oni_djf = col_double(),
                    enso_phase = col_character()))

#total claims and losses for each county and winter
county_winter <- claims %>%
  group_by(countyCode, winter_year) %>%
  summarise(n_claims = n(), 
           paid_2025 = sum(total_paid_2025), 
             .groups = "drop")

nrow(county_winter)
#1554 rows or the number that had claims

#add the winters with zero claims, including blanks
#complete() creates every county_winter combination, includes "quiet" winters, and helps averages
county_winter <- county_winter %>%
  complete(countyCode, winter_year = 1979:2025,
           fill = list(n_claims = 0, paid_2025 = 0))

nrow(county_winter)
mean(county_winter$n_claims == 0)
#56 counties x 47 winters, mean comes to ~.41 of county_winter with no claims

#attach el nino phase
#left join here since the result will keep the rows of county_winter
county_winter <- county_winter %>%
  left_join(oni_winter, by = "winter_year")

#confirm every county-winter got its el nino label
sum(is.na(county_winter$enso_phase))

#check it all
sum(county_winter$n_claims) == nrow(claims)
all.equal(sum(county_winter$paid_2025), sum(claims$total_paid_2025))

#statewide average per winter by el nino phase (in 2025 dollars)
county_winter %>%
  group_by(winter_year, enso_phase) %>%
  summarise(state_claims = sum(n_claims), state_paid = sum(paid_2025), .groups = "drop") %>%
  group_by(enso_phase) %>%
  summarise(n_winters = n(), 
           avg_claims = mean(state_claims),
    avg_paid_millions = mean(state_paid) / 1e6,
 median_paid_millions = median(state_paid) / 1e6)

#onto credibility analysis portion!
#add county names 
#FIPS code identifies countys (standard gov ID)
county_names <- maps::county.fips %>%
  filter(str_starts(polyname, "california,")) %>%
  transmute(countyCode = sprintf("%05d", fips), county = str_to_title(str_remove(polyname, "california,"))) %>%
  distinct(countyCode, .keep_all = TRUE)

county_winter <- county_winter %>%
  left_join(county_names, by = "countyCode")

#should be 0
sum(is.na(county_winter$county))

#strong el nino winters defined by the ONI value instead of the text label
#so the code doesn't depend on how the label is spelled
strong_years <- oni_winter %>% filter(oni_djf >= 1.5) %>% pull(winter_year)
strong_years 
#from before: 1983 1992 1998 2010 2016 2024

#statewide totals for each winter
state_winter <- county_winter %>%
  group_by(winter_year, enso_phase, oni_djf) %>%
  summarise(n_claims = sum(n_claims),
              paid_m = sum(paid_2025) / 1e6,
             .groups = "drop") %>%
mutate(enso_phase = factor(enso_phase,
           levels = c("La Niña", "Neutral", "Weak/Moderate El Niño", "Strong El Niño")))
enso_colors <- c("La Niña" = "#4575b4",
                 "Neutral" = "grey70", 
   "Weak/Moderate El Niño" = "#fdae61",
          "Strong El Niño" = "#d73027")

#plot losses by winter colored by el nino phase
lbw <- ggplot(state_winter, aes(winter_year, paid_m, fill = enso_phase)) + geom_col() +
  scale_fill_manual(values = enso_colors) +
  labs(title = "California NFIP flood losses by winter",
       x = "Winter (Oct–Sep, labeled by ending year)",
       y = "Paid losses ($ millions in 2025 dollars)",
       fill = "ENSO phase") + theme_minimal()

ggsave("output/01_losses_by_winter.png", lbw, width = 10, height = 5)
lbw

#plot el nino strength vs. losses, label strong winters
oni_loss <- ggplot(state_winter, aes(oni_djf, paid_m)) + geom_point() +
  ggrepel::geom_text_repel(data = filter(state_winter, winter_year %in% strong_years),
  aes(label = winter_year)) + geom_vline(xintercept = 1.5, linetype = "dashed") +
  labs(title = "El Niño strength vs. flood losses",
           x = "ONI (Dec–Feb): dashed line = strong El Niño cutoff",
           y = "Paid losses ($ millions in 2025 dollars)") + theme_minimal()

ggsave("output/02_oni_vs_losses.png", oni_loss, width = 8, height = 5)
oni_loss

#plot top 10 counties by total losses
top_counties <- county_winter %>%
  group_by(county) %>%
  summarise(total_m = sum(paid_2025) / 1e6) %>%
  slice_max(total_m, n = 10) %>%
  ggplot(aes(reorder(county, total_m), total_m)) + geom_col() + coord_flip() +
  labs(title = "Top 10 counties by flood losses, 1979–2025",
           x = NULL, 
           y = "Total paid ($ millions in 2025 dollars)") + theme_minimal()

ggsave("output/03_top_counties.png", top_counties, width = 8, height = 5)
top_counties

#compare the naive el nino load, first part of question
#how much worse is a strong el nino winter?


#average winter, $ millions
#average strong el nino winter, $ millions
#how many times worse a strong el nino winter is, on average
state_avg_all <- mean(state_winter$paid_m)
state_avg_strong <- mean(state_winter$paid_m[state_winter$winter_year %in% strong_years])
el_nino_load <- state_avg_strong / state_avg_all

#check
state_avg_all
state_avg_strong
el_nino_load
#28.86642, 46.41369, 1.607878

#each strong winter on its own
state_winter %>% filter(winter_year %in% strong_years)


#buhlmann credibility functions
#data = the county_winter table
#col = the column to analyze
#years = the winters to use (strong_years, etc.)
buhlmann <- function(data, col, years) {stats <- data %>%
  filter(winter_year %in% years) %>%
  group_by(county) %>%
  summarise(xbar = mean(.data[[col]]), s2 = var(.data[[col]]), .groups = "drop")

#n = observations per county
#epv = expected process variance, account for noise in counties
#vhm = variance of hypothetical means (differences between counties)
#z = credibility weight
n <- length(years)
epv <- mean(stats$s2)
vhm <- var(stats$xbar) - epv / n
k <- epv / vhm
Z <- if (vhm > 0) n / (n + k) else 0
#if vhm <= 0, no evidence of real differences

list(n = n, epv = epv, vhm = vhm, k = k, Z = Z,
       stats = stats, overall = mean(stats$xbar))}


#second part of question: credibility on el nino relativities
#relativity = a winter's losses / that county's long run average; a relativity of 2 means the winter cost twice a normal year for that count
#dividing by each county's own average removes county size, so counties can be compared on how strongly they react to el nino
county_winter <- county_winter %>%
  group_by(county) %>%
  mutate(county_avg = mean(paid_2025),
      county_claims = sum(n_claims),
         relativity = paid_2025 / county_avg) %>%
  ungroup()

sum(county_winter$county_avg == 0)
#sum = 0 => every county has some losses

#run the buhlmann and save it
cred <- buhlmann(county_winter, "relativity", strong_years)
cred[c("n", "epv", "vhm", "k", "Z")]

#blend each county's own el nino factor with the statewide load
county_factors <- cred$stats %>%
  transmute(county, own_factor  = xbar, cred_factor = cred$Z * xbar + (1 - cred$Z) * el_nino_load)

#counties with the highest and lowest own el nino factors
county_factors %>% arrange(desc(own_factor)) %>% head(5)
county_factors %>% arrange(own_factor) %>% head(5)

#plot each county's own el nino factor vs. the statewide load
#bigger dots = more claims
#small counties scatter the most, which is the noise credibility detects
own <- county_factors %>%
  left_join(county_winter %>% distinct(county, county_claims), by = "county") %>%
  ggplot(aes(own_factor, reorder(county, own_factor))) + geom_point(aes(size = county_claims)) +
  geom_vline(xintercept = el_nino_load, color = "#d73027") + geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "Each county's own El Niño factor",
    subtitle = "Red line = statewide El Niño load; dashed line = no El Niño effect",
           x = "Own El Niño factor (average strong-winter losses / county's normal winter)",
           y = NULL,
        size = "Total claims") + theme_minimal()

ggsave("output/04_county_factors.png", own, width = 8, height = 10)
own

#check with the actuar package: its credibility factor should match cred$Z
#actuar needs one column per winter, so the data is spread out for this check only
wide <- county_winter %>%
  filter(winter_year %in% strong_years) %>%
  select(county, winter_year, relativity) %>%
  pivot_wider(names_from = winter_year, values_from = relativity, names_prefix = "y") %>%
  as.data.frame()

n_cols <- ncol(wide)
fit <- cm(~ county, data = wide, ratios = 2:n_cols)
summary(fit)


#sensitivity tests
#each scenario reruns the same calculation with a different set of winters or counties to see whether the conclusions hold up

oni_years <- function(cutoff) oni_winter %>% filter(oni_djf >= cutoff) %>% pull(winter_year)

#calculate first, while years still means the list of winters

run_scenario <- function(name, years, data = county_winter) {
  load <- mean(state_winter$paid_m[state_winter$winter_year %in% years]) / state_avg_all
     Z <- buhlmann(data, "relativity", years)$Z
  
tibble(scenario = name,
      n_winters = length(years),
     n_counties = n_distinct(data$county),
        winters = paste(years, collapse = ", "),
   el_nino_load = load,
              Z = Z)}

#small counties can produce extreme relativities from a single bad winter
#*this scenario keeps only counties with at least 100 claims over 1979–2025
big_counties <- county_winter %>% filter(county_claims >= 100)

sensitivity <- bind_rows(
  run_scenario("Baseline (ONI >= 1.5)", strong_years),
  run_scenario("Drop 1998", setdiff(strong_years, 1998)),
  run_scenario("Drop 1983 and 1998", setdiff(strong_years, c(1983, 1998))),
  run_scenario("Cutoff ONI >= 1.0", oni_years(1.0)),
  run_scenario("Cutoff ONI >= 2.0", oni_years(2.0)),
  run_scenario("Strong winters since 2000", strong_years[strong_years >= 2000]),
  run_scenario("Counties with 100+ claims", strong_years, data = big_counties))

sensitivity

#plot how the el nino load changes across scenarios
#*the 100+ claims scenario is left out because it doesn't change the statewide load
sense <- sensitivity %>%
  filter(scenario != "Counties with 100+ claims") %>%
  ggplot(aes(el_nino_load, reorder(scenario, el_nino_load))) + geom_col(fill = "#d73027") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "How the El Niño load changes across scenarios",
    subtitle = "Dashed line = no El Niño effect (1.0)",
           x = "El Niño load (strong-winter losses / average winter)",
           y = NULL) + theme_minimal()

ggsave("output/05_sensitivity.png", sense, width = 8, height = 5)
sense

#prediction for winter 2026–27 in 2025 dollars
#expected loss = county's long run average x el nino factor (credibility-weighted)
#*limitation: the long run average covers 1979–2025, but the number of NFIP policies per county
#has changed over time. policy counts are only available from 2009
prediction <- county_winter %>%
  distinct(county, countyCode, county_avg, county_claims) %>%
  left_join(county_factors, by = "county") %>%
  mutate(predicted_2027_m = county_avg * cred_factor / 1e6) %>%
  arrange(desc(predicted_2027_m))

prediction %>% select(county, own_factor, cred_factor, predicted_2027_m) %>% head(10)

sum(prediction$predicted_2027_m)
state_avg_strong
#they match

prediction %>%
  mutate(prediction_date = as.Date("2026-11-01")) %>%
  write_csv("output/prediction_2026_27.csv")

#save it all!
write_csv(county_winter, "data_clean/county_winter.csv")
write_csv(sensitivity, "output/sensitivity.csv")

