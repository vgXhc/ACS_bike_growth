library(acs)
library(tidycensus)
library(tidyverse)
library(purrr)

######################
######################
## Data prep        ## 
######################
######################

#define variables for total number commuters and total bike commuters
vars <- c("B08006_001", "B08006_014")


us <- unique(fips_codes$state)[1:51]

# Process all tracts by state
df <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = vars, state = x, output = "wide")
})

df2010 <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = vars, state = x, year = 2010, output = "wide")
})

df_all <- inner_join(df, df2010, by = ("GEOID"))

bike_growth <- df_all %>%
  mutate(share_2010 = B08006_014E.y / B08006_001E.y, share_2017 = B08006_014E.x / B08006_001E.x, growth = share_2017 - share_2010) %>%
  filter(share_2010 > 0.03, B08006_001E.x > 500, str_detect(NAME.x, 'Dane')) %>%
  select(GEOID, NAME.x, B08006_001E.x, share_2010:growth) %>%
  arrange(desc(growth))

bike_growth <- df_all %>%
  mutate(share_2010 = B08006_014E.y / B08006_001E.y, share_2017 = B08006_014E.x / B08006_001E.x, growth = share_2017 - share_2010) %>%
  filter(share_2010 > 0.03, B08006_001E.x > 500) %>%
  select(GEOID, NAME.x, B08006_001E.x, share_2010:growth) %>%
  arrange(desc(growth))

#calculate bike mode share and sort descending
hi_bike <- df %>%
  filter(B08006_001E > 500) %>%
  mutate(bike_share = B08006_014E/B08006_001E) %>%
  filter(bike_share > 0.1) %>%
  arrange(desc(bike_share))

hi_bike <- df2010 %>%
  filter(B08006_001E > 500) %>%
  mutate(bike_share = B08006_014E/B08006_001E) %>%
  filter(bike_share > 0.1) %>%
  arrange(desc(bike_share))

view(hi_bike)
