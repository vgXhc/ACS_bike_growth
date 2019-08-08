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
