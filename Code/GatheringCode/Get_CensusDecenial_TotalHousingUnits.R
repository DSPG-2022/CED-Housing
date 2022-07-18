library(tidycensus)

##Tidycensus Call to get 2020 Census Data for Total Households by County for State of Iowa
House2020 <- get_decennial(
  geography = "county",
  state = "IA",
  variables = "H1_001N",
  year = 2020
)
