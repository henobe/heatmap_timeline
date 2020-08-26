# load lbraries ---------------------------------
library("ggplot2")
library("lubridate")
library("magrittr")
library("tibble")
library("dplyr")
library("tidyr")

# set up data -----------------------------------
day_generator <- function(number_days, year_span=3){
  start_date <- today() - years(year_span)
  # generate sequence of random, non ovelapping numbers
  day_offsets <- base::sample(1:(year_span*365), size=number_days, replace = FALSE)
  # add these numbers as number of days to a start date
  start_date + days(day_offsets)
}

base_data <- tibble(
  p1 = day_generator(15),
  p2 = day_generator(15),
  p3 = day_generator(15)
) %>%
  pivot_longer(cols = everything(), names_to = "presenter", values_to="presentation_date") %>%
  mutate_at("presenter", factor, levels = c("p1", "p2", "p3"), labels = c("Peter", "Günther", "Max"))
