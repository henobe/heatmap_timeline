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

# transform data for plotting -------------------
grouped_data <- base_data %>%
  group_by(presenter, presentation_month = floor_date(presentation_date, unit = "month")) %>%
  summarise(n_presentations = n())

# plot transformed data -------------------------
htplot <- ggplot(grouped_data, aes(x = presentation_month, y = presenter)) +
  geom_tile(aes(fill = n_presentations)) +
  scale_fill_gradient2(low = "green", high = "darkgreen",
                       breaks = c(0:max(grouped_data$n_presentations)), guide = "legend") +
  labs(title = "Wer hat wann vorgetragen?",
       x = NULL,
       y = NULL,
       fill = "Anzahl Präsentationen\nin Monat") +
  scale_x_date(date_breaks = "6 months", minor_breaks = "1 month", date_labels = "%b %y") +
  theme_light() +
  theme(legend.position = "bottom")

ggsave("heatmap_timeline_plot.png", htplot, width = 5, height = 2)
