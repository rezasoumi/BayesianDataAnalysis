library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
install.packages("data.table")
library(data.table)
library(lubridate)
install.packages("tibble")
library(tibble)
library(brms)

data_dir <- "C:\\Users\\Reza\\Desktop\\archive\\tennis"
files <- list.files(data_dir, pattern = ".csv$", full.names = TRUE)
tennis_data <- lapply(files, fread) |> rbindlist(fill = TRUE)
#str(tennis_data)
summary(tennis_data)

tennis_data <- tennis_data |>
  mutate(
    tourney_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
    winner_age = as.numeric(winner_age),
    loser_age = as.numeric(loser_age),
    winner_rank = as.numeric(winner_rank),
    loser_rank = as.numeric(loser_rank),
    w_ace = as.numeric(w_ace),
    l_ace = as.numeric(l_ace)
  ) |> 
  filter(!is.na(winner_id) & !is.na(loser_id))


# doesn't matter for us based on the result
# Distribution of winner and loser ages
tennis_data |> 
  select(winner_age, loser_age) |> 
  rename(
    winner = winner_age,
    loser = loser_age
  ) |>
  pivot_longer(cols = everything(), names_to = "role", values_to = "age") |> 
  ggplot(aes(x = age, fill = role)) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(title = "Age Distribution of Players", x = "Age", y = "Frequency")

# Surface type has impact on the number of aces (Grass more, clay less)
# Calculate the total number of aces per match for each surface
tennis_data |> 
  filter(!is.na(surface) & surface != "") |> 
  group_by(surface) |> 
  summarise(avg_aces = mean(w_ace + l_ace, na.rm = TRUE)) |> 
  ggplot(aes(x = surface, y = avg_aces, fill = surface)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Aces per Match by Surface", x = "Surface", y = "Average Aces")


# Calculate win rate for each player
win_rate <- tennis_data |> 
  group_by(winner_name) |> 
  summarise(wins = n()) |> 
  rename(player_name = winner_name) |> 
  left_join(
    tennis_data |> 
      group_by(loser_name) |> 
      summarise(losses = n()) |> 
      rename(player_name = loser_name),
    by = "player_name"
  ) |> 
  mutate(win_rate = wins / (wins + losses))

# Display top players by win rate
win_rate |>
  arrange(desc(win_rate)) |>
  head(10)

# Plot ranking progression for a specific player
player_name <- "Novak Djokovic" # replace with desired player name
tennis_data |>
  filter(winner_name == player_name | loser_name == player_name) |>
  mutate(rank = ifelse(winner_name == player_name, winner_rank, loser_rank),
         date = tourney_date) |>
  arrange(date) |>
  ggplot(aes(x = date, y = rank)) +
  geom_line() +
  labs(title = paste("Ranking Progression of", player_name), x = "Date", y = "Rank") +
  scale_y_reverse() + # Reverse to show higher ranks at the top
  theme(
    axis.text.x = element_text(size = 20), # Adjust x-axis label size
    axis.text.y = element_text(size = 20),  # Adjust y-axis label size
    axis.title.x = element_text(size = 20),  # Increase x-axis label size
    axis.title.y = element_text(size = 20),
    title = element_text(size = 20)
  )

# Calculate total wins by country
country_wins <- tennis_data |>
  group_by(winner_ioc) |>
  summarise(wins = n()) |>
  arrange(desc(wins))

# Plot total wins by country
country_wins |>
  ggplot(aes(x = reorder(winner_ioc, -wins), y = wins, fill = winner_ioc)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Wins by Country", x = "Country", y = "Number of Wins") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8)) +
  guides(fill = FALSE) # Hide the legend for fill





# Filter data for players with a rank of 20 or better
top_20_players <- tennis_data |>
  filter(winner_rank <= 20 | loser_rank <= 20) |>
  mutate(
    player_name = ifelse(winner_rank <= 20, winner_name, loser_name),
    player_ioc = ifelse(winner_rank <= 20, winner_ioc, loser_ioc),
    player_rank = ifelse(winner_rank <= 20, winner_rank, loser_rank),
    player_date = tourney_date
  ) |>
  group_by(player_name) |>
  summarise(
    first_top_20_date = min(player_date, na.rm = TRUE),
    player_ioc = first(player_ioc),
    player_rank = first(player_rank)
  )

top_20_players |>
  ggplot(aes(x = first_top_20_date)) +
  geom_histogram(binwidth = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "First Appearance of Top-20 Players", x = "First Appearance Date", y = "Count of Players") +
  theme_minimal()


# Find each player's first recorded match date
first_match_date <- tennis_data |>
  bind_rows(
    tennis_data |> 
      mutate(player_name = winner_name, player_date = tourney_date),
    tennis_data |>
      mutate(player_name = loser_name, player_date = tourney_date)
  ) |>
  group_by(player_name) |>
  filter(!is.na(player_name)) |>
  summarise(first_match_date = min(player_date, na.rm = TRUE))

convert_days <- function(days) {
  years <- floor(days / 365)  # Approximate years
  remaining_days <- days %% 365
  months <- floor(remaining_days / 30)  # Approximate months
  days <- remaining_days %% 30  # Remaining days
  
  return(paste(years, "years", months, "months", days, "days"))
}

# Combine the two datasets
time_to_top_20 <- top_20_players |>
  left_join(first_match_date, by = "player_name") |>
  mutate(time_to_top_20 = as.numeric(difftime(first_top_20_date, first_match_date, units = "days"))) |>
  filter(!is.na(time_to_top_20)) |>
  mutate(time_friendly = convert_days(time_to_top_20))

# Group by country and calculate average time
country_time_to_top_20 <- time_to_top_20 %>%
  group_by(player_ioc) %>%
  summarise(avg_time_to_top_20 = mean(time_to_top_20, na.rm = TRUE)) %>%
  arrange(avg_time_to_top_20)

# Plot the time to top 20 by country
country_time_to_top_20 %>%
  ggplot(aes(x = reorder(player_ioc, avg_time_to_top_20), y = avg_time_to_top_20, fill = player_ioc)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Time to Reach Top 20 by Country",
    x = "Country",
    y = "Average Time (Days)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8)) +
  guides(fill = FALSE) # Hide legend




# Prepare data for Bayesian analysis
time_to_top_20_data <- time_to_top_20 %>%
  select(player_ioc, time_to_top_20)

unique_countries <- time_to_top_20_data %>%
  distinct(player_ioc) %>%
  arrange(player_ioc)

# Print the unique countries
print(unique_countries)

region_map <- data.frame(
  player_ioc = c(
    "ARG", "ARM", "AUS", "AUT", "BEL", "BIH", "BLR", "BRA", "BUL", "CAN",
    "CHI", "CHN", "COL", "CRO", "CYP", "CZE", "DEN", "DOM", "ECU", "ESP",
    "FIN", "FRA", "GBR", "GEO", "GER", "GRE", "HUN", "ISR", "ITA", "JPN",
    "KAZ", "KOR", "LAT", "LUX", "MAR", "MDA", "MEX", "NED", "NOR", "NZL",
    "PER", "POL", "POR", "ROU", "RSA", "RUS", "SLO", "SRB", "SUI", "SVK",
    "SWE", "THA", "TPE", "TUN", "UKR", "URU", "USA", "UZB", "ZIM"
  ),
  region = c(
    # Americas
    "Americas", "Europe", "Oceania", "Europe", "Europe", "Europe", "Europe", "Americas", "Europe", "Americas",
    "Americas", "Asia", "Americas", "Europe", "Europe", "Europe", "Europe", "Americas", "Americas", "Europe",
    # Europe
    "Europe", "Europe", "Europe", "Europe", "Europe", "Europe", "Europe", "Europe", "Europe", "Asia",
    # Asia
    "Asia", "Asia", "Europe", "Europe", "Africa", "Europe", "Americas", "Europe", "Europe", "Oceania",
    # Americas
    "Americas", "Europe", "Europe", "Europe", "Africa", "Europe", "Europe", "Europe", "Europe", "Europe",
    # Europe, Asia, Africa, Americas
    "Europe", "Asia", "Asia", "Africa", "Europe", "Americas", "Americas", "Asia", "Africa"
  )
)


time_to_top_20_regional <- time_to_top_20_data %>%
  left_join(region_map, by = "player_ioc") %>%
  filter(!is.na(region)) # Remove any players without region data

# Inspect data
summary(time_to_top_20_regional)


get_prior(time_to_top_20 ~ 1 + (1 | region) + (1 | player_ioc), data = time_to_top_20_regional)

time_to_top_20_regional <- time_to_top_20_regional %>%
  mutate(scaled_time_to_top_20 = time_to_top_20 / max(time_to_top_20, na.rm = TRUE))

time_to_top_20_regional <- time_to_top_20_regional %>%
  mutate(time_to_top_20_smoothed = time_to_top_20 + 1e-6)

time_to_top_20_regional_nonzero <- time_to_top_20_regional %>%
  filter(time_to_top_20 > 0)


time_to_top_20_regional %>%
  filter(player_ioc == "ARG") %>%
  ggplot(aes(x = time_to_top_20)) +
  geom_histogram(binwidth = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Time to Top 20 for ARG Players", x = "Time to Top 20 (Days)", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 24), # Adjust x-axis label size
    axis.text.y = element_text(size = 24),  # Adjust y-axis label size
    title = element_text(size = 20)
  )

time_to_top_20_regional %>%
  filter(player_ioc == "USA") %>%
  ggplot(aes(x = time_to_top_20)) +
  geom_histogram(binwidth = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Time to Top 20 for Argentina Players", x = "Time to Top 20 (Days)", y = "Frequency") +
  theme_minimal()

bayesian_model_gamma <- brm(
  time_to_top_20 ~ 1 + (1 | region) + (1 | player_ioc),
  data = time_to_top_20_regional_nonzero,
  family = Gamma(), # Gamma distribution with log-link
  prior = c(
    prior(normal(1000, 1000), class = "Intercept"), # Approx. log(mean time around 1000-2000)
    prior(cauchy(0, 2.5), class = "sd", group = "player_ioc"), # Prior on random effect std. dev
    prior(cauchy(0, 2.5), class = "sd", group = "region") # Prior on random effect std. dev
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)




# This is better (better loo result and pp_check plot)
bayesian_model <- brm(
  time_to_top_20 ~ 1 + (1 | region) + (1 | player_ioc),
  data = time_to_top_20_regional_nonzero,
  family = gaussian(),
  prior = c(
    prior(normal(1000, 1000), class = "Intercept"),
    prior(cauchy(0, 3), class="sd", group="player_ioc"), #  prior(normal(0, 1000), class="sd", group="player_ioc")
    prior(cauchy(0, 3), class = "sd", group="region") # prior(normal(0, 1000), class = "sd", group="region")
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

# Summarize the model
summary(bayesian_model_gamma)
summary(bayesian_model)
# Plot group-level effects
plot(bayesian_model, pars = "^r_") # Effects for regions and countries

# Posterior predictive checks
pp_check(bayesian_model_gamma)

pp_plot <- pp_check(bayesian_model_gamma)

# Adjust the tick sizes using `theme`
pp_plot +
  theme(
    axis.text.x = element_text(size = 20),  # Increase x-axis tick label size
    axis.text.y = element_text(size = 20)   # Increase y-axis tick label size
  )


pp_check(bayesian_model)

pp_plot <- pp_check(bayesian_model)

# Adjust the tick sizes using `theme`
pp_plot +
  theme(
    axis.text.x = element_text(size = 20),  # Increase x-axis tick label size
    axis.text.y = element_text(size = 20)   # Increase y-axis tick label size
  )

# Extract posterior samples for regions
posterior_samples <- as.data.frame(posterior_samples(bayesian_model, pars = "^r_region"))

# Plot posterior distributions for regions


# Transform the data to long format
posterior_samples_long <- posterior_samples %>%
  pivot_longer(cols = everything(), names_to = "region", values_to = "effect")

# Create the density plot
posterior_samples_long %>%
  ggplot(aes(x = effect, fill = region)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Posterior Distributions of Regional Effects",
    x = "Effect on Time to Top 20",
    y = "Density"
  ) +
  theme_minimal()

# Add more diagnostics and model refinement as needed


# Perform PSIS-LOO cross-validation
loo_result_ <- loo(bayesian_model)
loo_result <- loo(bayesian_model_gamma)
# Print the results
print(loo_result_)
print(loo_result)





# Additional analysis



tennis_aces <- tennis_data %>%
  # Create a new column to differentiate wins and losses with respect to aces
  mutate(outcome = "Win") %>%
  select(outcome, aces = w_ace) %>%
  bind_rows(
    tennis_data %>%
      mutate(outcome = "Loss") %>%
      select(outcome, aces = l_ace)
  ) %>%
  # Remove rows with NA values in aces
  filter(!is.na(aces)) %>%
  filter(aces <= 45) %>%
  # Count the occurrences of each outcome (Win or Loss) for each number of aces
  group_by(aces, outcome) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(outcome_binary = ifelse(outcome == "Win", 1, 0))

# Plot the result
ggplot(tennis_aces, aes(x = aces, y = count, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Impact of Serve Aces on Match Outcome",
       x = "Number of Aces",
       y = "Number of Matches") +
  scale_fill_manual(values = c("Win" = "blue", "Loss" = "red")) +
  theme_minimal()



tennis_aces <- tennis_data %>%
  mutate(outcome_binary = ifelse(winner_id == winner_id, 1, 0)) %>%
  select(outcome_binary, w_ace, l_ace)

# Combine winner and loser aces into a single dataset
tennis_aces_data <- tennis_aces %>%
  bind_rows(
    tennis_data %>%
      mutate(outcome_binary = 0) %>%
      select(outcome_binary, w_ace = l_ace, l_ace = w_ace)  # Swap aces for losers
  ) %>%
  filter(!is.na(w_ace) & !is.na(l_ace))  # Remove rows with missing ace values

# View the structure of the prepared data
str(tennis_aces_data)

# Fit a Bayesian logistic regression model
bayesian_model <- brm(
  outcome_binary ~ w_ace + l_ace,  # Predict outcome using the aces of winner and loser
  data = tennis_aces_data,
  family = bernoulli(),  # Logistic regression model
  prior = c(
    prior(normal(0, 2), class = "Intercept"),  # Prior for intercept
    prior(normal(0, 2), class = "b")  # Prior for coefficients (w_ace, l_ace)
  ),
  chains = 4,
  iter = 2000,
  cores = 4,
  control = list(adapt_delta = 0.95)  # Adjust control parameters if needed
)

# Summarize the posterior distributions
summary(bayesian_model)

# Plot the posterior distributions for aces
plot(bayesian_model)

# Plot the relationship between aces and the probability of winning
# We will plot the effect of the number of aces on the probability of winning
posterior_samples <- as.data.frame(posterior_samples(bayesian_model, pars = c("b_w_ace", "b_l_ace")))
ggplot(posterior_samples, aes(x = b_w_ace, y = b_l_ace)) +
  geom_point(alpha = 0.5) +
  labs(title = "Posterior Distribution of Coefficients for Aces",
       x = "Effect of Winner's Aces",
       y = "Effect of Loser's Aces") +
  theme_minimal()




# 2nd analysis



# Add regions to winner and loser data
tennis_data <- tennis_data %>%
  left_join(region_map, by = c("winner_ioc" = "player_ioc")) %>%
  rename(winner_region = region) %>%
  left_join(region_map, by = c("loser_ioc" = "player_ioc")) %>%
  rename(loser_region = region)

# Join nationality and region to surface_data
surface_data <- tennis_data %>%
  # Wins data
  select(player_name = winner_name, player_ioc = winner_ioc, surface, outcome_binary = w_ace) %>%
  mutate(outcome_binary = 1) %>%  # All winners are assigned outcome = 1
  bind_rows(
    # Losses data
    tennis_data %>%
      select(player_name = loser_name, player_ioc = loser_ioc, surface, outcome_binary = l_ace) %>%
      mutate(outcome_binary = 0)  # All losers are assigned outcome = 0
  ) %>%
  filter(!is.na(surface) & !is.na(player_name)) %>%  # Filter missing values
  group_by(player_name, player_ioc, surface) %>%
  summarise(
    matches_played = n(),
    win_count = sum(outcome_binary, na.rm = TRUE),  # Count wins
    .groups = "drop"
  ) %>%
  mutate(win_rate = win_count / matches_played)  # Calculate win rate

# Add region information
surface_data <- surface_data %>%
  left_join(region_map, by = "player_ioc") %>%
  filter(!is.na(region))  # Remove entries without region info

# Inspect the resulting data
summary(surface_data)

# Prepare data for Bayesian analysis
# Group by region and surface
bayesian_data <- surface_data %>%
  group_by(region, surface) %>%
  summarise(
    win_rates = list(win_rate),  # Collect all win rates as a list for Bayesian analysis
    .groups = "drop"
  )
bayesian_data <- bayesian_data %>%
  filter(surface != "")

# Inspect Bayesian data
print(n=30, bayesian_data)

# Bayesian model for each region-surface combination
bayesian_models <- list()

for (i in 1:nrow(bayesian_data)) {
  region <- bayesian_data$region[i]
  surface <- bayesian_data$surface[i]
  win_rates <- bayesian_data$win_rates[[i]]
  
  # Fit Bayesian model
  bayesian_models[[paste(region, surface, sep = "_")]] <- brm(
    win_rates ~ 1,  # Single group model for win rates
    data = data.frame(win_rates = win_rates),
    family = gaussian(),
    prior = c(
      prior(normal(0.35, 0.35), class = "Intercept"),  # Centered around 0.5, wide SD
      prior(cauchy(0, 0.3), class = "sigma")  # Prior for standard deviation
    ),
    chains = 4,
    iter = 2000,
    cores = 4,
    seed = 123
  )
}

# Summarize models
model_summaries <- lapply(bayesian_models, summary)

# Example: Inspect a specific region-surface model
print(model_summaries[["Asia_Grass"]])  # Replace with desired region_surface key

# Optional: Visualize posterior distributions
plot(bayesian_models[["Asia_Clay"]])

# Compare all posterior distributions for insights
posterior_effects <- lapply(bayesian_models, function(model) {
  posterior_samples(model, pars = "^b_")
})

# Combine and visualize posterior effects
posterior_combined <- do.call(rbind, lapply(names(posterior_effects), function(name) {
  # Extract posterior samples for the current model
  post_samples <- posterior_effects[[name]]
  
  # Convert to data frame and attach the region-surface label
  data.frame(
    region_surface = name,
    Estimate = as.numeric(unlist(post_samples)) # Flatten samples into a numeric vector
  )
}))


ggplot(posterior_combined, aes(x = Estimate, fill = region_surface)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Posterior Distributions of Win Rates by Region and Surface",
    x = "Win Rate",
    y = "Density"
  ) +
  theme_minimal()






