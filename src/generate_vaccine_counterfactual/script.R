# # Use to switch into this file's directory for development
# root = here::here()
# setwd(file.path(root, "src/generate_vaccine_counterfactual"))
# setwd(root) # Use to get back to root

gen_cfact_with_prod = function(base_series, prod_series, shift_days) {
  dose2_delay = 21 # how many days after first shot can a person get second
  dose2_max_share = 0.5 # 2nd doses can only take up to this share

  # contains the basic interest for vaccination with vaccines available sooner
  # combined with how many vaccines are available until each day
  base_shifted_with_prod = base_series %>%
    mutate(date = as.Date(date) - shift_days) %>%
    left_join(prod_series) %>%
    replace(is.na(.), 0) %>%
    mutate(first_doses_cfact = 0,
           second_doses_cfact = 0)

  cumulative_vac = 0 # keep track of number of vaccines administered so far
  dose1_overflow = 0 # how many people want a vaccine from base data but didn't get yet

  for (i in 1:nrow(base_shifted_with_prod)) {
    available_vac = base_shifted_with_prod$cumulative_available_vaccines[i] - cumulative_vac

    dose1_interest = base_shifted_with_prod$first_doses[i] + dose1_overflow
    dose2_interest = 0
    if (i > dose2_delay) {
      first_eligible_for_2 = sum(base_shifted_with_prod$first_doses_cfact[1:(i - dose2_delay)])
      dose2_base_interest = sum(base_shifted_with_prod$second_doses[1:i])
      dose2_cfact_given = sum(base_shifted_with_prod$second_doses_cfact[1:(i-1)])

      dose2_interest = min(first_eligible_for_2, dose2_base_interest) - dose2_cfact_given
    }

    # dose2 should left something for dose1
    dose2_vaccinated = floor(min(dose2_interest, available_vac * dose2_max_share))
    dose1_vaccinated = floor(min(dose1_interest, available_vac - dose2_vaccinated))
    # if dose1 doesn't take all available up, dose2 can take more if interested
    if (available_vac - dose1_vaccinated - dose2_vaccinated > 0) {
      dose2_vaccinated = floor(min(dose2_interest, available_vac - dose1_vaccinated))
    }

    dose1_overflow = dose1_interest - dose1_vaccinated

    base_shifted_with_prod$second_doses_cfact[i] = dose2_vaccinated
    base_shifted_with_prod$first_doses_cfact[i] = dose1_vaccinated

    cumulative_vac = cumulative_vac + dose2_vaccinated + dose1_vaccinated
  }

  # third doses stay the same (just shifted in time) since they most probably
  # don't face production constraints
  cfact_df = base_shifted_with_prod %>%
    mutate(first_doses = first_doses_cfact,
           second_doses = second_doses_cfact) %>%
    select(country, iso3c, date, first_doses, second_doses, third_doses)

  return(cfact_df)
}

# read in real world vaccination series
base_vaccination = read.csv("owid-raw.csv") %>%
  # Cutoff later data that looks unreasonable (negative number and zeros)
  filter(date < as.Date("2022-06-17"))

# read in the counterfactual production time series
counterfactual_production = readRDS("counterfactual_production.Rds")

dir.create("counterfactual_timelines")

countries_of_interest = c("USA", "GBR")

# create the counterfactual shifted series
cfact_with_prod = map_dfr(countries_of_interest, function(country_iso) {
  country_vacc = base_vaccination %>% filter(iso3c == country_iso)
  country_prod = counterfactual_production %>% filter(iso3c == country_iso)
  return(gen_cfact_with_prod(country_vacc, country_prod, shift_by))
})
saveRDS(cfact_with_prod, "counterfactual_timelines/counterfactual_vaccination.Rds")

# create a counterfactual with no vaccines
cfact_no_vaccines = base_vaccination %>%
  mutate(first_doses = 0, second_doses = 0, third_doses = 0)
saveRDS(cfact_no_vaccines, "counterfactual_timelines/no_vaccines.Rds")

# save the base real scenario from OWID
saveRDS(base_vaccination, "counterfactual_timelines/owid_raw.Rds")



### Check whether with unlimited production and no shift the counterfactual
# with calculated second doses looks like the real vaccination
unlimited_production = counterfactual_production %>%
  mutate(cumulative_available_vaccines = Inf)
shift_by = 0
cfact_0 = map_dfr(countries_of_interest, function(country_iso) {
  country_vacc = base_vaccination %>% filter(iso3c == country_iso)
  country_prod = unlimited_production %>% filter(iso3c == country_iso)
  return(gen_cfact_with_prod(country_vacc, country_prod, shift_by))
})
walk(countries_of_interest, function(country_iso) {
  plot = ggplot() +
    geom_line(data = base_vaccination %>% filter(iso3c == country_iso),
              aes(x = as.Date(date), y = second_doses, color = "Second real")) +
    geom_line(data = base_vaccination %>% filter(iso3c == country_iso),
              aes(x = as.Date(date), y = first_doses, color = "First real")) +
    geom_line(data = cfact_0 %>% filter(iso3c == country_iso),
              aes(x = as.Date(date), y = second_doses, color = "Second calculated")) +
    geom_line(data = cfact_0 %>% filter(iso3c == country_iso),
              aes(x = as.Date(date), y = first_doses, color = "First calculated")) +
    xlab("Date") +
    ylab("Vaccines administered") +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
    labs(color=country_iso)
  print(plot)
})

