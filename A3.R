


library(dplyr)
library(ggplot2)
Incarceration_df <- read.csv("/Users/Sam/Desktop/Info201/assignment-3---incarceration-samgarciaperez/data/incarceration_trends.csv",stringsAsFactors = FALSE)
jail_admissions_year <- 
  select(Incarceration_df,
         year, 
         state, 
         county_name,
         total_pop,
         total_jail_adm
  ) 

#Find the top 5 most populated states in 2018 
jail_admissions_year <- jail_admissions_year %>%
  arrange(-year, -total_pop) %>%
  filter(state %in% c( "CA", "NY", "IL", "TX", "AZ")
  )

grouped_states <- group_by(jail_admissions_year, state, year) 
grouped_states <- summarize(
  grouped_states, 
  sum_total_pop = sum(total_pop),
  sum_jail_adm = sum(total_jail_adm, na.rm = TRUE)
)

#Graph
graph1 <- ggplot(data = grouped_states, mapping = aes(x=year, y=sum_jail_adm)) + 
  geom_point(aes(color = state)) + 
  scale_color_brewer(palette = "Set2") +  
  scale_x_continuous(name="Year", limits=c(1978, 2018)) + 
  scale_y_continuous(name= "Jail Admissions", limits = c(0, 1700000)) +
  labs(
    title = "U.S Jail Admissions Trends of the Top 5 Most Populated States from 1978 to 2018"
  )


#Two Continuous variables: Racial/Ethnic Groups Prison population over time 

jail_population_df <- select(
  Incarceration_df, 
  year,
  state, 
  total_pop,
  aapi_jail_pop,
  black_jail_pop, 
  latinx_jail_pop,
  native_jail_pop,
  white_jail_pop,
  other_race_jail_pop
)


grouped_years <- group_by(jail_population_df, year)
year_pop_sum <- summarize(
  grouped_years,
  sum_total_pop_yr = sum(total_pop, na.rm = TRUE),
  sum_aapi_jail_pop = sum(aapi_jail_pop,na.rm = TRUE),
  sum_black_jail_pop = sum(black_jail_pop,na.rm = TRUE),
  sum_latinx_jail_pop = sum(latinx_jail_pop, na.rm= TRUE),
  sum_native_jail_pop = sum(native_jail_pop, na.rm = TRUE),
  sum_white_jail_pop = sum(white_jail_pop, na.rm = TRUE),
  sum_other_race_jail_pop = sum(other_race_jail_pop, na.rm = TRUE)
)

graph2 <- ggplot (data = year_pop_sum) + 
  geom_line(mapping = aes(x=year, y=sum_aapi_jail_pop, color="AAPI")) + 
  geom_line(mapping = aes(x=year, y=sum_black_jail_pop, color = "Black")) + 
  geom_line(mapping = aes(x=year, y=sum_latinx_jail_pop, color = "Latinx")) +
  geom_line(mapping = aes(x=year, y=sum_white_jail_pop,color = "White")) +
  geom_line(mapping = aes(x=year, y=sum_native_jail_pop, color = "Native")) +
  geom_line(mapping = aes(x=year, y=sum_other_race_jail_pop, color = "Other Race")) + 
  scale_x_continuous(name="Year", limits=c(1984, 2018)) + 
  scale_y_continuous(name= "Population", limits = c(0, 400000)) + 
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Comparison of U.S Prison Population by Racial/Ethnic Group from 1984 to 2018"
  )


# Map 

states2018 <-  Incarceration_df %>%
  filter(year == 2018) %>%
  select(
    year, 
    state, 
    total_jail_pop,
    aapi_jail_pop,
    black_jail_pop, 
    latinx_jail_pop,
    native_jail_pop,
    white_jail_pop,
    other_race_jail_pop
  ) 

grouped_2018 <- group_by(states2018, state)
grouped_2018$state<- state.name[match(grouped_2018$state, state.abb)] 
grouped_2018 <- mutate(grouped_2018,state =tolower(state))

grouped_state_2018 <- grouped_2018 %>%
  summarize(
    sum_aapi_jail_pop = sum(aapi_jail_pop,na.rm = TRUE),
    sum_black_jail_pop = sum(black_jail_pop,na.rm = TRUE),
    sum_latinx_jail_pop = sum(latinx_jail_pop, na.rm= TRUE),
    sum_native_jail_pop = sum(native_jail_pop, na.rm = TRUE),
    sum_white_jail_pop = sum(white_jail_pop, na.rm = TRUE),
    sum_other_race_jail_pop = sum(other_race_jail_pop, na.rm = TRUE)
  ) %>%
  mutate(
    black_latinx_sum = sum_latinx_jail_pop + sum_black_jail_pop 
  ) %>%
  mutate(
    black_latinx_w_ratio =  black_latinx_sum / sum_white_jail_pop
  )


states_map <- map_data("state") %>%
  rename(state = region) %>%
  left_join(grouped_state_2018, by="state")
ratio_map <- ggplot(states_map) + 
  geom_polygon(
    mapping = aes(x=long, y =lat, group = group, fill = black_latinx_w_ratio),
    color = "white", 
    size = .1 
  ) + 
  coord_map() + scale_color_brewer(palette = "Set2") + 
  labs(
    title= "Ratio of the Population Black and Latinx Jail Inmates to White Jail Inmates in 2018",
    fill = "Ratio"
  )

