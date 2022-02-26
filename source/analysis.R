# A3 r script for creation of .Rmd file and website

# Load needed packages
library("ggplot2")
library("dplyr")
library("maps")

# Load data 
incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",
                               stringsAsFactors = FALSE)

# Summary value of dataset
# Chosen to primarily focus on how the AA/PI race fairs in jail numbers. More specifically,
# I wish to see the areas and trends of AA/PI's count in jail numbers compared to
# another race, lets say for now black. 

# What is the highest jail count of the AA/PI inmates?
count_highest_aapi_jail_pop <- incarceration_data %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE)) %>%
  pull(aapi_jail_pop)

# Which county has the highest jail population of AA/PI inmates?
county_highest_aapi_jail_pop <- incarceration_data %>%
  filter(aapi_jail_pop == count_highest_aapi_jail_pop) %>%
  pull(county_name)

# In the county which has the highest jail population of AA/PI inmates, 
# what is the average number of AA/PI inmates from all years
# the dataset covers (1970 - 2018)?
avg_aapi_count_highcounty <- incarceration_data %>%
  filter(county_name == county_highest_aapi_jail_pop) %>%
  summarise(mean_aapi_jail_pop = mean(aapi_jail_pop, na.rm = TRUE)) %>%
  pull(mean_aapi_jail_pop)

avg_aapi_count_highcounty <- round(avg_aapi_count_highcounty, 2)

# Lets see the county where the jail pop of blacks is the highest. First, find county with 
# highest jail population of Black inmates
county_highest_black_jail_pop <- incarceration_data %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

# Also Los Angeles county! Now lets see the average number of black inmates from all years
avg_black_count_highcounty <- incarceration_data %>%
  filter(county_name == county_highest_black_jail_pop) %>%
  summarise(mean_black_jail_pop = mean(black_jail_pop, na.rm = TRUE)) %>%
  pull(mean_black_jail_pop)

avg_black_count_highcounty <- round(avg_black_count_highcounty, 2)

# Much higher than the AA/PI count!


# Trends over time chart
# What is the trend for the population of aapi, white, and black jail inmates 
# within Los Angeles County over the last 10 years?

linechart_df <- incarceration_data %>%
  filter(county_name == "Los Angeles County") %>%
  top_n(10, year)

linechart <- ggplot(data = linechart_df, aes(x = year)) +
  geom_line(mapping = aes(y = black_jail_pop, color = "blue4")) +
  geom_line(mapping = aes(y = white_jail_pop, color = "green4")) +
  geom_line(mapping = aes(y = aapi_jail_pop, color = "red4")) +
  
  labs(
    title = "Trend of Recent Black, White, and Asian American/Pacific Islander 
(AA/PI) counts in LA County Jail",
    x = "Year",
    y = "Count of Inmates",
    color = "Race"
  ) +
  
  scale_color_manual(labels = c("Black", "White", "AA/PI"), 
                     values = c("blue4", "green4", "red4"))
  
print(linechart)


# Variable comparison chart
# Use a scatterplot for looking at two continous variables

# Within the most recent year in California, what is the relationship between 
# total population counts of aapi citizens per county and jail 
# population counts of aapi inmates per county? 
scatterplot_df <- incarceration_data %>%
  filter(state == "CA",
         year == max(year, na.rm = TRUE)) %>%
  select(county_name, aapi_pop_15to64, aapi_jail_pop)

# Need to clean up the dataset in order to plot values which exist (remove NA's from the two columns in question)
# Thank you stackoverflow for this solution : https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame 

scatterplot_df <- scatterplot_df[complete.cases(scatterplot_df[ ,2:3]),]

# Create scatterplot
# San Bernardino County, Santa Clara County, Orange County, Los Angeles County, Sacramento County, San Diego County, Alameda County
# Noteworthy counties to showcase

scatterplot <- ggplot(data = scatterplot_df, aes(x = aapi_pop_15to64, y = aapi_jail_pop)) +
  geom_point() +
  
  labs(
    title = "Comparison of all California County's AA/PI population and their respective count 
of AA/PI county jail inmates in 2018",
    x = "Population Count of County",
    y = "Inmate Count of County Jail"
  ) +
    annotate("text", x = 116171, y = 300.00, label = "San Bernardino County") +
    annotate("text", x = 534342, y = 275.00, label = "Santa Clara County") +
    annotate("text", x = 481727, y = 225.00, label = "Orange County") +
    annotate("text", x = 1000000, y = 105.00, label = "Los Angeles County") +
    annotate("text", x = 193758, y = 205.00, label = "Sacramento County") + 
    annotate("text", x = 313257, y = 155.00, label = "San Diego County") + 
    annotate("text", x = 390851, y = 110.00, label = "Alameda County") 

print(scatterplot)


# Map chart
# In the year in which the aapi jail population was at it highest, what is the trend of aapi jail counts
# across the united states by state?

# Find year where aapi count was at its highest
year_highest_aapipop <- incarceration_data %>%
  group_by(year) %>%
  summarise(total_aapi_year_count = sum(aapi_jail_pop, na.rm = TRUE)) %>%
  filter(total_aapi_year_count == max(total_aapi_year_count, na.rm = TRUE)) %>%
  pull(year)

# Sum each state's aapi jail count by the highest year
aapi_jail_state_count <- incarceration_data %>%
  filter(year == year_highest_aapipop) %>%
  group_by(state) %>%
  summarise(aapi_state_total = sum(aapi_jail_pop, na.rm = TRUE)) 

# Import state data from R to effectively merge datasets 
# Incarceration data has only State Abbreviations, and the 
# 'state_shapes' variable has only state names. We'll need to merge them.
data(state)
merge_df <- data.frame(state.abb, state.name, stringsAsFactors = FALSE)

# Mutate the merge_df data frame to have lower-case state names, rename
# state.abb to state for easier merging
merge_df <- merge_df %>%
  mutate(state.name = tolower(state.name)) %>%
  rename(state = state.abb)

# Merge the aapi_jail_state_count df with the merge_df on state abbev.
aapi_jail_state_count <- aapi_jail_state_count %>%
  left_join(merge_df, by = "state")

# Now join the aapi data to the U.S Shapefile.
state_shapes <- map_data("state") %>%
  rename(state.name = region) %>%
  left_join(aapi_jail_state_count, by = "state.name")

# Theme here is used for making the map look cleaner
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# The creation of the map can now begin
mapchart <- ggplot(state_shapes) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = aapi_state_total),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#132B42", high = "Red") +
  labs(fill = "AA/PI Jail Count") +
  blank_theme +
  labs(
    title = "Overview of Total AA/PI Inmate Jail Count in 2014 per State"
  )

print(mapchart)

