library(COVID19)
library(ggplot2)
library(dplyr)
library(scales)
library(data.table)
library(ggthemes)
pacman::p_load(ggrepel)
library(ggrepel)


virus_data = covid19("IRN")
View(virus_data)

# creating dataset containing daily count of tests, new cases, deaths and recovered
daily_data = data.table(virus_data)

# creating data frame with daily data
daily_data = daily_data[ , list(date,
                                confirmed,
                                daily_confirmed = confirmed - shift(confirmed),
                                deaths,
                                daily_deaths = deaths - shift(deaths),
                                tests,
                                daily_recovered = recovered - shift(recovered),
                                daily_tests = tests - shift(tests))]


# setting y axis to decimal
options(scipen = 999)


################## CONFIRMED CASES ##################


# daily confirmed cases distribution plot
# Distribution satisfy Normal distribution
daily_data %>%
  ggplot(aes(x = date, y = daily_confirmed)) +
  geom_line(colour = "#FFCE00", size = 2) +
  geom_smooth(span = 0.4, colour = "#12C9F0", size = 2, alpha = 0.8, se = FALSE) +
  scale_x_date(labels = date_format("%d-%m-%Y"), breaks = pretty_breaks(10)) +
  labs(title = "Daily number of confirmed cases from COV19 in Iran",
       subtitle = "Linear scale",
       x = "Date",
       y = "Confirmed cases count") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text(angle = 45, hjust = 0.9))

View(daily_data)


# all confirmed cases distribution plot
# Distribution satisfy Cumulative distribution function of Normal distribution
virus_data %>%
  ggplot(aes(x = date, y = confirmed)) +
  geom_line(colour = "#FFCE00", size = 2) +
  scale_x_date(labels = date_format("%d-%m-%Y"), breaks = pretty_breaks(10)) +
  labs(title = "Number of all confirmed cases from COV19 in Iran",
       subtitle = "Linear scale",
       x = "Date",
       y = "All confirmed cases") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text(angle = 45, hjust = 0.9))


# all confirmed cases base 2 log scale distribution plot
# Distribution satisfy Cumulative distribution function of Exponential distribution 
# Logarithmic scale let us keep proportionality of the plot.
# Plot flattening is a good sign as it means virus spread is slowing down (even tho more people are infected, they don't infect as many people in proportion to earlier situation)
virus_data %>%
  ggplot(aes(x = date, y = confirmed)) +
  geom_line(colour = "#FFCE00", size = 2) +
  scale_x_date(labels = date_format("%d-%m-%Y"), breaks = pretty_breaks(10)) +
  scale_y_continuous(trans = "log2") +
  labs(title = "Number of all confirmed cases from COV19 in Iran",
       subtitle = "Base 2 logarithmic scale",
       x = "Date",
       y = "All confirmed cases") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text(angle = 45, hjust = 0.9))



################### DEATHS ##########################

# daily confirmed deaths distribution plot
# it is not a perfect normal distribution plot. We think that important here is fact, that some people suffer longer time from covid before they die, so it wont follow daily new cases distribution plot ideally
daily_data %>%
  ggplot(aes(x = date, y = daily_deaths)) +
  geom_line(colour = "black", size = 2, alpha = 0.8) +
  geom_smooth(span = 0.4, colour = "#12C9F0", size = 2, alpha = 0.8, se = FALSE) +
  scale_x_date(labels = date_format("%d-%m-%Y"), breaks = pretty_breaks(10)) +
  labs(title = "Daily number of deaths from COV19 in Iran",
       subtitle = "Linear scale",
       x = "Date",
       y = "Deaths count") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text(angle = 45, hjust = 0.9))


# all confirmed deaths distribution plot
virus_data %>%
  ggplot(aes(x = date, y = deaths)) +
  geom_line(colour = "black", size = 2, alpha = 0.8) +
  scale_x_date(labels = date_format("%d-%m-%Y"), breaks = pretty_breaks(10)) +
  labs(title = "Number of all deaths from COV19 in Iran",
       subtitle = "Linear scale",
       x = "Date",
       y = "Deaths") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text(angle = 45, hjust = 0.9))


# all confirmed deaths base 2 log scale distribution plot
# analogical to all confirmed cases, as virus spread slow down, all deaths curve will also flatten
virus_data %>%
  ggplot(aes(x = date, y = deaths)) +
  geom_line(colour = "black", size = 2) +
  scale_x_date(labels = date_format("%d-%m-%Y"), breaks = pretty_breaks(10)) +
  scale_y_continuous(trans = "log2") +
  labs(title = "Number of all confirmed deaths from COV19 in Iran",
       subtitle = "Base 2 logarithmic scale",
       x = "Date",
       y = "All confirmed deaths") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text(angle = 45, hjust = 0.9))


############### FEATURES #################

# creating data frame with means and standard deviation
features = data.frame(
  category = c("daily_cases", "daily_deaths", "daily_recovered", "daily_tests"),
  mean = c(
    mean(daily_data$daily_confirmed, na.rm=TRUE),
    mean(daily_data$daily_deaths, na.rm=TRUE),
    mean(daily_data$daily_recovered, na.rm=TRUE),
    mean(daily_data$daily_tests, na.rm=TRUE)
    ),
  standard_deviation = c(
    sd(daily_data$daily_confirmed, na.rm=TRUE),
    sd(daily_data$daily_deaths, na.rm=TRUE),
    sd(daily_data$daily_recovered, na.rm=TRUE),
    sd(daily_data$daily_tests, na.rm=TRUE)
  )
  
)
View(features)


# Tutaj mozna zaobserwowac zmiennosc danych, warto dodac, ze liczba testow codziennie mocno sie wahala
features %>%
  ggplot(x = category, y = mean) +
  geom_errorbar(aes(x = category, ymin = mean - standard_deviation, ymax = mean + standard_deviation), width = 0.25) +
  geom_point(aes(x = category, y = mean)) +
  labs(title = "Mean and standard deviation of the data",
       x = "Population") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())


View(daily_data)


remove(restrictions)
restrictions = data.frame()

# creating restrictions loosening of dataframe with dates
s = virus_data %>% filter(school_closing < shift(school_closing))
# dropping some columns
s = s[, c("date", "school_closing")]
# renaming column school_closing
names(s)[names(s)=="school_closing"] <- "coding"
# adding restriction type column with value
s$restriction = "school_closing"
# adding s data frame to restrictions data frame
restrictions = rbind(restrictions, s)



s = virus_data %>% filter(workplace_closing < shift(workplace_closing))
s = s[, c("date", "workplace_closing")]
names(s)[names(s)=="workplace_closing"] <- "coding"
s$restriction = "workplace_closing"
restrictions = rbind(restrictions, s)


s = virus_data %>% filter(cancel_events < shift(cancel_events))
s = s[, c("date", "cancel_events")]
names(s)[names(s)=="cancel_events"] <- "coding"
s$restriction = "cancel_events"
restrictions = rbind(restrictions, s)



s = virus_data %>% filter(gatherings_restrictions < shift(gatherings_restrictions))
s = s[, c("date", "gatherings_restrictions")]
names(s)[names(s)=="gatherings_restrictions"] <- "coding"
s$restriction = "gatherings_restrictions"
restrictions = rbind(restrictions, s)



s = virus_data %>% filter(stay_home_restrictions < shift(stay_home_restrictions))
s = s[, c("date", "stay_home_restrictions")]
names(s)[names(s)=="stay_home_restrictions"] <- "coding"
s$restriction = "stay_home_restrictions"
restrictions = rbind(restrictions, s)


# empty (no restriction loosening):
virus_data %>% filter(internal_movement_restrictions < shift(internal_movement_restrictions))
virus_data %>% filter(international_movement_restrictions < shift(international_movement_restrictions))
                                       
View(restrictions)

# creating plot
# red lines marks reduction of restrictions dates
# we can observe that reduction of some restrictions might have had influence on rapid increase of cases count
p =  ggplot(daily_data, aes(x = date, y = daily_confirmed)) +
  geom_line(colour = "#FFCE00", size = 2) +
  geom_vline(xintercept=as.numeric(restrictions$date),
             linetype=2, color = "red", size = 2) +
  geom_text_repel(data = restrictions, aes(x = date, label = paste(restriction, ":", coding, sep = " ")), y = 8000, angle = 90) +
  scale_x_date(labels = date_format("%d-%m-%Y"), breaks = pretty_breaks(20)) +
  labs(title = "Restrictions reduction dates with new restriction code",
       subtitle = "Codes explanation at: https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md",
       x = "Date",
       y = "All confirmed daily cases") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.text.x = element_text(angle = 45, hjust = 0.9))

p

