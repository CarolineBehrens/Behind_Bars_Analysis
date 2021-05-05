library(shiny)
library(tidyverse)
# library(tidycensus)
# library(shinyWidgets)
library(gtsummary)
library(shinythemes)
library(gt)
library(broom.mixed)
#library(wesanderson)
# library(ggplot2)
library(viridis)
#library(hrbrthemes)
# library(dplyr)




#loading in the appropriate data sets for the project

library(readr)
total_mortality <- msfp0116stt12 <- read_csv("raw_data/msfp0116stt12.csv",
                                             col_types = cols(
                                               .default = col_character(),
                                               `2001` = col_double(),
                                               `2006` = col_double(),
                                               `2007` = col_double(),
                                               `2008` = col_double(),
                                               `2009` = col_double(),
                                               `2010` = col_double(),
                                               `2011` = col_double(),
                                               `2012` = col_double(),
                                               `2013` = col_double(),
                                               `2014` = col_double(),
                                               `2015` = col_double(),
                                               `2016` = col_double()
                                             ))
#View(msfp0116stt12)
crime_and_incarceration_by_state <- read_csv("raw_data/crime_and_incarceration_by_state.csv",
                                             col_types = cols(
                                               jurisdiction = col_character(),
                                               includes_jails = col_logical(),
                                               year = col_double(),
                                               prisoner_count = col_double(),
                                               crime_reporting_change = col_logical(),
                                               crimes_estimated = col_logical(),
                                               state_population = col_double(),
                                               violent_crime_total = col_double(),
                                               murder_manslaughter = col_double(),
                                               rape_legacy = col_double(),
                                               rape_revised = col_double(),
                                               robbery = col_double(),
                                               agg_assault = col_double(),
                                               property_crime_total = col_double(),
                                               burglary = col_double(),
                                               larceny = col_double(),
                                               vehicle_theft = col_double()
                                             )) 


fit_prisoner_data <- readRDS("raw_data/prisoner_fit.RDS")


total_mortality <- total_mortality %>%
  rename(jurisdiction = `State/Federal`) %>%
  filter(!(jurisdiction %in% c("Federal", "State"))) %>%
  select(-"2001 caveat", -"2006 caveat", -"2007 caveat", -"2008 caveat", -"2009 caveat",
         -"2010 caveat", -"2011 caveat", -"2012 caveat", -"2013 caveat", -"2014 caveat",
         -"2015 caveat", -"2016 caveat")

state_crime <- crime_and_incarceration_by_state %>%
  sample_n(1000, replace = TRUE) %>%
  mutate(name = str_to_sentence(jurisdiction)) %>%
  left_join(total_mortality, by = "jurisdiction") %>%
  select(name, year, violent_crime_total,agg_assault, 
         murder_manslaughter, robbery, property_crime_total,
         larceny, vehicle_theft) %>%
  pivot_longer(names_to = "type",
               values_to = "total",
               cols = c(property_crime_total, violent_crime_total))



#plot_1 <- state_crime %>%
  #filter(name.x == input$name) %>%
  #ggplot(aes(x = year, y = total/1000, color = type)) +
  #geom_point(size = 5)






 
#Creating new two new columns titled year and mortality number 
#to clean up the data and make it easier to manage. Also created 
#the south variable so I could investigate differences between 
#northern and southern states.


mortality_numbers <- total_mortality %>%
  pivot_longer(names_to = "Year",
               values_to = "Mortality_Number",
               cols = c(`2001`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`,
                        `2013`, `2014`,`2015`,`2016`)) %>%
  mutate(south = ifelse(jurisdiction %in% c("Alabama", "Florida", "Georgia", "Kentucky",
                                            "Louisiana", "Maryland", "Mississippi",
                                            "North Carolina", "Oklahoma", "South Carolina",
                                            "Tennesse", "Texas", "Virginia", "West Virginia"),
                        TRUE, FALSE))



#created a plot now to show the difference between north and south 
#mortality numbers in prisons 

 mortality_plot <- qplot(Year, Mortality_Number, data = mortality_numbers, 
        geom= "violin", fill = south, outlier.color = "transparent") 

 
 #Now I am working with my other data set. Need to recreate the 
 #south variable and assign states to it

 clean_crime <- crime_and_incarceration_by_state %>%
   mutate(south = ifelse(jurisdiction %in% c("ALABAMA", "FLORIDA", "GEORGIA", "KENTUCKY",
                                             "LOUISIANA", "MARYLAND", "MISSISSIPPI",
                                             "NORTH CAROLINA", "OKLAHOMA", "SOUTH CAROLINA",
                                             "TENNESSEE", "TEXAS", "VIRGINIA", "WEST VIRGINIA"),
                         TRUE, FALSE)) %>%
   
   #dividing the data by 1000 so it is easier to manage and interpret
   
   mutate(population_in_thousands = state_population/1000) %>%
   mutate(prisoner_count_in_thousands = prisoner_count/1000) %>%
   mutate(violent_crime_in_thousands = violent_crime_total/1000) %>%
   
   #dont want missing values that will mess up data
   
   drop_na(population_in_thousands) %>%
   drop_na(prisoner_count_in_thousands) %>%
   drop_na(violent_crime_in_thousands)
 
 #taking previous data set I created and now creating bins for the 
 #population and violent crime columns because when creating a newobs
 #it will produce too many outcomes. Grouping these values allows for 
 #easier data control and more manageable outcomes 
 
 crime_w_bins <- clean_crime %>%
   mutate(south = ifelse(jurisdiction %in% c("ALABAMA", "FLORIDA", "GEORGIA", "KENTUCKY",
                                             "LOUISIANA", "MARYLAND", "MISSISSIPPI",
                                             "NORTH CAROLINA", "OKLAHOMA", "SOUTH CAROLINA",
                                             "TENNESSEE", "TEXAS", "VIRGINIA", "WEST VIRGINIA"),
                         TRUE, FALSE)) %>%
   mutate(population_in_thousands = state_population/1000) %>%
   mutate(prisoner_count_in_thousands = prisoner_count/1000) %>%
   mutate(violent_crime_in_thousands = violent_crime_total/1000) %>%
   drop_na(population_in_thousands) %>%
   drop_na(prisoner_count_in_thousands) %>%
   drop_na(violent_crime_in_thousands) %>%
   mutate(population_bins = case_when(population_in_thousands < 8000 ~ 1,
                                      population_in_thousands >= 8000 & population_in_thousands <16000 ~ 2,
                                      population_in_thousands >= 16000 & population_in_thousands <24000 ~ 3,
                                      population_in_thousands >= 24000 & population_in_thousands < 32000 ~ 4,
                                      population_in_thousands >= 32000 & population_in_thousands < 40000 ~ 5)) %>%
   mutate(violent_crime_bins = case_when(violent_crime_in_thousands < 43 ~ 1,
                                         violent_crime_in_thousands  >= 43 & violent_crime_in_thousands < 86 ~2,
                                         violent_crime_in_thousands >= 86 & violent_crime_in_thousands < 129 ~ 3,
                                         violent_crime_in_thousands >= 129 & violent_crime_in_thousands < 172 ~ 4,
                                         violent_crime_in_thousands >= 172 & violent_crime_in_thousands < 215 ~ 5))
 
 
 
 
 
 
 
 
 
 
 
 
 #created a fit below, but ten saved it to an RDS file so it will load faster 
 #into my shiny app. Also eliminates the use of rstanarm which 
 #causes problems with shiny 

 # fit_1 <- stan_glm(prisoner_count_in_thousands ~ population_bins + south + violent_crime_bins + south*violent_crime_bins,
 #                   data = crime_w_bins, 
 #                    seed = 17,
 #                    refresh = 0)
 #  print(fit_1, digits = 4)
 #  
 #  
 #   saveRDS(fit_1, file = "prisoner_fit2.RDS")
 

fit_1 <- read_rds("raw_data/prisoner_fit2.RDS")

 #creating my regression table using the fit I have created and 
 #setting the digits to 3 for a good view of the data. 

 model_prisoner_count <-tbl_regression(fit_1,
                                       intercept = TRUE,
                                       estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
   as_gt() %>%
   tab_header(title = "Prisoner Count Varies Greatly by Population") %>%
   tab_source_note(md("Source: Data.world "))

 
 #lastly creating a newobs and using the unique function 
 #makes it much easier. 
 
 population_bins <- unique(crime_w_bins$population_bins)
 south <- unique(crime_w_bins$south)
 violent_crime_bins <- unique(crime_w_bins$violent_crime_bins)
 
 #expand grid to obtain all possible outcomes for posterior
 
 newobs_1 <- expand_grid(population_bins, south, violent_crime_bins)
 
 real_p <-add_fitted_draws(newobs_1, fit_1) 
  #making a nice looking plot to show the differnce between 
 #the variables I selected for the newobs. Colour argument 
 #is nice because it allows me to show my third variable 
 #which plays  great impact on my data 
   model_plot <- real_p %>%
   ggplot(aes(x = population_bins, y = .value, colour = violent_crime_bins)) +
   geom_jitter(alpha = .5) +
   geom_smooth(formula = y ~ x,
               method = "lm") +
    #facet wrap is key to show the difference between geographical location 
   facet_wrap(~south) +
   labs(title = "Prisoner Count", subtitle = "Geographical location has great effect",
        x = "Population Bins", y = "Prisoner Count  (Thousands)")



