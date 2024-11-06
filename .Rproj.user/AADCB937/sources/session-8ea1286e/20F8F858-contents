#initialising renv ----
renv::init()
#load libraries ----
library(tidyverse)
library(palmerpenguins)
library(here)
library(janitor)
library(modelsummary)
library(ggplot2)


source(here("functions", "cleaningfunction.R"))
#the source() function allows us to import functions into R 
  #it causes R to accept input from the named file/ URL/ expression
#here > functions folder > cleaning function file

#----
here::here()
  #returns the path to the root directory of the project and helps us create paths relative to this root 
    #better practise than setwd() or getwd() which are very fragile and not reproducible

#looking at our raw data ----
head(penguins_raw)
  #penguins_raw is a variable (object used to store values)
  #we're looking at the first 6 rows of the dataset we're working with
    #this helps us understand it's structure and check it has loaded as we expected
colnames(penguins_raw)
  #these are poorly formatted; white spaces, inconsistent capitalisation, and special characters
    #should clean the data before we work with it 
  #to make this process reproducible, it should occur in R rather than in Excel (where  there would be limited record of what we did)

#preserving our raw data ----
#best practise to save a copy of the raw data, so we always can return to an unaltered copy
write.csv(penguins_raw, here("data", "penguins_raw.csv"))
  #write.csv() saves it as a csv file
    #first argument is the variable to save. second is the location + file name
      #uses here as a starting point, then directs to the data file. names the raw data file.
        #use of here makes more reproducible than a raw file path would be!
  #best practice = make this read-only (select file in explorer > info > protect document > always open read only)

#data cleaning ----

#first, load a copy of the raw data into r to work with
penguins_raw <- read.csv(here("data", "penguins_raw.csv"))

#we could manually clean this, by writing code that removes columns and alters column names line by line
  #but, this would be inefficient. needlessly copying and pasting is bad practice.
  #it is better to write a function that could be widely applied to similar scenarios
  #functions should be stored in a different file. this makes them easier to apply to multiple scripts, and avoids clogging up this script!

#we loaded the function when we opened the relevant libraries
colnames(penguins_raw)
penguins_column_clean <- clean_column_names(penguins_raw) #calling the clean_column_names function loaded in from cleaningfunction script on the penguins_raw data, 
  #puts the output into penguins_column_clean to avoid overwrititng 
colnames(penguins_column_clean)

penguins_cleaned <- complete_clean(penguins_raw)
colnames(penguins_cleaned)
#should then save the cleaned data:
write.csv(penguins_cleaned, here("data", "penguins_cleaned.csv"))

#importing the clean data ----
penguins_clean <- read.csv(here("data", "penguins_cleaned.csv"))

#subsetting data by column ----
  #we often want to subset data for plotting or analysis
#e.g. if we only want to look at species and body mass
species_body_mass_data <- penguins_clean %>% select(c("species", "body_mass_g"))
head(species_body_mass_data)

#note, i already removed rows that contained nas
  #others hadn't. this would be the stage to remove them if you didn't already
#methods for counting the number of nas:
#is.na = logical martix (T/F)
  #per column: colSumns(is.na(data))
    #counts T values (i.e. NA/ missing value)
  #per row: sum(rowSums(is.na(data))>0)
    #rowSums(is.na(data))>0 checks which rows have missing values. 
    #sum then counts how many rows meet this condition
  #total: sum(is.na(data))
    #counts T values (i.e. NA/ missing value)

#or could just remove na from species and body mass columns
species_body_mass_with_na <- penguins_column_clean %>% 
  select(c("species", "body_mass_g"))
head(species_body_mass_with_na)
species_body_mass_na_omit <- penguins_column_clean %>% 
  select(c("species", "body_mass_g")) %>%
  na.omit()
head(species_body_mass_na_omit)

#subsetting by specific value ----
adelie_data <- subset(penguins_clean, species == "Adelie")

#can subset to get just certain columns for specific values
  #e.g. just body mass and species for adelie penguins only
adelie_body_mass <- penguins_clean[penguins_clean$species == "Adelie", c("species", "body_mass_g")]
#another way to do this would be body_mass <- penguins_clean %>% select(species, body_mass_g)

#boxplot ----
flipper_boxplot <- ggplot( 
  data = penguins_cleaned,
  aes(x= species,
      y = flipper_length_mm)) +#acts like a pipe!
  geom_boxplot() 
#make a box plot
flipper_boxplot

#get errors if we have NA values. 
#in reality would delete the above code, and go straight to fixing NAs - here I am keeping it to show the course of the lesson
#i had actually already fixed this i think? with the cleaning function - though in retrospect removing ALL NAs wasn't the move (see below) 
#but otherwise this would have given errors
#should subset columns THEN remove NA values from only the columns we are interested in - deleting all NA would delete rows that are actually complete for all the data of interest

penguins_flippers <- penguins_cleaned %>% 
  select("species", "flipper_length_mm") %>%
  drop_na()
colnames(penguins_flippers)
head(penguins_flippers)
#removes NAs - piping avoid overwriting!

#when you do aes(color = species) you can't choose the colours yourself (and therefore they may be colour blind unfriendly)
#so we map the color we want for each species

species_colours <- c("Adelie" = "darkorange",
                     "Chinstrap" = "purple",
                     "Gentoo" = "cyan4") #mapping the colours ensures that the colours are consistent regardless of the order of the species in the data set (A list of colours would just assign in order of apperance for each data set )

#plotting graph without NA values 
flipper_boxplot_no_na <- ggplot( 
  data = penguins_flippers, 
  aes(x= species,
      y = flipper_length_mm)) +
  geom_boxplot(aes(color = species),
               width = 0.3,
               show.legend = F) +
  geom_jitter(aes(color = species), 
              alpha = 0.3, #alpha denotes transparency
              show.legend = F, 
              position = position_jitter(
                width = 0.2, #width across points for a sp are plotted
                seed = 0, #removes random element - points will plot in same place each time 
                )) + #adds data points to the plot
  scale_color_manual(values = species_colours) +
  theme_bw() +
  labs(x= "Species", y = "Flipper length (mm)")

#geom_jitter = random, when you run it the location of the plots changes (not reproducible)
  #gives each point a random x value w/in a certain width. [missed] about a random seed
    #seed = 0 does use a random number for the x location of each point, but does it in a reproducible way? not sure 
# + acts like a pipe!
flipper_boxplot_no_na
