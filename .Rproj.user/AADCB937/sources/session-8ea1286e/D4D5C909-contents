## ---------------------------
##
## Script name: Cleaning.r
##
## Purpose of script: 
##      # A file of functions for cleaning the Palmer Penguins dataset
##
## Author: Dr. Lydia France
##
## Date Created: 2024-10-01 
##
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------



# A function to make sure the column names are cleaned up, 
# eg lower case and snake case
clean_column_names <- function(penguins_data) {
  print("Converted the column names to snake case and lower case") #putting this first is good. 
    #putting it after the pipe system seems to confuse r and though it prints the message, the function output is a blank string
  penguins_data %>% #pipe operator allows multistep commands using the same variable without overwriting
    clean_names() #clean_names() is from the janitor package
}

# A function to remove columns based on a vector of column names
remove_columns <- function(penguins_data, column_names) {
  message("Removed columns based on the submitted vector of column names") #can also use message - signals a message, which can be suppressed by the user using suppressMessages()
    penguins_data %>%
    select(-starts_with(column_names)) #- removes columns that meet the criteria following the hyphen 
}

# A function to make sure the species names are shortened
shorten_species <- function(penguins_data) {
  print("Shortened penguin species names given in the format Common Name Latin Name to just the common name")
    penguins_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

# A function to remove any empty columns or rows
remove_empty_columns_rows <- function(penguins_data) {
  print("Removed empty columns or rows")
    penguins_data %>%
    remove_empty(c("rows", "cols")) #the c allows us to join the row and collumn conditions, making the function more efficient
}


# A function to remove rows which contain NA values
remove_NA <- function(penguins_data) {
  print("Removed rows that contain NA values")
    penguins_data %>%
    na.omit()
}


complete_clean <- function(raw_data){ #does all of these
  print("Data cleaning")
  raw_data %>% 
    clean_names() %>%
    shorten_species() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-comments) %>% #not capitalised because clean_names() has already been used
    select(-starts_with("delta")) %>%
    na.omit()
}
