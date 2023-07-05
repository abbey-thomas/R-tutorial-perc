# Load necessary packages----
# Note: download the latest version of speechcollectr using the following command:
# install.packages("devtools")
devtools::install_github("abbey-thomas/speechcollectr")
library(dplyr)
library(tidyr)
library(shiny)
library(speechcollectr)

# Demographic survey prep----
# Get the basic demographic survey data frame from speechcollectr
data("demographics")
# If desired, view this table of survey questions using the following:
# View(demographics)

# Create another vector with the same names as the column in the demographics file
# To ask participants a question about their native language
# Note that since this is a free-form text response (as denoted by `type = 'textInput'`),
# We do not need to add an answer options (hence options = NA),
# but we still need to include the options element in the vector
# to ensure it joins properly to the table of other questions in "demographics"
nat_lang <- c(id = "native",
              priority = "required",
              label = "What do you consider to be your first or native language?",
              type = "textInput",
              options = NA)

# Bind the native language to the rest of the demographic survey
survey <- rbind(demographics, nat_lang)

# Save the complete survey
write.csv(survey, "survey.csv", row.names = FALSE)

# Create the application's "www" directory
# Move the survey into the 'www' directory
# Both of these can be accomplished with a single call to speechcollectr::wwwPrep()
wwwPrep("survey.csv")

# Check the survey to make sure everything is formatted properly
feedback <- surveyPrep(questionFile = "www/survey.csv",
                       notListedLab = "Not listed:")

# Perception Experiment prep----
## Headphone screen----
# R implementation of the Huggins Pitch screening tool by Milne et al. (2021)
# used to check whether participant is wearing headphones of sufficient quality.
# This command will place the audio files necessary for the experiment in the www folder
wwwPrep(HugginsPitchScreen = TRUE)

## Perception stimuli----
wwwPrep("perc_stim", is_dir = TRUE)

## Make a folder for outputs
dir.create("www/outputs")





