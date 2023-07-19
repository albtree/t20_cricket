library(dplyr)

## Function to clean ipl contract player names into same abbreviations as cricketdata player names
## Can use in a mutate function e.g. mutate(player = clean_ipl_players(player))

clean_ipl_players <- function(x){x <- case_when(x == "Varun Chakravarthy" ~ "CV Varun",
                                                x == "Varun Chakravathy" ~ "CV Varun",
                                                x == "Gurkeerat Singh Mann" ~ "Gurkeerat Singh",
                                                x == "Kamlesh Nagarkoti " ~ "KL Nagarkoti",
                                                x == "Gowtham Krishnappa" ~ "K Gowtham",
                                                x == "Asif KM" ~ "KM Asif",
                                                x == "MS Dhoni *" ~ "MS Dhoni",
                                                x == "Mujeeb Zadran" ~ "M Rahman",
                                                x == "Wanindu Hasaranga" ~ "PWH de Silva",
                                                x == "Ravindra Jadela" ~ "RA Jadeja",
                                                x == "Shahbaz Ahamad" ~ "Shahbaz Ahmed",
                                                x == "Harpreet Bhatia" ~ "Harpreet Singh",
                                                x =="Kamlesh Nagarkoti " ~ "KL Nagarkoti",
                                                x == "Kshitiz Sharma" ~ "Kshitiz Sharmaa",
                                                x == "Sanjay Yadav" ~ "Sanjay Yadavb",
                                                x == "Shahrukh Khan" ~ "M Shahrukh Khan",
                                                TRUE ~ x)}

