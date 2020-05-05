# Question 3)

library(dplyr)
library(data.table)
library(imputeTS)

rm(list = ls())
cat("\f")

ratings=fread("/Users/Boray/Downloads/ETM58D_Spring20_HW1_q3_data/ETM58D_Spring20_HW1_q3_Netflix_data.txt")


names(ratings)= c("Miss Congeniality",
                  "Independence Day",
                  "The Patriot",
                  "The Day After Tomorrow",
                  "Pirates of the Caribbean: The Curse of the Black Pearl",
                  "Pretty Woman",
                  "Forrest Gump",
                  "The Green Mile",
                  "Con Air",
                  "Twister",
                  "Sweet Home Alabama",
                  "Pearl Harbor",
                  "Armageddon",
                  "The Rock",
                  "What Women Want",
                  "Bruce Almighty",
                  "Ocean's Eleven",
                  "The Bourne Identity",
                  "The Italian Job",
                  "I Robot",
                  "American Beauty",
                  "How to Lose a Guy in 10 Days",
                  "Lethal Weapon 4",
                  "Shrek 2",
                  "Lost in Translation",
                  "Top Gun",
                  "Pulp Fiction",
                  "Gone in 60 Seconds",
                  "The Sixth Sense",
                  "Lord of the Rings: The Two Towers",
                  "Men of Honor",
                  "Gladiator",
                  "Lord of the Rings: The Fellowship of the Ring",
                  " Sister Act",
                  "Double Jeopardy",
                  "Two Weeks Notice",
                  "Troy",
                  "The Royal Tenenbaums",
                  "National Treasure",
                  "50 First Dates",
                  "Indiana Jones and the Last Crusade",
                  "My Big Fat Greek Wedding",
                  "Mystic River",
                  "Titanic",
                  "Dirty Dancing",
                  "Catch Me If You Can",
                  "Finding Nemo (Widescreen)",
                  "The Matrix",
                  "Kill Bill: Vol. 1",
                  "The Wedding Planner",
                  "The Shawshank Redemption Special Edition",
                  "The Last Samurai",
                  "John Q",
                  "Swordfish",
                  "The Bourne Supremacy",
                  "The Terminal",
                  "Men in Black II",
                  "Spider-Man 2",
                  "Braveheart",
                  "Men in Black",
                  "Ghost",
                  " Air Force One",
                  "Lord of the Rings: The Return of the King",
                  "Man on Fire",
                  "The Incredibles",
                  "Mr Deeds",
                  "Collateral",
                  "Spider-Man",
                  "Saving Private Ryan",
                  "Erin Brockovich",
                  "Monsters Inc.",
                  "Shrek (Full-screen)",
                  "The Silence of the Lambs",
                  "Memento",
                  "Tomb Raider",
                  "Ferris Bueller's Day Off",
                  "Maid in Manhattan",
                  "Entrapment",
                  "Meet the Parents",
                  "Dodgeball A True Underdog Story",
                  "Rain Man",
                  "Patch Adams",
                  "Big Fish",
                  "Fight Club",
                  " S.W.A.T.",
                  "Good Will Hunting",
                  "A Few Good Men",
                  "Enemy of the State",
                  "The General's Daughter",
                  "Minority Report",
                  "Something's Gotta Give",
                  "Raiders of the Lost Ark",
                  "Anger Management",
                  "Sideways",
                  "Kill Bill: Vol. 2",
                  "American Pie",
                  "The Fast and the Furious",
                  "The School of Rock",
                  "Napoleon Dynamite",
                  "The Notebook")

summary(ratings)
str(ratings)
ratings[ratings==0] = NA_real_
str(ratings)
ratings = as.data.frame(ratings)
ratings = ratings %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
str(ratings)
ratings = t(ratings)
abc=dist(ratings)
mat_ratings=as.matrix(abc)
mds_coord=cmdscale(abc,2)

plot(mds_coord)
text(mds_coord,names(abc))