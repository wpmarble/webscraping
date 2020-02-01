## Webscraping tutorial
# Will Marble - August 2016

# This code goes along with the tutorial found at 
# http://stanford.edu/~wpmarble/webscraping_tutorial/webscraping_tutorial.pdf

## Before going through this tutorial, you should download google chrome
## and the SelectorGadget chrome extension (http://selectorgadget.com/)
## Then run the following code to make sure you have all the required packages:

pkgs = c("rvest", "magrittr", "httr", "stringr", "ggplot2", "rjson")
for (pkg in pkgs){
  if (!require(pkg, character.only = T)){
    install.packages(pkg)
    library(pkg)
  }
}



# simple example ----------------------------------------------------------


## Read my example html with read_html()
silly_webpage = read_html("http://stanford.edu/~wpmarble/webscraping_tutorial/html/silly_webpage.html")

# get paragraphs (css selector "p")
my_paragraphs = html_nodes(silly_webpage, "p")
my_paragraphs

# get elements with class "thisOne" -- use a period to denote class
thisOne_elements = html_nodes(silly_webpage, ".thisOne")
thisOne_elements

# get elements with id "myDivID" -- use a hashtag to denote id
myDivID_elements = html_nodes(silly_webpage, "#myDivID")
myDivID_elements

# extract text from myDivID_elements
myDivID_text = html_text(myDivID_elements)
myDivID_text

# extract links from myDivID_elements. first i extract all the "a" nodes (as in a href="website.com")
# and then extract the "href" attribute from those nodes
myDivID_link = html_nodes(myDivID_elements, "a") %>% html_attr("href")
myDivID_link



# harder example ----------------------------------------------------------

# STEP 1, OUTSIDE OF R
# Open that webpage on Chrome and search for the relevant set of ballot measures 
# (in this case, everything from 2016). Then download the page source.
# I did this and saved it to my website.

# STEP 2
# Use rvest to read the html file
measures = read_html("http://stanford.edu/~wpmarble/webscraping_tutorial/html/ballot_measures_2016.html")

# STEP 3 
# Select the nodes I want -- I can use the | character to return both types of 
# Xpath selectors I want
selector = '//*[contains(concat( " ", @class, " " ), concat( " ", "divRepeaterResults", " " ))]|//*[contains(concat( " ", @class, " " ), concat( " ", "h2Headers", " " ))]'
my_nodes = measures %>% html_nodes(xpath=selector) 

# let's look at what we got
my_nodes[1:9]

# the first 6 nodes don't have information I want, so get rid of them
my_nodes = my_nodes[-c(1:6)]

## work thru one entry first ##

# randomly chose 128 as an example to work thru
thetext = html_text(my_nodes[[128]])  

# get rid of all those extra spaces
thetext = gsub(pattern = "[ ]+", replacement = " ", thetext)

# let's split up the string using the "\r\n \r\n" identifier plus the one field that's 
# not separated by two line breaks -- topic areas
thetext = strsplit(thetext, split= "\r\n \r\n|\r\n Topic")[[1]]
thetext

# get rid of the \r\n,  extra whitespace, and empty entries
thetext = gsub(pattern="\\r|\\n", replacement="", thetext) %>% str_trim
thetext = thetext[thetext != ""]
thetext

# finally extract results
title = thetext[1]
election = thetext[grepl(pattern = "^Election", thetext)] %>% gsub("Election:", "", x = .) %>% str_trim
type = thetext[grepl(pattern = "^Type", thetext)] %>% gsub("Type:", "", x = .) %>% str_trim
status = thetext[grepl(pattern = "^Status", thetext)] %>% gsub("Status:", "", x = .) %>% str_trim
topic_areas = thetext[grepl(pattern = "^Area:|Areas:", thetext)] %>% gsub("Area:|Areas:", "", x = .) %>% str_trim

# summary is a little trickier to get because the actual summary comes
# the entry after the one that says "Summary: Click for Summary"
summary_index = grep(pattern="^Summary", thetext) + 1
summary = thetext[summary_index]

# we're done! print the results:
for (x in c("title", "election", "type", "status", "summary", "topic_areas")){
  cat(x,": ", get(x), "\n")
}


## Now loop thru all our nodes ##

# create state / info indicator vector
state_or_info = my_nodes %>% html_attr("class")
state_or_info = ifelse(state_or_info == "h2Headers", "state", "info")


# set up data frame to store results
results_df = data.frame(state = rep(NA_character_, length(my_nodes)),
                        title = NA_character_,
                        election = NA_character_,
                        type = NA_character_,
                        status = NA_character_,
                        topic_areas = NA,
                        summary = NA_character_, 
                        stringsAsFactors = F)


state = NA_character_ # this variable will keep track of what state we're in

# loop through all the nodes
for (i in 1:length(my_nodes)){
  
  # first see if the node tells us what state we're in; if so, update
  # the state variable
  if (state_or_info[i] == "state") {
    state = html_text(my_nodes[[i]])
  }
  
  # if it doesn't say what state we're in, apply the parsing code from above
  else {
    results_df$state[i] = state # fill in state
    
    # parse text like above
    thetext = html_text(my_nodes[[i]]) 
    thetext = gsub(pattern = "[ ]+", replacement = " ", thetext)
    thetext = strsplit(thetext, split= "\r\n \r\n|\r\n Topic")[[1]]
    
    thetext = gsub(pattern="\\r|\\n", replacement="", thetext) %>% str_trim
    thetext = thetext[thetext != ""]
    
    
    results_df$title[i] = thetext[1]
    results_df$election[i] = thetext[grepl(pattern = "^Election", thetext)] %>% 
      gsub("Election:", "", x = .) %>% str_trim
    results_df$type[i] = thetext[grepl(pattern = "^Type", thetext)] %>% 
      gsub("Type:", "", x = .) %>% str_trim
    results_df$status[i] = thetext[grepl(pattern = "^Status", thetext)] %>% 
      gsub("Status:", "", x = .) %>% str_trim
    results_df$topic_areas[i] = thetext[grepl(pattern = "^Area:|Areas:", thetext)] %>% 
      gsub("Area:|Areas:", "", x = .) %>% str_trim
    
    summary_index = grep(pattern="^Summary", thetext) + 1
    results_df$summary[i] = thetext[summary_index]
  }
}
results_df = results_df[!is.na(results_df$state),]

# let's have a look at a bit of the final product (some variables omitted for space)
head(results_df)
View(results_df)




# Briefly on API's --------------------------------------------------------



list_of_shows = c("breaking bad", "mad men", "game of thrones", 
                  "homeland", "house of cards", "true detective", 
                  "orange is the new black", "the americans", "mr robot",
                  "boardwalk empire", "the good wife", "dexter",
                  "lost", "true blood", "house", "big love", "downton abbey",
                  "damages", "boston legal", "grey's anatomy", "the sopranos", 
                  "heroes", "better call saul")

show_db = data.frame(title = list_of_shows,
                     year = NA, genre = NA, plot = NA, country = NA,
                     awards = NA, metascore = NA, imdbrating = NA,
                     imdbvotes = NA, imdbid = NA, totalseasons = NA)

# construct the url for each show by pasting the name of the show after
# the API base, and encoding using URLencode(). 
for (show in list_of_shows){
  show_url = paste0("http://omdbapi.com/?&t=", URLencode(show, reserved = T))
  show_info = read_html(show_url) %>% html_text %>% fromJSON
  
  show_db$year[show_db$title==show] = show_info$Year
  show_db$genre[show_db$title==show] = show_info$Genre
  show_db$plot[show_db$title==show] = show_info$Plot
  show_db$country[show_db$title==show] = show_info$Country
  show_db$awards[show_db$title==show] = show_info$Awards
  show_db$metascore[show_db$title==show] = show_info$Metascore
  show_db$imdbrating[show_db$title==show] = show_info$imdbRating
  show_db$imdbvotes[show_db$title==show] = show_info$imdbVotes 
  show_db$imdbid[show_db$title==show] = show_info$imdbID
  show_db$totalseasons[show_db$title==show] = show_info$totalSeasons
}
show_db[1:5, c(1:3, 8)]

# make a plot
show_db = show_db[order(show_db$imdbrating),]
show_db$title = factor(show_db$title, levels = show_db$title)
ggplot(show_db, aes(x = title, y = as.numeric(imdbrating))) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle=65, hjust=1)) + 
  scale_y_continuous(breaks = seq(0, 10, 1)) + 
  coord_cartesian(ylim = c(0, 10)) + 
  labs(x = NULL, y = "IMDb rating")
