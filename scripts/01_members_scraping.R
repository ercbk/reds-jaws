# Members Scraping
# JAWS project, 9-18-2017

# Scraping Names of Reds HOF members from Wiki page

library(XML)
url <- "https://en.wikipedia.org/wiki/Cincinnati_Reds_Hall_of_Fame_and_Museum#Cincinnati_Reds_Hall_of_Fame_members"
member <- readHTMLTable(URL, which=2)

### (explained further below) 'which' refers to which table on the web page. Figured this out
### by examining the html code for each object I thought might be a table.

### This failed. Gave me an error, "Error in (function (classes, fdef, mtable)  : 
### unable to find an inherited method for function 'readHTMLTable' for signature '"NULL"'.
### I did a quick google search and maybe it's not xml content. I did also get a "may not be
### XML" warning which I don't totally get since I'm assuming by the fun name that html is cool.
### Maybe I was looking at Java. Didn't get into it much since I had this rvest method in my back pocket.



library(rvest)
url <- "https://en.wikipedia.org/wiki/Cincinnati_Reds_Hall_of_Fame_and_Museum#Cincinnati_Reds_Hall_of_Fame_members"
members <- url %>% read_html() %>% html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>% html_table()
members <- members[[1]]
str(members)
head(members)
write_rds(members, "data/01 - memberScrape.rds")

### Year is the year inducted into the Reds HOF. Tenure is years played as a Red.
### Tenure is formatted as a range and there are missing values in number.

### Used Google Chrome inspect feature to obtain the xpath.
### r-click an object on the webpg -> inspect element -> window opens -> hovering over code
### highlights the object on the webpg -> r-click line of code -> copy -> copy xpath

### Intitial object saved to members is a df but something is off. Acts like it's first 
### of multiple objects when it's the only one. Hence the extra line subsetting the initial df 
### and assigning it to members again.



## Copied names of the 2017 nominees from Cinci Enq article (link in notes)
## third baseman Aaron Boone (1997-2003), outfielder Adam Dunn (2001-2008), pitcher John Franco 
## (1984-1989),pitcher Danny Graves (1997-2005), third baseman Scott Rolen (2009-2012) and outfielder 
## Reggie Sanders (1991-1998)


