#Jason Zhang
#Jeremy Young
#Mike Tran
#Patrick Lin

#DATA FRAME IS IN VARIABLE "final_df", assigned on line 202

#Step 1
library("XML", lib.loc="~/R/win-library/3.1")
require(XML)
url_format = "http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2012/xxx.xml"

states = scan(url("http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2012/stateNames.txt"),
              what="character", sep=",")

states = states[which(states!="states")] #Remove states header 
states = states[which(states!="alaska")] #Alaska is missing.

XML_files = c()
state_results = c()

capitalize = function(string) {
  s = strsplit(string, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}

#Loop through state files.
for (i in seq(length(states))) {
  state = states[i]
  target = sub("xxx", state, url_format)
  file = xmlParse(target, isURL=TRUE)
  root = xmlRoot(file)
  state = gsub("-", " ", state)
  state = capitalize(state)
  state = gsub("\\s*$", "", gsub("^\\s*", "", state))
  #Pull county name and Dem, GOP percentages. returns c(name, dem, gop)
  process_county = function(county) {
    header = xmlValue(xpathSApply(county, "tr/th[@class=\"results-county\"]")[[1]])
    name = gsub("\\s[0-9]+\\.[0-9]+% Reporting", "", header)
    dems = 0
    gop = 0
    results = xpathSApply(county, "tr/td")
    value = xmlValue(results[[2]])
    value2 = xmlValue(results[[5]])
    key = xmlValue(results[[1]])
    if (length(grep(key, "Dem")) == 1) {
      dems = value
      gop = value2
    } else {
      gop = value
      dems = value2
    }
    name = paste(paste(state, ","), name, separator="", collapse=" ")
    name = gsub("\\s", "", name)
    name = tolower(name)
    data = c(name, dems, gop)
    return(data)
  }
  results = xpathSApply(root, "/table/tbody", process_county)
  state_results = c(state_results, results)
  XML_files = c(XML_files, file)
}

state_results = matrix(state_results, ncol=3, byrow=TRUE)
results = data.frame(state_results)
colnames(results) = c("County", "Dem", "GOP")

#Step 2
bmeta = read.table("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/B01_metadata.txt", sep = "\n")
bcsv = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/B01003.csv")

dp2meta = read.table("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP02_metadata.txt", sep = "\n")
dp2csv = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP02.csv")

dp3meta = read.table("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP03_metadata.txt", sep = "\n")
dp3csv = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP03.csv")

# Renaming csv columns with meta-data column names
newNames1 = c()
for (i in 1:7) {
  label = as.character(bmeta[[1]][i])
  splits = strsplit(label, ",")
  newNames1 = c(newNames1, splits[[1]][2])
}

newNames1[4] = "Id3"
colnames(bcsv) = newNames1

# Renaming csv columns with meta-data column names
newNames2 = c()
for (i in 1:nrow(dp2meta)) {
  label = as.character(dp2meta[[1]][i])
  splits = strsplit(label, ",")
  newNames2 = c(newNames2, splits[[1]][2])
}

newNames2[4] = "Id3"
colnames(dp2csv) = newNames2

# Renaming csv columns with meta-data column names
newNames3 = c()
for (i in 1:nrow(dp3meta)) {
  label = as.character(dp3meta[[1]][i])
  splits = strsplit(label, ",")
  newNames3 = c(newNames3, splits[[1]][2])
}

newNames3[4] = "Id3"
colnames(dp3csv) = newNames3

# merging socioeconomic csv together
dp = merge(dp2csv, dp3csv, by = "Id2")
# merge socioeconomic with county
withoutcounty = gsub( " County", "", bcsv[,3])
bcsv[,3] = withoutcounty
result = merge(bcsv, dp)

format = function(x) {
  words = strsplit(x, ", ")
  reformat = paste(paste(words[[1]][2], ","), words[[1]][1], separator="", collapse = "`")
  reformat = gsub(" ", "", reformat)
  return(tolower(reformat))
}

result = data.frame(result[1:2], apply(result[3], 1, format), result[4:length(result)])
colnames(result)[3] = "County"

#Step 3
# Example of what we're parsing here. Leave commented out.
#
# <?xml version="1.0"?>  
#   <doc xmlns:gml="http://www.opengis.net/gml">   
#     <state>    
#       <gml:name abbreviation="AL"> ALABAMA </gml:name>    
#       <county>     
#         <gml:name> Autauga County </gml:name>     
#         <gml:location>      
#           <gml:coord>       
#             <gml:X>  -86641472 </gml:X>       
#             <gml:Y>  32542207  </gml:Y>      
#           </gml:coord>     
#         </gml:location>    
#       </county>
#


doc = xmlParse("http://www.stat.berkeley.edu/users/nolan/data/Project2012/counties.gml")
root = xmlRoot(doc)

# Capitalize first letter of each word in string
capitalize = function(str) {
  s = strsplit(str, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# Extract data from county XML node
process_county = function(county) {
  
  # Extract name of county's state
  state = xmlParent(county)
  state_name = xmlValue(state[[1]])
  
  # Trim leading and trailing whitespace
  state_name = gsub("^\\s+|\\s+$", "", state_name)
  
  # State name should be properly capitalized
  state_name = capitalize(tolower(state_name))
  
  # Extract county's name
  county_name = xmlValue(county[[1]])
  
  # Trim leading and trailing whitespace
  county_name = gsub("^\\s+|\\s+$", "", county_name)
  
  if (nchar(strsplit(county_name, " County")[[1]]) != nchar(county_name)) {
    county_name = strsplit(county_name, " County")[[1]]
  }
  
  county_name = paste(state_name, ",", county_name, sep="")
  county_name = tolower(county_name)
  
  # Extract x and y coordinates
  location = county[[2]]
  coord = location[[1]]
  x = xmlValue(coord[[1]])
  y = xmlValue(coord[[2]])
  
  # Trim leading and trailing whitespace
  x = gsub("^\\s+|\\s+$", "", x)
  y = gsub("^\\s+|\\s+$", "", y)
  
  data = c(county_name, x, y)
  
  return(data)
  
}



counties = xpathSApply(root,
                       "/doc/state/county",
                       process_county)

counties_matrix = matrix(counties, byrow=TRUE, nrow=ncol(counties))
counties_df = data.frame(counties_matrix)
names(counties_df) = c("County", "X", "Y")

#combine
interim = merge(results, counties_df, by="County")
final_df = merge(interim, result, by="County")

#final_df = subset(final_df, POPGROUP == "Total population")




# 3b

library("class", lib.loc="C:/Program Files/R/R-3.1.1/library")

predicting_variables = c("Percent; HOUSEHOLDS BY TYPE - Households with one or more people 65 years and over",
                         "Estimate; HOUSEHOLDS BY TYPE - Average family size",  
                         "Percent; MARITAL STATUS - Divorced",  
                         "Percent; EDUCATIONAL ATTAINMENT - Some college",
                         "Percent; EMPLOYMENT STATUS - Percent Unemployed",
                         "Percent; HOUSEHOLDS BY TYPE - Households with one or more people under 18 years",
                         "Percent; INDUSTRY - Agriculture",  
                         "Percent; INDUSTRY - Professional",   
                         "Estimate; INCOME AND BENEFITS (IN 2010 INFLATION-ADJUSTED DOLLARS) - Median household income (dollars)",
                         "Percent; PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL - All families")

results_04 = read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2004.txt", sep="")
colnames(results_04)[1] = "County"

final_df = subset(final_df, County %in% results_04["County"][[1]])
results_04 = subset(results_04, County %in% final_df["County"][[1]])

final_df = final_df[order(final_df$County),]
results_04 = results_04[order(results_04$County),]

find_winner = function(r, d) {
  if (r > d) {return("R")}
  else return("D")
} 
cl = apply(results_04[, c('bushVote', 'kerryVote')], 1, function(x) find_winner(x[1], x[2]))

#knn_df = final_df[, c("X", "Y", predicting_variables)]
total_pop_df = final_df[final_df$POPGROUP == "Total population", ]
knn_df = total_pop_df[ , c("X", "Y", predicting_variables)]

train = knn_df
test = knn_df

results_04 = read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2004.txt", sep="")
colnames(results_04)[1] = "County"

total_votes = results_04$bushVote + results_04$kerryVote
results_04$bushPct = round(results_04$bushVote / total_votes, digits=3)
results_04$kerryPct = round(results_04$kerryVote / total_votes, digits=3)


knn_results = knn(train, test, cl, 1, prob=TRUE)

# 4b

# use 2012 results to find winning party
# calculate difference in percent between 2012 and 2004 for winning party
# use difference for length of arrows in map

# GOP and Dem vote percentages in each county for 2012 elections
GOP_Pct_12 = as.numeric(gsub("%", "", as.character(total_pop_df[, "GOP"]))) / 100
Dem_Pct_12 = as.numeric(gsub("%", "", as.character(total_pop_df[, "Dem"]))) / 100

# GOP and Dem vote percentages in each county for 2004 elections
GOP_Pct_04 = results_04$bushPct
Dem_Pct_04 = results_04$kerryPct

getWinningParty = function(index) {
  if (GOP_Pct_12[index] > Dem_Pct_12[index]) {
    return("GOP")
  } else {
    return("Dem")
  }
}

# Which party won in each county for 2012 elections
winningParties = sapply(1:length(GOP_Pct_12), getWinningParty)

getVoteShift = function(party) {
  if (party == "GOP") {
    return(GOP_Pct_12 - GOP_Pct_04)
  } else {
    return(Dem_Pct_12 - Dem_Pct_04)
  }
}

# Difference in percent between 2012 and 2004 for winning party
vote_shifts = sapply(winning_parties, getVoteShift)