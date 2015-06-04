#Jason Zhang
#Jeremy Young
#Michael Tran
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
    name = paste(paste(name, ", "), state, separator="", collapse=" ")
    name = gsub("\\s,\\s", ", ", gsub("\\s+", " ", name))
    name = gsub("^\\s+|\\s+$", "", name)
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
results$County = gsub("St\\.", "St", gsub("Saint", "St", results$County))

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
colnames(result)[3] = "County"
#Saint to St
result$County = gsub("St\\.", "St", gsub("Saint", "St", result$County))
#Remove parish
result$County = gsub("\\sParish", "", result$County)

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
  
  county_name = paste(county_name, ", ", state_name, sep="")
  
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
#Change Saints to St
counties_df$County = gsub("St\\.", "St", gsub("Saint", "St", counties_df$County))
#Remove Parish
counties_df$County = gsub("\\sParish", "", counties_df$County)


#combine
interim = merge(results, counties_df, by="County")
final_df = merge(interim, result, by="County")


# Setup for part 3 and 4

require("class")

predicting_variables = c("Percent; HOUSEHOLDS BY TYPE - Households with one or more people 65 years and over",
                         "Estimate; HOUSEHOLDS BY TYPE - Average family size",  
                         "Percent; MARITAL STATUS - Divorced",  
                         "Percent; EDUCATIONAL ATTAINMENT - Some college",
                         "Percent; EMPLOYMENT STATUS - Percent Unemployed",
                         "Percent; HOUSEHOLDS BY TYPE - Households with one or more people under 18 years",
                         "Percent; INDUSTRY - Agriculture",  
                         "Percent; INDUSTRY - Professional",   
                         "Estimate; INCOME AND BENEFITS (IN 2010 INFLATION-ADJUSTED DOLLARS) - Median household income (dollars)",
                         "Percent; PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL - All families",
                         "Estimate; HOUSEHOLDS BY TYPE - Nonfamily households",
                         "Estimate; HOUSEHOLDS BY TYPE - Average household size",
                         "Estimate; MARITAL STATUS - Never married",
                         "Estimate; HOUSEHOLDS BY TYPE - Nonfamily households - Householder living alone",
                         "Estimate; HOUSEHOLDS BY TYPE - Family households (families) - Female householder")

# 3b - Jason and Jeremy

# total_pop_df contains stats for the overall population of each county, rather than each individual ethnicity
total_pop_df = final_df[final_df$POPGROUP == "Total population",]

# subset to only include the variables we're concerned with for this project
knn_df = total_pop_df[, c("County", "Dem", "GOP", "X", "Y", predicting_variables)]

# 2004 election results, containing county name, number of Bush votes, and number of Kerry votes
results_04 = read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2004.txt", sep="")
colnames(results_04)[1] = "County"

# Reformat county name to match rest of data from Step 2 (i.e. [COUNTY_NAME], [STATE])
counties_04 = sapply(results_04$County, 
                     function(county) capitalize(paste(strsplit(as.character(county), ",")[[1]][2], 
                                                       strsplit(as.character(county), ",")[[1]][1],
                                                       sep=", ")))
results_04$County = factor(counties_04)

# Calculate percentage who voted for Bush and Kerry in each county
total_votes = results_04$bushVote + results_04$kerryVote
results_04$bushPct = round(results_04$bushVote / total_votes, digits=3)
results_04$kerryPct = round(results_04$kerryVote / total_votes, digits=3)

find_winner = function(r, d) {
  if (r > d) {return("R")}
  else return("D")
} 

# Determine victor for each county
results_04$cl = apply(results_04[, c('bushVote', 'kerryVote')], 1, function(x) find_winner(x[1], x[2]))

# Match 2004 election results with data from Step 2
combined_df = merge(knn_df, results_04)

# For use in 3a
rpart_df = merge(knn_df, results_04)


train = combined_df[, c("X", "Y", predicting_variables)]
test = train
cl = combined_df$cl

# GOP and Dem vote percentages in each county for 2012 elections
GOP_Pct_12 = as.numeric(gsub("%", "", as.character(combined_df[, "GOP"]))) / 100
Dem_Pct_12 = as.numeric(gsub("%", "", as.character(combined_df[, "Dem"]))) / 100
truth = factor(sapply(1:nrow(combined_df), function(i) if (GOP_Pct_12[i] > Dem_Pct_12[i]) "R" else "D"))

get_knn_misclass_rate = function(k) {
  knn_results = knn(train, test, cl, k, prob=TRUE)
  
  pred = knn_results
  
  assess = table(pred, truth)
  err = (assess[1,2] + assess[2, 1]) / length(truth)
  return(err)
}

# Misclassification rate for k = 1..20
k_range = 1:20
misclass = sapply(k_range, get_knn_misclass_rate)


# 3a - Michael and Patrick

names(rpart_df)[6:20] = c("PctElder", "AvgFamSize", "PctDivorced", "PctCollege", 
                          "PctUnemployed", "PctUnder18", "PctFarm", "PctProfessional",
                          "MedianIncome", "PctBelowPoverty", "Nonfamily", "AvgHouseSize", 
                          "Bachelors", "Alone", "Female")
library(rpart)
fit = rpart(cl ~ PctElder + AvgFamSize + PctDivorced + PctCollege + PctUnemployed 
            + PctUnder18 + PctFarm + PctProfessional + MedianIncome 
            + PctBelowPoverty, method = "class", data = rpart_df)



fit1 = rpart(cl ~ Bachelors, method = "class", data = rpart_df)
fit2 = rpart(cl ~  PctBelowPoverty + Bachelors, method = "class", data = rpart_df)
fit3 = rpart(cl ~  PctBelowPoverty + Bachelors + Nonfamily, method = "class", data = rpart_df)
fit4 = rpart(cl ~  PctBelowPoverty + Bachelors + Nonfamily + Alone, method = "class", data = rpart_df)
fit5 = rpart(cl ~  PctBelowPoverty + Bachelors + Nonfamily + Alone + Female, 
             method = "class", data = rpart_df)
fit6 = rpart(cl ~  PctBelowPoverty + Bachelors + Nonfamily + Alone + Female + MedianIncome, 
             method = "class", data = rpart_df)
fit7 = rpart(cl ~  PctBelowPoverty + Bachelors + Nonfamily + Alone + Female + MedianIncome +
               PctUnemployed, method = "class", data = rpart_df)
fit8 = rpart(cl ~  PctBelowPoverty + Bachelors + Nonfamily + Alone + Female + MedianIncome +
               PctUnemployed + PctProfessional, method = "class", data = rpart_df)
fit9 = rpart(cl ~  PctBelowPoverty + Bachelors + Nonfamily + Alone + Female + MedianIncome +
               PctUnemployed + PctProfessional + PctElder, method = "class", data = rpart_df)
fit10 = rpart(cl ~  PctBelowPoverty + Bachelors + Nonfamily + Alone + Female + MedianIncome +
                PctUnemployed + PctProfessional + PctElder + PctFarm, 
              method = "class", data = rpart_df)
fit11 = rpart(cl ~  PctBelowPoverty + Bachelors + Nonfamily + Alone + Female + MedianIncome +
                PctUnemployed + PctProfessional + PctElder + PctFarm + PctCollege, 
              method = "class", data = rpart_df)
fit12 = rpart(cl ~  PctBelowPoverty + Bachelors + Nonfamily + Alone + Female + MedianIncome +
                PctUnemployed + PctProfessional + PctElder + PctFarm + PctCollege
              + AvgFamSize, 
              method = "class", data = rpart_df)
fit13 = rpart(cl ~  PctBelowPoverty + Bachelors + Nonfamily + Alone + Female + MedianIncome +
                PctUnemployed + PctProfessional + PctElder + PctFarm + PctCollege
              + AvgFamSize + AvgHouseSize, 
              method = "class", data = rpart_df)
fit14 = rpart(cl ~  PctBelowPoverty + Bachelors + Nonfamily + Alone + Female + MedianIncome +
                PctUnemployed + PctProfessional + PctElder + PctFarm + PctCollege
              + AvgFamSize + AvgHouseSize + PctDivorced, 
              method = "class", data = rpart_df)
fit15 = rpart(cl ~ PctElder + AvgFamSize + PctDivorced + PctCollege + PctUnemployed 
              + PctUnder18 + PctFarm + PctProfessional + MedianIncome 
              + PctBelowPoverty + Nonfamily + AvgHouseSize
              + Bachelors + Alone + Female, method = "class", data = rpart_df)


plot(fit, uniform=TRUE, main="Classification Tree for President")
text(fit, use.n=TRUE, all=TRUE, cex=.5)

prediction_rate = function(model, data_frame) {
  finding = predict(model, newdata = data_frame)
  truth = factor(sapply(1:nrow(data_frame), function(i) if (GOP_Pct_12[i] > Dem_Pct_12[i]) "R" else "D"))
  rpart_findings = factor(sapply(1:nrow(finding), function(i) if (finding[i, 1] > finding[i, 2]) "D" else "R"))
  percentage = sum(truth == rpart_findings) / length(truth)
  return (percentage)
  
}


# 4b - Jason and Jeremy

# for each county, find party that saw an increase in voting percentage in 2012
# calculate increase in percentage for this party from 2012 to 2004 
# use increase amount for arrow length in map, use party for arrow color

# To install maps package, run: install.packages("maps")
require("maps")

# GOP and Dem vote percentages in each county for 2004 elections
GOP_Pct_04 = combined_df$bushPct
Dem_Pct_04 = combined_df$kerryPct

GOP_diff = GOP_Pct_12 - GOP_Pct_04
Dem_diff = Dem_Pct_12 - Dem_Pct_04

# Party that saw greatest increase in voting percentage from 2004 to 2012. If neither party increased, party denoted as "N"
party_increase = factor(sapply(1:nrow(combined_df), 
                               function(i) if (GOP_diff[i] < 0 & Dem_diff[i] < 0) "N" else if (GOP_diff[i] >= Dem_diff[i]) "R" else "D"))

# Amount of increase for parties determined above.
vote_shifts = 10 * sapply(1:nrow(combined_df), 
                          function(i) if (GOP_diff[i] < 0 & Dem_diff[i] < 0) 0 else if (GOP_diff[i] >= Dem_diff[i]) GOP_diff[i] else Dem_diff[i])

# Adjust latitude/longitude per professor's announcement
lat = as.integer(as.character(combined_df$X)) / 1000000
lon = as.integer(as.character(combined_df$Y)) / 1000000

map('county', xlim=c(min(lat) - 1, max(lat)) + 1, ylim=c(min(lon) - 1, max(lon) + 1), main="Map of vote shifts")
cols = c("blue", "gray", "red")
arrows(lat, lon, lat + vote_shifts, lon + vote_shifts, length=0.03, col=cols[party_increase])


# 4a - Michael and Patrick

rates = c()
for (i in 1:15) {
  rates = c(rates, prediction_rate(get(paste("fit", toString(i), sep = "")), rpart_df))
}
plot(1:15, rates, xlab = "Number of Variables", ylab = "Accuracy Percentage", 
     main = "Correct Classification Rate", xlim=c(1,15))

pred_false = which(pred != truth)
rpart_findings = prediction_rate(fit, rpart_df)
rpart_false = which(rpart_findings != truth)
county_index = intersect(pred_false, rpart_false)

# Plot counties that were incorrectly predicted
rpart_df$X = lat
rpart_df$Y = lon
wrong_counties = rpart_df[county_index, ]
map('county', xlim=c(min(lat) - 1, max(lat)) + 1, ylim=c(min(lon) - 1, max(lon) + 1), main="Map of vote shifts")
#arrows(lat, lon, lat + vote_shifts, lon + vote_shifts, length=0.03, col=cols[party_increase])
points(wrong_counties$X, wrong_counties$Y, col="red", pch=19, cex=0.7)

# Plot misclassification rate for k = 1..20
plot(misclass ~ k_range,
     xlab="k", ylab="Misclassification Rate",
     main="Knn Misclassification Rates",
     pch=19)


