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
