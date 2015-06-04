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

library("XML", lib.loc="~/R/win-library/3.1")

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