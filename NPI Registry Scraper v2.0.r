################################# NPI Registry Scraper Tool TM ##########################

# Authors 				  : Mohit Raut, Mantraraj Dash, Shreya Devendra Gupta
# Last Updated 			: 2018-09-12
# Contact Team      : Oncology Analytics Pod (Pune)

#########################################################################################

# Set working directory
setwd("C:/Projects/P99_STAT/08. NPI Scraper")


# Import rvest for manipulating HTML
library(rvest)

# Read input csv file with NPIs with header
input <- read.csv("input.csv", header = T)
head(input, 5)

##################Colname Edit#######################
input$qid=NA
colnames(input)=c("npi","name_last","name_first", "city","state","zip","qid")



# Specify NPI registry home URL as "root"
root = 'https://npiregistry.cms.hhs.gov/'

# Create an empty data frame to collect information scraped from NPI registry and name the columns
collected_output=data.frame(matrix(ncol=11,nrow=0))
names(collected_output)=c("NPI","Name","NPI.Type","Primary.Practice.Address","Phone","Primary.Taxonomy","Address","City","State","dummy","Zipcode")     



# Iterate over all HCPs in the input file
for(i in (1:nrow(input)))
#for(i in (1:2))
{
  
  # Print row and NPI for debugging
  # sprintf("%d out of %d",i,nrow(input))
  print(paste0("Progress: ", i," out of ",nrow(input)))
  print("=============================================")

  #Generate query strings corresponding to each of the columns in input
  city_string = ""
  fname_string = ""
  lname_string = ""
  npi_string = ""
  state_string = ""
  zip_string = ""

  if (!(is.na(input$city[i])))
    {city_string = paste('city=',input$city[i],"&", sep = "")}
  
  if (!(is.na(input$name_first[i])))
    {fname_string = paste('first_name=',input$name_first[i],"&", sep = "")}
  
  if (!(is.na(input$name_last[i])))  
    {lname_string = paste('last_name=',input$name_last[i],"&", sep = "")}
  
  if (!(is.na(input$npi[i])))
    {npi_string = paste('number=',input$npi[i],"&", sep = "")}
  
  if (!(is.na(input$state[i])))
  {state_string = paste('state=',input$state[i],"&", sep = "")}
  
  if (!(is.na(input$zip[i])))
  {zip_string = paste('postal_code=',input$zip[i], sep = "")}
  
  
  # Generate URL for querying details with given information
  u = paste(root,'registry/search-results-table?addressType=ANY&',city_string,fname_string,lname_string,npi_string,state_string,zip_string, sep = "");
  
  # Drop trailing ambersand if zip not given as input
  if(substr(u,nchar(u),nchar(u))=="&")
    {u = substr(u,1,nchar(u)-1)}
  
  # Percent-encode the URL generated above for ensuring it's a valid URL
  url  = URLencode(u)
  
  
  print(url)
  
  # Parse the HTML - print to console if read_html fails and skip the record
  data = tryCatch (read_html(url), error = function(e){'NA'})
  if(data[1] == 'NA')
  { 
    print("Skip Record")
    print(input$npi[i])
    next
  }
  
  # Extract data contained within "table" tag of the HTML and parse it as a R table
  newresult<- html_nodes(data, "table")[1] %>%html_table()
  
  #Extracting NPI Type ( From 'title' in XML )
  if(ncol(newresult[[1]])<6)
  {
    print(input$npi[i])
    print("deactivated")
  }
  else{
    newresult[[1]][3]<-data.frame(xml_attrs(xml_child(xml_child(xml_child(xml_child(html_nodes(data, "table")[[1]], 2), 1), 3), 1))[3])[1,1]
  
  
  # Cast the table as a data frame object
  output<-as.data.frame(newresult)
  
  #Check if dataframe is null
  if(ncol(output)<6)
  {
    next
  }
  
  # Split address to address, city, state and ZIP
  # Handle possible multiple matches by running a loop
  for(j in (1:nrow(output)))
  {

    
    temp=do.call(rbind,strsplit(gsub("\r","",gsub("\t","",as.character(output$Primary.Practice.Address[j]))), "\n"))
    output$Address[j]=temp[1]
    temp2 = do.call(rbind,strsplit(as.character(temp[2]), ", "))
    output$City[j]=temp2[1]
    output$State[j]=gsub(" ","",temp2[2])
    output$dummy[j]=temp[4]
    output$Zipcode[j]=temp[5]
    output$Input_index[j]=i
  }
      
  # Print to console for debugging
  # str(output)
  
  # Drop dummy and concatenated address columns
  drops <- c("Primary.Practice.Address","dummy")
  output = output[ , !(names(output) %in% drops)]
  
  # Append information for NPI to the consolidated data frame
  collected_output=rbind(collected_output,output)
  }
  print("=============================================")
}

head(collected_output,5)

print(data)
write.csv(collected_output,"output.csv")
