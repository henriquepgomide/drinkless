#---- Get started ----

# The following commands works on linux and maybe on a mac. You will be able to download all csv's. Before you start, you will need to use firefox to export your cookies. You can download it here: https://addons.mozilla.org/en-US/firefox/addon/export-cookies/

# After the extension download, clear your cookies in firefox (optional) and open your portal with your credentials - admin and password. Once you are logged in, go to "tools" in Firefox and click on export cookies. Save the file and put it in the drinkless folder.

# You will also need to edit the file links-oms-br. This file is located in 'supplements' folder. You can use notepad to do this. Replace the url suffix pattern with yours. Save the file and execute the code below. 
# After this, copy and paste the 'links-oms-br' file in the drinkless folder.

# set working directory
setwd("~/drinkless/csvs/")

# Get csv's using cookies authentication - links-oms have all the urls to csv files separated by line.
system("wget -i links-oms-br --load-cookies cookies.txt") 

# rename files inserting the sufix .zip
system("rename 's/$/.zip/' *") 

# extract all csvs
system("unzip '*.zip'") 

# Now, you'll set the main directory for data merge and data cleaning.

setwd("~/drinkless/csvs/sites/default/files/gwiaa_export/")


# To retrieve all csvs, run the code below.

files  <- list.files() # listing files 
element  <- as.logical(rep(1, length(files))) # creating object to store elements

for (i in 1:length(files)) {
  data <- read.csv(files[[i]])
  dataNames  <- names(data)
  element[i]  <- is.element("Client_Id", dataNames) | is.element("Client_id", dataNames) | is.element("client_id", dataNames) | is.element("client_ID", dataNames) | is.element("UID", dataNames)
  
}    
  report  <- data.frame(files, element)
  csvwId  <- subset(report, element == TRUE)
  varNames  <- gsub("export_|.csv", "", csvwId$files) # picking good names for dataframes

for (j in 1:length(csvwId$files)) {
  dfname  <- varNames[j]
  assign(dfname, read.csv(as.character(csvwId[j, 1]), na.strings = c("NA","")))
  
}




