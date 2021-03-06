####################
# R GOOGLE ANALYTICS
####################

####################
# AIM
####################

# This script retrieves your data from Google Analytics platform. If you need any help reach me at: henriquepgomide@gmail.com.
# I strongly recommend the package webpage - https://github.com/Tatvic/RGoogleAnalytics


# ===================
# PACKAGES 
# ===================

# You have to install the following packages 'rjson' and 'RCurl' in order to run the following analyses.
# To do this on a linux distro (debian and derivatives, ex. Ubuntu), install RCurl via terminal.
# You can do it in R. Just run the commmand below.

system("sudo apt-get install libcurl4-gnutls-dev")


# Install RGoogleAnalytics package ---
install.packages(RGoogleAnalytics)

# Load package
library(RGoogleAnalytics)

# 1. Authorize your account and paste the accesstoken 
query <- QueryBuilder()
# Retrieve acess token
access_token <- query$authorize()                                                
acess_token  <- "ya29.AHES6ZQ6Vb0vLO8yHeqU69WkDm8dMaTqwJQ9jl708jjuNxuGvDWUZg"

# 2.  Create a new Google Analytics API object
ga <- RGoogleAnalytics()
ga.profiles <- ga$GetProfileData(access_token)

# List the GA profiles 
ga.profiles

# 3. Build the query string, use the profile by setting its index value. You can try different queries using this helpful link: http://ga-dev-tools.appspot.com/explorer/

query$Init(start.date = "2012-12-01",
           end.date = "2013-08-27",
           dimensions = "ga:date,ga:pagePath,ga:country",
           metrics = "ga:visits,ga:pageviews,ga:timeOnPage",
           sort = "ga:visits",
           #filters="",
           #segment="",
           max.results = 50000,
           table.id = paste("ga:",ga.profiles$id[1],sep="",collapse=","),
           access_token=access_token)

# 4. Make a request to get the data from the API
ga.data <- ga$GetReportData(query)

# 5. Look at the returned data
head(ga.data)

# Converting dates to R
ga.data$date  <- as.Date(ga.data$date, "%Y%m%d")

######################
# EXPLORATORY ANALYSES
######################

# loading packages
library(ggplot2)

# Access by Date
graphAccess  <- ggplot(ga.data, aes(date))
graphAccess + geom_freqpoly(colour = "blue", binwidth = 10) + labs(x = "Month", y = "Frequency")
