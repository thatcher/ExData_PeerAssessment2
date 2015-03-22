# Homework assignment for Coursera exdata-011
# Week 3
# etl.R
#
# Basic routines for fetching the data from the web, read into R,
# transform any fields we need to clean/manipulate, and load
# the required subset for exploratory analysis.
#
# Extract will only download the data if it cant find the file locally or
# is called with force=TRUE
#
# Tranform ensures the extract has been run will only load/manipulate/slice 
# the source data if the data slice does not exist on disk.
#
# Load ensures the transform has been performed and reads in and returns 
# just the serialized slice.
#
# All of the operations can be called with refresh=TRUE to force the step
# to be re-performed, even if there data exists on disk.
#
# Chris Thatcher
library(data.table)
library(lubridate)

NEI_DATA_URL = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
NEI_SCC_FILE = "data/Source_Classification_Code.rds"

NEI_PM25_FILE = "data/summarySCC_PM25.rds"
NEI_PM25_BY_YEAR_FILE = "data/pm25_by_year.csv"

NEI_PM25_BALTIMORE_FILE = "data/pm25_baltimore.csv"
NEI_PM25_BALTIMORE_VEHICLE_FILE = "data/pm25_baltimore_vehicle.csv"
NEI_PM25_BALTIMORE_VEHICLE_BY_YEAR_FILE = "data/pm25_baltimore_vehicle_by_year.csv"
NEI_PM25_BALTIMORE_BY_YEAR_FILE = "data/pm25_baltimore_by_year.csv"

NEI_PM25_LOS_ANGELES_FILE = "data/pm25_los_angeles.csv"
NEI_PM25_LOS_ANGELES_VEHICLE_FILE = "data/pm25_los_angeles_vehicle.csv"
NEI_PM25_LOS_ANGELES_VEHICLE_BY_YEAR_FILE = "data/pm25_los_angeles_vehicle_by_year.csv"
NEI_PM25_LOS_ANGELES_BY_YEAR_FILE = "data/pm25_los_angeles_by_year.csv"

NEI_PM25_COAL_FILE = "data/pm25_coal.csv"
NEI_PM25_COAL_BY_YEAR_FILE = "data/pm25_coal_by_year.csv"


# The following globals will be exported from etl.extract
#   NEI_SCC
#   NEI_PM25
etl.extract = function(refresh=FALSE){
    # Make sure we have the data to work with locally, otherwise go get it.
    if( refresh || !file.exists(NEI_PM25_FILE) ){

        message("Extracting data from url.")

        data_zip = "data/temp.zip"

        if("Windows" == Sys.info()["sysname"])
            download.file(NEI_DATA_URL, destfile=data_zip)
        else
            download.file(NEI_DATA_URL, destfile=data_zip, method="curl")

        unzip(data_zip, exdir='data')
        file.remove(data_zip)
    }
    
    if(!exists('NEI_SCC')){
        message('Reading NEI SCC codes.')
        NEI_SCC <<- readRDS(NEI_SCC_FILE)
        message('Complete.')
    }
    
    if(!exists('NEI_PM25')){
        message('Reading NEI PM25 data.')
        NEI_PM25 <<- readRDS(NEI_PM25_FILE)
        NEI_PM25$type = as.factor(NEI_PM25$type)
        message('Complete.')
    }
}

# The following globals will be exported from etl.transform
#   NEI_PM25_BY_YEAR
#   NEI_PM25_BALTIMORE
#   NEI_PM25_BALTIMORE_VEHICLE
#   NEI_PM25_BALTIMORE_BY_YEAR
#   NEI_PM25_BALTIMORE_VEHICLE_BY_YEAR
#   NEI_PM25_LOS_ANGELES
#   NEI_PM25_LOS_ANGELES_VEHICLE
#   NEI_PM25_LOS_ANGELES_BY_YEAR
#   NEI_PM25_LOS_ANGELES_VEHICLE_BY_YEAR
#   NEI_PM25_COAL
#   NEI_PM25_COAL_BY_YEAR
etl.transform = function(refresh=FALSE){
    # loads the raw source data set
    if(refresh || !file.exists(NEI_PM25_FILE)){
        etl.extract(refresh=refresh)
    }
    # ensure the raw data is in scope
    if(!exists('NEI_DATA') || !exists('SCC_CODES')){
        etl.extract(refresh=refresh)
    }
    
    # Summarize the data for fine particulate matter by year, 
    if(!exists('NEI_PM25_BY_YEAR') || !file.exists(NEI_EPMI25_BY_YEAR_FILE)){
        message('Calculating PM25 by year.')
        NEI_PM25_BY_YEAR <<- aggregate(
            Emissions ~ year, 
            data=NEI_PM25, 
            FUN=sum
        )
        write.csv(
            NEI_PM25_BY_YEAR, 
            file=NEI_EPMI25_BY_YEAR_FILE, 
            row.names=FALSE
        )
    }
    
    # Subset the data for fine particulate matter for Baltimore, 
    if(!exists('NEI_PM25_BALTIMORE') || !file.exists(NEI_PM25_BALTIMORE_FILE)){
        message('Subsetting PM25 by fips 24510 (Baltimore).')
        NEI_PM25_BALTIMORE <<- subset(NEI_PM25, fips == "24510")
        write.csv(
            NEI_PM25_BALTIMORE, 
            file=NEI_PM25_BALTIMORE_FILE, 
            row.names=FALSE
        )
    }
    
    
    # Summarize the data for fine particulate matter for Baltimore, 
    if(!exists('NEI_PM25_BALTIMORE_BY_YEAR') || !file.exists(NEI_PM25_BALTIMORE_BY_YEAR_FILE)){
        message('Calculating Baltimore PM25 by year.')
        NEI_PM25_BALTIMORE_BY_YEAR <<- aggregate(
            Emissions ~ year, 
            data=NEI_PM25_BALTIMORE, 
            FUN=sum
        )
        write.csv(
            NEI_PM25_BALTIMORE_BY_YEAR, 
            file=NEI_PM25_BALTIMORE_BY_YEAR_FILE, 
            row.names=FALSE
        )
    }
    
    # Subset the data for vehicle emissions from baltimore
    if(!exists('NEI_PM25_BALTIMORE_VEHICLE') || !file.exists(NEI_PM25_BALTIMORE_VEHICLE_FILE)){
        message('Subsetting PM25 by "Mobile Sources" in SCC.Level.One.')
        vehicle_sources = subset(NEI_SCC, grepl("Mobile Sources", NEI_SCC$SCC.Level.One))
        NEI_PM25_BALTIMORE_VEHICLE <<- subset(NEI_PM25_BALTIMORE, SCC %in% vehicle_sources$SCC)
        write.csv(
            NEI_PM25_BALTIMORE_VEHICLE, 
            file=NEI_PM25_BALTIMORE_VEHICLE_FILE, 
            row.names=FALSE
        )
    }
    
    # Summarize the data for vehicle emissions for Baltimore, 
    if(!exists('NEI_PM25_BALTIMORE_VEHICLE_BY_YEAR') || !file.exists(NEI_PM25_BALTIMORE_VEHICLE_BY_YEAR_FILE)){
        message('Calculating Baltimore vehicle PM25 by year.')
        NEI_PM25_BALTIMORE_VEHICLE_BY_YEAR <<- aggregate(
            Emissions ~ year, 
            data=NEI_PM25_BALTIMORE_VEHICLE, 
            FUN=sum
        )
        write.csv(
            NEI_PM25_BALTIMORE_VEHICLE_BY_YEAR, 
            file=NEI_PM25_BALTIMORE_VEHICLE_BY_YEAR_FILE, 
            row.names=FALSE
        )
    }
    
    # Subset the data for fine particulate matter for Los Angeles, 
    if(!exists('NEI_PM25_LOS_ANGELES') || !file.exists(NEI_PM25_LOS_ANGELES_FILE)){
        message('Subsetting PM25 by fips 06037 (Los Angeles).')
        NEI_PM25_LOS_ANGELES <<- subset(NEI_PM25, fips == "06037")
        write.csv(
            NEI_PM25_LOS_ANGELES, 
            file=NEI_PM25_LOS_ANGELES_FILE, 
            row.names=FALSE
        )
    }
    
    
    # Summarize the data for fine particulate matter for Los Angeles, 
    if(!exists('NEI_PM25_LOS_ANGELES_BY_YEAR') || !file.exists(NEI_PM25_LOS_ANGELES_BY_YEAR_FILE)){
        message('Calculating Los Angeles PM25 by year.')
        NEI_PM25_LOS_ANGELES_BY_YEAR <<- aggregate(
            Emissions ~ year, 
            data=NEI_PM25_LOS_ANGELES, 
            FUN=sum
        )
        write.csv(
            NEI_PM25_LOS_ANGELES_BY_YEAR, 
            file=NEI_PM25_LOS_ANGELES_BY_YEAR_FILE, 
            row.names=FALSE
        )
    }
    
    # Subset the data for vehicle emissions from Los Angeles
    if(!exists('NEI_PM25_LOS_ANGELES_VEHICLE') || !file.exists(NEI_PM25_LOS_ANGELES_VEHICLE_FILE)){
        message('Subsetting PM25 by "Mobile Sources" in SCC.Level.One.')
        vehicle_sources = subset(NEI_SCC, grepl("Mobile Sources", NEI_SCC$SCC.Level.One))
        NEI_PM25_LOS_ANGELES_VEHICLE <<- subset(NEI_PM25_LOS_ANGELES, SCC %in% vehicle_sources$SCC)
        write.csv(
            NEI_PM25_LOS_ANGELES_VEHICLE, 
            file=NEI_PM25_LOS_ANGELES_VEHICLE_FILE, 
            row.names=FALSE
        )
    }
    
    # Summarize the data for vehicle emissions for Los Angeles, 
    if(!exists('NEI_PM25_LOS_ANGELES_VEHICLE_BY_YEAR') || !file.exists(NEI_PM25_LOS_ANGELES_VEHICLE_BY_YEAR_FILE)){
        message('Calculating Los Angeles vehicle PM25 by year.')
        NEI_PM25_LOS_ANGELES_VEHICLE_BY_YEAR <<- aggregate(
            Emissions ~ year, 
            data=NEI_PM25_LOS_ANGELES_VEHICLE, 
            FUN=sum
        )
        write.csv(
            NEI_PM25_LOS_ANGELES_VEHICLE_BY_YEAR, 
            file=NEI_PM25_LOS_ANGELES_VEHICLE_BY_YEAR_FILE, 
            row.names=FALSE
        )
    }
    
    # Subset the data for fine particulate matter from coal sources, 
    if(!exists('NEI_PM25_COAL') || !file.exists(NEI_PM25_COAL_FILE)){
        message('Subsetting PM25 by "Coal" in Energy Industry Sector.')
        coal_sectors = subset(NEI_SCC, grepl("Coal", NEI_SCC$EI.Sector))
        NEI_PM25_COAL <<- subset(NEI_PM25, SCC %in% coal_sectors$SCC)
        write.csv(
            NEI_PM25_COAL, 
            file=NEI_PM25_COAL_FILE, 
            row.names=FALSE
        )
    }
    
    # Summarize the data for fine particulate matter from coal sources. 
    if(!exists('NEI_PM25_COAL_BY_YEAR') || !file.exists(NEI_PM25_COAL_BY_YEAR_FILE)){
        message('Calculating PM25 from Coal by year.')
        NEI_PM25_COAL_BY_YEAR <<- aggregate(
            Emissions ~ year, 
            data=NEI_PM25_COAL, 
            FUN=sum
        )
        write.csv(
            NEI_PM25_COAL_BY_YEAR, 
            file=NEI_PM25_COAL_BY_YEAR_FILE, 
            row.names=FALSE
        )
    }
}


etl.load = function(data, refresh=FALSE){
    # loads the data slice we need for our plot exploration
    etl.transform(refresh=refresh)
    
    if( 'pm25' == data ){
        return(NEI_PM25)
    }
    
    if( 'pm25_by_year' == data ){
        return(NEI_PM25_BY_YEAR)
    }
    
    if( 'pm25_baltimore' == data ){
        return(NEI_PM25_BALTIMORE)
    }
    
    if( 'pm25_baltimore_vehicle' == data ){
        return(NEI_PM25_BALTIMORE_VEHICLE)
    }
    
    if( 'pm25_baltimore_vehicle_by_year' == data ){
        return(NEI_PM25_BALTIMORE_VEHICLE_BY_YEAR)
    }
    
    if( 'pm25_baltimore_by_year' == data ){
        return(NEI_PM25_BALTIMORE_BY_YEAR)
    }
    
    if( 'pm25_los_angeles' == data ){
        return(NEI_PM25_LOS_ANGELES)
    }
    
    if( 'pm25_los_angeles_vehicle' == data ){
        return(NEI_PM25_LOS_ANGELES_VEHICLE)
    }
    
    if( 'pm25_los_angeles_vehicle_by_year' == data ){
        return(NEI_PM25_LOS_ANGELES_VEHICLE_BY_YEAR)
    }
    
    if( 'pm25_los_angeles_by_year' == data ){
        return(NEI_PM25_LOS_ANGELES_BY_YEAR)
    }
    
    if( 'pm25_coal' == data ){
        return(NEI_PM25_COAL)
    }
    
    if( 'pm25_coal_by_year' == data ){
        return(NEI_PM25_COAL_BY_YEAR)
    }
}
