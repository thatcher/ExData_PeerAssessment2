# Homework assignment for Coursera exdata-011
# Week 3

# See https://github.com/thatcher/ExData_PeerAssessment2/blob/master/etl.R 
# for the data extract/transform/load routines.  We consolidated
# them into one file so each of the plots  can leverage the same process
# without duplicating the code.  This saves a lot of time during
# development since we arent reloading the data to develop the graph.

source('etl.R')

pm25_by_year = etl.load('pm25_by_year')

# Finally construct the plot
with(pm25_by_year, {
    
    png('plot1.png')
    
    tryCatch({
        barplot(
            Emissions/1000.0,
            names.arg=year,
            main='Total PM2.5 by Year',
            xlab='Year',
            ylab='Total PM2.5 (in thousands)'
        )
    }, finally=dev.off())
    
})


