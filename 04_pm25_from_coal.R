# Homework assignment for Coursera exdata-011
# Week 3
# 01_pm25_by_year.R
# See etl.R for the data extract/transform/load routines.  We consolidated
# them into one file so each of the plots  can leverage the same process
# without duplicating the code.  This saves a lot of time during
# development since we arent reloading the data to develop the graph.

source('etl.R')

pm25_coal_by_year = etl.load('pm25_coal_by_year')

with(pm25_coal_by_year, {
    
    # open the png for writing but make sure we close it even in the
    # event of an error.
    png('plot4.png')
    
    tryCatch({
        barplot(
            Emissions/1000.0,
            names.arg=year,
            main='Total PM2.5 from Coal Emissions by Year',
            xlab='Year',
            ylab='Total PM2.5 (in thousands)'
        )
    }, finally=dev.off())
    
})
