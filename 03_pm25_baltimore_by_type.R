# Homework assignment for Coursera exdata-011
# Week 3
# 01_pm25_by_year.R
# See etl.R for the data extract/transform/load routines.  We consolidated
# them into one file so each of the plots  can leverage the same process
# without duplicating the code.  This saves a lot of time during
# development since we arent reloading the data to develop the graph.

source('etl.R')

pm25_baltimore = etl.load('pm25_baltimore')

# open the png for writing but make sure we close it even in the
# event of an error.
png('plot3.png')

tryCatch({
    m = ggplot(pm25_baltimore, aes(
        x=year, 
        weight=Emissions
    ))
    m = m + ggtitle("Baltimore - Total PM2.5 by Type")
    m = m + ylab("Baltimore - Total PM2.5 (in thousands)")
    m = m + geom_histogram(binwidth=0.5)
    m = m + facet_wrap(~ type)
    print(m)
}, finally=dev.off())

