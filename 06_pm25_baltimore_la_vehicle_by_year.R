# Homework assignment for Coursera exdata-011
# Week 3
# 01_pm25_by_year.R
# See etl.R for the data extract/transform/load routines.  We consolidated
# them into one file so each of the plots  can leverage the same process
# without duplicating the code.  This saves a lot of time during
# development since we arent reloading the data to develop the graph.

source('etl.R')
pm25_los_angeles_vehicle = etl.load('pm25_los_angeles_vehicle')
pm25_baltimore_vehicle = etl.load('pm25_baltimore_vehicle')
combined_counties = rbind(
    pm25_los_angeles_vehicle,
    pm25_baltimore_vehicle
)

png('plot6.png')
tryCatch({
    m = ggplot(combined_counties, aes(
        x=year, 
        weight=Emissions
    ))
    m = m + ggtitle("Vehicle Emissions \n Total PM2.5 by County (LA:06037/Baltimore:24510)")
    m = m + ylab("Total PM2.5")
    m = m + geom_histogram(binwidth=0.5)
    m = m + facet_wrap(~ fips)
    print(m)
}, finally=dev.off())


