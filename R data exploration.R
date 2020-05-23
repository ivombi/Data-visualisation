#Author:Kubam Ivo
#Date: 4/4/2020
#Course: Visualisation in Data Science

#Visual Data Exploration For the Nigerial violence project

setwd("E:/SEED/OneDrive/Msc. Biostatistics/Level Two/Second Semester/Visualisation")
violence<-read.csv("E:\\SEED\\OneDrive\\Msc. Biostatistics\\Level Two\\Second Semester\\Visualisation\\Datasets\\2016-01-01-2018-01-01-Western_Africa-Nigeria.csv")
violence<-subset(violence,violence$year!=2018)
library("altair")

#Plot one#
chart_1 <- alt$Chart(violence)$
  mark_circle()$
  encode(
    x = alt$X('month(event_date):T', title = 'Month'),
    y = alt$Y('admin1:N' , title = 'Administrative Unit'),
    size = 'mean(fatalities):Q',
    color = 'admin1:N'
  )$
  properties(
    height = 500,
    width = 400,
    title = 'Mean Fatalities over the month for different administattive Units'
  )
vegawidget::vw_examine(chart_1, mode = "code")

htmlwidgets::saveWidget(vegawidget(chart_1),'chart_1.html')

#####PLOTONE Geo Map #########
chart_2 <- alt$Chart(violence)$
  mark_circle()$
  encode(
    x = 'longitude:Q',
    y = 'latitude:Q',
    size = 'sum_fatality:Q',
    opacity = 'sum_fatality:Q',
    tooltip = c("location:N","sum_fatality:Q")
  )$
  transform_aggregate(
    sum_fatality='sum(fatalities)',
    groupby=c("location","longitude","latitude"))$
  
  properties(
    title="Total deaths by Location",
    height = 300,
    width = 500
  )
vegawidget::vw_examine(chart_2, mode = "code")

htmlwidgets::saveWidget(vegawidget(chart_2),'chart_2.html')

######Plot 3- Event type by year########
chart_3 <- alt$Chart(violence)$
  mark_bar()$
  encode(
    x = 'total_fatality:Q',
    y = alt$Y('event_type:N',sort='-x'),
    color = 'year:N',
    tooltip = c('year:N','total_fatality:Q')
  )$
  transform_aggregate(
    total_fatality ='sum(fatalities)',
    groupby = c("year","event_type")
  )$
  properties(
    width= 300,
    height = 200,
    title = "Total fatalities by Event type and Year"
  )
vegawidget::vw_examine(chart_3, mode = "code")
htmlwidgets::saveWidget(vegawidget(chart_3),'chart_3.html')

#####Plot 4: monthly total death trend##########
chart_4 <- alt$Chart(violence)$
  mark_line()$
  encode(
    x = alt$X('month:T',title = 'Event month', axis=alt$Axis(format='%b')),
    y = alt$Y('avg_fatality:Q',title = 'Average fatality'),
    color = 'year:N',
    tooltip = c('year:N','avg_fatality:Q')
  )$
  transform_timeunit(
    month='month(event_date)'
  )$
  transform_aggregate(
    avg_fatality = 'mean(fatalities)',
    groupby = c("month","year") 
  )$
  properties(
    width = 400,
    height = 200,
    title = 'Average monthly death trend by Year'
  )
vegawidget::vw_examine(chart_4, mode = "code")
htmlwidgets::saveWidget(vegawidget(chart_4),'chart_4.html')

####Plot 5:daily mean death#######
chart_5 <- alt$Chart(violence)$
  mark_point()$
  encode(
    x = alt$X('day:T',title = "Day of the week",axis = alt$Axis(format = '%a')),
    y = alt$Y('avg_fatality:Q', title = "Mean Fatality"),
    color = 'event_type:N',
    size = 'avg_fatality:Q'
  )$
  transform_timeunit(
    day = 'day(event_date)'
  )$
  transform_aggregate(
    avg_fatality = 'mean(fatalities)',
    groupby = c("day","event_type")
  )$
  properties(
    width = 300,
    height = 300
  )
vegawidget::vw_examine(chart_5, mode = "code")
htmlwidgets::saveWidget(vegawidget(chart_5),'chart_5.html')


####Plot 6: Faceted Mean fatality by event type across months#####
chart_6 <- alt$Chart(violence)$
  mark_area()$
  encode(
    x = alt$X('month(event_date):T', title = 'Month'),
    y = alt$Y('mean(fatalities):Q',title = "Mean Fatalities"),
    facet = alt$Facet('event_type:N', columns = 3),
    color = 'year:N'
  )$
  properties(
    height = 200,
    width = 300,
    title = 'Mean Fatalities over the month by Event types'
  )
vegawidget::vw_examine(chart_6, mode = "code")

htmlwidgets::saveWidget(vegawidget(chart_6),'chart_6.html')

####Plot 7: Map######
background = alt$Chart(violence)$
  mark_geoshape(
  stroke='white',
  strokeWidth=2
)$
  encode(
  color='fatalities:Q'
)$
  properties(
  width=700,
  height=500
)

labels = alt$Chart(violence)$
  mark_text()$
  encode(
  longitude='longitude:Q',
  latitude='latitude:Q',
  text='bLabel:N',
  size=alt$value(8),
  opacity=alt$value(0.6)
)$
  transform_calculate(
  "bLabel", "indexof (datum.location,' ') > 0  ? substring(datum.location,0,indexof(datum.location, ' ')) : datum.location"
)
chart_7 = background+labels
vegawidget::vw_examine(chart_7, mode = "code")

htmlwidgets::saveWidget(vegawidget(chart_7),'chart_7.html')


chart_all <- ( chart_2 & chart_3)
htmlwidgets::saveWidget(vegawidget(chart_all),'chart_all.html')
