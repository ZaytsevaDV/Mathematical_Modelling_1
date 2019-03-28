library(tidyverse)
library(rnoaa)
library(lubridate)

station_data= read.csv('station_data.csv', header =  TRUE, sep=",", dec=".")
Lipetsk = data.frame(id= "Lipetsk", latitude= 52.62, longitude= 39.6)
Lipetsk_around= meteo_nearby_stations(lat_lon_df =  Lipetsk, station_data =  station_data,
                                       limit= 8, var =  c("PRCP", "TAVG"),
                                       year_min = 2005, year_max=2015)
All_Lipetsk_data= list()
for(i in 1: 8){
  Lipetsk_id = Lipetsk_around[["Lipetsk"]] [["id"]] [ i]
  All_Lipetsk_data[i] = list(meteo_tidy_ghcnd(stationid =  Lipetsk_id,
                                               var= c("PRCP", "TAVG"),
                                               date_min = "2005-04-01",
                                               date_max = "2015-10-31"))}

act_sum5= vector()
d_coeff= vector()

for(year in 2005 : 2015){
  for( month in 4: 10){
    month_sort= vector()
    for(station in 1 : 8){
      month_sort= (filter(All_Lipetsk_data[[station]],
                          date>ymd(paste(toString(year),
                                             toString(month), "01", sep="-"))
                          &date<ymd(paste(toString(year),toString(month+1),"01",sep="-"))))
      c[station]=sum(month_sort[month_sort>50],na.rm = TRUE)/10
    }
    act_sum5=c(act_sum5,mean(a))
    d_coeff=c(d_coeff,length(month_sort[month_sort>70]/(length(month_sort[station]))))
  }
}

