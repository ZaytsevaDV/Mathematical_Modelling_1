library(tidyverse)  #Целая вселенная
library(rnoaa)      #Библиотека для работы с метеостанциями
library(lubridate)  #Библиотека для работы с датами

#Данные для расчета (так как di не нулевой только с 4 по 8 месяц, то оставим только нужные значения):
ai = c(32.11, 26.31, 25.64, 23.20, 18.73)
bi = c(11.30, 9.26, 9.03, 8.16, 6.59)
#di вычисляем самостоятельно ниже
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры

station_data = ghcnd_stations() #Может занять несколько минут лучше выполнить один раз в месте с хорошим интернетом и сохранить результат
setwd("E:/R")
write.csv(station_data, file = "station_data.csv")

station_data = read.csv("station_data.csv") [,-1]

#Создадим таблицу, содержащую координаты Липецка
Lipetsk = data.frame(id = "Lipetsk", latitude = 52.62,  longitude = 39.6)
write.csv(Lipetsk, file = "Lipetsk.csv")
#Получим список всех станций вблизи Липецка
Lipetsk_around = meteo_nearby_stations(lat_lon_df = Lipetsk, station_data = station_data,
                                      limit = 8, var = c("TAVG"),
                                      year_min = 2005, year_max = 2015)
#Lipetsk_around это список, единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их удалленности от Samara

#Получим данные с 1,4 и 6 метеостанций
#Создадим пустую таблицу куда запишем данные с метеостанций
all_Lipetsk_data = data.frame()
#Для получения всех данных с метеостанции, зная ее идентификатор, используем цикл
for(i in c(1,4,6)){
  #Получим данные со стании и запишем во временную переменную
  temp = meteo_tidy_ghcnd(stationid = Lipetsk_around[["Lipetsk"]][["id"]][i], date_min="2005-04-1", date_max="2015-08-31")
  #Оставим нужные столбцы
  temp = select(temp, id, date, tavg)
  #Присоединим полученные данные в результирующую таблицу с данными
  all_Lipetsk_data = rbind(all_Lipetsk_data, temp)
}

#преобразуем дату в месяц и день и добавим столбики
all_Lipetsk_data = mutate(all_Lipetsk_data, month = month(date), day = day(date))
all_Lipetsk_data_without0 = all_Lipetsk_data #сохраним резервную таблицу без обнуления

#обнулим значение температуры в невегетативный период
all_Lipetsk_data[(all_Lipetsk_data$month == 4 & all_Lipetsk_data$day <= 14),"tavg"] = 0
all_Lipetsk_data[(all_Lipetsk_data$month == 8 & all_Lipetsk_data$day >= 16),"tavg"] = 0

#Сгруппируем по месяцам
all_Lipetsk_data = all_Lipetsk_data %>% group_by(month)
all_Lipetsk_data_without0 = all_Lipetsk_data_without0 %>% group_by(month)

#Вычислим di для каждого месяца
di = summarize(all_Lipetsk_data, di = length(tavg[tavg>70])/length(tavg))[,-1]

#Вычислим сумму температур больше 5 градусов в каждом месяце. Делим сумму на 8, т.к. у нас данные с 8ми метеостанций
St = summarize(all_Lipetsk_data_without0, St = sum(tavg[tavg>50])/10/8)[,-1]

#Найдем урожаность по формуле:
Fi = ai + bi * 1.0 * St
yield = 10^6*sum(Fi*di*Kf/(Qj*Lj*(100-Ej)))
yield
