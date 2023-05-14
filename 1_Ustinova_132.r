install.packages("tidyverse")
install.packages("rnoaa")
library(tidyverse)
library(rnoaa)
library(dplyr)
library(lubridate)

# Скачиваем станции
station_data = ghcnd_stations()
write.csv(station_data, file = "station_data.csv")
station_data = read_csv("station_data.csv")
# После поучения списка станция получим список ближайших станций к столице региона и координатами его столицы
stavropol = data.frame(id="STAVROPOL", latitude = 44.953551,
                       longitude = 43.344521)
stavropol_around = meteo_nearby_stations(lat_lon_df = stavropol, station_data = station_data,
                                         limit = 25, var = c("PRCP","TAVG"),
                                         year_min = 1990, year_max = 1991)
stavropol_id = stavropol_around[["STAVROPOL"]][["id"]][1]
all_stavropol_data = meteo_tidy_ghcnd(stationid = stavropol_id)

summary(all_stavropol_data)

all_stavropol_data$date
class(all_stavropol_data$date)
all_stavropol_data$date+1
as.numeric(all_stavropol_data$date)

all_stavropol_data1 = all_stavropol_data %>% mutate(
  year = year(all_stavropol_data$date),
  month = month(all_stavropol_data$date),
  day = yday(all_stavropol_data$date)
) %>% select(year, month, day, tavg)

all_stavropol_data2 = all_stavropol_data1 %>% mutate(tavg = case_when(TRUE ~ tavg/10)
)
all_stavropol_data2 = filter(all_stavropol_data2,year > 1989 & year < 1991)

# перевод на в 0 и температуры меньше 10 в 0
all_stavropol_data2[is.na(all_stavropol_data2$tavg),"tavg"] = 0
all_stavropol_data2[all_stavropol_data2$tavg<10, "tavg"] = 0
summary(all_stavropol_data2)

#сгруппируем по году и месяцу и просуммируем температуры
group_meteodata =all_stavropol_data2 %>% group_by(year,month)

sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)

sumT_month=groups_month%>%summarise(St=mean(tsum))

# подготовка данных - стандарт с сайта
y = 1.0 #коэффициент для экспозиции склона
afi = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)#константа, из табл.1

bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)#константа, из табл.1
di = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)#отношение числа i-го месяца, входящих в период вегетации культуры, к общему числу дней в месяце из табл.1
Kf = 300 # коэффициент использования ФАР посевом
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 # коэффициент «Сумма частей основной и побочной продукции
Ej = 25 # коэффициент «Стандартная влажность культуры

#Рассчитаем Fi по месяцам
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)

#Рассчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))

#Рассчитаем урожай
Yield = (sum(sumT_month$Yi))
Yield #Ответ 19,67193

