# Задание 1 Эминян Г.Г. – для региона 61 рассчитайте урожайность пшеницы в 1999 году, 
# Взяв для рассчета средние суммы активных температур за предыдущие 2 года, с метеостанций в радиусе не более 100 км
# 61 регион - Ростовская область область

library(tidyverse)
library(rnoaa)
library(lubridate)

#Данные для расчета:
ai = c(0.00, 32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)# константа по табл. 1. Создаем вектор
bi = c(0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)# константа по табл. 1. Создаем вектор
di = c(0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)# отношение числа дней i-го месяца, входящих в период вегетации культуры, к общему числу дней в месяце,константа по табл. 1.
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры

# Загрузка данных из сохраненного файла с данными метеостанций
station_data = read.csv('station_data.csv',header = TRUE,sep=",",dec=".")

# Создаём таблицу с именем региона и координатами его столицы (Ростовская область - г. Ростов, координаты: n 57.2000, e 39.4170)
Rostov = data.frame(id = "ROSTOV",latitude=57.2000,longitude=39.4170)

# Список метостанций на расстоянии близжайшей 100 метров.
Rostov_around=meteo_nearby_stations(lat_lon_df = Rostov, station_data = station_data,
                                    limit = 4, var = c("PRCP", "TAVG"),
                                    year_min = 1997, year_max = 1998)

Rostov_id = Rostov_around[["ROSTOV"]][["id"]][1]
all_Rostov_data = meteo_tidy_ghcnd(stationid = Rostov_id)

# x %in% y  -  выберет каждую строку, где x является одним из значений y. 
# mutate() - добавляет новые столбцы, которые являются функциями существующих столбцов.
# Filter () - позволяет выбрать подмножество наблюдений(строк), основанных на их значениях(по значениям в отдельных колонках). 
all_Rostov_data = all_Rostov_data %>% mutate(year = date %>% year()) # выделяем колонку с годом
all_Rostov_data = all_Rostov_data %>% mutate(month = date %>% month())# выделяем колонку с номером месяца года
all_Rostov_data = all_Rostov_data %>% mutate(yday = date%>% yday())# выделяем колонку с номером месяца года
all_Rostov_data = all_Rostov_data %>% mutate(tavg = tavg/10) # приводим значени температуры к нормальным значениям 
all_Rostov_data = filter(all_Rostov_data,year<1997)# выборка по заданным годам
all_Rostov_data = filter(all_Rostov_data,year>1998)# выборка по заданным годам

all_Rostov_data = all_Rostov_data %>%  group_by(id,month,year) # группировка по месяцам года
all_Rostov_data = filter(all_Rostov_data,tavg>5) #выборка среднесуточной температуры выше 5 градусов
new_data = all_Rostov_data%>% summarize(sum_temp=sum(tavg, na.rm=T)) # сумма температур выше 5 градусов по месяцам года
new_data = new_data %>% group_by(month) # группировка по месяцам
new_data = new_data %>% summarize(sum_temp = mean(sum_temp,na.rm=T)) # суммируем и выводим среднее значение суммы температур выше 5 градусов 
# по месяцам за перид 5 лет (по условиям задания)

new_data = new_data%>% mutate (a = ai, b = bi, d = di) # добавляем векторы (значения) коэффициентов к таблице
new_data = new_data%>% mutate (Fi = ((a + b * 1.0 * sum_temp) * d * Kf) / (Qj * Lj * (100-Ej))) # расчитываем формулу по месяцам

yeild = sum(new_data$Fi) # по сумме выражений по месяцам получаем урожайность 20 ц/га
