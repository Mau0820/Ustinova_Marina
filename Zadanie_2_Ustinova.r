# создайте модель множественной линейной регрессии ночных потоков паров воды за весенний период 
# 2013 года по данным измерений методом турбулентной пульсации

library(dplyr)
library(tidyverse)
library(ellipse)
library(lubridate)
library(car) #библиотека для доп регрессион анализа
library(caret) #библиотека для доп регрессион анализа

# Загрузка данных
#tbl1 = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1", skip = 1, 
               #na =c("","NA","-9999","-9999.0"), comment=c("["))
tbl = read.csv("eddypro.csv", skip = 1, 
                na =c("","NA","-9999","-9999.0"), comment=c("["))
# ПОДГОТОВКА ДАННЫХ К АНАЛИЗУ
tbl = tbl[-1,]
tbl

tbl=select(tbl,-(roll)) #в этой строчке нет значений

tbl$date <- as.Date (tbl$date, format = "%Y-%m-%d")

# Добавляем дополнительные столбцы с годом и месяцем замера
tbl = mutate(tbl, month = months(tbl$date))
tbl = mutate(tbl, year = year(tbl$date))

# Делаем все текстовые значения факторными
#tbl=tbl|>mutate_if(is.character,factor)
#glimpse(tbl)       

# Меняем некоректные символы из названий переменных
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>%
  str_replace_all("[?]","_quest_") %>%
  str_replace_all("[*]","_star_") %>%
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_")
glimpse(tbl)


glimpse(tbl)
# Оставляем только ночные измерения
tbl = filter(tbl, daytime %in% FALSE)
# Оставляем необходимый год
tbl = filter(tbl, year %in% 2013)
# Оставляем необходимые месяцы
tbl = filter(tbl, month %in% c("Сентябрь","Октябрь","Ноябрь"))
unique(tbl$month) # проверяем какие месяца остались
# Убираем пустые значения
tbl = drop_na(tbl)

#######ДЕЛЕНИЕ НА ТОЛЬКО ЧИСЛОВЫЕ ДАННЫЕ И НЕ ЧИСЛОВЫЕ######

#проверяем тип переменных
sapply(tbl,is.numeric)

tbl_numeric = tbl[,sapply(tbl,is.numeric)]#числинные данные

tbl_non_numeric = tbl[,!sapply(tbl,is.numeric)]#не числинные данные

# Проводим корреляционный анализ
cor_td = cor(tbl_numeric)%>% as.data.frame %>% select(co2_flux)

cor_td
# Оставляем столбец имен с эмиссией CO2 и со всеми значениями
varsCor2 = row.names(cor_td)
# Оставляем столбец имен с эмиссией CO2 и значениями > 0,1
varsCor1 = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
varsCor1

# запись параметров с кореляцией >0,1 для модели регрессионного анализа
formula1 = as.formula(paste("co2_flux~", paste(varsCor1,collapse = "+"), sep=""))
formula1

# запись всех параметров  для второй модели регрессионного анализа
formula2 = as.formula(paste("co2_flux~", paste(varsCor2,collapse = "+"), sep=""))
formula2

### РЕГРЕССИЯ ########################################## РЕГРЕССИЯ #######################
### РЕГРЕССИЯ ########################################## РЕГРЕССИЯ #######################

### МОДЕЛЬ 1 ############################# МОДЕЛЬ 1 #########################

#прописываем модель1 (зависимая переменная эмиссия CO2/независимые - коеф кор>0,1)
Model1 = lm(formula1, data = tbl_numeric)
Model1
# # ЕСЛИ ОШИБКА - Предупреждение "отклик появился справа и поэтому был удален" указывает на то, 
# что зависимая переменная (отклик) была указана в правой части уравнения, вместо левой, что является
# нестандартным для большинства моделей регрессии в R. Обычно зависимая переменная указывается в левой 
# части уравнения. Это предупреждение указывает на то, что R автоматически переместил зависимую переменную 
# в левую часть уравнения и проигнорировал ее на правой стороне.
# 
# #Второе предупреждение "проблема с термом 2 в 'model.matrix': не присвоены колонки" указывает на то, что 
# второй фактор в вашей модели не может быть преобразован в матрицу модели. Это может произойти, если фактор 
# содержит неизвестные значения или если некоторые уровни категориального фактора отсутствуют в данных. 
# Попробуйте проверить данные и убедитесь, что все уровни категориальных факторов присутствуют в данных 
# и что они правильно записаны.

# Оценка мультиколлинеарности #Variance Inflation Factors
vif_values <- car::vif(Model1)
vif_values = as.matrix(vif_values)

# новый набор переменных без переменных которые имеют значение VIF больше 5. - НЕ ТРЕБУЕТСЯ
# vars_to_remove <- c("e", "specific_humidity")
# formula1_new <- update(formula1, paste(". ~ . -", paste(vars_to_remove, collapse = " - ")))
# 
# Model1_new = lm(formula1_new, data = tbl_numeric)

# коэффицент детерминации
summary(Model1)$r.squared

# Оценка значимости переменных
Anova_results <- car::Anova(Model1, type="III")
summary(Anova_results)

# Оценка важности переменных
varImp_values <- caret::varImp(Model1)
print(varImp_values)

# Вывод результатов модели
summary(Model1)

# Оценка значимости переменных
anova(Model1)

# Построение графиков для визуализации результатов
plot(Model1)


summary(Model1)$r.squared
#varsRegr = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude 

coef(Model1) # коэфицены модели

############################# МОДЕЛЬ 2 ############################################

#прописываем модель2 (зависимая переменная эмиссия CO2/независимые - все переменные)
Model2 = lm(formula2, data = tbl_numeric) 
Model2
# Вывод результатов модели
summary(Model2)

# Оценка значимости переменных
anova(Model2)

# Построение графиков для визуализации результатов
plot(Model2)

# коэффицент детерминации
summary(Model2)$r.squared

coef(Model2) # коэфиценты модели

anova(Model1,Model2) #Сравнение двух моделей
