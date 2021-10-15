library("ggplot2")

# Проверим, есть ли зависимость содержания озона от солнечного излучения, температуры и 
# скорости ветра.

df <- airquality
sapply(df[1:4], shapiro.test)

# Распределение содержания озона отличается от нормального. Следовательно. для проверки 
# взаимосвязи факторов будем использовать ранговые коэффициенты корреляции.
# Для характеристики силы связи будем использовать шкалу Чеддока.

cor.test(df$Ozone, df$Solar.R, method = "spearman")
cor.test(df$Ozone, df$Solar.R, method = "kendall")
ggplot(df, aes(Solar.R, Ozone)) + geom_point()

# Влияние солнечного излучения на содержание озона согласно критерию Спирмена слабое,
# согласно критерию Кенделла - очень слабое.

cor.test(df$Ozone, df$Temp, method = "spearman")
cor.test(df$Ozone, df$Temp, method = "kendall")
ggplot(df, aes(Temp, Ozone)) + geom_point()

# Влияние температуры на содержание озона сильное согласно коэффициенту Спирмена, 
# согласно коэффициенту Кенделла - среднее.

cor.test(df$Ozone, df$Wind, method = "spearman")
cor.test(df$Ozone, df$Wind, method = "kendall")
ggplot(df, aes(Wind, Ozone)) + geom_point()

# Согласно критерию Спирмена между содержанием озона и скоростью ветра существует
# средняя связь, согласно критерию Кенделла - связь слабая.
