library("ggplot2")

# Проверим гипотезу оравенстве средних температур августа и сентября.

df_temp <- subset(airquality, Month == 8 | Month == 9)
df_temp$Month <- as.factor(df_temp$Month)
shapiro.test(df_temp$Temp[df_temp$Month == 8])
shapiro.test(df_temp$Temp[df_temp$Month == 9])
bartlett.test(Temp ~ Month, df_temp)
hist(df_temp$Temp, col = "royalblue")

# Распределение соответствует нормальному, требование гомогенности дисперсий
# выполнено, поэтому для проверки будем использовать t-критерий Стьюдента.

t.test(Temp ~ Month, df_temp)

ggplot(df_temp, aes(Month, Temp)) + geom_boxplot()

ggplot(df_temp, aes(Month, Temp)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
  stat_summary(fun = mean, geom = "point", size = 4)

# Вывод: различие статистически значимо.

# Проверим гипотезу о том, что средняя температура в августе не превышает 80
# градусов по Фаренгейту.

t.test(df_temp$Temp[df_temp$Month == 8], mu = 80, alternative = "greater")

# Вывод: нулевая гипотеза отклоняется, средняя температура августа выше 80 градусов.