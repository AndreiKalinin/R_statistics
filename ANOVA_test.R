library("ggplot2")

# С помощью дисперсионного анализа проверим, существуют ли различия скорости ветра по месяцам.

df <- airquality
df$Month <- as.factor(df$Month) # преобразуем в фактор
hist(df$Wind)
shapiro.test(df$Wind)
bartlett.test(Wind ~ Month, df)
fit <- aov(Wind ~ Month, df)
summary(fit)

# На уровне значимости 0.05 нулевая гипотеза об отсутсвии различий отклоняется.
# С помощью теста Тьюки найдем группы. между которыми существуют различия.

TukeyHSD(fit)
ggplot(df, aes(Month, Wind)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
  stat_summary(fun = mean, geom = "point", size = 4)

# Вывод: скорость ветра значимо отличается в мае и июле, а также в мае и августе.