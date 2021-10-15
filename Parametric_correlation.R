library("ggplot2")

# Проверим, существует ли связь между температурой и солнечным излучением в июне.
# Для начала определим, как распределены данные.

df <- subset(df, Month == 6)
hist(df$Temp)
hist(df$Solar.R)
shapiro.test(df$Temp)
shapiro.test(df$Solar.R)
ggplot(df, aes(Temp)) + geom_boxplot()
ggplot(df, aes(Solar.R)) + geom_boxplot()

# Данные распределены нормально, для определения наличия связи между показателями будем
# использовать коэффициент корреляции Пирсона.

cor.test(df$Temp, df$Solar.R)
ggplot(df, aes(Solar.R, Temp)) + geom_point()

# Вывод: в июне связь между температурой и солнечным излучением слабая.