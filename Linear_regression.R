library("ggplot2")

# Построим линейную регрессию зависимости содержания озона от температуры.
# Для начала убедимся, что переменные значимо коррелируют.

df<- airquality
shapiro.test(df$Ozone)
shapiro.test(df$Temp)
cor.test(df$Ozone, df$Temp, method = "spearman")

# P-value намного меньше 0.05, поэтому связь между переменными является значимой.
# Построим линейную модель.

fit <- lm(Ozone ~ Temp, df)
fit$coefficients
ggplot(df, aes(Temp, Ozone)) +
  geom_point() + 
  geom_smooth(method = "lm")
