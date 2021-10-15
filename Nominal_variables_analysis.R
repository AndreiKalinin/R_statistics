library("ggplot2")

# Проверим, различается количество дней с температурой равной или выше 85 градусов по 
# Фаренгейту в июле и августе.

df <- subset(airquality, Month == 7 | Month == 8)
df$hot_days <- ifelse(df$Temp >= 85, "hot", "not_hot")

chisq.test(factor(df$Month), factor(df$hot_days))
fisher.test(factor(df$Month), factor(df$hot_days))

# На уровне значимости 5% критерий хи-квадрат и точный критерий Фишера свидетельствуют
# об отсутствии значимых различий в количестве дней с температурой равной или выше 
# 85 градусов в июле и августе.

ggplot(df, aes(x = factor(Month), fill = hot_days)) +
  geom_bar(position = "dodge") + xlab("Month")

mosaicplot(table(df[c(5, 7)]), color = T, shade = T, main = "Standartized residuals")
