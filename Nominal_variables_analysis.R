library("ggplot2")

# ��������, ����������� ���������� ���� � ������������ ������ ��� ���� 85 �������� �� 
# ���������� � ���� � �������.

df <- subset(airquality, Month == 7 | Month == 8)
df$hot_days <- ifelse(df$Temp >= 85, "hot", "not_hot")

chisq.test(factor(df$Month), factor(df$hot_days))
fisher.test(factor(df$Month), factor(df$hot_days))

# �� ������ ���������� 5% �������� ��-������� � ������ �������� ������ ���������������
# �� ���������� �������� �������� � ���������� ���� � ������������ ������ ��� ���� 
# 85 �������� � ���� � �������.

ggplot(df, aes(x = factor(Month), fill = hot_days)) +
  geom_bar(position = "dodge") + xlab("Month")

mosaicplot(table(df[c(5, 7)]), color = T, shade = T, main = "Standartized residuals")
