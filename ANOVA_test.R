library("ggplot2")

# � ������� �������������� ������� ��������, ���������� �� �������� �������� ����� �� �������.

df <- airquality
df$Month <- as.factor(df$Month) # ����������� � ������
hist(df$Wind)
shapiro.test(df$Wind)
bartlett.test(Wind ~ Month, df)
fit <- aov(Wind ~ Month, df)
summary(fit)

# �� ������ ���������� 0.05 ������� �������� �� ��������� �������� �����������.
# � ������� ����� ����� ������ ������. ����� �������� ���������� ��������.

TukeyHSD(fit)
ggplot(df, aes(Month, Wind)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
  stat_summary(fun = mean, geom = "point", size = 4)

# �����: �������� ����� ������� ���������� � ��� � ����, � ����� � ��� � �������.