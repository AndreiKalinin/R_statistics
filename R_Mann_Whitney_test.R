library("ggplot2")

# ��������, ����������� �� ���������� ����� � ��� � ����.

df_ozone <- subset(airquality, Month == 5 | Month == 6)
df_ozone$Month <- as.factor(df_ozone$Month)
shapiro.test(df_ozone$Ozone[df_ozone$Month == 5])
shapiro.test(df_ozone$Ozone[df_ozone$Month == 6])

# � ������ ������� ������������� ���������� �� �����������, ������� � ��������
# �������� �������� ����� ������������ �������� �����-�����.

wilcox.test(Ozone ~ Month, df_ozone)

ggplot(df_ozone, aes(Month, Ozone)) + geom_boxplot()

ggplot(df_ozone, aes(Month, Ozone)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
  stat_summary(fun = mean, geom = "point", size = 4)

# P-value = 0.1925, �������������, ����� ������� �����, ��� �������� �������� ���.
