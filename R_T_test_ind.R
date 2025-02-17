library("ggplot2")

# �������� �������� ���������� ������� ���������� ������� � ��������.

df_temp <- subset(airquality, Month == 8 | Month == 9)
df_temp$Month <- as.factor(df_temp$Month)
shapiro.test(df_temp$Temp[df_temp$Month == 8])
shapiro.test(df_temp$Temp[df_temp$Month == 9])
bartlett.test(Temp ~ Month, df_temp)
hist(df_temp$Temp, col = "royalblue")

# ������������� ������������� �����������, ���������� ������������ ���������
# ���������, ������� ��� �������� ����� ������������ t-�������� ���������.

t.test(Temp ~ Month, df_temp)

ggplot(df_temp, aes(Month, Temp)) + geom_boxplot()

ggplot(df_temp, aes(Month, Temp)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
  stat_summary(fun = mean, geom = "point", size = 4)

# �����: �������� ������������� �������.

# �������� �������� � ���, ��� ������� ����������� � ������� �� ��������� 80
# �������� �� ����������.

t.test(df_temp$Temp[df_temp$Month == 8], mu = 80, alternative = "greater")

# �����: ������� �������� �����������, ������� ����������� ������� ���� 80 ��������.