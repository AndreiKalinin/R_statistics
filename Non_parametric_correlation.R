library("ggplot2")

# ��������, ���� �� ����������� ���������� ����� �� ���������� ���������, ����������� � 
# �������� �����.

df <- airquality
sapply(df[1:4], shapiro.test)

# ������������� ���������� ����� ���������� �� �����������. �������������. ��� �������� 
# ����������� �������� ����� ������������ �������� ������������ ����������.
# ��� �������������� ���� ����� ����� ������������ ����� �������.

cor.test(df$Ozone, df$Solar.R, method = "spearman")
cor.test(df$Ozone, df$Solar.R, method = "kendall")
ggplot(df, aes(Solar.R, Ozone)) + geom_point()

# ������� ���������� ��������� �� ���������� ����� �������� �������� �������� ������,
# �������� �������� �������� - ����� ������.

cor.test(df$Ozone, df$Temp, method = "spearman")
cor.test(df$Ozone, df$Temp, method = "kendall")
ggplot(df, aes(Temp, Ozone)) + geom_point()

# ������� ����������� �� ���������� ����� ������� �������� ������������ ��������, 
# �������� ������������ �������� - �������.

cor.test(df$Ozone, df$Wind, method = "spearman")
cor.test(df$Ozone, df$Wind, method = "kendall")
ggplot(df, aes(Wind, Ozone)) + geom_point()

# �������� �������� �������� ����� ����������� ����� � ��������� ����� ����������
# ������� �����, �������� �������� �������� - ����� ������.
