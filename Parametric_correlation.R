library("ggplot2")

# ��������, ���������� �� ����� ����� ������������ � ��������� ���������� � ����.
# ��� ������ ���������, ��� ������������ ������.

df <- subset(df, Month == 6)
hist(df$Temp)
hist(df$Solar.R)
shapiro.test(df$Temp)
shapiro.test(df$Solar.R)
ggplot(df, aes(Temp)) + geom_boxplot()
ggplot(df, aes(Solar.R)) + geom_boxplot()

# ������ ������������ ���������, ��� ����������� ������� ����� ����� ������������ �����
# ������������ ����������� ���������� �������.

cor.test(df$Temp, df$Solar.R)
ggplot(df, aes(Solar.R, Temp)) + geom_point()

# �����: � ���� ����� ����� ������������ � ��������� ���������� ������.