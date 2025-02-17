library("ggplot2")

# �������� �������� ��������� ����������� ���������� ����� �� �����������.
# ��� ������ ��������, ��� ���������� ������� �����������.

df<- airquality
shapiro.test(df$Ozone)
shapiro.test(df$Temp)
cor.test(df$Ozone, df$Temp, method = "spearman")

# P-value ������� ������ 0.05, ������� ����� ����� ����������� �������� ��������.
# �������� �������� ������.

fit <- lm(Ozone ~ Temp, df)
fit$coefficients
ggplot(df, aes(Temp, Ozone)) +
  geom_point() + 
  geom_smooth(method = "lm")
