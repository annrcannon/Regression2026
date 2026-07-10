si <- read_csv("possible data/N_09_extent_v4.0.csv")

#Linear regression
 # original data

si_lm1 <- lm(extent~year, data=si)
tidy(si_lm1) |> kable(digits=3)

par(mfrow=c(2,2))

plot(si_lm1)

 #centered year

si |>
  mutate(c_year = year-1978) -> si

 #regression with centered year

si_lmc <- lm(extent~c_year, data=si)
tidy(si_lmc) |> kable(digits=3)

 #quadratic regression

si_qr <- lm(extent~poly(c_year,2), data=si)
tidy(si_qr) |> kable(digits=3)

 #cubic regression

si_cu <- lm(extent~poly(c_year,3), data=si)
tidy(si_cu) |> kable(digits=3)

 #compare models

model1_glance <- glance(si_lmc) |>
  select(r.squared, adj.r.squared, AIC, BIC)
model2_glance <- glance(si_qr) |>
  select(r.squared, adj.r.squared, AIC, BIC)
model3_glance <- glance(si_cu) |>
  select(r.squared, adj.r.squared, AIC, BIC)

model1_glance |>
  bind_rows(model2_glance) |>
  bind_rows(model3_glance) |>
  bind_cols(model = c("Linear", "Quadratic", "Cubic")) |>
  select(model, everything()) |>
  kable(digits = 3)

plot(si_cu)

#Plot cubic function with data

ggplot(data=si, aes(x=year, y=extent))+
  geom_point() +
  geom_smooth(formula=y~poly(x, 3), method="lm")

#Plot linear step function

mod <- lm(extent~I(year < 1990) + I(year>= 1990 & year <2005)
          + I(year>=2005), data=si)
p <- predict(mod)
ggplot(si, aes(x=year, y=extent))+
  geom_point() +
  geom_line(aes(x=year, y=p), color="blue")

#compute linear spline function
d <- tibble(
  b1 = si$year,
  b2 = ifelse(si$year< 1990, 0, si$year-1990),
  b3 = ifelse(si$year<2005, 0, si$year-2005),
  y = si$extent
)


lm(y~ b1+b2+b3, data=d) |>
  tidy() |>
  knitr::kable(digits=3)

p_ <- predict(lm(y~b1+b2+b3, data=d))

#Plot linear spline function with original data

ggplot(d, aes(x=b1, y=p_)) +
  geom_point() +
  geom_point(aes(x=b1, y=y), color="blue")

#compute cubic spline function

d |>
  mutate(b2 = b1^2,
         b3 = b1^3,
         b4 = ifelse(b1>1990, (b1-1990)^3,0),
         b5 = ifelse(b1>2005, (b1-2005)^3, 0)
  ) -> d

si_cu_sp <- lm(y~b1+b2+b3+b4+b5, data=d) 
  tidy(si_cu_sp) |>
  kable(digits=3)

#Plot cubic spline function

p_2 <- predict(lm(y~b1+b2+b3+b4+b5, data=d))

ggplot(d, aes(x=b1, y=p_2)) +
  geom_point() +
  geom_point(aes(x=b1, y=y), color="blue") +
  geom_smooth(formula = y~poly(x,3), method="lm")


glance(si_cu_sp) |>
  select(r.squared, adj.r.squared, AIC, BIC)  


#Redo spline with just one knot at 2000

d |>
  mutate(b2 = b1^2,
         b3 = b1^3,
         b4 = ifelse(b1>2000, (b1-2000)^3,0)
  ) -> d

si_cu_sp_1k <- lm(y~b1+b2+b3+b4, data=d) 
tidy(si_cu_sp_1k) |>
  kable(digits=3)

#Plot cubic spline function

p_3 <- predict(lm(y~b1+b2+b3+b4, data=d))

ggplot(d, aes(x=b1, y=p_3)) +
  geom_point() +
  geom_point(aes(x=b1, y=y), color="blue") +
  geom_smooth(formula = y~poly(x,3), method="lm")


glance(si_cu_sp_1k) |>
  select(r.squared, adj.r.squared, AIC, BIC)  

#Redo cubic spline with 3 knots 1990, 2001, 2012

d |>
  mutate(b2 = b1^2,
         b3 = b1^3,
         b4 = ifelse(b1>1990, (b1-1990)^3,0),
         b5 = ifelse(b1>2001, (b1-2001)^3, 0),
         b6 = ifelse(b1>2012, (b1-2012)^3,0)
  ) -> d

si_cu_sp_3k <- lm(y~b1+b2+b3+b4+b5+b6, data=d) 
tidy(si_cu_sp) |>
  kable(digits=3)

#Plot cubic spline function

p_4 <- predict(lm(y~b1+b2+b3+b4+b5+b6, data=d))

ggplot(d, aes(x=b1, y=p_4)) +
  geom_point() +
  geom_point(aes(x=b1, y=y), color="blue") +
  geom_smooth(formula = y~poly(x,3), method="lm")


glance(si_cu_sp_3k) |>
  select(r.squared, adj.r.squared, AIC, BIC) 


#Lab stuff

ggplot(seaice, aes(x=c_year, y=area)) +
  geom_point()

model1 <- lm(area ~ c_year, data = seaice)
tidy(model1) |> kable(digits=3)

model2 <- lm(area ~ poly(c_year , 2), data = seaice)
tidy(model2) |> kable(digits=3)

model3 <- lm(area ~ poly(c_year , 3), data = seaice)
tidy(model3) |> kable(digits=3)

model4 <- lm(area ~ poly(c_year , 4), data = seaice)
tidy(model4) |> kable(digits=3)

model5 <- lm(area ~ poly(c_year , 5), data = seaice)
tidy(model5) |> kable(digits=3)

anova(model5)
anova(model3)
anova(model1)

ggplot(seaice, aes(x=c_year, y=area)) +
  geom_point()+
  geom_smooth(formula = y~poly(x,3), method="lm")

# choose knots at 10, 20, 30

model6 <- lm(area~bs(c_year, knots=c(10,20,30)), data=seaice)
coef(summary(model6))
anova(model6)

model7 <- lm(area~bs(c_year, df=6),data=seaice)
coef(summary(model7))


#natural splines
model8 <- lm(area ~ ns(c_year , df = 6), data = seaice)
coef(summary(model8))

#compare models

model3_glance <- glance(model3) |>
  select(r.squared, adj.r.squared, AIC, BIC)
model5_glance <- glance(model5) |>
  select(r.squared, adj.r.squared, AIC, BIC)
model6_glance <- glance(model6) |>
  select(r.squared, adj.r.squared, AIC, BIC)
model7_glance <- glance(model7) |>
  select(r.squared, adj.r.squared, AIC, BIC)
model8_glance <- glance(model8) |>
  select(r.squared, adj.r.squared, AIC, BIC)

model3_glance |>
  bind_rows(model5_glance) |>
  bind_rows(model6_glance) |>
  bind_rows(model7_glance) |>
  bind_rows(model8_glance) |>
  bind_cols(model = c("Model3", "Model5", "Model6",
                      "Model7", "Model8")) |>
  select(model, everything()) |>
  kable(digits = 3)

par(mfrow=c(2,2))

plot(model3)
plot(model5)
plot(model6)
plot(model7)
plot(model8)