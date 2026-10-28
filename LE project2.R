### Let's create our necessary data set.
new_data <- data %>% mutate(status = factor(status),
                            adMortality= adMortality/10,
                            gdp= log(gdp),
                            population = log(population),
                            HIV = log(HIV),
                            thin9 = log(thin9),
                            thin19 = log(thin19),
                            polio = log(100 - polio),
                            diphtheria = log(100 - diphtheria),
                            hepB = log(100 - hepB),
                            measles = log(measles2)) %>% 
                    dplyr::select(-le_transformed, -measles2)
head(new_data)
names(new_data)


##### Forward step-wise Regression:
attach(new_data)
fm1 <- lm(le~inComposition)
summary(fm1)

fm2 <- lm(le~inComposition+HIV)
summary(fm2)
vif(fm2)

fm3 <- lm(le~inComposition+HIV+adMortality)
summary(fm3)
vif(fm3)

fm4 <- lm(le~inComposition+HIV+adMortality+thin9)
summary(fm4)
vif(fm4)

fm5 <- lm(le~inComposition+HIV+adMortality+thin9+polio)
summary(fm5)
vif(fm5)

fm6 <- lm(le~inComposition+HIV+adMortality+thin9+polio+status)
summary(fm6)
vif(fm6)


par(mfrow =c(2,2))
plot(fm6)
par(mfrow = c(1,1))



##### Backward Stepwise Regression: 

bm1 <- lm(le~inComposition+adMortality+schooling+gdp+population+status+HIV+thin9+thin19+polio+diphtheria+hepB+measles)
summary(bm1)
vif(bm1)

bm2 <- lm(le~inComposition+adMortality+schooling+gdp+population+status+HIV+thin9+polio+diphtheria+hepB+measles)
summary(bm2)
vif(bm2)

bm3 <- lm(le~inComposition+adMortality+schooling+gdp+population+status+HIV+thin9+polio+hepB+measles)
summary(bm3)
vif(bm3)

bm4 <- lm(le~adMortality+schooling+gdp+population+status+HIV+thin9+polio+hepB+measles)
summary(bm4)
vif(bm4)

bm5 <- lm(le~adMortality+schooling+gdp+population+status+HIV+thin9+polio+measles)
summary(bm5)
vif(bm5)

bm6 <- lm(le~adMortality+schooling+gdp+status+HIV+thin9+polio+measles)
summary(bm6)
vif(bm6)

bm7 <- lm(le~adMortality+schooling+gdp+status+HIV+thin9+polio)
summary(bm7)
vif(bm7)

final_model = bm7



