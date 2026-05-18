

library(tidyverse)
library(tidymodels)
library(lme4)
library(broom.mixed)

m1 <- lmer(ambiguity ~ ideology + incumbent + (1 |distID),data = ambiguity)

amb.mod1 <- lmer(ambiguity ~ democrat + incumbent +
                   demHeterogeneity + attHeterogeneity + mismatch + ideology +
                   distLean + (1 |distID), data=ambiguity)

amb.mod2 <- lmer(ambiguity ~ democrat + incumbent +
                   demHeterogeneity + attHeterogeneity + ideology + distLean +
                   (1 |distID), data=ambiguity)

amb.mod3 <- lmer(ambiguity ~ democrat + incumbent +
                   demHeterogeneity + attHeterogeneity + mismatch + ideology +
                   distLean + democrat:ideology + (1 |distID), data=ambiguity)
