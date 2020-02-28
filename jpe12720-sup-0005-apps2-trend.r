## Script for estimating trend from Swedish bird survey data

## To install package poptrend, run the following two lines:
## > library(devtools) 
## > install_github("jknape/poptrend")

library(poptrend)

load("goldcrestData.Rdata")
par(mfcol = c(2,2))
ylim = c(0,1.5)

# Fit trend model with temporal random effects and automatic selection of degrees of freedom
trend = fitTrend(count ~ trend(yr, k = 8, tempRE = TRUE, type = "smooth") + s(site, bs = "re") + offset(log(lineCov)) + s(observerAge, k = 20) + s(day) + firstSurvey + s(latitude), 
                 family = quasipoisson, data = goldcrest)

# Fit trend model without temporal random effects and with automatic selection of degrees of freedom
trendGAM = fitTrend(count ~ trend(yr, k = 8, tempRE = FALSE, type = "smooth") + s(site, bs = "re") + offset(log(lineCov)) + s(observerAge, k = 20) + s(day) + firstSurvey + s(latitude), 
                    family = quasipoisson, data = goldcrest)

# Fit trend model with temporal random effects and fixed degrees of freedom
trendFX = fitTrend(count ~ trend(yr, k = 8, fx = TRUE, tempRE = TRUE, type = "smooth") + s(site, bs = "re") + offset(log(lineCov)) + s(observerAge, k = 20) + s(day) + firstSurvey + s(latitude), 
                   family = quasipoisson, data = goldcrest)

# Fit trend model without temporal random effects and with fixed degrees of freedom
trendGAMFX = fitTrend(count ~ trend(yr, k = 8, fx = TRUE, tempRE = FALSE, type = "smooth") + s(site, bs = "re") + offset(log(lineCov)) + s(observerAge, k = 20) + s(day) + firstSurvey + s(latitude), 
                      family = quasipoisson, data = goldcrest)


#Plot trends
par(mfcol = c(2,2), cex.main = 1, font.main = 1, bty = "L", mar = c(2,2,2,.5), oma = c(0,2,0,0))
plot(trend, ylim = c(0,1.6), ciBase = mean, xlab = "year", main = "random year effects, automatic df")
mtext("trend", side = 2, line = 3)
plot(trendGAM, ylim = c(0,1.6), ciBase = mean, xlab = "year", main = "no random year effects, automatic df")
mtext("trend", side = 2, line = 3)
plot(trendFX, ylim = c(0,1.6), ciBase = mean,xlab = "year", main = "random year effects, fixed df")
plot(trendGAMFX, ylim = c(0,1.6), ciBase = mean, xlab = "year", main = "no random year effects, fixed df")



