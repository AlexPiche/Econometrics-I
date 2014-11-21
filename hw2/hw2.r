library('zoo')

tbills <- read.table('tbrate.txt')
names(tbills) <- c('Obs', 'Year', 'Q', 'r', 'GDP', 'pi')
tbills <- zoo(tbills)

tbills$dGDP1 <- lag(tbills$GDP,-1) - lag(tbills$GDP,-2)

tbills$dr <- tbills$r - lag(tbills$r,-1)
tbills$dr1 <- lag(tbills$dr, -1, na.pad=TRUE)
tbills$dr2 <- lag(tbills$dr, -2, na.pad=TRUE)
tbills$pi1 <- lag(tbills$pi, -1, na.pad=TRUE)

tbills <- subset(tbills, Year > 1950 | Q==4)

tbills.lm <- lm(dr~pi1+dGDP1+dr1+dr2, data= tbills)
tbills.res <- residuals(tbills.lm)
tbills$fit <- fitted(tbills.lm)

#residuals from the regression
plot(tbills$Year, tbills.res, pch=20) 

#fitted vs actual values of dr
plot(tbills$Year, tbills$dr, type='points') + lines(tbills$Year, tbills$fit) 
ggplot(tbills, aes(Year, dr)) + geom_point(aes(Year, dr)) + geom_line(aes(Year, fit))

#regress the residuals on the fitted value and a constant
res.lm <- lm(tbills.res ~ tbills.fit)

#regress the fitted value on the residuals and a constant
fit.lm <- lm(tbills.fit ~ tbills.res)

########################## 2.24 ###########################

#ols w/o inflation
dr.lm <- lm(dr~dGDP1+dr1+dr2, data=tb2)
e <- residuals(dr.lm)

#ols of inflation
infl.lm <- lm(pi1 ~ dGDP1 + dr1 + dr2, data = tbills)
v <- residuals(infl.lm)

#regress e ~ v
ll <- lm(e~v+0)
