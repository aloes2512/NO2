install.packages("mgcViz")
library(mgcViz)
library(mgcv)
url <- "https://cran.r-project.org/web/packages/mgcViz/vignettes/mgcviz.html"
browseURL(url)
url2 <-"https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/"
browseURL(url2)
url3 <- "https://cran.r-project.org/web/packages/mgcViz/vignettes/mgcviz.html"
browseURL(url3)
# make testdataset from vignette "mgcViz"
n  <- 1e3
dat <- data.frame("x1" = rnorm(n), "x2" = rnorm(n), "x3" = rnorm(n))
dat$y <- with(dat, sin(x1) + 0.5*x2^2 + 0.2*x3 + pmax(x2, 0.2) * rnorm(n))
b <- gam(y ~ s(x1) + s(x2) + x3, data = dat, method = "REML")
# convert the fitted object to the gamViz class
b <- getViz(b, nsim = 20)# nsim positive integer giving the number of simulated responses
?mgcViz::sm
dat %>% head()
b$store$sim %>% class()
b$store$sim[2,]%>% head()
#first smooth component using the sm function
o <- plot(sm(b,1)) # object of class gam
o+ggtitle( "Dies ist mein Titel")
str(o)
class(o) #[1] "plotSmooth" "gg" 
summary(o)
o + l_fitLine(col = "red") + #  layer add lines graphical parameters as with geom_line
  l_ciLine(col = "blue") + # adds confidence intervalls 95%
  l_rug() + #geom_rug {ggplot2}
  l_points() #' graphical arguments from ggpoint'
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()
# 2D smootheffect
b <- gam(y ~ s(x1, x2) + x3, data = dat, method = "REML")
b <- getViz(b)
plot(sm(b, 1)) + l_fitRaster() + l_fitContour() + l_points()
plot(pterm(b, 1)) + l_ciPoly() + l_fitLine() # effect of parameter x3
g <- getGam(b)
summary(g) # gett the gam parms back from mgcViz object b
class(g) # [1] "gam" "glm" "lm" 
# plot all effects
print(plot(getViz(b),allTerms = TRUE),pages =1)
# new dataset
dat <- gamSim(1,n=1e3,dist="normal",scale=2)
dat$fac <- as.factor( sample(letters[1:6], nrow(dat), replace = TRUE) )
b <- gam(y~s(x0)+s(x1, x2)+s(x3)+fac, data=dat)
b <- getViz(b)
print(plot(b,allTerms = TRUE),pages =1)
pl <- plot(b, allTerms = T) + l_points() + l_fitLine(linetype = 3) + l_fitContour() + 
  l_ciLine(colour = 2) + l_ciBar() + l_fitPoints(size = 1, col = 2) + theme_get() + labs(title = NULL)
pl$empty # FALSE: because we added gamLayers

