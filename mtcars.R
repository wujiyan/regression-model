library(ggplot2)
library(car)
library(GGally)
data(mtcars)
#look at relationship between mpg and transmission type
mtcars$am <- factor(mtcars$am, levels = c("0","1"), labels = c("automatic", "manual"))
mtcars$vs <- factor(mtcars$vs, levels = c("0","1"), labels = c("vans", "suvs"))
mtcars$cyl <- factor(mtcars$cyl, levels = c("4","6","8"))
mtcars$gear <- factor(mtcars$gear, levels = c("3","4","5"))
mtcars$carb <- factor(mtcars$carb, levels = c("1","2","3","4","6","8"))
g <- ggplot(data = mtcars, aes(x = am, y = mpg))
g <- g + geom_boxplot(fill = c("orange","lightblue")) + geom_point(col = "red")
g <- g + xlab("transmission type") + labs(title = "Relationship between mpg and transmission type")
g

#test whether there are significant differences between two transmission groups
mpg_ma <- mtcars[mtcars$am == "manual",]
mpg_ma <- mpg_ma$mpg
mpg_au <- mtcars[mtcars$am == "automatic",]
mpg_au <- mpg_au$mpg
t.test(mpg_ma,mpg_au)

#step the linear model based on AIC
model1 <- lm(mpg ~., data = mtcars)
model2 <- step(model1, direction = "backward")
summary(model2)$coefficient
#delete regressor am
model3 <- lm(mpg ~ cyl + hp + wt, data = mtcars)
summary(model3)
#delete regressor cyl or hp
model4 <- lm(mpg ~ hp + wt, data = mtcars)
summary(model4)
model5 <- lm(mpg ~ cyl + wt,data = mtcars)
summary(model5)
#find differences on average weight for different transmission type
ggpairs(mtcars[,c(1,2,4,6,9)], lower = NULL, upper = list(continuous = "smooth"), title = "relationships between variables")
t.test(wt ~ am, data = mtcars)
final.model <- lm(mpg ~ hp + am, data = mtcars)
summary(final.model)
vif(final.model)
#test multicolinearity
par(mfrow = c(2,2))
plot(model5, main = "residual analysis")

