library("ggplot2")
library("rpart")

/* Logistic Regression */

LogitDataset <- read.csv("/Users/gauravsharma/Desktop/Data_Science/alpha_extracted.csv")

logit.fit <- glm(alpha ~ ., family='binomial', data=LogitDataset)
summary(logit.fit)

exp(coef(logit.fit))

summary(LogitDataset)

# predict probs for new data (varying visits)
new.LogitDataset <- with(LogitDataset, data.frame(visits=seq(from = 0, to = 100, length.out = 100), 
content.find.similar.songs.click = mean(content.find.similar.songs.click), 
content.similar.song.click = mean(content.similar.song.click),
content.song.detail.click = mean(content.song.detail.click),
content.playlist.detail.click = mean(content.playlist.detail.click),
header.browse.new.releases.click = mean(header.browse.new.releases.click),
header.logo.click = mean(header.logo.click),
home.songmap = mean(home.songmap),
home.weekly.top.click = mean(home.weekly.top.click),
share.song.fb.click = mean(share.song.fb.click)))
new.LogitDataset$pred <- predict(logit.fit, newdata=new.LogitDataset, type='response')
ggplot(new.LogitDataset, aes(x=visits, y=pred)) + geom_line()
ggplot(new.LogitDataset, aes(x=visits, y=pred)) + geom_line(size=1) + geom_point()

# predict probs for new data (varying song detail clicks)
new.LogitDataset2 <- with(LogitDataset, data.frame(visits = mean(visits), 
content.find.similar.songs.click = mean(content.find.similar.songs.click), 
content.similar.song.click = mean(content.similar.song.click),
content.song.detail.click = seq(from = 0, to = 100, length.out=100),
content.playlist.detail.click = mean(content.playlist.detail.click),
header.browse.new.releases.click = mean(header.browse.new.releases.click),
header.logo.click = mean(header.logo.click),
home.songmap = mean(home.songmap),
home.weekly.top.click = mean(home.weekly.top.click),
share.song.fb.click = mean(share.song.fb.click)))
new.LogitDataset2$pred <- predict(logit.fit, newdata=new.LogitDataset2, type='response')
ggplot(new.LogitDataset2, aes(x=visits, y=pred)) + geom_line(size=1) + geom_point()
ggplot(new.LogitDataset2, aes(x=content.song.detail.click, y=pred)) + geom_line(aes(colour=blue), size=4)

/* Recursive Partitioning/Decision Tree */

TreeDataset <- read.csv("/Users/gauravsharma/Desktop/Data_Science/alpha_extracted.csv", header = TRUE)
fit1 <- rpart(alpha ~ ., data = TreeDataset)
summary(fit1)
plot(fit1, uniform = TRUE, margin = 0.1, branch = 0.3, compress = TRUE)
text(fit1)
post(fit1,file="")
plotcp(fit1)

TreeDataset2 <- read.csv("/Users/gauravsharma/Desktop/Data_Science/alpha_extracted.csv", header = TRUE)
fit2 <- rpart(alpha ~ content.song.detail.click, data = TreeDataset3)
summary(fit2)
plot(fit2, uniform = TRUE, margin = 0.1, branch = 0.3, compress = TRUE)
text(fit2)
post(fit2,file="")
plotcp(fit2)

/* Disregard - Using separate data set here
TreeDataset3 <- read.csv("/Users/gauravsharma/Desktop/Data_Science/eventstreams-per-user-filt.csv", header = TRUE)
fit3 <- rpart(alpha ~ ., data = TreeDataset2)
summary(fit3)
plot(fit3, uniform = TRUE, margin = 0.1, branch = 0.3, compress = TRUE)
text(fit3)
post(fit3,file="")
plotcp(fit3)
*/

/* Dimensionality Reduction: Principal Component Analysis */

PCADataset <- read.csv("/Users/gauravsharma/Desktop/Data_Science/alpha_extracted.csv")
alpha_pca <- prcomp(PCADataset[,-1], retx = TRUE, center = TRUE, scale = TRUE)

LinComb1 <- alpha_pca$rotation[,1]
LinComb1
plot(alpha_pca)

