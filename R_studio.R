dataset <- read.table('G:/My Drive/Colab Notebooks/soybean_data.csv',header=T,sep=",")
head(dataset)
dataset2<-dataset[,3:22]
n <- names(dataset2)
n
f <- as.formula(paste("price_next_week ~", paste(n[!n %in% "price_next_week"], collapse = " + ")))
f
f_model <- glm(f,data = dataset2, family = binomial)
r_model <- step(f_model, direction = "backward", trace = F)
r_model$anova
r_model$coefficients
summary(r_model)

fullmodel <- glm(price_next_week ~ ., family = binomial(link="logit"), 
                 data = dataset2)
fullmodel$anova
fullmodel$coefficients
summary(fullmodel)

plot(r_model, which=1)