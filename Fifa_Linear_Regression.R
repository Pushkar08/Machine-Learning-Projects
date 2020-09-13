# Read CSV, note the delimiter (sep)
fifa_df <- read.csv('fifa18.csv')

head(fifa_df)

summary(fifa_df)

library(tidyverse)

#Checking if NA or NULL Values exist in the dataset
any(is.na(fifa_df))








irrelevant_columns <- c("X", "ID","Photo","Flag", "?lub.Logo", "Preferred.Positions")



fifav1 <- fifa_df[,! names(fifa_df) %in% irrelevant_columns, drop = F]

fifav1 = fifav1[-c(50:75)]

fifav1 = fifav1[-c(44:49)]


fifav1$Value <- gsub("â,¬", "", fifav1$Value)
fifav1$Wage <- gsub("â,¬", "", fifav1$Wage)
?fifav1$Wage <- as.numeric(gsub("K", "", fifav1$Wage))

fifav1$Value <- as.numeric(gsub("M", "", fifav1$Value))

fifav1$Value <- fifav1$Value * 1000








for (i in c(10:43))
  fifav1[,i]<- as.numeric(gsub("- | +", ".", fifav1[,i],fixed=TRUE))

fifav1 = f?fav1[complete.cases(fifav1), ]
fifav1 = na.omit(fifav1)


wage_breaks <- c(0, 100, 200, 300, 400, Inf)
wage_labels <- c("0-100k", "100k-200k", "200k-300k", "300k-400k", "400k+")

wage_brackets <- cut(x=fifav1$Wage, breaks=wage_breaks, 
                    ?labels=wage_labels, include.lowest = TRUE)
df <- mutate(fifav1, wage_brackets)

df %>% ggplot(aes(x = Overall, fill = factor(Overall))) +geom_bar(color = "grey20") + guides(fill = FALSE)+labs(title="Player's Overall Distribution")

not0To50K <- filter(df, ?age_brackets != "0-100k") 
ggplot(not0To50K, aes(x = wage_brackets)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Distribution of top Wage between 100K-400K+")

g_age_overall <- ggplot(df, aes(Age, Overall))
g_age_overall + 
  geom_point(aes(color=wa?e_brackets)) + geom_smooth(color="darkblue") +
   
  ggtitle("Distribution between Age and Overall of players based  on Wage bracket")



value_breaks <- c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000,Inf)
value_labels <- c("0-10M", "10?-20M", "20M-30M", "30M-40M", "40M-50M", "50M-60M", "60M-70M", "70M-80M", "80M-90M", "90M+")

value_brackets <- cut(x=fifav1$Value, breaks=value_breaks, 
                     labels=value_labels, include.lowest = TRUE)
df <- mutate(fifav1, value_brackets)

?_age_overall <- ggplot(df, aes(Age, Overall))
g_age_overall + geom_point(aes(color=value_brackets)) + geom_smooth(color="darkblue") + 
  ggtitle("Distribution between Age and Overall of players based on Value bracket")



features = str(fifav1)

fifa_model?= fifav1[, c('Wage', 'Reactions', 'Composure', 'Overall', 'Potential', 'Value', 'Special')]

num.cols <- sapply(fifa_model, is.numeric)
cor.data = cor(fifa_model[, num.cols])


fifav1 = fifav1[, num.cols]

fifav1 = fifav1[, c('Wage', 'Reactions', 'Overall'? 'Potential', 'Value')]
cor.data = cor(fifav)


unique(fifav1$Wage)



library(corrplot)

install.packages('corrgram')
library(corrgram)

corrplot(cor.data, method = 'color' )


features = str(fifav1)


#Visualization
fifav1 %>% ggplot(aes(x = Potential, f?ll = factor(Potential))) +geom_bar(color = "yellowgreen") + guides(fill = FALSE)+labs(title="Player's Potential ")
fifav1 %>% ggplot(aes(x = Potential, fill = factor(Reactions))) +geom_bar(color = "grey20") + guides(fill = FALSE)+labs(title="Player's Poten?ial ")

# Import Library
library(caTools)
# Set a random see so your "random" results are the same as this notebook
set.seed(1234) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(fifav1$Wage, S?litRatio = 0.75) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(fifav1, sample == TRUE)

# Testing Data
test = subset(fifav1, sample == FALSE)

model <- lm(formula = Wage ~ ., data = train)
summary(model)

#Grab Residuals
res1 <- re?iduals(model)
 



# Convert to DataFrame for gglpot
res1 <- as.data.frame(res1)
head(res1)
ggplot(res1,aes(res1)) +  geom_histogram(fill='red',alpha=0.8)
par(mfrow = c(2,2))
plot(model)


Wage_predictions <- predict(model,test)
results123 <- cbind(Wage_pr?dictions,test$Wage)
colnames(results123) <- c('Predicted','Real')
results123 <- as.data.frame(results123)


library(scorer)
mean_absolute_error(Wage_predictions, test$Wage)
mean_squared_error(test$Wage,Wage_predictions)






