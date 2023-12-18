install.packages('dplyr')
install.packages('caret')
install.packages('rsample')
install.packages('openintro')
install.packages('vip')
install.packages("AICcmodavg")

library('dplyr')
library('caret')
library('rsample')
library('openintro')
library(vip)
library(AICcmodavg)
library(ggplot2)

my_data <- read.csv("C:\\Users\\vadim\\OneDrive\\Desktop\\AD\\Lucrul final\\Global YouTube Statistics.csv" , fileEncoding = "ISO-8859-1")
head(my_data)
glimpse(my_data)
summary(my_data)

my_data <- my_data[complete.cases(my_data), ]
my_data$Youtuber <- iconv(my_data$Youtuber, from = "UTF-8", to = "ASCII//TRANSLIT")


library(ggplot2)

ggplot(my_data, aes(x = Abbreviation)) +
  geom_bar(fill = "#79CDCD") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  labs(title = "Numărul de Canale în Funcție de Țara de Origine",
       x = "Țara de Origine",
       y = "Număr de Canale")


ggplot(my_data, aes(x = category)) +
  geom_bar(fill = "brown1") +
  labs(title = "Distribuția Categoriilor de Canale",
       x = "Categorie",
       y = "Număr de Canale") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  


ggplot(my_data, aes(x = category, y = subscribers)) +
  geom_bar(stat = "identity", fill = "#79CDCD") +
  labs(title = "Distribuția Numărului de Abonați pe Categorii",
       x = "Categorie",
       y = "Număr de Abonați") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(my_data, aes(x = category, y = highest_yearly_earnings)) +
  geom_bar(stat = "identity", fill = "brown1", width = 0.7) +
  labs(title = "Cele mai mari venituri anuale pe categorii de canale",
       x = "Categorie de canal",
       y = "Venituri anuale (USD)")+
      theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Eliminați rândurile cu valori lipsă
my_data <- na.omit(my_data)
my_data$Country <- as.factor(my_data$Country)
my_data$category <- as.factor(my_data$category)


# Split set de date
set.seed(123)
split <- initial_split(my_data, prop = 0.7)
dataset_train <- training(split)
dataset_test <- testing(split)


# Crearea unui model de regresie liniar
model_reg <- lm(subscribers ~ video_views + Country + category, data = my_data)
summary(model_reg)

# Perform cross-validation pentru Model 1
set.seed(123)
cv_model1 <- train(
  form = subscribers ~ video_views + Country + category,
  data = dataset_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
)
cv_model1

new_data <- data.frame(
  video_views = 1.5E+11,
  Country = "United States",      
  category = "Music"       
)


predicted_subscribers <- predict(model_reg, newdata = new_data)

cat("Prezicerea pentru numărul de abonați:", predicted_subscribers, "\n")

predictions <- predict(cv_model1, newdata = my_data)

# Crearea unui cadru de date pentru compararea observatelor cu predictiile
comparison_data <- data.frame(Observate = my_data$subscribers, Predictii = predictions)

# Crearea unui grafic de dispersie cu linia de regresie
ggplot(comparison_data, aes(x = Observate, y = Predictii)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(title = "Comparare observate vs. prezise",
       x = "Număr real de abonați",
       y = "Număr prezis de abonați")


