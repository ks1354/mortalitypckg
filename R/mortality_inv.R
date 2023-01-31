#' Mortality Prediction
#'
#' Fit poisson GLM to predict mortality rates
#' @param input Mortality data
#' @return Predicted mortality rates
#' @examples
#' predictions <- mortality_model("mortality_data.csv");
#' @import caret
#' @import dplyr
#' @import fastDummies
#' @export

mortality_model <- function(input) {

  #library(caret)
  #library(dplyr)
  #library(fastDummies)

  mortality_table_original <- read.csv(input,stringsAsFactors=TRUE)   #Importing the mortality data
  #head(mortality_table_original)
  #str(mortality_table_original)
  #summary(mortality_table_original)

  ###Data Cleaning

  mortality_table <- na.omit(mortality_table_original)
  mortality_table <- mortality_table[mortality_table$Gender != "", ]
  mortality_table$Gender <- droplevels(mortality_table$Gender)
  mortality_table <- mortality_table[mortality_table$Age >= 20 & mortality_table$Age <= 65,]
  mortality_table <- mortality_table[mortality_table$Industry_Code <= 4, ]


  ###Data Management

  crude_mortality_table <- summarise(group_by(mortality_table, Age), crude_rate = sum(Death)/n())
  head(crude_mortality_table)

  price_rate_table <- summarise(group_by(mortality_table, Age), price_rate = sum(Price_Rate)/n())
  head(price_rate_table)

  mortality_table <- mortality_table %>% filter(Salary > 0) %>%
    mutate(Log_Salary = log(Salary))


  mortality_table <- dummy_cols(mortality_table, select_columns = "Gender",remove_first_dummy = TRUE)
  mortality_table <- dummy_cols(mortality_table, select_columns = "Industry_Code",remove_first_dummy = TRUE)

  ###Model Building
  split <-  createDataPartition(mortality_table$Death, p = 0.7, list = FALSE)
  train_data <- mortality_table[split, ]
  test_data  <- mortality_table[-split, ]

  #deaths_GLM <- lm(Death ~ Age + Gender_Male,data = train_data)
  #summary(deaths_GLM)

  deaths_GLM <- glm(Death ~ Age + Gender_Male + Salary + Industry_Code_2 + Industry_Code_3 + Industry_Code_4,
                    family = poisson(link = "log"),
                    data = train_data)

  summary(deaths_GLM)

  deaths_GLM_predict_ALL <- as.vector(predict(deaths_GLM, type = "response", newdata = test_data))
  mort_table_dummy <- test_data
  mort_table_dummy$GLM <- deaths_GLM_predict_ALL
  mortality_agg_table_dummy <- as.data.frame(summarize(group_by(mort_table_dummy, Age), GLM = sum(GLM)/n()))
  mortality_agg_table <- as.data.frame(summarize(group_by(mortality_table, Age), crude_mortality = sum(Death)/n(),
                                                 price_rate = sum(Price_Rate)/n()))
  mortality_agg_table$predicted <- mortality_agg_table_dummy$GLM

  return(mortality_agg_table)

  return(mortality_table_original)

  write.csv(mortality_agg_table, "dashboard/mortality_agg_table.csv", row.names=FALSE)
  write.csv(mortality_table_original, "dashboard/mortality_table_original.csv", row.names=FALSE)


}
