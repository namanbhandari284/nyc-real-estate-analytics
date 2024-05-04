library(tidyverse)
library(data.table)
library(lubridate)


nycdt <- fread("NYC_TRANSACTION_DATA.csv",header=TRUE)
nycnbrhd <- fread("NEIGHBORHOOD.csv",header = TRUE)
nycbldcls <- fread("BUILDING_CLASS.csv",header = TRUE)
nycbrgh <- fread("BOROUGH.csv",header = TRUE)
#DATA IS LOADED


DJ <- left_join(nycdt, nycnbrhd, by= "NEIGHBORHOOD_ID")%>%
  left_join(y=nycbrgh, by="BOROUGH_ID")%>%
  inner_join(y=nycbldcls, by=c("BUILDING_CLASS_FINAL_ROLL"="BUILDING_CODE_ID"))
#TABLES JOINED IN SINGLE DATAFRAME


DJF <- DJ%>%
  filter(TYPE=="RESIDENTIAL" , SALE_PRICE>=25000 & BOROUGH_ID== 2, GROSS_SQUARE_FEET != 0)
#DATA IS FILTERED ACCORDING TO ASSIGNMENT

DJFD <- DJF%>%
  mutate(SALE_YEAR = as.integer(substr(SALE_DATE, 1, 4)))
#ADDED A COLUMN NAMED SALES_YEAR Y EXTRACTING IT FROM SALES_DATE

DJFD2 <- DJFD%>%
  mutate(SALE_PRICE = as.double(SALE_PRICE, GROSS_SQUARE_FEET = as.double(GROSS_SQUARE_FEET)))
#CONVERTED DATA TO NUMERIC DATATYPE


K1 <- DJFD2%>%
  group_by(SALE_YEAR)    %>%
  summarise(Sales_count = n())


K2 <- DJFD2%>%
  group_by(SALE_YEAR)    %>%
  summarise(Median_sale_price =median(SALE_PRICE))


K3 <- DJFD2%>%
  group_by(SALE_YEAR)%>%
  summarise(std_dev_sale_price = sd(SALE_PRICE))


K4 <- DJFD2%>%
  group_by(SALE_YEAR)%>%
  summarise(Mean_sales_Price_per_sqft = mean(SALE_PRICE / GROSS_SQUARE_FEET))



K5 <- DJFD2%>%
  group_by(SALE_YEAR)%>%
  summarise(Std_dev_Sales_Price_per_sqft = sd(SALE_PRICE / GROSS_SQUARE_FEET))


K6 <- K2 %>%
  arrange(SALE_YEAR) %>%
  group_by(SALE_YEAR) %>%
  mutate(
    Market_Growth_Rate = ifelse(row_number() == 1, NA, (Median_sale_price - lag(Median_sale_price)) / lag(Median_sale_price) * 100)
  )
K7 <- K1 %>%
  left_join(K2, by = "SALE_YEAR")%>%
  mutate(POTENTIAL_REVENUE= Sales_count*Median_sale_price)
## CALCULATED ALL THE KPI


NEIGHBOR_BATH <- DJFD2 %>%
  filter(NEIGHBORHOOD_NAME == "BATHGATE")%>%
  select("SALE_YEAR","SALE_PRICE","GROSS_SQUARE_FEET")

NEIGHBOR_BAY <- DJFD2 %>%
  filter(NEIGHBORHOOD_NAME == "BAYCHESTER")%>%
  select("SALE_YEAR","SALE_PRICE","GROSS_SQUARE_FEET")

#FILTERED DATA FOR NEIGHBORHOOD

AGG_KPI_BATH <- NEIGHBOR_BATH %>%
  group_by(SALE_YEAR) %>%
  summarise(
    Median_sale_price_BATH = median(SALE_PRICE, na.rm = TRUE),
    std_dev_sale_price_BATH = sd(SALE_PRICE, na.rm = TRUE),
    Mean_sales_Price_per_sqft_BATH = mean(SALE_PRICE / GROSS_SQUARE_FEET, na.rm = TRUE),
    Std_dev_Sales_Price_per_sqft_BATH = sd(SALE_PRICE / GROSS_SQUARE_FEET, na.rm = TRUE),
    Sales_count_BATH = n()
  )



AGG_KPI_BAY <- NEIGHBOR_BAY %>%
  group_by(SALE_YEAR) %>%
  summarise(
    Median_sale_price_BAY = median(SALE_PRICE, na.rm = TRUE),
    std_dev_sale_price_BAY = sd(SALE_PRICE, na.rm = TRUE),
    Mean_sales_Price_per_sqft_BAY = mean(SALE_PRICE / GROSS_SQUARE_FEET, na.rm = TRUE),
    Std_dev_Sales_Price_per_sqft_BAY = sd(SALE_PRICE / GROSS_SQUARE_FEET, na.rm = TRUE),
    Sales_count_BAY = n()
  )
# CALCULATED ALL THE KPIs FOR NEIGHBORHOOD

ALL_KPI_BATH <- AGG_KPI_BATH%>%
  mutate(POTENTIAL_REVENUE_BATH = Sales_count_BATH*Median_sale_price_BATH)

# ADDED COLUMN AND CALCULATED POTENTIAL REVENUE
ALL_KPI_BAY <- AGG_KPI_BAY%>%
  mutate(POTENTIAL_REVENUE_BAY = Sales_count_BAY*Median_sale_price_BAY)


# JOINED ALL KPI TABLES FOR NEIGHBORHOOD
COMBINED_NEIGHBOR <- full_join(ALL_KPI_BATH, ALL_KPI_BAY, by = "SALE_YEAR")



library(ggplot2)

ggplot(data = COMBINED_NEIGHBOR, aes(x = SALE_YEAR)) +
  geom_line(aes(y = Sales_count_BATH, color = "BATHGATE")) +
  geom_line(aes(y = Sales_count_BAY, color = "BAYCHESTER")) +
  labs(title = "Sales Count Over Time",
       x = "Year",
       y = "Sales Count",
       color = "Neighborhood") +
  scale_color_manual(values = c("BATHGATE" = "blue", "BAYCHESTER" = "red"))



ggplot(data = COMBINED_NEIGHBOR, aes(x = SALE_YEAR)) +
  geom_line(aes(y = Median_sale_price_BATH, color = "BATHGATE")) +
  geom_line(aes(y = Median_sale_price_BAY, color = "BAYCHESTER")) +
  labs(title = "Median Sale Price Over Time",
       x = "Year",
       y = "Median Sale Price",
       color = "Neighborhood") +
  scale_color_manual(values = c("BATHGATE" = "blue", "BAYCHESTER" = "red"))



ggplot(data = COMBINED_NEIGHBOR, aes(x = SALE_YEAR)) +
  geom_line(aes(y = std_dev_sale_price_BATH, color = "BATHGATE")) +
  geom_line(aes(y = std_dev_sale_price_BAY, color = "BAYCHESTER")) +
  labs(title = "Standard Deviation of Sales Price Over Time",
       x = "Year",
       y = "Standard Deviation",
       color = "Neighborhood") +
  scale_color_manual(values = c("BATHGATE" = "blue", "BAYCHESTER" = "red"))



ggplot(data = COMBINED_NEIGHBOR, aes(x = SALE_YEAR)) +
  geom_line(aes(y = Mean_sales_Price_per_sqft_BATH, color = "BATHGATE")) +
  geom_line(aes(y = Mean_sales_Price_per_sqft_BAY, color = "BAYCHESTER")) +
  labs(title = "Mean Price per Sqft Over Time",
       x = "Year",
       y = "Mean Price per Sqft",
       color = "Neighborhood") +
  scale_color_manual(values = c("BATHGATE" = "blue", "BAYCHESTER" = "red"))



ggplot(data = COMBINED_NEIGHBOR, aes(x = SALE_YEAR)) +
  geom_line(aes(y = POTENTIAL_REVENUE_BATH, color = "BATHGATE")) +
  geom_line(aes(y = POTENTIAL_REVENUE_BAY, color = "BAYCHESTER")) +
  labs(title = "Potential Revenue Over Time",
       x = "Year",
       y = "Potential Revenue of Sales", 
       color = "Neighborhood") +
         scale_color_manual(values = c("BATHGATE" = "blue", "BAYCHESTER" = "red"))
       
       
 ggplot(data = COMBINED_NEIGHBOR, aes(x = SALE_YEAR)) +
         geom_line(aes(y = Std_dev_Sales_Price_per_sqft_BATH, color = "BATHGATE")) +
         geom_line(aes(y = Std_dev_Sales_Price_per_sqft_BAY, color = "BAYCHESTER")) +
         labs(title = "Std. Dev. of Sales Price per Sqft Over Time",
              x = "Year",
              y = "Standard Deviation",
              color = "Neighborhood") +
                scale_color_manual(values = c("BATHGATE" = "blue", "BAYCHESTER" = "red"))
 

              
              
              
              