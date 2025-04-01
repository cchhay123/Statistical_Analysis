load('~/Downloads/sales.Rda')
load('~/Downloads/NYOP.Rda')

# Create a new data frame with the aggregate function to calculate the total revenue from
# NYOP and NYOP Charity 
NYOP_Revenue <- aggregate(Price ~ Condition, data = dfNYOP, FUN = sum)
NYOP_Revenue$Total <- c(sum(NYOP$Number), sum(NYOP_charity$Number))


# Finding the profit for the NYOP and finding the average daily profit 
# Revenue - Cost 
NYOP_Revenue$Profit <- NYOP_Revenue$Price - (NYOP_Revenue$Total * 1.2)
NYOP_Revenue$Profit_PerDay <- NYOP_Revenue$Price - (NYOP_Revenue$Total * 1.2)
NYOP_Revenue$Profit_PerDay[1] <- round((NYOP_Revenue$Profit_PerDay[1] /2), 2)
NYOP_Revenue$Profit_PerDay[2] <- round((NYOP_Revenue$Profit_PerDay[2] /3), 2)
NYOP_Revenue$Profit_PerDay[2] <- round((NYOP_Revenue$Profit_PerDay[2] /2), 2)

# Flat Rate revenue 
FR_r <- 12.95 * sum(dfSales$NumberSold[dfSales$Condition == 'FR'])
FRC_r <- 12.95 *  sum(dfSales$NumberSold[dfSales$Condition == 'FR Charity'])

# Flat Rate profit and average flat rate profit per day 
FR_profit <- (12.95 - 1.2) * sum(dfSales$NumberSold[dfSales$Condition == 'FR'])
FR_profit_PerDay <- FR_profit/2
FR_Charity <- (12.95 - 1.2) * sum(dfSales$NumberSold[dfSales$Condition == 'FR Charity']) *.5
FR_Charity_profit_PerDay <- FR_Charity/2

# creating a new data frame for the Flat rate revenue
FR_Revenue <- data.frame(Condition = c('FR', 'FR Charity'),
                        Revenue = c(FR_r, FRC_r),
                        Total = c(sum(dfSales$NumberSold[dfSales$Condition == 'FR']), 
                                  sum(dfSales$NumberSold[dfSales$Condition == 'FR Charity'])),
                        Profit = c(FR_profit,FR_Charity),
                        Profit_Day = c(FR_profit_PerDay,FR_Charity_profit_PerDay))

# Changing the name of the column from price to revenue 
colnames(NYOP_Revenue)[colnames(NYOP_Revenue) == 'Price'] <- 'Revenue'

# finding the average sales on merchandise on different price date 
Sales_Merchandise <- aggregate(MerchandiseRevenues ~ Condition, data = dfSales, FUN = sum)
Sales_Merchandise$Reve_perday <- Sales_Merchandise$MerchandiseRevenues
Sales_Merchandise$Reve_perday[1] <- Sales_Merchandise$Reve_perday[1]/2
Sales_Merchandise$Reve_perday[2] <- Sales_Merchandise$Reve_perday[2]/2
Sales_Merchandise$Reve_perday[3] <- Sales_Merchandise$Reve_perday[3]/2
Sales_Merchandise$Reve_perday[4] <- Sales_Merchandise$Reve_perday[4]/3


# Finding the average unit price for NYOP and NYOP Charity 
NYOP_Revenue$UnitPrice <- round(NYOP_Revenue$Revenue/NYOP_Revenue$Total, 2)


dfSales$proportion <- dfSales$NumberSold/dfSales$Riders

# Creating scatter plots and bar graph to investigate the relation between the pricing strategy 
# and the mercahndise revenue 
install.packages('ggplot2')
library(ggplot2)

ggplot(dfSales, aes(proportion, MerchandiseRevenues)) +
   geom_point() + geom_smooth(method = 'lm') + theme_bw()

ggplot(dfSales, aes(NumberSold, MerchandiseRevenues)) +
  geom_point() + geom_smooth(method = 'lm') + theme_bw()

ggplot(dfSales, aes(Condition, MerchandiseRevenues)) +
  geom_col() + theme_bw()

# looking at the linear model to see the significance of the relationship between merchandise
# revenue and number of ticket sold
lm1 <- lm(MerchandiseRevenues ~ NumberSold, dfSales)
summary(lm1)

Merchandise_avg <- aggregate(MerchandiseRevenues ~ Condition, dfSales, FUN = mean)
ggplot(Merchandise_avg, aes(Condition, MerchandiseRevenues)) +
   geom_col() + theme_bw()

# Hypothesis testing 
t.test(FR_Revenue[FR_Revenue$Condition == 'FR', 'Revenue'], 
       FR_Revenue[FR_Revenue$Condition == 'FR Charity', 'Revenue'],
       alternative = 'less', conf.level = .99)

t.test(dfNYOP[dfNYOP$Condition == 'NYOP', 'UnitPrice'],
       dfNYOP[dfNYOP$Condition == 'NYOP Charity', 'UnitPrice'], 
       alternative = 'less', conf.level = .99)
        
Number_soldFR <- sum(dfSales$NumberSold[dfSales$Condition == 'FR'])
Number_soldFRC <- sum(dfSales$NumberSold[dfSales$Condition == 'FR Charity'])
Number_RiderFR <- sum(dfSales$Riders[dfSales$Condition == 'FR'])
Number_RiderFRC <- sum(dfSales$Riders[dfSales$Condition == 'FR Charity'])

prop.test(c(Number_soldFR, Number_soldFRC),
          c(Number_RiderFR, Number_RiderFRC),
          alternative = 'less', conf.level = .99)

Number_soldNYOP <- sum(dfSales$NumberSold[dfSales$Condition == 'NYOP'])
Number_soldNYOPC <- sum(dfSales$NumberSold[dfSales$Condition == 'NYOP Charity'])
Number_RiderNYOP <- sum(dfSales$Riders[dfSales$Condition == 'NYOP'])
Number_RiderNYOPC<- sum(dfSales$Riders[dfSales$Condition == 'NYOP Charity'])

prop.test(c(Number_soldNYOP, Number_soldNYOPC),
          c(Number_RiderNYOP, Number_RiderNYOPC),
          alternative = 'less', conf.level = .99)

# Finding the donation for the charity conditions of the pricing 
NYOP_Revenue$Donation[2] <- NYOP_Revenue$Revenue[2]/2
FR_Revenue$Donation[2] <- FR_Revenue$Revenue[2]/2


FR_Revenue$UnitPrice <- 12.95

colnames(NYOP_Revenue)[colnames(NYOP_Revenue) == 'Profit_PerDay'] <- 'Profit_Day'

# Combining the FR and NYOP data frame and find the yearly profit
Total_Revenue <- rbind(FR_Revenue, NYOP_Revenue)
Total_Revenue$Yearly_Proft <- Total_Revenue$Profit_Day * 365

# Plotting daily profit between the four categories 
ggplot(Total_Revenue, aes(Profit_Day, Condition, fill = Condition)) + 
  geom_col() + theme_minimal()

# Finding the average donation per day and the yearly donation from each charity category 
Total_Revenue$Donation_Day[2] <- Total_Revenue$Donation[2]/2
Total_Revenue$Donation_Day[4] <- Total_Revenue$Donation[4]/3 
Total_Revenue$Donation_Year[2] <- Total_Revenue$Donation_Day[2] * 365
Total_Revenue$Donation_Year[4] <- Total_Revenue$Donation_Day[4] * 365


