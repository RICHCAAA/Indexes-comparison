library(tidyverse)
library(TTR)
library(readr)
library(quantmod)
#install.packages("broom")
library(ggplot2)
library(magrittr)
library(broom)
library(xts)
library(rvest)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)

#install.packages('quantmod')



 
## pays émergents
symboles <- c( "^BSESN","^KS11", "^JKSE","^MXX")

##pays développé
symboles2 <- c("^HSI","^FCHI","^FTSE","^BFX")


#### Analyse des indices des pays dévelopés

### on récupère les données de la liste émergents
## Seule  méthode trouver fonctionnant avec les indices et gérant les erreurs. source : https://stackoverflow.com/questions/54076126/how-can-i-delete-na%C2%B4s-within-data-downloaded-daily-from-yahoo-finance-with-the-u
my_data <- lapply(symboles, function(x) try(na.omit(getSymbols(x, from="2000-01-01", to="2021-12-31", auto.assign = FALSE)[,6])))

names(my_data) <- symboles

sapply(my_data, function(x) sum(is.na(x)))

my_data
### on enlève les données NAN
priceData <- na.omit(do.call(merge,my_data))
#names(priceData) <- paste0("price_",symbolList)
priceData

### on calcul le log return
Index_return <- na.omit(Return.calculate(priceData, method = "log"))
rendement <-na.omit(Index_return)
rendement

## On calcul le rendement annualisé sur la base de 252 jours, avec un risk free rate à 6.86% https://www.multpl.com/20-year-treasury-rate/table/by-year
## source utilisé : https://towardsdatascience.com/how-to-measure-stock-portfolio-performance-using-r-847c992195c2

performance_table <- as.data.frame(table.AnnualizedReturns(rendement,  Rf = 0.0686/252))
performance_table <- rownames_to_column(performance_table)
names(performance_table)[1] <- 'Performance'

## On crée un dataframe 
performance_df <- performance_table %>% gather(key = 'Ticker', value = 'Values', -Performance) %>% spread(key = Performance, value = Values) %>%
  rename('Annualized_Return' = 'Annualized Return', 'Annualized_Sharpe' = 'Annualized Sharpe (Rf=6.86%)','Annualized_StdDev' = 'Annualized Std Dev' ) %>%
  select(Ticker,Annualized_Return, Annualized_StdDev, Annualized_Sharpe)

performance_df


### Calcul des rendements cumulés
rendement_cumulé <- cumsum(rendement)
rendement_cumulé


###plot de l'évolution des prix

### source utilisé : https://medium.com/analytics-vidhya/plot-stock-prices-with-r-6bdbaebc8ec1
stocks_series2 = tidy(priceData) %>% 
  
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line() +
  facet_grid(series~.,scales = "free") + 
  labs(title = " Indices des pays émergents: évolution du prix dans les monnaies locales",
       
       subtitle = "End of Day Adjusted Prices",
       caption = " Source: Yahoo Finance") +
  
  xlab("Date") + ylab("Adjusted Price") +
  scale_color_manual(values = c("Red", "Black", "DarkBlue","Orange", "Grey"))
stocks_series2


###plot de l'évolution des rendements cumulés
### source utilisé : https://medium.com/analytics-vidhya/plot-stock-prices-with-r-6bdbaebc8ec1

stocks_series3 = tidy(rendement_cumulé) %>% 
  
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line() +
  facet_grid(series~.,scales = "free") + 
  labs(title = "Indices des pays émergents: évolution des rendements cumulés logarithmiques  dans les monnaies locales",
       
       subtitle = "End of Day Adjusted Prices",
       caption = " Source: Yahoo Finance") +
  
  xlab("Date") + ylab("Cumulative log return") +
  scale_color_manual(values = c("Red", "Black", "DarkBlue","Orange", "Grey"))
stocks_series3



#### Analyse des indices des pays dévelopés

### on récupère les données de la liste émergents

## Seule  méthode trouver fonctionnant avec les indices et gérant les erreurs. source : https://stackoverflow.com/questions/54076126/how-can-i-delete-na%C2%B4s-within-data-downloaded-daily-from-yahoo-finance-with-the-u

my_data2 <- lapply(symboles2, function(x) try(na.omit(getSymbols(x, from="2000-01-01", to="2021-12-31", auto.assign = FALSE)[,6])))

names(my_data2) <- symboles2

sapply(my_data2, function(x) sum(is.na(x)))

my_data2
### on enlève les données NAN
priceData2 <- na.omit(do.call(merge,my_data2))
#names(priceData2) <- paste0("price_",symbolList)
priceData2

### on calcul le log return
Index_return2 <- na.omit(Return.calculate(priceData2, method = "log"))
rendement2 <-na.omit(Index_return2)
rendement2


## On calcul le rendement annualisé sur la base de 252 jours, avec un risk free rate à 6.86% https://www.multpl.com/20-year-treasury-rate/table/by-year
## source utilisé : https://towardsdatascience.com/how-to-measure-stock-portfolio-performance-using-r-847c992195c2

performance_table2 <- as.data.frame(table.AnnualizedReturns(rendement2,  Rf = 0.0686/252))
performance_table2 <- rownames_to_column(performance_table2)
names(performance_table2)[1] <- 'Performance'

## On crée un dataframe 
performance_df2 <- performance_table2 %>% gather(key = 'Ticker', value = 'Values', -Performance) %>% spread(key = Performance, value = Values) %>%
  rename('Annualized_Return' = 'Annualized Return', 'Annualized_Sharpe' = 'Annualized Sharpe (Rf=6.86%)','Annualized_StdDev' = 'Annualized Std Dev' ) %>%
  select(Ticker,Annualized_Return, Annualized_StdDev, Annualized_Sharpe)

performance_df2

### Calcul des rendements cumulés
rendement_cumulé_dev <- cumsum(rendement2)
rendement_cumulé_dev


###plot de l'évolution des prix
## source utilisé : https://medium.com/analytics-vidhya/plot-stock-prices-with-r-6bdbaebc8ec1

stocks_series4 = tidy(priceData2) %>% 
  
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line() +
  facet_grid(series~.,scales = "free") + 
  labs(title = " Indices des pays développés: évolution du prix dans les monnaies locales",
       
       subtitle = "End of Day Adjusted Prices",
       caption = " Source: Yahoo Finance") +
  
  xlab("Date") + ylab("Adjusted Price") +
  scale_color_manual(values = c("Red", "Black", "DarkBlue","Orange", "Grey"))
stocks_series4


###plot de l'évolution des rendements cumulés
## source utilisé : https://medium.com/analytics-vidhya/plot-stock-prices-with-r-6bdbaebc8ec1

stocks_series5 = tidy(rendement_cumulé_dev) %>% 
  
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line() +
  facet_grid(series~.,scales = "free") + 
  labs(title = "Indices des pays développés: évolution des rendements cumulés logarithmiques  dans les monnaies locales",
       
       subtitle = "End of Day Adjusted Prices",
       caption = " Source: Yahoo Finance") +
  
  xlab("Date") + ylab("Cumulative log return") +
  scale_color_manual(values = c("Red", "Black", "DarkBlue","Orange", "Grey"))
stocks_series5

### Construction des portefeuilles


## attributions de tous le rendements à une variable
d1 <- data.frame(c(rendement))
d2 <- data.frame(c(rendement2))


my.list <- list(d1, d2)
my.list
## on attribut à chaque liste le nom de leur catégorie
names(my.list) <- c("Portefeuille indices émergents", "Portefeuille indices développés")

my.list
liste_rendement
## on attribut les poids en utilisant une seule liste, car même longueur pour tous -->4
weight = rep(1/length(symboles), length(symboles)) 

names(my.list)

### on fait une boucle pour faire l'analyse des portefeuilles à la suite.
## source utilisé : https://towardsdatascience.com/how-to-measure-stock-portfolio-performance-using-r-847c992195c2
# permet d'attribuer le bon nom de portefeuilles
e= 1
for (i in my.list) {
  
  Portfolio_Return <- Return.portfolio(i, weights = weight, rebalance_on = 'quarters')
  
  charts.PerformanceSummary(Portfolio_Return,main = names(my.list[0+e]),colorset=rainbow12equal)
  e=1+1
  
}

