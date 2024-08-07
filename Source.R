


# Install Packages & Load Data -------------------------------------------------

library('tidyverse')

covid <- read_csv('covidprices.csv')
dotcom <- read_csv('dotcomprices.csv')
gfc08 <- read_csv('08prices.csv')



# Functions --------------------------------------------------------------------

BetaSorting <- function(df) {
  
  df01 <- df %>% 
    mutate(Date = as.Date(Date),
           Month = format(Date, "%Y-%m")
    ) %>%
    group_by(Ticker, Month) %>%
    mutate(
      AvgMonthlyBeta = mean(Beta) # Get Average Monthly Betas for each stock
    ) %>%
    ungroup() %>%
    group_by(Month) %>%
    mutate(Decile = ntile(desc(AvgMonthlyBeta), 10)) %>%  # Sort into deciles based on monthly average betas
    select(Date, Month, Ticker, Open, Close, Volume, PreviousDate, CloseLength, ClosetoCloseReturn, OpentoCloseReturn,
           ClosetoOpenReturn, NightReturn, SP500Return, Beta, AvgMonthlyBeta, Decile)  # Re-Order columns
  
  
  return(df01)
  
}

Averaging <- function(df) {
  
  df02 <- df %>%
    group_by(Decile) %>%  # Get average statistics for each decile each month
    summarise(
      AvgCCReturn = mean(ClosetoCloseReturn, na.rm = TRUE),
      AvgOCReturn = mean(OpentoCloseReturn, na.rm = TRUE),
      AvgCOReturn = mean(ClosetoOpenReturn, na.rm = TRUE),
      AvgBeta = mean(AvgMonthlyBeta)
    )
  
  return(df02)
}

Plotting <- function(df) {
  df %>%
    gather('Variable', 'Value', -Decile, -AvgBeta) %>%
    mutate(
      Variable = ifelse(Variable == 'AvgCCReturn', 'Close-to-Close Return',
                        ifelse(Variable == 'AvgCOReturn', 'Close-to-Open Return', 'Open-to-Close Return'))
    ) %>%
    ggplot(aes(x = AvgBeta, y = Value, color = Variable)) +
    geom_point() +
    # facet_wrap(~Variable) + 
    # geom_smooth(method = 'lm', se = FALSE) +
    stat_smooth(method = 'lm', se = FALSE) +
    labs(x = 'Average Beta', y = 'Avgerage Return') +
    theme_bw() + 
    theme(
      legend.title = element_blank() , legend.position = 'bottom', 
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.title = element_text(size = 14), legend.text = element_text(size = 12),
      axis.text = element_text(size = 12)
    )
}


Regression <- function(df) {
  
  regCC <- lm('AvgCCReturn ~ AvgBeta', data = df)
  regOC <- lm('AvgOCReturn ~ AvgBeta', data = df)
  regCO <- lm('AvgCOReturn ~ AvgBeta', data = df)
  
  print(summary(regCC))
  print(summary(regOC))
  print(summary(regCO))
  
}



# Recessionary Period 1: Covid Pandemic ----------------------------------------

covid <- covid %>% filter(Date <= '2020-04-30')

covid01 <- BetaSorting(covid)
covid02 <- Averaging(covid01)
Plotting(covid02)
Regression(covid02)




# Recessionary Period 2: Dotcom Bubble -----------------------------------------

dotcom01 <- BetaSorting(dotcom)
dotcom02 <- Averaging(dotcom01)
Plotting(dotcom02)
Regression(dotcom02)



# Recessionary Period 3: '08 Global Financial Crisis ---------------------------

gfc0801 <- BetaSorting(gfc08)
gfc0802 <- Averaging(gfc0801)
Plotting(gfc0802)
Regression(gfc0802)