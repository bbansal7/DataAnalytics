library(lubridate)
data <- read.csv("clipboard", sep = "\t", header= T)
data$recentdate <- make_date(year = data$LastTransYear, month = data$LastTransMonth, day = "01") 
data <- data[,-c(2:8,10,14:20)]
max(data$recentdate)
analysisDate <- as_date('2002-01-01')

?rfm_table_customer_2
is.na(data)
rfm_result <- rfm_table_customer_2(data= data,
                                  customer_id = CustID,
                                  n_transactions = Num_Games,
                                  latest_visit_date = recentdate,
                                  total_revenue = Tot_Sales,
                                  analysis_date = analysisDate)
rfm_data <- data.frame(rfm_result$rfm)
rfm_histograms(rfm_result)
rfm_bar_chart(rfm_result)
segment_titles <- c("LoveGames", "NewToGames", "RegularToGames", "Occasionally", "Rare", "OutOfLoop")
#-----
## r___f___m
#1 4-5,4-5,5-5
#2 3-4,3-4,3-4
#3 2-3,2-3,2-3
#4 1-2,1-2,2-3
#5 1-2,1-1,1-5
#6 1-1,1-1,1-1
#-----
r_low  <- c(4,3,2,1,1,1)
r_high <- c(5,5,3,2,2,1)
f_low  <- c(4,1,2,1,1,1)
f_high <- c(5,5,3,2,1,1)
m_low  <- c(5,3,2,2,1,1)
m_high <- c(5,4,3,3,5,1)
 divisions <- rfm_segment(rfm_result, segment_titles, r_low, r_high, f_low, f_high, m_low, m_high)

rfm_plot_median_frequency(divisions) 
rfm_plot_median_monetary(divisions) 
rfm_plot_median_recency(divisions)

