#RFM analysis using transactional data.
library(rfm) # install the rfm package
data <- read.csv("clipboard", sep ="\t", header = T) #read data using clipboard
str(data) # structure of data
library(lubridate) # package for handling date time object.
data$order_date <- as.Date(data$order_date, format = "%m/%d/%Y")
str(data)
summary(data$order_date)
max(data$order_date)

analysis_date <- as_date("2006-12-31") #setting date for RFM algorithm
rfm_result <- rfm_table_order(
  data = data,
  customer_id = customer_id,
  order_date = order_date,
  revenue = revenue,
  analysis_date = analysis_date
)
rfm_data <- data.frame(rfm_result$rfm) # retrieving the customer data from the transaction data
write.csv(rfm_data, "Customer.csv")

# segmenting the customer based on RFM score
segment_titles <- c("First Grade", "Loyal", "Likely to be Loyal", "New Ones",
                    "Could be Promising", "Require Assistance", "Getting Less Frequent", 
                    "Almost Out", "Can't Lose Them", "Don't Show up at All")
#-------
##_R__F__M__
#1 4-5,4-5,4-5
#2 2-5,3-5,3-5
#3 3-5,1-3,1-3
#4 4-5,1-1,1-1
#5 3-4,1-1,1-1
#6  2-3,2-3,2-3
#7  2-3,1-2,1-2
#8  1-2,2-5,2-5
#9  1-1,4-5,4-5
#10 1-2,1-2,1-2
r_low <- c(4,2,3,4,3,2,2,1,1,1)
r_high<- c(5,5,5,5,4,3,3,2,1,2)
f_low <- c(4,3,1,1,1,2,1,2,4,1)
f_high<- c(5,5,3,1,1,3,2,5,5,2)
m_low <- c(4,3,1,1,1,2,1,2,4,1)
m_high<- c(5,5,3,1,1,3,2,5,5,2)

divisions <- rfm_segment(rfm_result, segment_titles, r_low, r_high, f_low, f_high, m_low, m_high)
write.csv(divisions, "Segments.csv")
library(dplyr)
segments <- divisions %>% count(segment) %>% arrange(desc(n)) %>% rename(SEGMENT= segment, FREQUENCY = n)%>% mutate(PERCENTAGE = FREQUENCY/ sum(FREQUENCY)*100)

average <- divisions %>% group_by(segment) %>% summarize(AF = median(transaction_count),
                                                          ARD = median(recency_days),
                                                AMT = median(amount)) %>% arrange(desc(AMT))
#understanding divisions
rfm_plot_median_recency(divisions)
rfm_plot_median_frequency(divisions)
rfm_plot_median_monetary(divisions)

#understanding customers
rfm_histograms(rfm_result)
rfm_order_dist(rfm_result)
rfm_bar_chart(rfm_result)
