# 
# for(year in 2022:2023) {
#   for(month in 1:12) {
#     date_string <- sprintf("%02d/%d", month, year)
#     date_strings <- c(date_strings, date_string)
#   }
# }
paste(c(1, "/", 8), collapse = "")

date_strings = axis_labels = unlist(lapply(2022:2023,
              function(year) {
                sapply(1:12,
                       function(month) {
                         if(month %% 6 == 0){
                           paste(c(month, "/", year), collapse = "")
                         }
                         else{ "" }
                       })
              }))
date_strings[1] = "1/2022"
data <- data.frame(x = 1:24, y = rnorm(24))  # Example data

ggplot(data, aes(x = x, y = y)) +
  geom_line() +
  scale_x_continuous(breaks = 1:24, labels = date_strings) +
  labs(x = "Date", y = "Value") +
  theme_minimal()
