dat = read.csv("assignments/NY_Temperature_Data.csv")
set.seed(11292023)
library(zoo)
library(gridExtra)
library(grid)
library(ggplot2)
x = zoo(dat$TMAX, zoo::as.Date(dat$Date))

monthly_max = aggregate(x, as.yearmon, FUN=max)

# # 34 years and the start of one quarter
# length(aggregate(x, as.yearqtr, FUN=max))/4
# # reflected here
# min(dat$Date), max(dat$Date)

# library(ggfortify)
# p2df <- fortify.zoo(monthly_max)
# ggplot(p2df, aes(x = Index, y = monthly_max)) +
#   geom_line(color = "#f48c06") + theme_bw() +
#   labs(
#     x = "Year",
#     y =  expression(paste("Monthly Maximum (", degree, "C)")),
#     title = expression(paste(
#       "Monthly Maximum Temperature (", degree, "C) in New York"
#     )),
#     subtitle = "Measured From 1990-2024, Sourced from NOAA"
#   ) +
#   coord_fixed(ratio = 0.275)

month_ts <- ts(zooreg(monthly_max), 
               start = c(1990, 1),
               end = c(2024, 1),
               frequency = 12)

month_decomp <- decompose(month_ts, type = "additive")
mseasonal = month_decomp$seasonal
mtrend = month_decomp$trend
mnoise = month_decomp$random

sp2c <- ggplot(data = fortify.zoo(mseasonal), aes(x = Index, y = mseasonal)) +
  geom_line(color = "#9d0208", linewidth = 0.5) +
  theme_bw() +
  labs(
    title = "Seasonal Component of Additive Model",
    y = "st",
    x = NULL
  ) +  xlim(1990, 2024)

mp2c <- ggplot(data = fortify.zoo(mtrend), aes(x = Index, y = mtrend)) +
  geom_line(color = "#dc2f02", linewidth = 0.5) +
  theme_bw() +
  labs(
    title = "Trend Component of Additive Model",
    y = "mt",
    x = NULL
  )  +  xlim(1990, 2024)

zp2c <- ggplot(data = fortify.zoo(mnoise), aes(x = Index, y = mnoise)) +
  geom_line(color = "#e85d04", linewidth = 0.5) +
  theme_bw() +
  labs(
    title = "Random Component of Additive Model",
    y = "Zt",
    x = "Year"
  )  +  xlim(1990, 2024)

grid.arrange(sp2c, mp2c, zp2c)

##### part d

deseasonalized = month_ts - mseasonal
time(deseasonalized)
# lag on 1 year : evidence of perhaps un-accounted for serial dependence dependent
# on the month of the year. cyclical seasonal dependence
acf(deseasonalized, na.action = na.pass, lag.max = 12)

# lag on many years: this pattern repeats on a consistent cycle. 
# although, declines over time
acf(deseasonalized, na.action = na.pass, lag.max = 408)

# calculating what's one year in the TS
delta = time(deseasonalized)[2] - time(deseasonalized)[1]
# calculating what's the entire TS
(2024-1990)/delta

d_acf <- acf(deseasonalized, 
             na.action = na.pass, 
             lag.max = 408, plot = FALSE)
# calculating what's one year in the TS
delta = time(deseasonalized)[2] - time(deseasonalized)[1]

hmax = (2024-1990)/delta

# create plotting dataframe
p2ddata = data.frame(
  h = 0:hmax,
  rh = d_acf$acf
)
# determine n
n = length(deseasonalized)
# determine significance bars for iid noise serial dependence
bars = c(-2, 2)/sqrt(n)
# create plot
p2d <- ggplot(p2ddata, aes(x = h, y = rh)) +
    geom_segment(aes(xend = h, yend = 0),
                 color = "#606c38",
                 linewidth = 0.5) +
    geom_hline(yintercept = bars[1], linetype = "dashed", col = "#283618")+
    geom_hline(yintercept = bars[2], linetype = "dashed", col = "#283618")+
    ylim(-0.2, 1)+
    geom_hline(yintercept = 0,
               linetype = "dashed",
               color = "darkgray") +
    labs(x = "Lag", y = "Autocorrelation", 
         title = "Correlogram of Monthly Max Temperature Data",
         subtitle = "In New York 1990-2024, Data Sourced from NOAA") +
    theme_bw()
print(p2d)


# same as mean( abs(d_acf$acf) >  bars[2] )
p_greater = sum( abs(d_acf$acf) >  bars[2] ) / (hmax + 1)
p_greater*100


z_1 = rnorm(n = 2000, mean = 0, sd = 1)

acf(z_1, plot = FALSE)$acf[2]

######## part 3

m = 8000; n = 2000
simulation = sapply(1:m, 
             function(i){
               z_i = rnorm(n)
               rho_i = acf(z_i, plot = FALSE)
               r_i1 = rho_i$acf[2]
               r_i2 = rho_i$acf[3]
               c(r_i1, r_i2)
             })
results = data.frame(t(simulation))
colnames(results) = c("R1", "R2")
head(results)

mean(results$R1)
mean(results$R2)
var(results$R1)
var(results$R2)

library(gt)
library(gtExtras)
tableDF <- data.frame(names =colnames(results),
                      means =sapply(results, function(r) {sprintf("%.2e", mean(r))}),
                      vars  =sapply(results, function(r) {sprintf("%.2e", var(r))})
)
colnames(tableDF) = c("Lag", "Mean", "Variance")

gt_table <- gt(tableDF) %>%
  tab_header(title = "Simulation Results",
             subtitle = md("From `m = 8000` Iterations")) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) %>% tab_options(
    stub.border.width = px(2),
    stub.border.color = "black"
  ) %>%
  gt_add_divider(columns = "Lag", style = "solid", col = 'grey70')

(gt_table)
-1/n
1/n
?t.test
mean(results$R2)

t.test(x = results$R2, mu = -5e-4)
t.test(x = results$R1, mu = -5e-4)
