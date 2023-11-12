# Foreign Growth Analysis
# December 2022
# Focus Response Analysis


### ACTION
# run the R Code F Setup first to build out the DF_Lot_cut
# Adjust Filter and Focus in Setup code as desired prior to running or re-running it


## Explore the response variable in-focus
# Build a month-bucketed dataframe
DF_Response_infocus <- data_frame(date = DF_Lot_cut$date_inoc, resp = DF_Lot_cut$response_infocus)
DF_Response_infocus$rownum = 1:nrow(DF_Response_infocus)
DF_Response_infocus$tick <- 1
DF_Response_infocus <- DF_Response_infocus %>% group_by(month=floor_date(date, "month")) %>% summarise(run_count=sum(tick), resp_count=sum(resp), resp_percent=sum(resp)/sum(tick))
DF_Response_infocus$month <- as.Date(DF_Response_infocus$month)
# calculate the mean-shift by 12-month bins
for (row in 12:(nrow(DF_Response_infocus)-11)) {
  DF_Response_infocus[row,"full_mean"] <- mean(DF_Response_infocus[1:nrow(DF_Response_infocus),]$resp_percent)
  DF_Response_infocus[row,"pre_mean"] <- mean(DF_Response_infocus[(row-11):row,]$resp_percent)
  DF_Response_infocus[row,"post_mean"] <- mean(DF_Response_infocus[row:(row+11),]$resp_percent)
  DF_Response_infocus[row,"mean_diff"] <- (DF_Response_infocus[row,]$pre_mean - DF_Response_infocus[row,]$post_mean) 
  DF_Response_infocus[row,"mean_shift"] <- abs((DF_Response_infocus[row,]$pre_mean - DF_Response_infocus[row,]$post_mean) / mean(DF_Response_infocus$resp_percent))
}
# plot the response curve with a smoothed fit
PLOT_Response_infocus <- ggplot (DF_Response_infocus, aes(x=month)) +
  geom_line(aes(y=resp_percent * 100), lwd = 1.5, color = "black") +
  geom_smooth(aes(y=resp_percent * 100), lwd = 1, method = "lm", formula = y ~ poly(x , 3), se = FALSE, color = "blue") +
  ylim(0, 100) + 
  ggtitle('[In-Focus] Foreign Growth Detection') +
  labs(subtitle = paste (VAR_subtitleFilter, VAR_subtitleFocus, sep='\n'), x = "Month", y = "Percent of FG Runs") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)), axis.text.x = element_text(angle=90, size=10, vjust=0.5))
PLOT_Response_infocus


## Explore the response variable out-of-focus
# Build a month-bucketed dataframe
DF_Response_outfocus <- data_frame(date = DF_Lot_cut$date_inoc, resp = DF_Lot_cut$response_outfocus)
DF_Response_outfocus$rownum = 1:nrow(DF_Response_outfocus)
DF_Response_outfocus$tick <- 1
DF_Response_outfocus <- DF_Response_outfocus %>% group_by(month=floor_date(date, "month")) %>% summarise(run_count=sum(tick), resp_count=sum(resp), resp_percent=sum(resp)/sum(tick))
DF_Response_outfocus$month <- as.Date(DF_Response_outfocus$month)
# calculate the mean-shift by 12-month bins
for (row in 12:(nrow(DF_Response_outfocus)-11)) {
  DF_Response_outfocus[row,"full_mean"] <- mean(DF_Response_outfocus[1:nrow(DF_Response_outfocus),]$resp_percent)
  DF_Response_outfocus[row,"pre_mean"] <- mean(DF_Response_outfocus[(row-11):row,]$resp_percent)
  DF_Response_outfocus[row,"post_mean"] <- mean(DF_Response_outfocus[row:(row+11),]$resp_percent)
  DF_Response_outfocus[row,"mean_diff"] <- (DF_Response_outfocus[row,]$pre_mean - DF_Response_outfocus[row,]$post_mean) 
  DF_Response_outfocus[row,"mean_shift"] <- abs((DF_Response_outfocus[row,]$pre_mean - DF_Response_outfocus[row,]$post_mean) / mean(DF_Response_outfocus$resp_percent))
}
# plot the response curve with a smoothed fit
PLOT_Response_outfocus <- ggplot (DF_Response_outfocus, aes(x=month)) +
  geom_line(aes(y=resp_percent * 100), lwd = 1.5, color = "black") +
  geom_smooth(aes(y=resp_percent * 100), lwd = 1, method = "lm", formula = y ~ poly(x , 3), se = FALSE, color = "blue") +
  ylim(0, 100) + 
  ggtitle('[Out-of-Focus] Foreign Growth Detection') +
  labs(subtitle = paste (VAR_subtitleFilter, VAR_subtitleFocus, sep='\n'), x = "Month", y = "Percent of FG Runs") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)), axis.text.x = element_text(angle=90, size=10, vjust=0.5))
PLOT_Response_outfocus


## Pareto charts of the key in-focus variables
DF_Lot_cut_infocus <- subset(DF_Lot_cut, DF_Lot_cut$response_infocus == 1)

# product code
TAB_counts <- DF_Lot_cut_infocus %>% count(data_product_code, sort = TRUE, name = "count")
VEC_count <- TAB_counts$count
names(VEC_count) <- TAB_counts$data_product_code
VEC_count <- VEC_count[! names(VEC_count) %in% c('na')]
pareto.chart(VEC_count, main="Pareto Product Code", ylab="count", ylab2="cum%", cumperc=seq(0,100, by=10))

# fermenter number
TAB_counts <- DF_Lot_cut_infocus %>% count(data_fermenter_num, sort = TRUE, name = "count")
VEC_count <- TAB_counts$count
names(VEC_count) <- TAB_counts$data_fermenter_num
VEC_count <- VEC_count[! names(VEC_count) %in% c('na')]
pareto.chart(VEC_count, main="Pareto Fermenter Number", ylab="count", ylab2="cum%", cumperc=seq(0,100, by=10))

# bug
TAB_counts <- DF_Lot_cut_infocus %>% count(data_bug_genus, sort = TRUE, name = "count")
VEC_count <- TAB_counts$count
names(VEC_count) <- TAB_counts$data_bug_genus
VEC_count <- VEC_count[! names(VEC_count) %in% c('na')]
pareto.chart(VEC_count, main="Pareto Bug Genus", ylab="count", ylab2="cum%", cumperc=seq(0,100, by=10))

# detection days
TAB_counts <- DF_Lot_cut_infocus %>% count(days_detect, sort = TRUE, name = "count")
VEC_count <- TAB_counts$count
names(VEC_count) <- TAB_counts$days_detect
VEC_count <- VEC_count[! names(VEC_count) %in% c('na')]
pareto.chart(VEC_count, main="Pareto Detection Days", ylab="count", ylab2="cum%", cumperc=seq(0,100, by=10))




