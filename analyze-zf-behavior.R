library(tidyverse)

csvFile <- './TFPd1/movement_monitoring_multiAutoRefZB5-20220822T154421-XY_data.csv'
exp_name <- "TFPd1"

#  binsize is 60 sec, maxtime derived from data
binsize <- 60

# cutoff, in timeCutoff move moveCutoff pixel
moveCutoff <- 0.6
timeCutoff <- 0.1

#import data from the original data
run_hdata <- read_csv(csvFile)

# only keep useful columns
run_hdata <- run_hdata[,c(1,4,6,7,11,13)]

maxtime <- tail(run_hdata, n=1)$RUNTIME

run_hdata <- run_hdata %>% filter(!near(X,0))

# calculate distance within timeCutoff with minimum moveCutoff
get_dist_denoise <- function(run) {
  xdata <- 0
  ydata <- 0
  run_n1 <- run[0,]
  # first get run_n1, the table allow any changes between lines
  for (i in 1:nrow(run)) {
    if(run[[3]][i]!=xdata | run[[4]][i]!=ydata) {
      run_n1 <- add_row(run_n1,run[i,])
      xdata <- run[[3]][i]
      ydata <- run[[4]][i]
    }
  }
  
  run_n <- run_n1[1,]
  for (i in 2:(nrow(run_n1)-1)) {
    #    print(i)
    
    if( run_n1[[3]][i-1] != run_n1[[3]][i+1] | run_n1[[4]][i-1] != run_n1[[4]][i+1]) {
      run_n <- add_row(run_n,run_n1[i,])
    }
  }
  
  # add dist colum
  run_n <- mutate(run_n, dist =0)
  #  run_n$dist <- 0
  
  i <- 1
  while (i < nrow(run_n)) {
    x0 <- run_n[[3]][i]
    y0 <- run_n[[4]][i]
    j <- i +1
    timeDiff <- run_n[[1]][j] - run_n[[1]][i]
    isMove <- 0
    while (j <= nrow(run_n) & timeDiff <= timeCutoff & !isMove) {
      #    print(c(i,j))
      if ( abs(x0-run_n[[3]][j]) + abs(y0-run_n[[4]][j]) >= moveCutoff) {
        isMove <- 1
        run_n$dist[j] = abs(x0-run_n[[3]][j]) + abs(y0-run_n[[4]][j])
        i <- j-1
      } else {
        j <- j + 1
        timeDiff <- run_n[[1]][j] - run_n[[1]][i]
      }
    }
    i <- i + 1
  }
  run_n <- filter(run_n, dist > 0)
  run_n
}

####################################################################################
# each arena, calculate distance
####################################################################################

run_arena <- run_hdata %>% group_by(ARENA)
run_arena <- group_split(run_arena)

run_n_comb <- run_hdata[0,]

for (i in seq_along(run_arena)) {
  print (i)
  run_n <- get_dist_denoise(run_arena[[i]])
  run_n_comb <- bind_rows(run_n_comb,run_n)
}

write_csv(run_n_comb, paste0("run_n.",exp_name,".csv"))

#  count number of moves per 60 second
hist_tb <- run_n_comb %>% group_by(ARENA) %>%
  count(cut_width(RUNTIME, 60, boundary = 0, labels = FALSE), name = 'y') %>%
  rename(x = `cut_width(RUNTIME, 60, boundary = 0, labels = FALSE)`) %>%
  mutate(x = x*60)

# fill in '0' to empty cells, by pivot wider and back
hist_tb$ARENA <- paste0( 'a', hist_tb$ARENA)
hist_tb1 <- hist_tb %>%
  pivot_wider(names_from = ARENA, values_from = y) %>%
  arrange(x)
hist_tb2 <- hist_tb1 
hist_tb2[is.na(hist_tb2)] <- 0

#add NA to certain cell not shown up in the input file
for (i in seq(1,96)) {
  if (! paste0('a',i) %in% colnames(hist_tb2) ) {
    hist_tb2[[paste0('a',i)]] <- NA
  }
}

hist_tb <- hist_tb2 %>%
  pivot_longer(paste0('a',seq(1,96)), names_to = 'ARENA', values_to='y') %>%
  select(ARENA, everything())

#assign certain cell to NA
#hist_tb$y[hist_tb$ARENA=='a3'] <- NA
#hist_tb$y[hist_tb$ARENA=='a22'] <- NA

hist_tb_total <- hist_tb %>% 
  group_by(ARENA) %>% 
  summarise(total=sum(y))


write_csv(hist_tb, paste0("hist_tb.",exp_name,".csv"))
#write_csv(hist_tb1, paste0("hist_tb1.",exp_name,".csv"))
#write_csv(hist_tb2, paste0("hist_tb2.",exp_name,".csv"))
write_csv(hist_tb_total, paste0("hist_tb_total.",exp_name,".csv"))


####################################################################################
y_lim = 100
x_lim = 2.0

# plot all 96 cells

hist_all <- hist_tb %>%
  group_by(x) %>%
  summarize(mean = mean(y, na.rm=TRUE)) %>%
  mutate(x = x/3600)

cp <- ggplot(head(hist_all,-1), aes(x,mean)) +
  theme_bw() +
  geom_ribbon(aes(ymin=mean*0.9, ymax=mean*1.1), alpha=0.2) +
  geom_line(size=1) +
  coord_cartesian(xlim=c(0,x_lim), ylim=c(0,y_lim)) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  xlab("Time (hour)") +
  ylab("Number of bouts/min")
cp
ggsave(paste0(exp_name,"_all96.png"), cp)


####################################################################################

grp_nm <- c("odd_row", "even_row")
grps <- tibble(
  "a"= paste0('a',seq(1,96)),
  "grp"= rep(c(rep(grp_nm[1],12),rep(grp_nm[2],12)),4)
)
hist_grps <- hist_tb %>% 
  left_join(grps, c("ARENA" = "a")) %>% 
  group_by(x, grp) %>%
  summarize(mean = mean(y, na.rm=TRUE)) %>%
  ungroup %>%
  group_by(grp) %>%
  mutate(cum = cummean(mean)) %>%
  mutate(x = x/3600)

#plot the histogram for two groupings
hist_total_grps <- hist_tb_total %>% 
  left_join(grps, c("ARENA" = "a")) 

p <- ggplot(hist_total_grps, aes(x=total, color=grp)) +
  geom_histogram(fill="white")+
  facet_grid(grp ~ .)
p
ggsave(paste0(exp_name,"_movement_histogram.png"), p)


#plot the mean and cummean
cp <- ggplot(head(hist_grps,-2), aes(x,mean)) +
  theme_bw() +
  geom_ribbon(aes(ymin=mean*0.9, ymax=mean*1.1,fill=grp), alpha=0.2) +
  scale_fill_manual(breaks = grp_nm, values=c("black", "red")) +
  geom_line(aes(color=grp), size=1) +
  scale_color_manual(breaks = grp_nm, values=c("black", "red")) +
  coord_cartesian(xlim=c(0,x_lim), ylim=c(0,y_lim)) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  xlab("Time (hour)") +
  ylab("Number of bouts/min")
cp
ggsave(paste0(exp_name,"_mean_",grp_nm[1],"_",grp_nm[2],".png"), cp)
ggsave(paste0(exp_name,"_mean_",grp_nm[1],"_",grp_nm[2],".pdf"), cp)

# cummean
cp <- ggplot(head(hist_grps,-2), aes(x,cum)) +
  theme_bw() +
  geom_ribbon(aes(ymin=cum*0.9, ymax=cum*1.1,fill=grp), alpha=0.2) +
  scale_fill_manual(breaks = grp_nm, values=c("black", "red")) +
  geom_line(aes(color=grp), size=1) +
  scale_color_manual(breaks = grp_nm, values=c("black", "red")) +
  coord_cartesian(xlim=c(0,x_lim), ylim=c(0,y_lim)) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold")) +
  xlab("Time (hour)") +
  ylab("Number of bouts/min")
cp
ggsave(paste0(exp_name,"_cummean_",grp_nm[1],"_",grp_nm[2],".png"), cp)
ggsave(paste0(exp_name,"_cummean_",grp_nm[1],"_",grp_nm[2],".pdf"), cp)


####################################################################################
# paired t.test output

g1 <- hist_grps %>% ungroup() %>% filter(grp==grp_nm[1]) %>% dplyr::select(mean)
g2 <- hist_grps %>% ungroup() %>% filter(grp==grp_nm[2]) %>% dplyr::select(mean)
sink(paste0(exp_name,"_mean_",grp_nm[1],"_",grp_nm[2],".ttest.txt"))
res <- t.test(g1[[1]], g2[[1]], paired = TRUE, na.action=na.omit)
print(res)
sink()

g1 <- hist_grps %>% ungroup() %>% filter(grp==grp_nm[1]) %>% dplyr::select(cum)
g2 <- hist_grps %>% ungroup() %>% filter(grp==grp_nm[2]) %>% dplyr::select(cum)
sink(paste0(exp_name,"_cummean_",grp_nm[1],"_",grp_nm[2],".ttest.txt"))
res <- t.test(g1[[1]], g2[[1]], paired = TRUE, na.action=na.omit)
print(res)
sink()





