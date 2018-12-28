library(ggplot2)
library(dplyr)
occ <- read.csv("../SOURCE_DATA/Participants/2013-2017 Bike MS Participants.csv",
                stringsAsFactors = FALSE)
# data cleansing
occ$Participant.Occupation[occ$Participant.Occupation == "Retail Wholesale"] <-
  "Retail/Wholesale"
occ_grp <- occ %>%
  group_by(Fiscal.Year, Participant.Occupation) %>%
  summarize(total = sum(Total.of.All.Confirmed.Gifts...),
            st_dev = sd(Total.of.All.Confirmed.Gifts...),
            mean = mean(Total.of.All.Confirmed.Gifts...),
            median = median(Total.of.All.Confirmed.Gifts...),
            count = n()) %>%
  ungroup() %>%
  filter(Participant.Occupation != "") %>%
  as.data.frame()

occ_grp_yr <- occ_grp %>%
  group_by(Fiscal.Year) %>%
  summarize(mean.yr = mean(total),
            sd.yr = sd(total)) %>%
  ungroup() %>%
  as.data.frame()
occ_grp <- left_join(occ_grp, 
                     occ_grp_yr,
                     by = "Fiscal.Year")
rm(occ_grp_yr)
occ_grp <- occ_grp %>%
  mutate(t = (total - mean.yr)/sd.yr,
         snr_ratio = total/st_dev)

temp <- occ_grp %>% 
  filter(!(Participant.Occupation %in% c('N/A', 'Retired'))) %>%
  arrange(desc(t))

temp_sum <- occ_grp %>%
  group_by(Participant.Occupation) %>%
  summarize(total_t = median(t)) %>%
  as.data.frame()

temp_sum <- temp_sum %>%
  arrange(desc(total_t))

temp$Participant.Occupation <- factor(temp$Participant.Occupation,
                                      levels = temp_sum$Participant.Occupation,
                                      ordered = TRUE)

ggplot(temp, 
       aes(x = Participant.Occupation,
           y = t,
           fill = factor(Fiscal.Year))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Participant Occupation (ordered by median t-score)") + 
  ylab("t-score") + 
  labs(fill='Fiscal Year') 
top_10_1 <- temp$Participant.Occupation

#|1/(x+1) - 1/x| <= 0.01 implies x >= 100
temp <- occ_grp %>% filter(count > 10)
temp_order <- temp %>%
  group_by(Participant.Occupation) %>%
  summarize(median_snr = median(snr_ratio)) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(desc(median_snr))
temp$Participant.Occupation <- factor(temp$Participant.Occupation,
                                      levels = temp_order$Participant.Occupation,
                                      ordered = TRUE)

ggplot(temp, 
       aes(x = Participant.Occupation,
           y = snr_ratio,
           fill = factor(Fiscal.Year))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
  xlab("Participant Occupation (ordered by median signal-to-noise ratio)") + 
  ylab("Signal-to-Noise Ratio") + 
  labs(fill='Fiscal Year') 
top_10_2 <- temp$Participant.Occupation

top_10_1 <- levels(top_10_1)[1:10]
top_10_2 <- levels(top_10_2)[1:10]
# In both
top_10_1[top_10_1 %in% top_10_2] %>% sort()
top_10_2[top_10_2 %in% top_10_1] %>% sort()

# Not in both
top_10_1[!(top_10_1 %in% top_10_2)]
top_10_2[!(top_10_2 %in% top_10_1)]
