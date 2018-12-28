library(data.table)
library(readr)
library(dplyr)

files_lst <- list.files("../SOURCE_DATA/Bike Donations")

for (i in 1:length(files_lst)){
  print(paste0("***** LOADING ", files_lst[i], " *****"))
  donations <- read_csv(paste0("../SOURCE_DATA/Bike Donations/", 
                               files_lst[i]),
                        col_types = c(
                          "ciciccidcccccliicclccccccccciicccdcldcicdddcccccc" 
                        ))
  orig_row <- nrow(donations)
  donations <- donations[-(attributes(donations)$problems %>% .$row %>% unique()),]
  print(paste0("**** DISCARDED ", orig_row - nrow(donations), " ROWS ****"))
  rm(orig_row)
  if (i == 1){
    output <- donations
    rm(donations)
  } else {
    output <- rbind(output, donations)
    rm(donations)
  }
}

write.csv(output, "../FORMATTED_DATA/bike_donations.csv", row.names = FALSE)

library(data.table)
library(readr)
library(dplyr)
def <- readxl::read_excel("../Documentation/Field Definitions.xlsx")
# bike_donations <- read_csv("../FORMATTED_DATA/bike_donations.csv",
#                            col_types = c(
#                              "ciciccidcccccliicclccccccccciicccdcldcicdddcccccc" 
#                            ))
bike_donations <- fread("../FORMATTED_DATA/bike_donations.csv")

affiliate_codes <- bike_donations %>%
  group_by(`Donor Affiliate Code`) %>%
  summarize(total_donations = sum(`Gift Amount($)`)) %>%
  as.data.frame() %>%
  arrange(desc(total_donations))

temp <- bike_donations %>%
  arrange(`Donor ZIP`, `Donor Employer`) %>%
  filter(!is.na(`Donor Employer`) & 
           `Donor ZIP` != 0 &
           `Donor Employer` != "Harmons")
# Harmons had the highest donations: it's a grocery store in Utah

zeroes <- bike_donations %>%
  filter(`Donor ZIP` == 0) %>%
  arrange(desc(`Gift Amount($)`))

bike_donations$COMPANY_FIRST_FIVE <- substr(bike_donations$`Donor Employer`, start = 1, stop = 5)
by_freq <- bike_donations %>%
  group_by(tolower(COMPANY_FIRST_FIVE)) %>%
  summarize(COUNT = n(),
            TOTAL_GIFT_AMOUNT = sum(`Gift Amount($)`)) %>%
  ungroup() %>%
  arrange(desc(COUNT))

write.csv(by_freq, "freq_first_five.csv", row.names = FALSE)

company_names <- bike_donations %>%
  select(`Donor Employer`) %>%
  unique() %>%
  arrange(`Donor Employer`)
write.csv(company_names, "company_names.csv", row.names = FALSE)

noble <- bike_donations %>%
  filter(tolower(COMPANY_FIRST_FIVE) == "noble") %>%
  group_by(`Donor Employer`) %>%
  summarize(COUNT = n(),
            TOTAL_GIFT_AMOUNT = sum(`Gift Amount($)`))%>%
  ungroup() %>%
  arrange(desc(TOTAL_GIFT_AMOUNT))

by_gift <- bike_donations %>%
  group_by(tolower(COMPANY_FIRST_FIVE)) %>%
  summarize(COUNT = n(),
            TOTAL_GIFT_AMOUNT = sum(`Gift Amount($)`)) %>%
  ungroup() %>%
  arrange(desc(TOTAL_GIFT_AMOUNT))

  group_by(`Fiscal Year`, COMPANY_FIRST_FIVE, `Donor State`, `Donor ZIP`) %>%
  summarize(COUNT = n(),
            TOTAL_GIFT_AMOUNT = sum(`Gift Amount($)`)) %>%
  ungroup() %>%
  arrange(COUNT, desc(TOTAL_GIFT_AMOUNT), `Donor Employer`)
