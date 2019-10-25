library(tidyverse)
library(ggrepel)

df <- read.csv(file = "repdata_data_StormData.csv")

# The events in the database start in the year 1950 and end in November 2011.
# In the earlier years of the database there are generally fewer events recorded,
# most likely due to a lack of good records.
# More recent years should be considered more complete.

# revise the time format
df$BGN_DATE <- as.Date(df$BGN_DATE, format="%m/%d/%Y 0:00:00")

# histgram by years
hist(df$BGN_DATE, breaks = "years")
# median of the BGN_date
median(df$BGN_DATE)
# pickup after 2002-01-01
df1 <- subset(df, BGN_DATE >= "2002-01-01")

# from data documentation
# Alphabetical characters used to signify magnitude include “K” for thousands, 
# “M” for millions, and “B” for billions.
# replace
df1$PROPDMGEXP_n <- df1$PROPDMGEXP %>%
        str_replace_all(c("K" = "1000", "M" = "1000000", "B" = "1000000000"))
df1$CROPDMGEXP_n <- df1$CROPDMGEXP %>%
        str_replace_all(c("K" = "1000", "M" = "1000000", "B" = "1000000000"))
df1$PROPDMGEXP_n <- as.numeric(df1$PROPDMGEXP_n)
df1$CROPDMGEXP_n <- as.numeric(df1$CROPDMGEXP_n)
df1$PROPDMG_n <- df1$PROPDMG * df1$PROPDMGEXP_n
df1$CROPDMG_n <- df1$CROPDMG * df1$CROPDMGEXP_n

# filter data (event types, fatalities, injuries, property damage, crop damage)
df2 <- df1[,c(8, 23, 24, 40, 41)]
df3 <- df2 %>%
        group_by(EVTYPE) %>%
        summarise(FATALITIES_total = sum(FATALITIES, na.rm = TRUE),
                  INJURIES_total = sum(INJURIES, na.rm =  TRUE),
                  PROPDMG_total = sum(PROPDMG_n, na.rm = TRUE),
                  CROPDMG_total = sum(CROPDMG_n, na.rm =  TRUE)
                  )
# scatterplot1. damage on poplulation health
g <- ggplot(df3, aes(x = log10(FATALITIES_total), y = log10(INJURIES_total), label = EVTYPE))
g + geom_point() + geom_label(alpha = 0.5)
df3 %>%
        arrange(desc(df3$FATALITIES_total+df3$INJURIES_total)) %>%
        head()

# scatterplot2. damage on economics
g <- ggplot(df3, aes(x = log10(PROPDMG_total), y = log10(CROPDMG_total), label = EVTYPE))
g + geom_point() + geom_label(alpha = 0.5)
df3 %>%
        arrange(desc(df3$PROPDMG_total+df3$CROPDMG_total)) %>%
        head()