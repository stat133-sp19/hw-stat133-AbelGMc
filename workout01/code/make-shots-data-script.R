#title: data preparation
#description:to create a csv data file shots-data.csv that will contain the required variables to be used in the visualization phase
#input(s):stephen-curry.csv;andre-iguodala.csv;draymond-green.csv;kevin-durant.csv;klay-thompson.csv
#output(s):stephen-curry-summary.txt;andre-iguodala-summary.txt;graymond-green-summary.txt;kevin-durant-summary.txt;
#          klay thompson-summary.txt;shots-data-summary.txt
#          shots-data.csv
library(dplyr)

curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
curry <- mutate(curry,name='Stephen Curry')
iguodala <- mutate(iguodala,name='Andre Iguodala')
green <- mutate(green,name='Graymond Green')
durant <- mutate(durant,name='Kevin Durant')
thompson <- mutate(thompson,name='Klay Thompson')

#
curry$shot_made_flag[curry$shot_made_flag=='n'] <- 'shot_no'
curry$shot_made_flag[curry$shot_made_flag=='y'] <- 'shot_yes'

iguodala$shot_made_flag[iguodala$shot_made_flag=='n'] <- 'shot_no'
iguodala$shot_made_flag[iguodala$shot_made_flag=='y'] <- 'shot_yes'

green$shot_made_flag[green$shot_made_flag=='n'] <- 'shot_no'
green$shot_made_flag[green$shot_made_flag=='y'] <- 'shot_yes'

durant$shot_made_flag[durant$shot_made_flag=='n'] <- 'shot_no'
durant$shot_made_flag[durant$shot_made_flag=='y'] <- 'shot_yes'

thompson$shot_made_flag[thompson$shot_made_flag=='n'] <- 'shot_no'
thompson$shot_made_flag[thompson$shot_made_flag=='y'] <- 'shot_yes'
#
curry <- mutate(curry,minute=period*12+(12-minutes_remaining))
iguodala <- mutate(iguodala,minute=period*12+(12-minutes_remaining))
green <- mutate(green,minute=period*12+(12-minutes_remaining))
durant <- mutate(durant,minute=period*12+(12-minutes_remaining))
thompson <- mutate(thompson,minute=period*12+(12-minutes_remaining))

#
sink('../output/stephen-curry-summary.txt')
summary(curry)
sink()

sink('../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

sink('../output/graymond-green-summary.txt')
summary(green)
sink()

sink('../output/kevin-durant-summary.txt')
summary(durant)
sink()

sink('../output/klay-thompson-summary.txt')
summary(thompson)
sink()
#
x <- rbind(curry,iguodala,green,durant,thompson)
#

write.csv(x,'../data/shots-data.csv')

#

sink('../output/shots-data-summary.txt')
summary(x)
sink()
