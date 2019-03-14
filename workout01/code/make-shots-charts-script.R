#title:make shots charts script
#description:make some charts
#input:
#output:some charts

library(jpeg)
library(grid)
library(ggplot2)
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
data <- read.csv('../data/shots-data.csv',stringsAsFactors = FALSE)
court_file <- "../images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

thompson_shot_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()
pdf('../images/klay-thompson-shot-chart.pdf',width = 6.5,height = 5)
thompson_shot_chart
dev.off()
#
iguodala_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart:  Andre Iguodala (2016 season)') +
  theme_minimal()
pdf('../images/andre-iguodala-shot-chart.pdf',width = 6.5,height = 5)
iguodala_shot_chart
dev.off()

#
green_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart:  Draymond Green (2016 season)') +
  theme_minimal()
pdf('../images/draymond-green-shot-chart.pdf',width = 6.5,height = 5)
green_shot_chart
dev.off()

#
durant_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart:  Kevin Durant (2016 season)') +
  theme_minimal()
pdf('../images/kevin-durant-shot-chart.pdf',width = 6.5,height = 5)
durant_shot_chart
dev.off()

#
curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart:  Stephen Curry (2016 season)') +
  theme_minimal()
pdf('../images/stephen-curry-shot-chart.pdf',width = 6.5,height = 5)
curry_shot_chart
dev.off()

#
shot_charts <- ggplot(data = data ) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart:  Stephen Curry (2016 season)') +
  theme_minimal()+
  facet_wrap(~name,nrow = 2,ncol = 3)
pdf('../images/gsw-shot-charts.pdf',width = 8,height = 7)
shot_charts
dev.off()
png('../images/gsw-shot-charts.png',width = 8,height = 7,units = 'in',res = 172)
shot_charts
dev.off()