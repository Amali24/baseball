library(tidyverse)
library(dplyr)
library(ggplot2)

rm(list = ls())

data(Batting, package="Lahman")
batting <- Batting %>%
  select(yearID, H, X2B, X3B, HR, BB, HBP, AB, SF) %>%
  filter(yearID >= 1901) %>%
  group_by(yearID) %>%
  summarise_all(~sum(.,na.rm = TRUE)) %>%
  mutate(G = c(1109,1115,1114,1248,1237,1227,1233,1244,1241,1249,1237,1232,
                   1234,1880,1864,1247,1247,1016,1118,1234,1229,1238,1233,1231,
                   1228,1234,1236,1231,1229,1234,1236,1233,1226,1223,1228,1238,
                   1239,1223,1231,1236,1244,1224,1238,1242,1230,1242,1243,1237,
                   1240,1238,1239,1239,1240,1237,1234,1239,1235,1235,1238,1236,
                   1430,1621,1619,1626,1623,1615,1620,1625,1946,1944,1938,1859,
                   1943,1945,1934,1939,2103,2102,2099,2105,1394,2107,2109,2105,
                   2103,2103,2105,2100,2106,2105,2104,2106,2269,1600,2017,2267,
                   2266,2432,2428,2429,2429,2426,2430,2428,2431,2429,2431,2428,
                   2430,2430,2429,2430,2431,2430,2429,2428,2430,2431,2429)) %>%
  mutate(X1B = H - X2B - X3B - HR) %>%
  mutate(TB = X1B + 2 * X2B + 3 * X3B + 4 * HR) %>%
  mutate(SLG = TB / AB) %>%
  mutate(OBP = (H + BB + HBP) / (AB + BB + HBP + SF)) %>%
  mutate(OPS = OBP + SLG) %>%
  mutate(AVG = H / AB) %>%
  mutate(ISO = SLG - AVG)

ISO_plot <- ggplot() +
  geom_line(data = batting, 
            aes(x = yearID, y = ISO), 
            color = "dodger blue", size = 2) +
  ggtitle("League wide Isolated Power (ISO) by Season") +
  xlab("Season") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank())
ISO_plot
                    

head(batting)


