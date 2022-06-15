# install.packages("truncnorm")
library(truncnorm)

data = NULL

data$pinf = rtruncnorm(1000, 0, Inf, 0.1, 0.05)

data$pcont = rtruncnorm(1000, 0, Inf,0.6, 0.05)

data$kk = rtruncnorm(1000, 0, Inf,0.3, 0.1)

data$cc = rtruncnorm(1000, 0, Inf, 12, 1)

data = as.data.frame(data)

p11 = ggplot(data, aes(x = pinf)) +
  geom_histogram(fill = "#e41a1c", color = "#e41a1c", alpha = 0.5) + ggtheme +
  ylab("Counts") +
  xlab("Probability of infection (pinf)")

p12 = ggplot(data, aes(x = pcont)) +
  geom_histogram(fill = "#377eb8", color = "#377eb8", alpha = 0.5) + ggtheme+
  ylab("Counts")+
  xlab("Probability of control (pcont)")

p13 = ggplot(data, aes(x = kk)) +
  geom_histogram(fill = "#984ea3", color = "#984ea3", alpha = 0.5) + ggtheme+
  ylab("Counts")+
  xlab("Replication rate (kk)")

p14 = ggplot(data, aes(x = cc)) +
  geom_histogram(fill = "#4daf4a", color = "#4daf4a", alpha = 0.5) + ggtheme+
  ylab("Counts") +
  xlab("Tolerance to disease (cc)")

p21 = ggplot(data, aes(x = 1, y = pinf)) +
  geom_violin(fill = "#e41a1c", color = "#e41a1c", alpha = 0.5) + ggtheme +
  scale_y_continuous(limits = c(0, 1)) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  ylab("Probability of infection (pinf)")

p22 = ggplot(data, aes(x = 1, y = pcont)) +
  geom_violin(fill = "#377eb8", color = "#377eb8", alpha = 0.5) + ggtheme +
  scale_y_continuous(limits = c(0, 1)) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  ylab("Probability of control (pcont)")

p23 = ggplot(data, aes(x = 1, y = kk)) +
  geom_violin(fill = "#984ea3", color = "#984ea3", alpha = 0.5) + ggtheme +
  scale_y_continuous(limits = c(0, 10)) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  ylab("Replication rate (kk)")

p24 = ggplot(data, aes(x = 1, y = cc)) +
  geom_violin(fill = "#4daf4a", color = "#4daf4a", alpha = 0.5) + ggtheme +
  scale_y_continuous(limits = c(0, 20)) +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())+
  ylab("Tolerance to disease (cc)")

(p11 | p12 | p13 | p14) / (p21 | p22 | p23 | p24)

write.table(data, "shinyapp/www/doseR_output_full.csv", row.names = F, sep = ";", dec = ".", quote = F)



