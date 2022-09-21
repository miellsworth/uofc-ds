library(dplyr)
library(ggplot2)
library(cowplot)

df <- data.frame(rbind(c(11, "Largest Connected Component Size", "250 Co-Features"),
                          c(35-11, "Remaining Characters", "250 Co-Features"),
                          c(210, "Largest Connected Component Size", "50 Co-Features"),
                          c(253-210, "Remaining Characters", "50 Co-Features"),
                          c(6393, "Largest Connected Component Size", "All"),
                          c(6411-6393, "Remaining Characters", "All")), stringsAsFactors = FALSE)
names(df) <- c("Count", "Count Type", "Dataset")
df$Count <- as.numeric(df$Count)
df <- df %>%
  group_by(Dataset) %>%
  mutate(Proportion = round(Count/sum(Count), 3))

df %>%
  ggplot(aes(x = Dataset,
             y = Count,
             fill = factor(`Count Type`, levels = c("Remaining Characters", "Largest Connected Component Size")))) +
  geom_col(position = position_fill()) +
  ylab("Proportion") +
  scale_fill_discrete(name = "") +
  geom_text(aes(label = Proportion), position = position_fill(vjust = 0.5))

r1 <- df %>%
  filter(Dataset == "250 Co-Features") %>%
  ggplot(aes(x = `Count Type`, y = Count, fill = reorder(`Count Type`, desc(Count)))) +
  geom_col() +
  xlab("") +
  geom_text(aes(label = Count), position = position_dodge(), vjust = 2) + 
  theme(legend.position = "none") +
  ggtitle("250 Co-Features")
r2 <- df %>%
  filter(Dataset == "50 Co-Features") %>%
  ggplot(aes(x = `Count Type`, y = Count, fill = reorder(`Count Type`, Count))) +
  geom_col() +
  xlab("") +
  geom_text(aes(label = Count), position = position_dodge(), vjust = 2) + 
  theme(legend.position = "none") +
  ggtitle("50 Co-Features")
r3 <- df %>%
  filter(Dataset == "All") %>%
  ggplot(aes(x = `Count Type`, y = Count, fill = reorder(`Count Type`, Count))) +
  geom_col() +
  geom_text(aes(label = Count), position = position_dodge(), vjust = 2) + 
  theme(legend.position = "none") +
  ggtitle("All")
plot_grid(r1, r2, r3, ncol = 1)
