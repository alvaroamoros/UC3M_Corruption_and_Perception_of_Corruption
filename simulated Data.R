# Simulated data cultural dimensios of corruption

A <- c(-0.8, +0.9, 0, 0.1, -0.1, 1)
B <- c(+0.9, -0.9, 0.1, 0.2, 0, -0.9)
C <- c(0, 0, -1, -1, -1, 0)
D <- c(-0.1, +0.1, 0, -1, -1, -1)
dim <- c("Deviation-Normallity", "Duty-Irresponsability", "Loyal-Disloyal", "Ethic-Unethic", "Democratic-Undemoctatic", "Logic-Ilogic")
x <- as.data.frame(cbind(dim, A,B,C,D))

x <- x %>%
  mutate(A = as.numeric(A),
         B = as.numeric(B),
         C = as.numeric(C),
         D = as.numeric(D))
class(x$A)

g <- x %>%
  ggplot(aes(dim, A, group = 1))+
  geom_point(aes(dim, A, colour = "Text A"))+
  geom_line(aes(dim, A, colour = "Text A")) +
  geom_point(aes(dim, B, colour = "Text B"))+
  geom_line(aes(dim, B, colour = "Text B")) +
  geom_point(aes(dim, C, colour = "Text C"))+
  geom_line(aes(dim, C, colour = "Text C")) +
  geom_point(aes(dim, D, colour = "Text D"))+
  geom_line(aes(dim, D, colour = "Text D")) 

g + theme_classic() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 25, hjust = 1, size = 12)) +
  ylab("") +
  xlab("")


## V-dem data
library(vdemdata)
data("vdem")
data("codebook")

latin_2019 <- vdem %>%
  filter(year == 2019,
         e_regionpol == 2,
         v2x_polyarchy > 0.6) %>%
  mutate(v2x_polyarchy = as.numeric(v2x_polyarchy))

latin_2016 <- vdem %>%
  filter(year == 2016,
         e_regionpol == 2,
         country_name %in% latin_2019$country_name) %>%
  mutate(v2x_polyarchy = as.numeric(v2x_polyarchy))

g_dem <- latin_2019 %>%
  ggplot(aes(y = v2x_polyarchy, x = reorder(country_text_id,  - latin_2019$v2x_corr), fill = country_name)) +
  geom_col() +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 9)) +
  ylab("Polyarchy Index") +
  xlab("")

g_corr <- latin_2019 %>%
  ggplot(aes(y = v2x_corr, x = reorder(country_text_id,  - latin_2019$v2x_corr), fill = country_name)) +
  geom_col() +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 8)) +
  ylab("Political Corruption Index") +
  xlab("")


g_gdppp <- latin_2016 %>%
  ggplot(aes(y = e_migdppc, x = reorder(country_text_id,  - latin_2019$v2x_corr), fill = country_name)) +
  geom_col() +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 0, hjust = 1, size = 8)) +
  ylab("Gdp per apita") +
  xlab("")

grid.arrange(g_corr, g_dem, g_gdppp)

