source("_rinit.R")

## Load Eurostat datasets ------

load("~/iCloud/website/data/bis/XRU_D.RData")
load("~/iCloud/website/data/flags/colors.RData")

figure1 <- XRU_D |>
  filter(iso3c %in% c("DEU", "FRA", "GBR"),
         date >= as.Date("1980-01-01"),
         date <= as.Date("1990-01-01")) |>
  select(date, value, `Reference area`) |>
  left_join(colors, by = c("Reference area" = "country")) 


write.csv(figure1, "csv/figure1.csv")

ggplot(data = figure1) + geom_line(aes(x = date, y = value, color = color)) +
  theme_minimal() + xlab("") + ylab("National Currency Per USD") +
  scale_x_date(breaks = seq(1940, 2030, 1) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_y_log10(breaks = seq(0, 10, 0.2),
                labels = dollar_format(accuracy = 0.1, prefix = "", suffix = "../$")) +
  theme(legend.position = c(0.6, 0.1),
        legend.title = element_blank(),
        legend.direction = "horizontal") +
  scale_color_identity()



ggsave("pdf/figure1.pdf", width = 7, height = 4, device = cairo_pdf)
ggsave("png/figure1.png", width = 7, height = 4)
