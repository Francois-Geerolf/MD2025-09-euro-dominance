source("_rinit.R")

## Load Eurostat datasets ------

load("~/iCloud/website/data/bis/XRU_D.RData")
load(url("https://fgeerolf.com/data/flags/colors.RData"))

figure1 <- XRU_D |>
  filter(iso3c %in% c("DEU", "FRA", "JPN"),
         date >= as.Date("1980-01-01"),
         date <= as.Date("1990-01-01")) |>
  left_join(colors, by = c("Reference area" = "country")) |>
  group_by(iso3c) |>
  mutate(color = ifelse(iso3c == "FRA", color2, color)) |>
  mutate(value = 100*value/value[date == as.Date("1985-09-20")]) |>
  ungroup() |>
  select(date, value, `Reference area`, color) |>
  arrange(date, `Reference area`)

# Build color mapping: each country gets its hex
country_colors <- figure1 %>% 
  distinct(`Reference area`, color) %>% 
  tibble::deframe()

write.csv(figure1, "csv/figure1.csv")

ggplot(data = figure1) +
  geom_line(aes(x = date, y = value, color = `Reference area`), size = 1.2) +
  scale_color_manual(values = country_colors) +
  theme_minimal(base_size = 14) + xlab("") + ylab("") +
  scale_x_date(breaks = seq(1940, 2100, 1) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_y_log10(breaks = seq(0, 1000, 10),
                labels = dollar_format(pre = "", acc = 1)) +
  theme(legend.position = c(0.8, 0.85),
        legend.title = element_blank(),
        legend.direction = "vertical") +
  geom_vline(xintercept = as.Date("1985-09-20"), linetype = "dashed", size = 1.2)


ggsave("pdf/figure1.pdf", width = 7, height = 4, device = cairo_pdf)
ggsave("png/figure1.png", width = 7, height = 4, bg = "white")
