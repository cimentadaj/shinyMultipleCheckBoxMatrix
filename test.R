library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  selectInput("working", "Working", choices = c("Select" = "", "Yes", "No")),
  conditionalPanel(
    condition = "input.working == 'Yes'",
    uiOutput("dynamicOccupationQuestion")
  )
)

server <- function(input, output, session) {
  output$dynamicOccupationQuestion <- renderUI({
    req(input$working)
    # if (input$working == "Yes") {
    selectInput(
      "occupation",
      "What is your current occupation?",
      choices = c("Write down your occupation" = "", "Manager"),
      selectize = TRUE,
      width = "100%"
    )
    # }
  })

  observeEvent(input$working, {
    if (input$working == "No") {
      print("Resetted input")
      reset("occupation")
    }
  })

  observe({
    print("Current occupation")
    print(input$occupation)
  })
}

shinyApp(ui = ui, server = server)


library(dplyr)
library(tidyr)

# Sample input provided as a list
input_responses <- list(
  `You live with:` = list("No one", "Partner"),
  `You regularly share/pool resources with:` = list("Children under 16"),
  `You sometimes receive income from:` = list("Children under 16")
)

# Possible responses for simplification in this example
possible_responses <- c("No one", "Partner", "Children under 16", "Child(ren) 16 or older", "Other relatives (fi. parents)")

# Initialize an empty data frame with dynamic column names
questions <- c("You live with:", "You regularly share/pool resources with:", "You sometimes receive income from:")

columns <- sapply(questions, function(q) {
  sapply(possible_responses, function(r) {
    gsub("[:/() ]", "", paste(tolower(gsub(" ", "_", q)), tolower(gsub(" ", "_", r)), sep = "_"))
  })
}, simplify = "vector")

df <- setNames(data.frame(matrix(0, nrow = 1, ncol = length(columns))), columns)

# Populate the data frame
for (question in names(input_responses)) {
  for (response in unlist(input_responses[[question]])) {
    col_name <- gsub("[:/() ]", "", paste(tolower(gsub(" ", "_", question)), tolower(gsub(" ", "_", response)), sep = "_"))
    if (col_name %in% names(df)) {
      df[1, col_name] <- 1
    }
  }
}

df


## edu_pay <- 110000 # 440 a day, 55 an hour
## average_ds <- 96000 # 384 a day, 48 an hour
## jorge_ds <- 75000 # 300 a day, 37 an hour
## 50 + (50 * 0.7)
## 80 * 8 * 30 * 3


## If pay is weekly or daily, my fee is 100 per hour.
## If pay is arranged per month, for projects of 3+ months I can apply a 20% discount to 80
## 25K a month for image recognition
## Weird reviews model
## No touching recommender systems
## computer vision model will be an LLM

# https://www4.wider.unu.edu/?ind=57,58,59,60,61,62,63,64,65,66&type=table&year=70&byCountry=false&slider=buttons&avg=precomputed&yearRange=0,70&cfg=MYewdgZglg5gdAZwBYgO4FkCmAXAhgXglwBsFMg&iso=AFG,ALB,DZA,AND,AGO,AIA,ATG,ARG,ARM,ABW,AUS,AUT,AZE,BHS,BHR,BGD,BRB,BLR,BEL,BLZ,BEN,BMU,BTN,BOL,BIH,BWA,BRA,VGB,BRN,BGR,BFA,BDI,KHM,CMR,CAN,CPV,CYM,CAF,TCD,CHL,CHN,COL,COM,COD,COG,CRI,CIV,HRV,CUB,CUW,CYP,CZE,DNK,DJI,DMA,DOM,ECU,EGY,SLV,GNQ,ERI,EST,SWZ,ETH,FJI,FIN,FRA,GAB,GMB,GEO,DEU,GHA,GRC,GRL,GRD,GTM,GIN,GNB,GUY,HTI,HND,HKG,HUN,ISL,IND,IDN,IRN,IRQ,IRL,ISR,ITA,JAM,JPN,JOR,KAZ,KEN,KIR,PRK,KOR,XKX,KWT,KGZ,LAO,LVA,LBN,LSO,LBR,LBY,LIE,LTU,LUX,MAC,MDG,MWI,MYS,MDV,MLI,MLT,MHL,MRT,MUS,MEX,FSM,MDA,MCO,MNG,MNE,MSR,MAR,MOZ,MMR,NAM,NRU,NPL,NLD,NZL,NIC,NER,NGA,MKD,NOR,OMN,PAK,PLW,PAN,PNG,PRY,PER,PHL,POL,PRT,PRI,QAT,ROU,RUS,RWA,KNA,LCA,VCT,WSM,SMR,STP,SAU,SEN,SRB,SYC,SLE,SGP,SXM,SVK,SVN,SLB,SOM,ZAF,SSD,ESP,LKA,SDN,SUR,SWE,CHE,SYR,TWN,TJK,TZA,THA,TLS,TGO,TON,TTO,TUN,TUR,TKM,TCA,TUV,UGA,UKR,ARE,GBR,USA,URY,UZB,VUT,VEN,VNM,PSE,YEM,ZMB,ZWE
wiid <- read_delim("~/Downloads/wiid-data.csv", delim = ";")

wiid_c <-
  wiid %>%
  rename_all(~ tolower(.x) %>% str_replace_all("\\(.+\\)", "")) %>%
  pivot_longer(-c(iso, country, year))

ppp <- read_csv("~/Downloads/ppp_2017.csv")
# https://www.xe.com/currencytables/?from=EUR&date=2017-05-01#table-section
euro_exchange <- read_csv("~/Downloads/euro_currency_exchange.csv")
country_currency <- read_csv("~/Downloads/country_currency.csv")

excluded_countries <-
  country_currency %>%
  count(country) %>%
  filter(n > 1) %>%
  pull(country)

excluded_countries <- c(
  excluded_countries,
  # countries with no currency value in database
  c(
    "Mauritania", "São Tomé and Príncipe", "Sierra Leone", "Somaliland",
    "South Sudan", "Transnistria"
  )
)

country_currency <-
  country_currency %>%
  filter(!country %in% excluded_countries)

currency_exchange <-
  select(country_currency, currency_code, country) %>%
  left_join(euro_exchange, by = c("currency_code" = "currency")) %>%
  select(country, eur_per_units)

res <-
  wiid_c %>%
  left_join(ppp, by = c("country")) %>%
  left_join(currency_exchange, by = c("country")) %>%
  select(country, name, value, Ppp_2017, eur_per_units) %>%
  mutate(
    income_year = value * Ppp_2017 * eur_per_units,
    income_month = income_year / 12
  ) %>%
  select(country, name, income_year, income_month) %>%
  mutate(name = trimws(name))


res %>%
  write_csv("~/Downloads/income_countries.csv")

wiid %>%
  rename_all(~ tolower(.x) %>% str_replace_all("\\(.+\\)", "")) %>%
  pivot_longer(-c(iso, country, year)) %>%
  filter(country == "Spain")


library(tidyverse)


res <- read_delim("~/Downloads/all_income/data_p0_p50.csv", delim = ";")
col_names <- str_extract(names(res), "(?<=\\n)[^\\n]+$")
col_names[1:2] <- c("Percentile", "Year")
names(res) <- col_names

p0_p10 <-
  res %>%
  pivot_longer(-c(Percentile, Year)) %>%
  mutate(
    world_region = if_else(name %in% c("North America", "Latin America", "Africa", "Europe", "Asia"), 1, 0)
  )

res <- read_csv("~/Downloads/all_income/data_p50_p100.csv")
col_names <- str_extract(names(res), "(?<=\\n)[^\\n]+$")
col_names[1:2] <- c("Percentile", "Year")
names(res) <- col_names

p50_p100 <-
  res %>%
  pivot_longer(-c(Percentile, Year)) %>%
  mutate(
    world_region = if_else(name %in% c("North America", "Latin America", "Africa", "Europe", "Asia"), 1, 0)
  )

p0_p10 %>%
  bind_rows(p50_p100) %>%
  group_by(name, Year) %>%
  distinct(Percentile, .keep_all = TRUE) %>%
  arrange(name, Percentile) %>%
  write_csv("~/Downloads/all_income/all_income_cleaned.csv")

all_income <- read_csv("~/Downloads/all_income/all_income_cleaned.csv")

world_regions <-
  world %>%
  select(Country = country, Region = regionun) %>%
  mutate(Region = case_when(
    Region == "Latin America/Caribbean" ~ "Latin America",
    Region == "Australia/New Zealand/Oceania" ~ "Asia",
    Region == "USA/Canada" ~ "North America",
    TRUE ~ Region
  )) %>%
  as_tibble()

# Modified function to process the request
process_income_data <- function(country_name, income_number) {
  data_country <- all_income %>% filter(country == country_name)

  if (nrow(data_country) == 0) {
    return(list(below = NA, above = NA, found = FALSE))
  }

  below_income <- sum(data_country$income_month < income_number) / nrow(data_country) * 100
  above_income <- sum(data_country$income_month >= income_number) / nrow(data_country) * 100
  return(list(below = below_income, above = above_income, found = TRUE))
}

process_income_data("United Kingdom", 4000)

library(DIGCLASS)
library(dplyr)

# Internal data for the European Social Survey round 6
# containing different ISCO variables
ess %>%
  transmute(
    isco68,
    isei = isco68_to_isei(isco68),
    egp = isco68_to_egp(isco68, self_employed, emplno),
    egp_labels = isco68_to_egp(isco68, self_employed, emplno, n_classes = 5, label = TRUE)
  ) %>%
  distinct(egp_labels)


ess %>%
  transmute(
    isco08,
    oesch5 = isco08_to_oesch(isco08, self_employed, emplno, n_classes = 5, label = TRUE),
  ) %>%
  distinct(oesch5)

ess %>%
  transmute(
    isco08_high_level = DIGCLASS::isco08_swap(isco08, from = 4, to = 1)
  ) %>%
  left_join(all_labels$isco08, by = c("isco08_high_level" = "ISCO08")) %>%
  distinct() %>%
  arrange(isco08_high_level)



library(httr)

request_token <- function() {
  # Your Reddit app details
  client_id <- ""
  client_secret <- ""
  username <- ""
  password <- ""

  # Authenticate and get a token
  response <- POST(
    url = "https://www.reddit.com/api/v1/access_token",
    body = list(grant_type = "password", username = username, password = password),
    encode = "form",
    authenticate(client_id, client_secret),
    verbose()
  )

  # Extract token from response
  token <- content(response)$access_token
  token
}

token <- request_token()

# Make a request using the token
response <- GET(
  url = "https://oauth.reddit.com/r/programming",
  add_headers(Authorization = paste("bearer", token)),
  verbose()
)

# Extract data from the response
data <- content(response)



results <- list()

for (i in 1:500) {
  shared_df <- data.frame()
  tryCatch(
    {
      shared_df[i, "location"] <- "hey"
      write_csv(shared_df[i, ])
    },
    error = function(e) {
      # Error handler
      NA
      write_csv(shared_df["event", ])
    }, finally = {
      # Finally block (optional)
      print("This is executed no matter what")
    }
  )
}


for (i in 1:2) {
  x <- i
  rm(x)
}


library(tidyverse)
library(lubridate)
library(readxl)
library(plotly)

parse_custom_date <- function(date_string) {
  if (grepl(" de ", date_string)) {
    # If "de" is present, use the original format
    parsed_date <- parse_date(date_string, format = "%d de %B %Y", locale = locale("es"))
  } else {
    # If "de" is absent, adjust the format accordingly
    parsed_date <- parse_date(date_string, format = "%d %B %Y", locale = locale("es"))
  }

  if (is.na(parsed_date)) {
    warning("Date parsing failed for: ", date_string)
  }

  return(parsed_date)
}

prepare_event <- function(event_index) {
  event_name <- excel_sheets("~/Downloads/Data Jorge.xlsx")[event_index]
  res <- suppressMessages(read_xlsx("~/Downloads/Data Jorge.xlsx", sheet = event_name))

  completely_empty <-
    res %>%
    rowwise() %>%
    group_split() %>%
    map_dbl(~ sum(is.na(.x)) / ncol(.x))

  df_sep <- which(completely_empty == 1)
  df_sep_index <- c(0, df_sep[1], nrow(res))
  event_details <- res[1:df_sep_index[2], ] %>% drop_na(`Evento:`)

  event_details <-
    event_details %>%
    select(1:2) %>%
    separate(2, into = c("ticket", "disponible", "precio"), sep = "->", fill = "right") %>%
    mutate(across(everything(), str_trim)) %>%
    mutate(
      ticket = tolower(ticket),
      disponible = str_extract(disponible, "\\d+"),
      precio = as.numeric(str_trim(gsub("RD|\\$|,|Precio|\\.", "", precio)))
    )

  event_date <- parse_custom_date(event_details$ticket[1])
  event_details$ticket[1] <- as.character(event_date)

  event <- res[df_sep_index[2]:nrow(res), ] %>% drop_na(`Evento:`)
  event <- set_names(event, gsub("\\s", "", tolower(unlist(event[1, ]))))
  event <- event[-1, ]

  event <-
    event %>%
    select(fecha, tickets, tipodeticket, total) %>%
    mutate(
      fecha = ymd_hms(fecha),
      tickets = as.numeric(tickets),
      total = as.numeric(gsub("RD\\$ ", "", total)),
      tipodeticket = tolower(str_trim(gsub(":.+", "", tipodeticket)))
    )

  given_away <-
    event %>%
    filter(total == 0) %>%
    group_by(tipodeticket) %>%
    summarize(tickets = sum(tickets))

  event_details <-
    event_details %>%
    left_join(given_away, by = c("ticket" = "tipodeticket")) %>%
    mutate(
      tickets = ifelse(is.na(tickets), 0, tickets),
      disponible = as.numeric(disponible) - tickets
    ) %>%
    select(-tickets)

  event_cleaned <-
    event %>%
    left_join(
      select(event_details, ticket, disponible, precio),
      by = c("tipodeticket" = "ticket")
    ) %>%
    select(fecha, tickets, tipodeticket, disponible, total, precio) %>%
    drop_na() %>%
    filter(total != 0) %>%
    group_by(tipodeticket) %>%
    arrange(fecha) %>%
    mutate(
      filling_curve = cumsum(tickets) / disponible,
      days_to_event = as.numeric(difftime(fecha, event_date, units = "days")),
      event_name = event_name
    ) %>%
    ungroup()

  list(event_cleaned = event_cleaned, event_details = event_details)
}

plot_event <- function(event_cleaned) {
  min_day <- min(event_cleaned$days_to_event)
  event_cleaned %>%
    ggplot(aes(days_to_event, filling_curve, group = tipodeticket, color = tipodeticket)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_x_continuous(
      limits = c(min_day, 0),
      breaks = seq(min_day, 0, by = 3),
      labels = function(x) round(x, 0)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_minimal()
}

plot_event(prepare_event(1)$event_cleaned) %>% ggplotly()

combined_events <- map_dfr(1:23, ~ {
  prepare_event(.x)$event_cleaned %>%
    mutate(
      unique_name = paste0(event_name, "_", tipodeticket)
    )
})

plt <-
  combined_events %>%
  ggplot(aes(days_to_event, filling_curve, group = unique_name)) +
  geom_line(alpha = 1 / 3) +
  geom_point(alpha = 1 / 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(
    limits = c(min(combined_events$days_to_event), 0),
    breaks = seq(min(combined_events$days_to_event), 0, by = 3),
    labels = function(x) round(x, 0)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal()

ggplotly(plt)


sales_data <-
  combined_events %>%
  filter(unique_name %in% c("Report Data_general"))

# Calculate the operational period in minutes
operational_period <- as.numeric(difftime(max(sales_data$fecha), min(sales_data$fecha), units = "mins"))

# Sum total tickets sold
total_tickets_sold <- sum(sales_data$tickets)

# Calculate lambda (average rate of sales per minute)
lambda <- total_tickets_sold / operational_period

# Maximum number of tickets you want to forecast for
max_tickets <- 3

# Calculate probabilities for selling 0 to max_tickets using Poisson distribution
probabilities <- dpois(x = 0:max_tickets, lambda = lambda)

# Create a data frame for better visualization
forecasted_demand <- tibble(
  Tickets_Sold = 0:max_tickets,
  Probability = probabilities
)

model_df <-
  combined_events %>%
  group_by(event_name, total) %>%
  summarize(
    tickets = sum(tickets)
  ) %>%
  mutate(event_name = trimws(event_name)) %>%
  ungroup() %>%
  arrange(total) %>%
  filter(total < 15000, total > 570) %>%
  filter(tickets < 230)

model_df %>%
  ggplot(aes(x = total, tickets)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(I(x / 500), 2, raw = TRUE), se = TRUE) +
  scale_x_continuous(labels = scales::dollar) +
  theme_minimal()

summary(lm(tickets ~ poly(I(total / 100), 4, raw = TRUE), data = model_df))


