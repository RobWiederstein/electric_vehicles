scrape_electric_vehicle_web_page <- function(){
# libraries ----
print("1. Loading Libraries . . .")
library(rvest)
library(dplyr)
# page
print("2. Scraping Page . . . ")
html <- read_html("https://ev-database.org/imp/#sort:path~type~order=.rank~number~desc|rs-price:prev~next=10000~100000|rs-range:prev~next=0~500|rs-fastcharge:prev~next=0~1000|rs-acceleration:prev~next=2~23|rs-topspeed:prev~next=60~260|rs-battery:prev~next=10~200|rs-towweight:prev~next=0~2500|rs-eff:prev~next=150~600|rs-safety:prev~next=-1~5|paging:currentPage=0|paging:number=10")

# variables
model <- html %>% html_elements("h2") %>% html_text2()
price <- html %>% html_elements(".country_uk") %>% html_text2()
acceleration <- html %>% html_elements(".acceleration") %>% html_text2()
top_speed <- html %>% html_elements(".topspeed") %>% html_text2()
range <- html %>% html_elements(".erange_real") %>% html_text2()
efficiency <- html %>% html_elements(".efficiency") %>% html_text2()
charge <- html %>% html_elements(".fastcharge_speed_print") %>% html_text2()
n_passenger <- html %>% html_elements(".fa-user+ span") %>% html_text2()
description <- html %>% html_elements(".not-current+ div, .current+div") %>% html_text2()

# array --> tibble ----
print("3. Building tibble . . .")
ev_scrp <- tibble::tibble(
    model = model,
    price = price,
    acceleration = acceleration,
    top_speed = top_speed,
    range = range,
    efficiency = efficiency,
    charge = charge,
    n_passenger = n_passenger,
    description = description
    )

# vars --> numbers ----

ev_scrp %>%
    mutate(incentives = ifelse(stringr::str_detect(pattern = "\\*", price), 1, 0)) %>%
    mutate(price = gsub("\\* |,|£", "", price) |> as.numeric(.)) %>%
    mutate(acceleration = gsub(" sec", "", acceleration) |> as.numeric(.)) %>%
    mutate(top_speed = gsub(" mph", "", top_speed) |> as.numeric(.)) %>%
    mutate(range = gsub(" mi", "", range) |> as.numeric(.)) %>%
    mutate(efficiency = gsub(" Wh/mi", "", efficiency) |> as.numeric(.)) %>%
    mutate(charge = gsub(" mph", "", charge) |> as.numeric(.)) %>%
    mutate(n_passenger = as.integer(n_passenger)) %>%
    mutate(battery_size = stringr::word(description, 1) |> as.numeric(.)) -> ev_mdfd

# American manufacturers
keep_mnfctr <- c("Audi", "BMW", "Fiat", "Ford", "Genesis", "Honda", "Hyundai", "Jaguar",
                 "Jeep", "Kia", "Lexus", "Mazda", "Mercedes-Benz", "Nissan", "Porsche",
                 "Subaru", "Tesla", "Toyota", "Volkswagen", "Volvo")

ev_mdfd |>
    filter(!is.na(price)) |>
    select(!description) |>
    mutate(manufacturer = stringr::word(model, 1)) |>
    filter(manufacturer %in% keep_mnfctr) |>
    mutate(hyundai = ifelse(manufacturer == "Hyundai", 1, 0) |> factor()) %>%
    #convert pounds to dollars
    mutate(price = 1.27 * price) |>
    select(manufacturer, everything())-> ev

saveRDS(ev, "./data/electric_vehicle_eu.rds")
print("4. File saved at './data/electric_vehicle_eu.rds'")
ev
}
ev <- scrape_electric_vehicle_web_page()
