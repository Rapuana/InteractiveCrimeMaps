
require(httr)
require(jsonlite)
require(stringr)
require(lubridate)
require(tibble)
require(dplyr)
require(tidyverse)
require(readxl)
require(urltools)
require(sf)
require(absmapsdata)
require(ggplot2)
require(ggiraph)

remove_bracket_words <- function(f) {
  return(str_replace_all(f, regex("(\\s+\\([\\w\\s-]+\\))$"), ""))
}
# Read in meshblock data from QGSO
get_lga_data <- function(lga, from_local = FALSE) {
  destfile <- "qgso_meshblock.xlsx.zip"
  url <- "https://www.qgso.qld.gov.au/issues/3736/meshblock-correspondence-file-asgs-2016.zip"
  unzip_destfile <- "meshblock-correspondence-file-asgs-2016.xlsx"
  if (!from_local) {
    download.file(url, destfile)
    # Unzip the file
    unzip(zipfile = destfile, exdir = ".", files = unzip_destfile)
  }
  lga_data <- read_excel(unzip_destfile, sheet = "Data")
  columns <- c(
    "MB_CODE_2016",
    "MB_CATEGORY_NAME_2016",
    "LGA_CODE_2016",
    "LGA_NAME_2016",
    "AREA_ALBERS_SQ_KM",
    "DWELLINGS_2016",
    "PERSONS_USUALLY_RESIDENT_2016",
    "SSC_NAME_2016",
    "SSC_CODE_2016",
    "POA_CODE_2016"
  )
  # Filter the data
  lga_data <- lga_data %>% select(all_of(columns))
  lga_data["LGA_NAME_2016"] <- lapply(
    lga_data["LGA_NAME_2016"], remove_bracket_words
  )
  names(lga_data) <- c(
    "meshblock_code",
    "meshblock_category",
    "lga_code",
    "lga_name",
    "sq_km",
    "n_dwellings",
    "n_residents",
    "suburb",
    "suburb_code",
    "postcode"
  )
  lga_data$meshblock_code <- as.numeric(lga_data$meshblock_code)
  lga_data <- lga_data %>% filter(lga_name == "Brisbane")
}

get_api_url <- function(lga_name, start_date, end_date) {
  url <- sprintf(
    "https://a5c7zwf7e5.execute-api.ap-southeast-2.amazonaws.com/dev/offences?locationType=Lga&startDate=%s&locationName=%s&endDate=%s&format=JSON",
    start_date,
    lga_name,
    end_date
  )
  return(url)
}

call_api_url <- function(url) {
  res <- GET(url = url)
  data <- as_tibble(fromJSON(rawToChar(res$content)))
  names(data) <- c(
    "offence_type",
    "date",
    "postcode",
    "lga_name",
    "meshblock_code"
  )
  return(data)
}

get_data <- function(
  lga_name,
  start_date,
  end_date,
  month_period = 2
) {
  end_date <- as.Date(end_date, "%Y-%m-%d")
  start_date <- as.Date(start_date, "%Y-%m-%d")
  if (end_date > today()) {
    end_date <- today()
  }
  offence_list <- list()
  end_date_str <- format(end_date, "%m-%d-%Y")
  counter <- 1
  while (start_date < end_date) {
    start_date_str <- format(start_date, "%m-%d-%Y")
    current_end_date <- min(start_date + months(month_period), end_date)
    api_url <- get_api_url(lga_name, start_date_str, end_date_str)
    data <- call_api_url(api_url)
    offence_list[[counter]] <- data
    counter <- counter + 1
    start_date <- current_end_date + days(1)
  }
  return(bind_rows(offence_list))
}

offence_list <- get_data(
  "Brisbane",
  "2023-06-30",
  "2024-06-30",
  check_local = TRUE
)
lga_data <- get_lga_data("Brisbane", from_local = TRUE)

# Add the suburb to the offence data
offence_list <- offence_list %>%
  left_join(select(lga_data, meshblock_code, suburb), by = "meshblock_code")

# Map data - combine with LGA data and group meshcodes
# Filter to Queensland only
suburbs_map_data <- suburb2016 %>% filter(state_name_2016 == "Queensland")

detach("package:absmapsdata", unload = TRUE)

# join the suburb data with the map data
map_data <- merge(
  x = lga_data,
  y = suburbs_map_data,
  by.x = "suburb_code",
  by.y = "suburb_code_2016"
) %>%
  filter(lga_name == "Brisbane") %>%
  group_by(suburb) %>%
  summarise(
    n_dwellings = sum(n_dwellings),
    n_residents = sum(n_residents),
    suburb = first(suburb),
    areasqkm = sum(sq_km),
    cent_lat = first(cent_lat),
    cent_long = first(cent_long),
    geometry = first(geometry)
  )

rm(lga_data, suburbs_map_data)
# Make a more readable grouping
map_data["suburb"] <- lapply(map_data["suburb"], remove_bracket_words)

# add pop_dens
map_data <- map_data %>% mutate(
  pop_dens = n_residents / areasqkm
)

offence_list["suburb"] <- lapply(offence_list["suburb"], remove_bracket_words)

# Now we need to summarise the offences by grouping offence type for each suburb
counts <- offence_list %>%
  group_by(suburb, offence_type) %>%
  summarise(count = n()) %>%
  na.omit()

totals <- counts %>%
  group_by(suburb) %>%
  summarise(count = sum(count)) %>%
  mutate(offence_type = "Total") %>%
  na.omit()

offence_data <- bind_rows(counts, totals)
offence_data <- offence_data[order(offence_data$suburb), ]
offence_data_wide <- pivot_wider(
  offence_data,
  id_cols = suburb,
  names_from = offence_type,
  values_from = count,
  values_fill = 0
)

update_map <- function(map_data, offence_data, offence_type, log_scale) {
  my_theme <- function() {
    theme_bw() + theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
  # Filter the data to only the columns required
  plotdf <- offence_data %>% select(all_of(c("suburb", offence_type)))
  names(plotdf) <- c("suburb", "count")
  .map_data <- left_join(map_data, plotdf, by = "suburb")
  .map_data$raw_count <- .map_data$count

  library(RColorBrewer)
  library(ggiraph)

  if (log_scale) {
    .map_data <- .map_data %>% mutate(
      count = log10(count)
    )
  }

  g <- ggplot(.map_data) +
    geom_sf_interactive(
      aes(
        geometry = geometry,
        fill = count,
        group = suburb,
        tooltip = sprintf("%s<br/>Number of Crimes: %s", suburb, raw_count),
        onclick = count
      ),
      show.legend = TRUE
    ) +
    scale_fill_gradientn(colours = brewer.pal(4, "GnBu"), na.value = "white") +
    coord_sf() +
    my_theme()
  if (log_scale) {
    g <- g + labs(fill = "Log10\n Count of Crimes")
  } else {
    g <- g + labs(fill = " Count of Crimes")
  }
  return(g)
}

# Start defining the app

ui <- fluidPage(
  titlePanel("Crime Statistics within Brisbane LGA"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "data_type",
        label = "Choose the type of Crime to visualise",
        choices = list(
          "Total" = "Total",
          "Arson" = "Arson",
          "Assualt" = "Assault",
          "Drug Offences" = "Drug Offences",
          "Fraud" = "Fraud",
          "Good Order Offences" = "Good Order Offences",
          "Handling Stolen Goods" = "Handling Stolen Goods",
          "Homicide (Murder)" = "Homicide (Murder)",
          "Liquor (excl. Drunkenness)" = "Liquor (excl. Drunkenness)",
          "Miscellaneous Offences" = "Miscellaneous Offences",
          "Other Offences Against the Person" = "Other Offences Against the Person",
          "Other Property Damage" = "Other Property Damage",
          "Other Theft (excl. Unlawful Entry)" = "Other Theft (excl. Unlawful Entry)",
          "Robbery" = "Robbery",
          "Traffic and Related Offences" = "Traffic and Related Offences",
          "Trespassing and Vagrancy" = "Trespassing and Vagrancy",
          "Unlawful Entry" = "Unlawful Entry",
          "Unlawful Use of Motor Vehicle" = "Unlawful Use of Motor Vehicle",
          "Weapons Act Offences" = "Weapons Act Offences",
          "Other Homicide" = "Other Homicide",
          "Prostitution Offences" = "Prostitution Offences"
        )
      ),
      checkboxInput("log_scale", label = "Log Base 10 Scale of crime count", value = FALSE)
    ),
    mainPanel = mainPanel(
      girafeOutput("distPlot")
    )
  )
)

# Define the server

server <- function(input, output) {
  # Create the interactive map
  output$distPlot <- renderGirafe(
    {
      girafe(ggobj = update_map(map_data, offence_data_wide, input$data_type, input$log_scale))
    }
  )
}

shinyApp(ui = ui, server = server)
