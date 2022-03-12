library(dash)
library(dashBootstrapComponents)
library(dashHtmlComponents)
library(dashCoreComponents)
library(plotly)
library(dplyr)

qwl_df <- readr::read_csv("./data/bei_vita_qwl_assessment.csv")
qwl_df$residence <- qwl_df$`Country of Residence`
residence_list <- unique(qwl_df[c("residence")])$residence
print(residence_list)
qwl_df$ts <- as.character(qwl_df$"Total score")

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
    dbcContainer(
        list(
          dbcRow(
            list(
              dbcCol(
                list(
                  dash::htmlBr(),
                  htmlH2("Bei Vita"),
                  htmlP(
                    dccMarkdown(
                      "
                            Visualization to represent how scores for quality of
                            work life are distributed
                            "
                    )
                  )
                ),
                width = 3
              ),
              dbcCol(
                list(
                  htmlH1("Quality of Work Life"),
                  htmlH2("Client Name")
                ),
                width = 6
              ),
              dbcCol(
                list(
                ),
                width = 3
              )
            ),
            align = "center"
          ),
          dbcRow(
            list(
              dbcCol(
                list(
                  htmlH6("Country of Residence"),
                  dccDropdown(
                    id="col-select",
                    options = residence_list %>% purrr::map(function(col) list(label = col, value = col)),
                    value = "HK & Macau"
                  ),
                  dccGraph(id="lineplot")
                  
                ),
                width = 12
              )
            ),
            align = "center"
          ),
          dbcRow(
            list(
              dbcCol(
                list(
                )
              ),
              dbcCol(
                list(
                )
              )
            ),
            align = "center"
          )
        )
    )
)


app$callback(
  output("lineplot", "figure"),
  list(input("col-select", "value")),
  function(xcol) {
    qwl_df_ts <- qwl_df %>% 
      filter(residence == xcol) %>%
      group_by(ts) %>% 
      count()
    
    qwl_df_ts$ts <- as.numeric(qwl_df_ts$ts)
    
    
    p <- ggplot(qwl_df_ts, aes(x = ts)) +
    geom_histogram(color = "lightblue") +
    scale_x_continuous(name="Total Score", limits=c(0, 41)) + 
    scale_y_continuous(name="Number of Employees", limits=c(0, 8)) +
    ggtitle("How healthy are the employees feeling overall?") +
    ggthemes::scale_color_tableau()

    ggplotly(p)
  }
)


app$run_server(debug = F)