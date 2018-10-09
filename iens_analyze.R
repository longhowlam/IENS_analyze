library(arules)
library(dplyr)
library(visNetwork)
library(igraph)
library(arulesViz)

IensReviewers = readRDS("IensReviewers.RDs") 
keukens = IensReviewers %>% group_by(keuken) %>% summarise(n=n())

ienstrx = as(
  split(
    IensReviewers$keuken,
    IensReviewers$reviewer
  ),
  "transactions"
)

## generate market basket rules
rules <- apriori(
  ienstrx,
  parameter = list(
    supp = 0.00010,
    conf = 0.18,
    maxlen = 2
  )
)

## eerste tien regels op basis van lft
 inspect( sort(rules, by = "lift")[2:20])

rulesDF = sort(rules, by = "lift")  %>% 
  DATAFRAME() %>%
  mutate(
    from = as.character(LHS),
    to = as.character(RHS),
    value = lift
  )

nodes = data.frame(
  id = base::unique(c(rulesDF$from, rulesDF$to)),
  stringsAsFactors = FALSE
) %>% mutate(
  title = id,
  label = id
) %>%  arrange(id)

nodes = nodes %>% 
  mutate(
    color = case_when(
      id %in%  c("{CHINEES}", "{JAPANS}", "{VIETNAMEES}", "{MALEISISCH}", "{WOKKEN}" , 
                 "{THAIS}", "{INDONESISCH}", "{KANTONEES}", "{KOREAANS}") ~ "red",
      stringr::str_detect (id , "FRANS") ~ "orange",
      id %in%  c("{ITALIAANS}", "{PIZZERIA}", "{GRIEKS}", "{PORTUGEES}", "{SPAANS}", "{MEDITERRAAN") ~ "orange",
      id %in%  c("{DUURZAAM}", "{BIOLOGISCH}") ~ "green",
      TRUE ~ "blue")
    ) %>% 
  mutate(
    group = case_when(
      color == "red" ~ "aziatisch",
      color == "orange" ~ "frans/mediterraans",
      color == "green" ~ "BIOLOGISCH",
      color == "blue" ~ "overig",
      TRUE ~ "kkk"
    )
  )

lnodes = nodes %>% group_by(color) %>% summarise(label = max(group))
visNetwork(nodes, rulesDF, width = 1600, height = 1200) %>%
  visOptions(highlightNearest = TRUE,  nodesIdSelection = TRUE) %>%
  visEdges(smooth = FALSE) %>% 
  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -200), minVelocity = 1) %>% 
  visLegend(addNodes = lnodes, useGroups = FALSE)


arulesViz::plotly_arules(rules, measure = c("support", "lift"))


