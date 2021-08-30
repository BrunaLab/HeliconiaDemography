library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("slice", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("Lag", "tsModel")
conflict_prefer("compose", "flextable")