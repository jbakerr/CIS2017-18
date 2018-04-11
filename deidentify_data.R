services <- read.csv("Services1718.csv")

services$Student <- NULL


simple_id <- function(data, cols_to_anon)
{
  to_anon <- subset(data, select = cols_to_anon)
  ids <- unname(apply(to_anon, 1, paste, collapse = ""))
  as.integer(factor(ids))
}
services$fake_ID <- simple_id(services, "Student.ID")

services$Student.ID <- NULL

write.csv(services, "services1718.csv")
