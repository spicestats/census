# Regions pages ----------------------------------------------------------------

regions <- c("Central Scotland", "Glasgow", "Highlands and Islands", "Lothian",
             "Mid Scotland and Fife", "North East Scotland",  "South Scotland",
             "West Scotland")

lapply(regions, function(x){
  
  output_file <- paste0("_site/", tolower(stringr::str_replace_all(x, " ", "_")), ".html")
  rmarkdown::render("dashboard/regions.Rmd", params = list(region = x),
                    output_file = output_file)
})


# Topics pages -----------------------------------------------------------------

files <- list.files("dashboard", pattern = ".Rmd")
files <- files[files != "regions.Rmd"]
files <- paste0("dashboard/", files)

lapply(files, rmarkdown::render, output_dir = "dashboard/_site")
