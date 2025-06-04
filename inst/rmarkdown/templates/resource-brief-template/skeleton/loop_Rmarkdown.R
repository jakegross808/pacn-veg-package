all_frames <- c("Olaa", "Nahuku/East Rift", "Mauna Loa", "Kahuku", "Kaloko-Honokohau", "Kipahulu District",
  "Haleakala", "Puu Alii", "Kalawao", "Hoolehua", "Tutuila", "Tau", "Guam", "Muchot")

cycle3_certified <- c("Olaa", "Nahuku/East Rift", "Mauna Loa", "Kahuku", "Kaloko-Honokohau")

cycle3_uncertified <- c("Hoolehua")

cycle2_certified <- c("Kipahulu District", "Haleakala", "Puu Alii", "Kalawao", "Hoolehua", "Tutuila", "Tau", "Guam", "Muchot")

for (i in cycle3_uncertified) {
  rmarkdown::render(
    input = "inst/rmarkdown/templates/resource-brief-template/skeleton/skeleton.Rmd",                   # 1. Search for your base report
    output_format = bookdown::html_document2( # 2. Establish the format
      toc= TRUE,
      toc_float = TRUE,
      fig_caption = TRUE,
      number_sections = FALSE,
      df_print = knitr::kable,
      theme = "journal",
      css = "journalnps.min.css",
      includes = rmarkdown::includes(before_body = "header.html")),
    output_file = paste0(i ,".html"),         # 3. Define the output file name
    output_dir = "html_reports/cycle3_uncertified",                         # 4. Define an output folder/directory
    params = list(sample_frame = i))          # 5. Integrate your parameters
}


