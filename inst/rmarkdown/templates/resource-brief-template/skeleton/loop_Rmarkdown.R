all_frames <- c("Olaa", "Nahuku/East Rift", "Mauna Loa", "Kahuku", "Kaloko-Honokohau", "Kipahulu District",
  "Haleakala", "Puu Alii", "Kalawao", "Hoolehua", "Tutuila", "Tau", "Guam", "Muchot")


for (i in all_frames) {
  rmarkdown::render(
    input = "skeleton.Rmd",                   # 1. Search for your base report
    output_format = bookdown::html_document2( # 2. Establish the format
      toc= TRUE,
      toc_float = TRUE,
      fig_caption = TRUE,
      number_sections = FALSE,
      df_print = kable,
      theme = "journal",
      css = "journalnps.min.css",
      includes = rmarkdown::includes(before_body = "header.html")),
    output_file = paste0(i ,".html"),         # 3. Define the output file name
    output_dir = ".",                         # 4. Define an output folder/directory
    params = list(sample_frame = i))          # 5. Integrate your parameters
}


