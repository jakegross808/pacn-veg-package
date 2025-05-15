

# for (i in unique(iris$Species)) {
#   rmarkdown::render(
#     input = "loop_test_report.Rmd",              # 1. Search for your base report
#     output_format = "html_document",         # 2. Establish the format
#     output_file = paste0(i ,"_report.html"), # 3. Define the output file name
#     output_dir = ".",                       # 4. Define an output folder/directory
#     params = list(species = i))             # 5. Integrate your parameters
# }

#bookdown::html_document2:
#  toc: true
#toc_float: true
#fig_caption: true
#number_sections: false
#df_print: kable
#theme: journal
#css: journalnps.min.css
#includes:
#  before_body:
#  - header.html

for (i in c("Kahuku")) {
  rmarkdown::render(
    input = "skeleton.Rmd",              # 1. Search for your base report
    output_format = html_document2(toc= TRUE,
                                   toc_float = TRUE,
                                   fig_caption = TRUE,
                                   number_sections = FALSE,
                                   df_print = kable,
                                   theme = "journal"),         # 2. Establish the format
    output_file = paste0(i ,".html"), # 3. Define the output file name
    output_dir = ".",                       # 4. Define an output folder/directory
    params = list(sample_frame = i))             # 5. Integrate your parameters
}


