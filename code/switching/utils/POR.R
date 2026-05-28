
#### POR

library(Hmisc)
library(foreign)
library(memisc)
library(haven)

convert_por_to_dta <- function(file_path) {
  
  message("Processing: ", file_path)
  
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  data <- tryCatch(
    {
      message("Trying Hmisc::spss.get() without value labels")
      Hmisc::spss.get(
        file_path,
        use.value.labels = FALSE
      )
    },
    error = function(e1) {
      message("Hmisc failed: ", conditionMessage(e1))
      message("Fallback to foreign::read.spss() without value labels")
      
      tryCatch(
        {
          foreign::read.spss(
            file = file_path,
            use.value.labels = FALSE,
            to.data.frame = TRUE,
            trim.factor.names = FALSE,
            trim_values = FALSE,
            reencode = FALSE
          )
        },
        error = function(e2) {
          message("foreign failed: ", conditionMessage(e2))
          message("Fallback to memisc")
          
          old_wd <- getwd()
          on.exit(setwd(old_wd), add = TRUE)
          
          setwd(dirname(file_path))
          
          as.data.frame(
            as.data.set(
              memisc::spss.portable.file(basename(file_path))
            )
          )
        }
      )
    }
  )
  
  out_path <- sub("\\.por$", ".dta", file_path, ignore.case = TRUE)
  
  haven::write_dta(data, out_path)
  
  message("Saved (modern .dta): ", out_path)
  
  invisible(data)
}


file1 <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files/fi1991/daF1088_eng.por"
file2 <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files/fi1999/daF1042_eng.por"
file3 <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files/be1999/p1693.por"
file4 <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files/be1995/p1422.por"
file5 <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files/nl1994/06740-0001-Data.por"
file6 <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files/nl1998/02836-0001-Data.por"


convert_por_to_dta(file1)
convert_por_to_dta(file2)
convert_por_to_dta(file3)
convert_por_to_dta(file4)
convert_por_to_dta(file5)
convert_por_to_dta(file6)


reticulate::install_python()
reticulate::py_install(c("pandas", "pyreadstat"))
reticulate::py_install("pyreadstat")


library(reticulate)
library(haven)

convert_por_to_dta2 <- function(file_path) {
  
  message("Processing: ", file_path)
  
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  pyreadstat <- reticulate::import("pyreadstat")
  
  res <- pyreadstat$read_por(file_path)
  data <- res[[1]]
  
  out_path <- sub("\\.por$", ".dta", file_path, ignore.case = TRUE)
  haven::write_dta(data, out_path)
  
  message("Saved: ", out_path)
  invisible(data)
}


file7 <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files/nl1989/p1000.por"
convert_por_to_dta2(file7)


library(haven)
library(dplyr)

file_dta <- "C:/Users/koend/OneDrive/Bureaublad/UVA/R_Project/VoteSwitching/VoteSwitching/data/files/nl1989/p1000.dta"

df <- read_dta(file_dta)

# basic structure
dim(df)
names(df)[1:30]
glimpse(df)

# missingness overview
colSums(is.na(df)) %>% sort(decreasing = TRUE) %>% head(10)

# check for labelled variables (SPSS-style)
labelled_vars <- names(df)[sapply(df, haven::is.labelled)]
labelled_vars[1:20]

# inspect a few variables
summary(df[[1]])
summary(df[[5]])

# check for duplicated column names
anyDuplicated(names(df))

# quick value distribution for first few variables
for (v in names(df)[1:5]) {
  cat("\n---", v, "---\n")
  print(head(table(df[[v]], useNA = "ifany")))
}















