library(R6)
library(AMR)

pathogen_info <- R6Class(
  "pathogen_info",
  public = list(
    name_pathogen = NULL,
    mo_code = NULL,
    kingdom = NULL,
    gramstrain = NULL,
    genus = NULL,
    mdr = NULL,
    ast_table = NULL,
    initialize = function(name_pathogen,
                          mo_code,
                          kingdom,
                          gramstrain,
                          genus,
                          ast_table) {
      self$name_pathogen <- name_pathogen
      self$mo_code <- mo_code
      self$kingdom <- kingdom
      self$gramstrain <- gramstrain
      self$genus <- genus
      self$mdr <- mdr
      self$ast_table <- ast_table
    }
  )
)

# Lớp mẫu bệnh phẩm
sample_info <- R6Class(
  "sample_info",
  public = list(
    sid = NULL,
    sample_type = NULL,
    sampling_date = NULL,
    pathogen_info = NULL,
    initialize = function(sid,
                          sampling_date,
                          sample_type,
                          pathogen_info) {
      self$sid <- sid
      seff$sampling_date <- sampling_date
      self$sample_type <- sample_type
      self$pathogen_info <- pathogen_info
    }
  )
)

# Lớp bệnh nhân
patient_info <- R6Class(
  "patient_info",
  public = list(
    pid = NULL,
    sample_infor = NULL,
    initialize = function(pid,sample_info) {
      self$pid <- pid
      self$sample_info <- sample_info
    }
  )
)
