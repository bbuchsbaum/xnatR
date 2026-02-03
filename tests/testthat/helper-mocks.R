# Helper functions for creating mock XNAT responses
#
# These helpers return data in the format that httr2::resp_body_json()
# produces with simplifyVector = TRUE (the default). JSON arrays of objects
# with consistent keys become data.frames.

# Standard XNAT ResultSet wrapper
mock_resultset <- function(results) {
  list(ResultSet = list(Result = results))
}

# Mock project data - returns data.frame to match resp_body_json() output
mock_projects <- function(n = 3) {
  data.frame(
    ID = paste0("PROJECT_", seq_len(n)),
    name = paste0("Test Project ", seq_len(n)),
    secondary_ID = paste0("SEC_", seq_len(n)),
    description = paste0("Description for project ", seq_len(n)),
    stringsAsFactors = FALSE
  )
}

# Mock subject data - returns data.frame
mock_subjects <- function(n = 3, project_id = "PROJECT_1") {
  data.frame(
    ID = paste0("SUBJECT_", seq_len(n)),
    label = paste0("Subject_", seq_len(n)),
    project = rep(project_id, n),
    gender = rep("M", n),
    age = rep(30L, n),
    stringsAsFactors = FALSE
  )
}

# Mock experiment data - returns data.frame
mock_experiments <- function(n = 2, project_id = "PROJECT_1", subject_id = "SUBJECT_1") {
  data.frame(
    ID = paste0("EXP_", seq_len(n)),
    label = paste0("Experiment_", seq_len(n)),
    project = rep(project_id, n),
    subject_ID = rep(subject_id, n),
    xsiType = rep("xnat:mrSessionData", n),
    date = format(Sys.Date() - seq_len(n), "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )
}

# Mock scan data - returns data.frame
mock_scans <- function(n = 3) {
  data.frame(
    ID = as.character(seq_len(n)),
    type = rep("T1", n),
    series_description = paste0("Series ", seq_len(n)),
    quality = rep("usable", n),
    xsiType = rep("xnat:mrScanData", n),
    frames = rep(200L, n),
    stringsAsFactors = FALSE
  )
}

# Mock resource data - returns data.frame
mock_resources <- function(n = 2) {
  labels <- c("DICOM", "NIFTI", "SNAPSHOTS")[seq_len(n)]
  formats <- c("DICOM", "NIFTI", "PNG")[seq_len(n)]
  data.frame(
    xnat_abstractresource_id = paste0("RES_", seq_len(n)),
    label = labels,
    file_count = rep(50L, n),
    file_size = rep(5000000L, n),
    format = formats,
    stringsAsFactors = FALSE
  )
}

# Mock file data - returns data.frame
mock_files <- function(n = 5) {
  data.frame(
    Name = paste0("file_", seq_len(n), ".dcm"),
    Size = rep(500000L, n),
    URI = paste0("/data/files/file_", seq_len(n), ".dcm"),
    collection = rep("DICOM", n),
    file_format = rep("DICOM", n),
    stringsAsFactors = FALSE
  )
}

# Mock assessor data - returns data.frame
mock_assessors <- function(n = 2) {
  data.frame(
    ID = paste0("ASSESSOR_", seq_len(n)),
    label = paste0("FreeSurfer_", seq_len(n)),
    xsiType = rep("fs:fsData", n),
    project = rep("PROJECT_1", n),
    stringsAsFactors = FALSE
  )
}

# Mock reconstruction data - returns data.frame
mock_reconstructions <- function(n = 1) {
  data.frame(
    ID = paste0("RECON_", seq_len(n)),
    type = rep("T1_RECON", n),
    xsiType = rep("xnat:reconstructedImageData", n),
    stringsAsFactors = FALSE
  )
}

# Mock data types - returns data.frame
mock_datatypes <- function() {
  data.frame(
    ELEMENT_NAME = c("xnat:projectData", "xnat:subjectData",
                     "xnat:mrSessionData", "xnat:mrScanData"),
    stringsAsFactors = FALSE
  )
}

# Mock queryable fields - returns data.frame
mock_fields <- function() {
  data.frame(
    FIELD_ID = c("xnat:mrSessionData/ID", "xnat:mrSessionData/label",
                 "xnat:mrSessionData/date", "xnat:mrSessionData/project"),
    TYPE = c("string", "string", "date", "string"),
    HEADER = c("ID", "Label", "Date", "Project"),
    DESC = c("Session ID", "Session label", "Session date", "Project ID"),
    stringsAsFactors = FALSE
  )
}

