require(shiny)
require(bslib)
require(shinydashboard)
require(scales)
require(DT)
require(tidyverse)
require(dplyr)
require(papayaWidget)
require(shinyjs)

#' Brain QC Shiny
#'
#' Start the Interactive QC Session through Rshiny.
#'
#' @param qc_type Specify the type of QC procedure: lesion, freesurfer, JLF, PRL, cvs.
#' @param main_path Provide the path where you saved the results from the first stage.
#'
#' @return `qc_type` returns a web link to start the interactive QC session.
#'
#' @import shiny
#' @import bslib
#' @import shinydashboard
#' @import scales
#' @import tidyverse
#' @import dplyr
#' @import papayaWidget
#'
#' @importFrom utils head
#' @importFrom readr read_csv write_csv
#' @importFrom stats na.omit
#' @importFrom DT datatable formatStyle styleEqual DTOutput renderDT
#' @importFrom shinyjs extendShinyjs useShinyjs js
#'
#' @export

qc_shiny = function(qc_type = "lesion", main_path){
  if(qc_type == "lesion"){
    lesion_qc(main_path)
  }else if(qc_type == "freesurfer"){
    freesurfer_qc(main_path)
  }else if(qc_type == "JLF"){
    jlf_qc(main_path)
  }else if(qc_type == "PRL"){
    prl_qc(main_path)
  }else if(qc_type == "cvs"){
    cvs_qc(main_path)
  }
}

