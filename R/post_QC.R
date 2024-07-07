#' Post-QC Interface
#'
#' Start the R Shiny application with the post-QC interface.
#'
#' @param main_path Provide the path where you saved the QC results.
#'
#' @return A web link to start the interactive QC session.
#'
#' @export
post_qc = function(main_path){
  jsCode <- "
shinyjs.removeImage = function() {
  if (papayaContainers.length > 0) {
    var viewer = papayaContainers[0].viewer;
    viewer.screenVolumes = [];
    viewer.updateViewer(true);
  }
};
"
  files = list.files(main_path, recursive = TRUE, full.names = TRUE)
  qc_files_path = files[which(grepl("*QC_result.csv$", files))]
  qc_files = lapply(qc_files_path, function(x) read_csv(x))
  names(qc_files) = sapply(qc_files_path, function(x) str_split(basename(x),"_")[[1]][1], USE.NAMES = FALSE)

  ## prepare dict files for freesurfer and JLF

  free_roi_name = c("Left-Cerebral-White-Matter", "Left-Cerebral-Cortex", "Left-Lateral-Ventricle", "Left-Inf-Lat-Vent", "Left-Cerebellum-White-Matter",
               "Left-Cerebellum-Cortex", "Left-Thalamus", "Left-Caudate", "Left-Putamen", "Left-Pallidum", "3rd-Ventricle", "4th-Ventricle", "Brain-Stem", "Left-Hippocampus",
               "Left-Amygdala", "CSF", "Left-Accumbens-area", "Left-VentralDC", "Left-vessel", "Left-choroid-plexus", "Right-Cerebral-White-Matter",
               "Right-Cerebral-Cortex", "Right-Lateral-Ventricle", "Right-Inf-Lat-Vent", "Right-Cerebellum-White-Matter", "Right-Cerebellum-Cortex", "Right-Thalamus", "Right-Caudate",
               "Right-Putamen", "Right-Pallidum", "Right-Hippocampus", "Right-Amygdala", "Right-Accumbens-area", "Right-VentralDC", "Right-vessel", "Right-choroid-plexus",
               "5th-Ventricle")
  free_roi_general = c("Cerebral-White-Matter", "Cerebral-Cortex", "Lateral-Ventricle", "Inf-Lat-Vent", "Cerebellum-White-Matter",
                  "Cerebellum-Cortex", "Thalamus", "Caudate", "Putamen", "Pallidum", "3rd-Ventricle", "4th-Ventricle", "Brain-Stem", "Hippocampus",
                  "Amygdala", "CSF", "Accumbens-area", "VentralDC", "vessel", "choroid-plexus", "Cerebral-White-Matter",
                  "Cerebral-Cortex", "Lateral-Ventricle", "Inf-Lat-Vent", "Cerebellum-White-Matter", "Cerebellum-Cortex", "Thalamus", "Caudate",
                  "Putamen", "Pallidum", "Hippocampus", "Amygdala", "Accumbens-area", "VentralDC", "vessel", "choroid-plexus",
                  "5th-Ventricle")
  free_roi_index = c(2:5, 7:8, 10:18, 24, 26, 28, 30:31, 41:44, 46:47, 49:54, 58, 60, 62:63, 72)
  free_index_df = data.frame(cbind(free_roi_name, free_roi_index, free_roi_general))
  jlf_roi_general = c("Corpus Callosum", "Ventral DC", "Ventral DC", "Cerebellar Vermal Lobules", "Cerebellar Vermal Lobules", "Cerebellar Vermal Lobules",
                  "Cerebellum Exterior", "Cerebellum White Matter", "Cerebellum Exterior", "Cerebellum White Matter", "Accumbens Area", "Caudate",
                  "Pallidum", "Putamen", "Accumbens Area", "Caudate", "Pallidum", "Putamen", "Thalamus Proper", "Thalamus Proper",
                  "Anterior Limb", "Anterior Limb", "Fornix", "Fornix", "Posterior Limb",
                  "Posterior Limb", "Amygdala", "Basal Forebrain", "Hippocampus", "Amygdala", "Basal Forebrain",
                  "Hippocampus", "Anterior Orbital Gyrus", "Lateral Orbital Gyrus", "Medial Orbital Gyrus", "Posterior Orbital Gyrus", "Anterior Orbital Gyrus",
                  "Lateral Orbital Gyrus", "Medial Orbital Gyrus", "Posterior Orbital Gyrus", "Anterior Insula", "Posterior Insula",
                  "Anterior Insula", "Posterior Insula", "Frontal Pole", "Middle Frontal Gyrus", "Inferior Frontal Gyrus",
                  "Inferior Frontal Gyrus", "Precentral Gyrus", "Superior Frontal Gyrus", "Inferior Frontal Gyrus",
                  "Frontal Pole", "Middle Frontal Gyrus", "Inferior Frontal Gyrus", "Inferior Frontal Gyrus", "Precentral Gyrus",
                  "Superior Frontal Gyrus", "Inferior Frontal Gyrus", "Gyrus Rectus", "Medial Frontal Cortex", "Precentral Gyrus Medial Segment",
                  "Superior Frontal Gyrus", "Subcallosal Area", "Supplementary Motor Cortex", "Gyrus Rectus", "Medial Frontal Cortex", "Precentral Gyrus",
                  "Superior Frontal Gyrus", "Subcallosal Area", "Supplementary Motor Cortex", "Central Operculum", "Frontal Operculum", "Parietal Operculum",
                  "Central Operculum", "Frontal Operculum", "Parietal Operculum", "Frontal Lobe", "Frontal Lobe", "Anterior Cingulate Gyrus", "Middle Cingulate Gyrus",
                  "Posterior Cingulate Gyrus", "Anterior Cingulate Gyrus", "Middle Cingulate Gyrus", "Posterior Cingulate Gyrus", "Entorhinal Area", "Parahippocampal Gyrus", "Entorhinal Area",
                  "Parahippocampal Gyrus", "Occipital Fusiform Gyrus", "Occipital Fusiform Gyrus", "Inferior Occipital Gyrus", "Middle Occipital Gyrus", "Occipital Pole",
                  "Superior Occipital Gyrus", "Inferior Occipital Gyrus", "Middle Occipital Gyrus", "Occipital Pole",
                  "Superior Occipital Gyrus","Calcarine Cortex", "Cuneus", "Lingual Gyrus", "Calcarine Cortex", "Cuneus", "Lingual Gyrus", "Occipital Lobe",
                  "Occipital Lobe", "Angular Gyrus", "Postcentral Gyrus", "Supramarginal Gyrus", "Superior Parietal Lobule", "Angular Gyrus", "Postcentral Gyrus", "Supramarginal Gyrus",
                  "Superior Parietal Lobule", "Postcentral Gyrus", "Precuneus", "Postcentral Gyrus", "Precuneus", "Parietal Lobe", "Parietal Lobe", "Fusiform Gyrus",
                  "Fusiform Gyrus", "Inferior Temporal Gyrus", "Middle Temporal Gyrus", "Superior Temporal Gyrus", "Temporal Pole", "Inferior Temporal Gyrus", "Middle Temporal Gyrus", "Superior Temporal Gyrus",
                  "Temporal Pole", "Planum Polare", "Planum Temporale", "Transverse Temporal Gyrus", "Planum Polare", "Planum Temporale", "Transverse Temporal Gyrus", "Temporal Lobe",
                  "Temporal Lobe", "3rd Ventricle", "4th Ventricle", "Inf Lat Vent", "Lateral Ventricle", "Inf Lat Vent", "Lateral Ventricle", "Brain Stem")
  jlf_roi_name = c("corpus callosum", "Left Ventral DC", "Right Ventral DC", "Cerebellar Vermal Lobules I-V", "Cerebellar Vermal Lobules VIII-X", "Cerebellar Vermal Lobules VI-VII",
               "Left Cerebellum Exterior", "Left Cerebellum White Matter", "Right Cerebellum Exterior", "Right Cerebellum White Matter", "Left Accumbens Area", "Left Caudate",
               "Left Pallidum", "Left Putamen", "Right Accumbens Area", "Right Caudate", "Right Pallidum", "Right Putamen", "Left Thalamus Proper", "Right Thalamus Proper",
               "anterior limb of internal capsule left", "anterior limb of internal capsule right", "fornix left", "fornix right", "posterior limb of internal capsule inc. cerebral peduncle left",
               "posterior limb of internal capsule inc. cerebral peduncle right", "Left Amygdala", "Left Basal Forebrain", "Left Hippocampus", "Right Amygdala", "Right Basal Forebrain",
               "Right Hippocampus", "Left AOrG  anterior orbital gyrus", "Left LOrG  lateral orbital gyrus", "Left MOrG  medial orbital gyrus", "Left POrG  posterior orbital gyrus", "Right AOrG  anterior orbital gyrus",
               "Right LOrG  lateral orbital gyrus", "Right MOrG  medial orbital gyrus", "Right POrG  posterior orbital gyrus", "Left AIns  anterior insula", "Left PIns  posterior insula",
               "Right AIns  anterior insula", "Right PIns  posterior insula", "Left FRP   frontal pole", "Left MFG   middle frontal gyrus", "Left OpIFG opercular part of the inferior frontal gyrus",
               "Left OrIFG orbital part of the inferior frontal gyrus", "Left PrG   precentral gyrus", "Left SFG   superior frontal gyrus", "Left TrIFG triangular part of the inferior frontal gyrus",
               "Right FRP   frontal pole", "Right MFG   middle frontal gyrus", "Right OpIFG opercular part of the inferior frontal gyrus", "Right OrIFG orbital part of the inferior frontal gyrus", "Right PrG   precentral gyrus",
               "Right SFG   superior frontal gyrus", "Right TrIFG triangular part of the inferior frontal gyrus", "Left GRe   gyrus rectus", "Left MFC   medial frontal cortex", "Left MPrG  precentral gyrus medial segment",
               "Left MSFG  superior frontal gyrus medial segment", "Left SCA   subcallosal area", "Left SMC   supplementary motor cortex", "Right GRe   gyrus rectus", "Right MFC   medial frontal cortex", "Right MPrG  precentral gyrus medial segment",
               "Right MSFG  superior frontal gyrus medial segment", "Right SCA   subcallosal area", "Right SMC   supplementary motor cortex", "Left CO    central operculum", "Left FO    frontal operculum", "Left PO    parietal operculum",
               "Right CO    central operculum", "Right FO    frontal operculum", "Right PO    parietal operculum", "frontal lobe WM left", "frontal lobe WM right", "Left ACgG  anterior cingulate gyrus", "Left MCgG  middle cingulate gyrus",
               "Left PCgG  posterior cingulate gyrus", "Right ACgG  anterior cingulate gyrus", "Right MCgG  middle cingulate gyrus", "Right PCgG  posterior cingulate gyrus", "Left Ent   entorhinal area", "Left PHG   parahippocampal gyrus", "Right Ent   entorhinal area",
               "Right PHG   parahippocampal gyrus", "Left OFuG  occipital fusiform gyrus", "Right OFuG  occipital fusiform gyrus", "Left IOG   inferior occipital gyrus", "Left MOG   middle occipital gyrus", "Left OCP   occipital pole",
               "Left SOG   superior occipital gyrus", "Right IOG   inferior occipital gyrus", "Right MOG   middle occipital gyrus", "Right OCP   occipital pole",
               "Right SOG   superior occipital gyrus","Left Calc  calcarine cortex", "Left Cun   cuneus", "Left LiG   lingual gyrus", "Right Calc  calcarine cortex", "Right Cun   cuneus", "Right LiG   lingual gyrus", "occipital lobe WM left",
               "occipital lobe WM right", "Left AnG   angular gyrus", "Left PoG   postcentral gyrus", "Left SMG   supramarginal gyrus", "Left SPL   superior parietal lobule", "Right AnG   angular gyrus", "Right PoG   postcentral gyrus", "Right SMG   supramarginal gyrus",
               "Right SPL   superior parietal lobule", "Left MPoG  postcentral gyrus medial segment", "Left PCu   precuneus", "Right MPoG  postcentral gyrus medial segment", "Right PCu   precuneus", "parietal lobe WM left", "parietal lobe WM right", "Left FuG   fusiform gyrus",
               "Right FuG   fusiform gyrus", "Left ITG   inferior temporal gyrus", "Left MTG   middle temporal gyrus", "Left STG   superior temporal gyrus", "Left TMP   temporal pole", "Right ITG   inferior temporal gyrus", "Right MTG   middle temporal gyrus", "Right STG   superior temporal gyrus",
               "Right TMP   temporal pole", "Left PP    planum polare", "Left PT    planum temporale", "Left TTG   transverse temporal gyrus", "Right PP    planum polare", "Right PT    planum temporale", "Right TTG   transverse temporal gyrus", "temporal lobe WM left",
               "temporal lobe WM right", "3rd Ventricle", "4th Ventricle", "Left Inf Lat Vent", "Left Lateral Ventricle", "Right Inf Lat Vent", "Right Lateral Ventricle", "Brain Stem")

  jlf_roi_index = c(95, 62, 61, 71, 73, 72, 39, 41, 38, 40, 30, 37, 56, 58, 23, 36, 55, 57, 60, 59, 92, 91, 90, 89, 94, 93, 32, 75, 48, 31, 76, 47, 105, 137, 147, 179, 104, 136, 146, 178, 103, 173, 102, 172, 121,
                143, 163, 165, 183, 191, 205, 120, 142, 162, 164, 182, 190, 204,  125, 141, 151, 153, 187, 193, 124, 140, 150, 152, 186, 192, 113, 119, 175, 112, 118, 174, 82, 81, 101, 139, 167, 100, 138, 166,
                117, 171, 116, 170, 161, 160, 129, 145, 157, 197, 128, 144, 156, 196, 109, 115, 135, 108, 114, 134, 84, 83, 107, 177, 195, 199, 106, 176, 194, 198, 149, 169, 148, 168, 86, 85, 123, 122, 133, 155,
                201, 203, 132, 154, 200, 202, 181, 185, 207, 180, 184, 206, 88, 87, 4, 11, 50, 52, 49, 51, 35)
  jlf_tissue_seg = c(rep("WM", 3), rep("GM", 4), "WM", "GM", "WM", rep("GM", 10), rep("WM", 6), rep("GM", 50), rep("WM", 2), rep("GM", 26), rep("WM", 2), rep("GM", 12), rep("WM", 2), rep("GM", 16), rep("WM", 2), rep("None", 7))
  jlf_index_df = data.frame(cbind(jlf_roi_name, jlf_roi_index, jlf_roi_general, jlf_tissue_seg))

  ui = function(request) {
    fluidPage(
      useShinyjs(),  # Include shinyjs
      extendShinyjs(text = jsCode, functions = c("removeImage")),
      theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
      titlePanel("Post-QC Review"),
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(condition="input.tabselected==1",
                           fluidRow(
                             shinydashboard::box(
                               width = NULL,
                               title = "BrainQC Type",
                               selectInput("type", "Choose the QC type:", choices = names(qc_files), selected = names(qc_files)[1]),
                               uiOutput("defaultseg")
                             )
                           )
          ),
          conditionalPanel(condition="input.tabselected==2",
                           fluidRow(
                             shinydashboard::box(
                               width = NULL,
                               title = "Subject Id",
                               uiOutput("subject_selection")
                             )
                           ),
                           fluidRow(
                             shinydashboard::box(
                               width = NULL,
                               title = "Session Id",
                               uiOutput("session_selection")
                             )
                           ),
                           fluidRow(
                             shinydashboard::box(
                               width = 12,
                               title = "Detailed Information",
                               DT::DTOutput("info"))),
                           fluidRow(
                             shinydashboard::box(
                               width = 12,
                               title = "Segmentation Investigation",
                               uiOutput("lesion_invest")))
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Evaluation Form", value = 1,
                     DT::DTOutput("evaluation")),
            tabPanel("Interactive QC", value = 2,
                     fluidRow(
                       shinydashboard::box(
                         width = 12,
                         title = "MRI",
                         papayaOutput("image", height = "600px")))),
            id = "tabselected"
          )
        )
      )
    )
  }

  server = function(input, output, session) {

    output$defaultseg = renderUI({
      if(input$type == "freesurfer"){
        rois = sort(unique(free_index_df$free_roi_general))
        selectInput("seg", "Select a default roi:", choices = rois, selected = rois[1])
      }else if(input$type == "JLF"){
        rois = sort(unique(jlf_index_df$jlf_roi_general))
        selectInput("seg", "Select a default roi:", choices = rois, selected = rois[1])
      }
    })

    output$subject_selection = renderUI({
      selectInput("subject", "Choose a subject id", choices = unique(qc_files[[input$type]]$subject), selected = unique(qc_files[[input$type]]$subject)[1])
    })

    output$session_selection = renderUI({
      selectInput("session", "Choose a session id", choices = unique(qc_files[[input$type]][which(qc_files[[input$type]]$subject == input$subject), ]$session), selected = unique(qc_files[[input$type]][which(qc_files[[input$type]]$subject == input$subject), ]$session)[1])
    })

    output$evaluation = DT::renderDT({
      evaluation_df = qc_files[[input$type]]
      evaluation_df %>% DT::datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% formatStyle(
        'evaluation',
        target = 'row',
        backgroundColor = styleEqual(c("fail"), "lightyellow")
      )
    })

    output$image = renderPapaya({
      shinyjs::js$removeImage()
      evaluation_df = qc_files[[input$type]] %>% filter(subject == input$subject, session == input$session)
      if(input$type %in% c("cvs", "lesion", "PRL")){
        lesion_num = evaluation_df %>% filter(subject == input$subject, session == input$session) %>% pull(lesion_num)
        if(lesion_num > 0){
          if(input$lesion_id == "all"){
            papaya(img=list(evaluation_df$img_files, evaluation_df$seg_files), sync_view = TRUE,
                   hide_toolbar = FALSE, hide_controls = TRUE,
                   orthogonal = TRUE, options = list(papayaOptions(alpha = 0.5),
                                                     papayaOptions(alpha = 0.5, lut = "Red Overlay")))
          }else{
            seg_img = readnii(evaluation_df$seg_files)
            if(max(seg_img) > 1){labeled_lesion = seg_img}else{labeled_lesion = get_labeled_mask(seg_img)}
            papaya(img=list(evaluation_df$img_files, labeled_lesion==as.numeric(gsub("lesion ", "", input$lesion_id))), sync_view = TRUE,
                       hide_toolbar = FALSE, hide_controls = TRUE,
                       orthogonal = TRUE, options = list(papayaOptions(alpha = 0.5),
                                                         papayaOptions(alpha = 0.5, lut = "Red Overlay")))}
        }else{
          papaya(evaluation_df$img_files)
        }
      }else if(input$type == "freesurfer"){
        seg_img = readnii(evaluation_df$seg_files)
        roi_id = as.numeric(free_index_df[which(free_index_df$free_roi_general == input$lesion_id), "free_roi_index"])
        if(length(roi_id) == 1){
          papaya(img=list(evaluation_df$img_files, seg_img == roi_id), sync_view = TRUE,
                 hide_toolbar = FALSE, hide_controls = TRUE,
                 orthogonal = TRUE, options = list(papayaOptions(alpha = 0.5),
                                                   papayaOptions(alpha = 0.5, lut = "Overlay (Positives)")))
        }else if(length(roi_id) == 2){
          papaya(img=list(evaluation_df$img_files, (seg_img == roi_id[1] | seg_img == roi_id[2])), sync_view = TRUE,
                 hide_toolbar = FALSE, hide_controls = TRUE,
                 orthogonal = TRUE, options = list(papayaOptions(alpha = 0.5),
                                                   papayaOptions(alpha = 0.5, lut = "Overlay (Positives)")))
        }
      }else if(input$type == "JLF"){
        seg_img = readnii(evaluation_df$seg_files)
        roi_id = as.numeric(jlf_index_df[which(jlf_index_df$jlf_roi_general == input$lesion_id), "jlf_roi_index"])
        if(length(grep("Tissue*", input$lesion_id)) > 0){
          roi_id = as.numeric(jlf_index_df[which(jlf_index_df$jlf_tissue_seg == gsub("Tissue_", "", input$lesion_id)), "jlf_roi_index"])
        }else{roi_id = as.numeric(jlf_index_df[which(jlf_index_df$jlf_roi_general == input$lesion_id), "jlf_roi_index"])}
        if(length(roi_id) == 1){
          papaya(img=list(evaluation_df$img_files, seg_img == roi_id), sync_view = TRUE,
                 hide_toolbar = FALSE, hide_controls = TRUE,
                 orthogonal = TRUE, options = list(papayaOptions(alpha = 0.5),
                                                   papayaOptions(alpha = 0.5, lut = "Overlay (Positives)")))
        }else if(length(roi_id) >= 2){
          seg = seg_img
          seg_data = seg@.Data
          seg_new = seg_data %in% roi_id
          dim(seg_new) = dim(seg_data)
          seg@.Data = seg_new
          papaya(img=list(evaluation_df$img_files, seg), sync_view = TRUE,
                 hide_toolbar = FALSE, hide_controls = TRUE,
                 orthogonal = TRUE, options = list(papayaOptions(alpha = 0.5),
                                                   papayaOptions(alpha = 0.5, lut = "Overlay (Positives)")))
        }
      }
    })


    output$info = DT::renderDT({
      evaluation_df = qc_files[[input$type]] %>% filter(subject == input$subject, session == input$session)
      if(input$type == "lesion"){
        selected_column = c("subject", "session", "lesion_num", "evaluation", "note")
        evaluation_df %>% dplyr::select(selected_column) %>%
          DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                              targets = "_all"))))
      }else if(input$type == "cvs"){
        selected_column = c("subject", "session", "lesion_num", "evaluation", "note")
        if(input$lesion_id == "all"){
        cvs_score = read_csv(paste0(str_split(evaluation_df$img_files[1], "/data/")[[1]][1], "/data/", input$subject, "/", input$session, "/cvs/cvs_biomarker.csv"))
        evaluation_df %>% dplyr::select(selected_column) %>% mutate(cvs.score = round(cvs_score$cvs.biomarker, 3)) %>%
          DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                              targets = "_all")))) %>% formatStyle(
                                                                'evaluation',
                                                                target = 'row',
                                                                backgroundColor = styleEqual(c("fail"), "lightyellow")
                                                              )
        }else{
          cvs_score = read_csv(paste0(str_split(evaluation_df$img_files[1], "/data/")[[1]][1], "/data/", input$subject, "/", input$session, "/cvs/cvs_biomarker_lesion.csv")) %>% filter(lesion_id == as.numeric(str_split(input$lesion_id, " ")[[1]][2]))
          evaluation_df %>% dplyr::select(selected_column) %>% mutate(lesion_id = cvs_score$lesion_id, cvs.score = round(cvs_score$cvs.score, 3)) %>%
            DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                                targets = "_all")))) %>% formatStyle(
                                                                  'evaluation',
                                                                  target = 'row',
                                                                  backgroundColor = styleEqual(c("fail"), "lightyellow")
                                                                )
        }
      }else if(input$type == "PRL"){
        selected_column = c("subject", "session", "lesion_num", "evaluation", "note")
        if(input$lesion_id == "all"){
          evaluation_df %>% dplyr::select(selected_column) %>%
            DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                                targets = "_all")))) %>% formatStyle(
                                                                  'evaluation',
                                                                  target = 'row',
                                                                  backgroundColor = styleEqual(c("fail"), "lightyellow")
                                                                )
        }else{
          prl_pro = read_csv(list.files(paste0(str_split(evaluation_df$img_files[1], "/data/")[[1]][1], "/data/", input$subject, "/", input$session, "/prl"), pattern = "*_pred.csv", recursive = TRUE, full.names = TRUE))
          prl_pro = prl_pro %>% mutate(lesion_id = 1:nrow(prl_pro)) %>% filter(lesion_id ==  as.numeric(str_split(input$lesion_id, " ")[[1]][2]))
          evaluation_df %>% dplyr::select(selected_column) %>% mutate(lesion_id = prl_pro$lesion_id, prl.probability = round(prl_pro$rimpos, 3)) %>%
            DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                                targets = "_all")))) %>% formatStyle(
                                                                  'evaluation',
                                                                  target = 'row',
                                                                  backgroundColor = styleEqual(c("fail"), "lightyellow")
                                                                )

        }
      }else if(input$type == "freesurfer"){
        selected_column = c("subject", "session", "ICV", "evaluation", "note")
        stat = read_aseg_stats(list.files(paste0(str_split(evaluation_df$img_files[1], "/data/")[[1]][1], "/data/", input$subject, "/", input$session, "/freesurfer"), pattern = "aseg.stat", recursive = TRUE, full.names = TRUE))$structures
        roi_volume = stat[which(stat$SegId %in% as.numeric(free_index_df[which(free_index_df$free_roi_general == input$lesion_id), "free_roi_index"])), "Volume_mm3"] |> sum() |> round(2)
        evaluation_df %>% dplyr::select(selected_column) %>% mutate(roi = input$lesion_id, volume = roi_volume) %>%
          DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                              targets = "_all")))) %>% formatStyle(
                                                                'evaluation',
                                                                target = 'row',
                                                                backgroundColor = styleEqual(c("fail"), "lightyellow")
                                                              )


      }else if(input$type == "JLF"){
        selected_column = c("subject", "session", "evaluation", "note")
        seg_img = readnii(evaluation_df$seg_files)
        vol_table = table(seg_img)[-1] * voxres(seg_img, units = "mm")
        vol_df = data.frame(vol_table)
        colnames(vol_df) = c("index", "volume_mm3")
        vol_df$volume_mm3 = as.numeric(vol_df$volume_mm3)
        vol_df = vol_df %>% left_join(jlf_index_df %>% dplyr::select(jlf_roi_index, jlf_roi_general, jlf_tissue_seg, jlf_roi_name), by = c("index" = "jlf_roi_index")) %>% na.omit()
        tissue_df = vol_df %>% group_by(jlf_tissue_seg) %>% summarize(volume_mm3 = sum(volume_mm3)) %>% filter(jlf_tissue_seg != "None") %>% mutate(index = rep(NA, 2), jlf_roi_general = paste0("Tissue_", jlf_tissue_seg), jlf_roi_name = jlf_tissue_seg)
        tissue_df = tissue_df[colnames(vol_df)]
        vol_df = rbind(vol_df, tissue_df)
        if(length(grep("Tissue_*", input$lesion_id)) > 0){
          roi_volume = vol_df[which(vol_df$jlf_roi_general == input$lesion_id), "volume_mm3"] |> round(2)
          tissue_seg = vol_df[which(vol_df$jlf_roi_general == input$lesion_id), "jlf_tissue_seg"]
        }else{roi_volume = vol_df[which(vol_df$index %in% as.numeric(jlf_index_df[which(jlf_index_df$jlf_roi_general == input$lesion_id), "jlf_roi_index"])), "volume_mm3"] |> sum() |> round(2)
        tissue_seg = vol_df[which(vol_df$index %in% as.numeric(jlf_index_df[which(jlf_index_df$jlf_roi_general == input$lesion_id), "jlf_roi_index"])), "jlf_tissue_seg"] |> unique()}
        evaluation_df %>% dplyr::select(selected_column) %>% mutate(roi = input$lesion_id, volume = roi_volume, tissue_seg = tissue_seg) %>%
          DT::datatable(options = list(columnDefs = list(list(className = 'dt-center',
                                                              targets = "_all")))) %>% formatStyle(
                                                                'evaluation',
                                                                target = 'row',
                                                                backgroundColor = styleEqual(c("fail"), "lightyellow")
                                                              )
      }
    })

    output$lesion_invest = renderUI({
      evaluation_df = qc_files[[input$type]]
      if(input$type %in% c("cvs", "lesion", "PRL")){
        lesion_num = evaluation_df %>% filter(subject == input$subject, session == input$session) %>% pull(lesion_num)
        if(lesion_num > 0){
          lesion_id = paste0("lesion ", 1:lesion_num)
          selectInput("lesion_id", "Select a lesion to further investigate:", choices = c("all", lesion_id), selected = "all")
        }
      }else if(input$type == "freesurfer"){
        rois = sort(unique(free_index_df$free_roi_general))
        selectInput("lesion_id", "Select a roi to further investigate:", choices = rois, selected = input$seg)
      }else if(input$type == "JLF"){
        rois = sort(unique(jlf_index_df$jlf_roi_general))
        selectInput("lesion_id", "Select a roi to further investigate:", choices = rois, selected = input$seg)
      }
    })

  }

  shinyApp(ui = ui, server = server, enableBookmarking = "url")

}

utils::globalVariables(c("evaluation", "subject", "volume_mm3"))
