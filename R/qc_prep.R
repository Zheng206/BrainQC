require(stringr)
require(tidyverse)
require(parallel)
require(extrantsr)
require(neurobase)
require(oro.nifti)
require(dplyr)
require(fslr)
require(ANTsR)
require(freesurfer)

#' Brain QC Preparation
#'
#' Prepare brain images and segmentation images for evaluation.
#'
#' @param main_path Provide the path to neuroimaging data in BIDS format.
#' @param img_name The brain image name.
#' @param seg_name The segmentation image name.
#' @param subject Subject ID.
#' @param cores Number of cores used for parallel computing.
#' @param qc_type Specify the type of QC procedure: lesion, freesurfer, JLF, PRL, cvs.
#' @param default_seg Select a default ROI to be evaluated first (when choosing freesurfer or JLF as the `qc_type`).
#'
#' @return `stage1_qc` returns a list containing the images to be evaluated as well as the evaluation form.
#'
#' @import parallel
#' @import tidyverse
#' @import dplyr
#' @import stringr
#' @import fslr
#' @import ANTsR
#' @import freesurfer
#'
#' @importFrom neurobase readnii
#' @importFrom extrantsr ants2oro oro2ants
#' @importFrom utils head
#' @importFrom readr read_csv write_csv
#' @importFrom stats na.omit
#' @importFrom ANTsRCore labelClusters
#' @importFrom oro.nifti voxres
#'
#' @export

stage1_qc = function(main_path, img_name, seg_name, subject, cores = 1, qc_type = "lesion", default_seg = NULL){
    img_files = list.files(main_path, pattern = img_name, recursive = TRUE, full.names = TRUE)
    img_files = img_files[which(grepl(subject, img_files))]
    seg_files = list.files(main_path, pattern = seg_name, recursive = TRUE, full.names = TRUE)
    seg_files = seg_files[which(grepl(subject, seg_files))]
    subject = sapply(img_files, function(x) {
      candidates = str_split(x, "/")[[1]][which(grepl("^data", str_split(x, "/")[[1]]))+1]
      candidates = candidates[which(!grepl(".nii.gz", candidates))]
      return(candidates)
      }, USE.NAMES = FALSE)

    session = sapply(img_files, function(x) str_split(x, "/")[[1]][which(grepl("^data", str_split(x, "/")[[1]]))+2], USE.NAMES = FALSE)
    summary_df = data.frame(cbind(subject, session, img_files, seg_files))
    summary_df$evaluation = NA
    summary_df$note = NA
    if(qc_type == "lesion"){
      brain_imgs = mclapply(img_files, function(x) readnii(x), mc.cores = cores)
      seg_imgs = mclapply(seg_files, function(x) readnii(x), mc.cores = cores)
      labeled_lesion = mclapply(1:nrow(summary_df), function(x) {
        if(max(seg_imgs[[x]]) == 1){get_labeled_mask(seg_imgs[[x]])}else{seg_imgs[[x]]}}, mc.cores = cores)

      seg_imgs = mclapply(1:length(seg_imgs), function(x) {
        if(max(seg_imgs[[x]]) > 1){seg_imgs[[x]] = seg_imgs[[x]]>0}else{seg_imgs[[x]]}
      }, mc.cores = cores)

      summary_df$lesion_num = sapply(1:nrow(summary_df), function(x) {
        lesion_num = max(labeled_lesion[[x]])
        return(lesion_num)
        }, USE.NAMES = FALSE)
      summary_df$evaluator = NA
      qc_list = list("summary_df" = summary_df, "labeled_lesion " = labeled_lesion , "brain_imgs" = brain_imgs, "seg_imgs" = seg_imgs)
    }else if(qc_type == "freesurfer"){
      stats_files = list.files(main_path, pattern = "aseg.stat", recursive = TRUE, full.names = TRUE)
      stats_files = stats_files[which(grepl(subject, stats_files))]
      aseg_dfs = mclapply(stats_files, function(x) read_aseg_stats(x)$structures)
      brain_imgs = mclapply(img_files, function(x) read_mgz(x), mc.cores = cores)
      seg_imgs = mclapply(seg_files, function(x) read_mgz(x), mc.cores = cores)
      summary_df$ICV = sapply(stats_files, function(x) round(as.numeric(read_aseg_stats(x)$measures[which(read_aseg_stats(x)$measures$measure == "estimatedtotalintracranialvol"), "value"]), 2), USE.NAMES = FALSE)
      summary_df$evaluator = NA
      roi_name = c("Left-Cerebral-White-Matter", "Left-Cerebral-Cortex", "Left-Lateral-Ventricle", "Left-Inf-Lat-Vent", "Left-Cerebellum-White-Matter",
                   "Left-Cerebellum-Cortex", "Left-Thalamus", "Left-Caudate", "Left-Putamen", "Left-Pallidum", "3rd-Ventricle", "4th-Ventricle", "Brain-Stem", "Left-Hippocampus",
                   "Left-Amygdala", "CSF", "Left-Accumbens-area", "Left-VentralDC", "Left-vessel", "Left-choroid-plexus", "Right-Cerebral-White-Matter",
                   "Right-Cerebral-Cortex", "Right-Lateral-Ventricle", "Right-Inf-Lat-Vent", "Right-Cerebellum-White-Matter", "Right-Cerebellum-Cortex", "Right-Thalamus", "Right-Caudate",
                   "Right-Putamen", "Right-Pallidum", "Right-Hippocampus", "Right-Amygdala", "Right-Accumbens-area", "Right-VentralDC", "Right-vessel", "Right-choroid-plexus",
                   "5th-Ventricle")
      roi_general = c("Cerebral-White-Matter", "Cerebral-Cortex", "Lateral-Ventricle", "Inf-Lat-Vent", "Cerebellum-White-Matter",
                      "Cerebellum-Cortex", "Thalamus", "Caudate", "Putamen", "Pallidum", "3rd-Ventricle", "4th-Ventricle", "Brain-Stem", "Hippocampus",
                      "Amygdala", "CSF", "Accumbens-area", "VentralDC", "vessel", "choroid-plexus", "Cerebral-White-Matter",
                      "Cerebral-Cortex", "Lateral-Ventricle", "Inf-Lat-Vent", "Cerebellum-White-Matter", "Cerebellum-Cortex", "Thalamus", "Caudate",
                      "Putamen", "Pallidum", "Hippocampus", "Amygdala", "Accumbens-area", "VentralDC", "vessel", "choroid-plexus",
                      "5th-Ventricle")
      if(is.null(default_seg)){default_roi = rep("Cerebral-White-Matter", length(roi_general))}else{
        default_roi = rep(default_seg, length(roi_general))
      }
      roi_index = c(2:5, 7:8, 10:18, 24, 26, 28, 30:31, 41:44, 46:47, 49:54, 58, 60, 62:63, 72)
      index_df = data.frame(cbind(roi_name, roi_index, roi_general, default_roi))
      qc_list = list("summary_df" = summary_df, "brain_imgs" = brain_imgs, "seg_imgs" = seg_imgs, "dict" = index_df, "stat" = aseg_dfs)
    }else if(qc_type == "JLF"){
      brain_imgs = mclapply(img_files, function(x) readnii(x), mc.cores = cores)
      seg_imgs = mclapply(seg_files, function(x) readnii(x), mc.cores = cores)
      roi_general = c("Corpus Callosum", "Ventral DC", "Ventral DC", "Cerebellar Vermal Lobules", "Cerebellar Vermal Lobules", "Cerebellar Vermal Lobules",
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
      roi_name = c("corpus callosum", "Left Ventral DC", "Right Ventral DC", "Cerebellar Vermal Lobules I-V", "Cerebellar Vermal Lobules VIII-X", "Cerebellar Vermal Lobules VI-VII",
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
      if(is.null(default_seg)){default_roi = rep("Cerebellum White Matter", length(roi_general))}else{
        default_roi = rep(default_seg, length(roi_general))
      }
      roi_index = c(95, 62, 61, 71, 73, 72, 39, 41, 38, 40, 30, 37, 56, 58, 23, 36, 55, 57, 60, 59, 92, 91, 90, 89, 94, 93, 32, 75, 48, 31, 76, 47, 105, 137, 147, 179, 104, 136, 146, 178, 103, 173, 102, 172, 121,
                    143, 163, 165, 183, 191, 205, 120, 142, 162, 164, 182, 190, 204,  125, 141, 151, 153, 187, 193, 124, 140, 150, 152, 186, 192, 113, 119, 175, 112, 118, 174, 82, 81, 101, 139, 167, 100, 138, 166,
                    117, 171, 116, 170, 161, 160, 129, 145, 157, 197, 128, 144, 156, 196, 109, 115, 135, 108, 114, 134, 84, 83, 107, 177, 195, 199, 106, 176, 194, 198, 149, 169, 148, 168, 86, 85, 123, 122, 133, 155,
                    201, 203, 132, 154, 200, 202, 181, 185, 207, 180, 184, 206, 88, 87, 4, 11, 50, 52, 49, 51, 35)
      tissue_seg = c(rep("WM", 3), rep("GM", 4), "WM", "GM", "WM", rep("GM", 10), rep("WM", 6), rep("GM", 50), rep("WM", 2), rep("GM", 26), rep("WM", 2), rep("GM", 12), rep("WM", 2), rep("GM", 16), rep("WM", 2), rep("None", 7))
      index_df = data.frame(cbind(roi_name, roi_index, roi_general, tissue_seg, default_roi))
      aseg_dfs = lapply(seg_imgs, function(x){
        vol_table = table(x)[-1] * voxres(x, units = "mm")
        vol_df = data.frame(vol_table)
        colnames(vol_df) = c("index", "volume_mm3")
        vol_df$volume_mm3 = as.numeric(vol_df$volume_mm3)
        vol_df = vol_df %>% left_join(index_df %>% dplyr::select(roi_index, roi_general, tissue_seg, roi_name), by = c("index" = "roi_index")) %>% na.omit()
        tissue_df = vol_df %>% group_by(tissue_seg) %>% summarize(volume_mm3 = sum(volume_mm3)) %>% filter(tissue_seg != "None") %>% mutate(index = rep(NA, 2), roi_general = paste0("Tissue_", tissue_seg), roi_name = tissue_seg)
        tissue_df = tissue_df[colnames(vol_df)]
        vol_df = rbind(vol_df, tissue_df)
        return(vol_df)
      })
      qc_list = list("summary_df" = summary_df, "brain_imgs" = brain_imgs, "seg_imgs" = seg_imgs, "dict" = index_df, "stat" = aseg_dfs)
    }else if(qc_type == "PRL"){
      brain_imgs = mclapply(img_files, function(x) readnii(x), mc.cores = cores)
      seg_imgs = mclapply(seg_files, function(x) readnii(x), mc.cores = cores)
      labeled_lesion = mclapply(1:nrow(summary_df), function(x) {
        if(max(seg_imgs[[x]]) == 1){get_labeled_mask(seg_imgs[[x]])}else{seg_imgs[[x]]}}, mc.cores = cores)
      seg_imgs = mclapply(1:length(seg_imgs), function(x) {
        if(max(seg_imgs[[x]]) > 1){seg_imgs[[x]] = seg_imgs[[x]]>0}else{seg_imgs[[x]]}
      }, mc.cores = cores)
      summary_df$lesion_num = sapply(1:nrow(summary_df), function(x) {
        lesion_num = max(labeled_lesion[[x]])
        return(lesion_num)
      }, USE.NAMES = FALSE)
      summary_df$evaluator = NA
      prl_files = list.files(main_path, pattern = "*_preds.csv", recursive = TRUE, full.names = TRUE)
      prl_files = prl_files[which(grepl(subject, prl_files))]
      prl_score = lapply(prl_files, function(x) {
        df = read_csv(x)
        colnames(df) = c("lesion_id", "rimneg", "rimpos")
        return(df)
        })
      qc_list = list("summary_df" = summary_df, "labeled_lesion " = labeled_lesion , "brain_imgs" = brain_imgs, "seg_imgs" = seg_imgs, "prl_score" = prl_score)
    }else if(qc_type == "cvs"){
      brain_imgs = mclapply(img_files, function(x) readnii(x), mc.cores = cores)
      seg_imgs = mclapply(seg_files, function(x) readnii(x), mc.cores = cores)
      labeled_lesion = mclapply(1:nrow(summary_df), function(x) {
        if(max(seg_imgs[[x]]) == 1){get_labeled_mask(seg_imgs[[x]])}else{seg_imgs[[x]]}}, mc.cores = cores)
      seg_imgs = mclapply(1:length(seg_imgs), function(x) {
        if(max(seg_imgs[[x]]) > 1){seg_imgs[[x]] = seg_imgs[[x]]>0}else{seg_imgs[[x]]}
      }, mc.cores = cores)
      summary_df$lesion_num = sapply(1:nrow(summary_df), function(x) {
        lesion_num = max(labeled_lesion[[x]])
        return(lesion_num)
      }, USE.NAMES = FALSE)
      summary_df$evaluator = NA
      cvs_files = list.files(main_path, pattern = "cvs_biomarker_lesion.csv", recursive = TRUE, full.names = TRUE)
      cvs_files = cvs_files[which(grepl(subject, cvs_files))]
      cvs_score = lapply(cvs_files, function(x) read_csv(x))
      qc_list = list("summary_df" = summary_df, "labeled_lesion " = labeled_lesion , "brain_imgs" = brain_imgs, "seg_imgs" = seg_imgs, "cvs_score" = cvs_score)
    }
  return(qc_list)
}

#' Label Lesions
#'
#' Generate lesion labels for a binary lesion mask.
#'
#' @param mask The binary lesion mask.
#'
#' @return A labeled lesion image.
#'
#' @export

get_labeled_mask = function(mask){
  labeled_img = ants2oro(labelClusters(oro2ants(mask),minClusterSize=27))
  return(labeled_img)
}

#' Transform Image Types
#'
#' Transform mgz image type to nifti image type.
#'
#' @param file The mgz file path.
#'
#' @return An image in nifti format.
#'
#' @export
read_mgz = function(file){
  img = readmgz(file)
  L = fslr::rpi_orient(img)
  reoriented_img = L[["img"]]
  return(reoriented_img)
}


utils::globalVariables(c("evaluation", "subject", "volume_mm3"))

