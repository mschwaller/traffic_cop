PRO g_check_for_dupe_scenes_2016a, etm_multi_scene_dir, id_prefix, loglun, is_error

; checked April 6, 2016 looks like it handles all (current!) file naming conventions

; this subroutine is called by write_lat_lon and checks to see if there are
; duplicate Landsat scenes in the Landsat scene directory

; also called by f_check_4_letter_code_status

is_error = 0

multi_scene_list = FILE_SEARCH(etm_multi_scene_dir + '/*', COUNT=n_files)

IF (n_files LE 0) THEN BEGIN 
  PRINT, ' '
  PRINT, '***************EXITING!******************'
  PRINT, 'no Landsat files found in this directory:'
  PRINT, etm_multi_scene_dir
  PRINT, 'maybe the pathname is invalid?'
  IS_ERROR = 2
  RETURN
ENDIF

full_path_scene_list = STRARR(n_files)
scene_id_list = STRARR(n_files)

; make a list of all the Landsat scenes
; L8 formatted like this: LC80031122014014LGN00.hdf
; others formatted like this: lndcal.LE70641102003003EDC00.hdf
FOR is=0,n_files-1 DO BEGIN
  hdf_file_name = FILE_SEARCH(multi_scene_list[is] + '/*.hdf', COUNT=n_hdf_files)
  IF (n_hdf_files NE 1) THEN BEGIN
    PRINT, '***ERROR IN SEARCHING FOR HDF FILES***'
    PRINT, 'in this directory:
    PRINT, multi_scene_list[is]
    PRINT, 'expecting 1 hdf file, but found this many: ' + STRCOMPRESS(STRING(n_hdf_files), /REMOVE_ALL)
    PRINT, '*************EXITING!*****************'
    PRINTF, loglun, '***ERROR IN SEARCHING FOR HDF FILES***'
    PRINTF, loglun, 'in this directory:
    PRINTF, loglun, multi_scene_list[is]
    PRINTF, loglun, 'expecting 1 hdf file, but found this many: ' + STRCOMPRESS(STRING(n_hdf_files), /REMOVE_ALL)
    PRINTF, loglun, '*************EXITING!*****************'
    is_error = 2
    RETURN
  ENDIF
  full_path_scene_list[is] = FILE_SEARCH(multi_scene_list[is] + '/*.hdf', COUNT=n_lndcal_files)
  parsed = STRSPLIT(full_path_scene_list[is], '/', /EXTRACT)
  last_element = N_ELEMENTS(parsed) - 1
  extended_scene_id = parsed[last_element] ; in the old form lndcal.LE70631112001326EDC00.hdf or newer LE70641102001333EDC00.hdf
  IF (STRPOS(extended_scene_id, 'lndcal') GE 0) $
    THEN scene_id = STRMID(extended_scene_id, 7, 16) $
    ELSE scene_id = STRMID(extended_scene_id, 0, 16)
  scene_id_list[is] = scene_id
ENDFOR

unique_scene_id_list = scene_id_list[UNIQ(scene_id_list,SORT(scene_id_list))]
num_unique = N_ELEMENTS(unique_scene_id_list)
IF (num_unique EQ n_files) THEN RETURN ; everything is OK, no duplicates

FOR iout=0,num_unique-1 DO BEGIN
  dupe_count=0
  FOR iin=0,n_files-1 DO BEGIN
    IF (unique_scene_id_list[iout] EQ scene_id_list[iin]) THEN BEGIN
      dupe_count = dupe_count + 1
      IF (dupe_count EQ 1) THEN dupe_1 = unique_scene_id_list[iout]
      IF (dupe_count EQ 2) THEN BEGIN
        PRINT, '***DUPLICATE LANDSAT SCENE FOUND***'
        PRINT, dupe_1
        PRINT, full_path_scene_list[iin]
        is_error = 1
      ENDIF
      IF(dupe_count GT 2) THEN PRINT, full_path_scene_list[iin]
    ENDIF
  ENDFOR
  IF (dupe_count GE 2) THEN PRINT, ' ' 
ENDFOR

END