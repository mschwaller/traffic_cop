PRO seab_check_for_RAW_dupe_scenes_2017a, etm_multi_scene_dir, id_prefix, loglun, is_error

; revised January 22, 2017 to handle raw landsat files with filenames like this: LO82241092015061LGN00

is_error = 0

multi_scene_list = FILE_SEARCH(etm_multi_scene_dir + '/*', COUNT=n_files)

IF (n_files LE 0) THEN BEGIN 
  PRINT, ' '
  PRINT, '***************EXITING!******************'
  PRINT, 'no Landsat files found in this directory:'
  PRINT, etm_multi_scene_dir
  PRINT, 'maybe the pathname is invalid?'
  PRINTF, loglun, ' '
  PRINTF, loglun, '***************EXITING!******************'
  PRINTF, loglun, 'no Landsat files found in this directory:'
  PRINTF, loglun, etm_multi_scene_dir
  PRINTF, loglun, 'maybe the pathname is invalid?'
  IS_ERROR = 2
  RETURN
ENDIF

base_filename_list = STRARR(n_files)
FOR ib=0,n_files-1 DO BEGIN
  ; strip off just the base filename from the Landsat directory name
  start_point = STRPOS(multi_scene_list[ib], '/', /reverse_search) + 1
  base_filename_list[ib] = STRMID(multi_scene_list[ib], start_point, 16)
ENDFOR


unique_scene_id_list = base_filename_list[UNIQ(base_filename_list,SORT(base_filename_list))]
num_unique = N_ELEMENTS(unique_scene_id_list)
IF (num_unique EQ n_files) THEN RETURN ; everything is OK, no duplicates

FOR iout=0,num_unique-1 DO BEGIN
  dupe_count=0
  FOR iin=0,n_files-1 DO BEGIN
    IF (unique_scene_id_list[iout] EQ base_filename_list[iin]) THEN BEGIN
      dupe_count = dupe_count + 1
      IF (dupe_count EQ 1) THEN dupe_1 = unique_scene_id_list[iout]
      IF (dupe_count EQ 2) THEN BEGIN
        PRINT, ' '
        PRINT, '***DUPLICATE LANDSAT SCENE FOUND***'
        PRINT, dupe_1
        PRINT, multi_scene_list[iin]
        PRINTF, loglun, ' '
        PRINTF, loglun, '***DUPLICATE LANDSAT SCENE FOUND***'
        PRINTF, loglun, dupe_1
        PRINTF, loglun, multi_scene_list[iin]
        is_error = 1
      ENDIF
      IF(dupe_count GT 2) THEN PRINT, multi_scene_list[iin]
    ENDIF
  ENDFOR
  IF (dupe_count GE 2) THEN PRINT, ' ' 
ENDFOR

END