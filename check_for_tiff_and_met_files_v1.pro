PRO check_for_tiff_and_met_files_v1, multi_scene_dir, loglun, met_file_name, is_error

; this subroutine is called by write_lat_lon and checks to see if there are
; .TIF files and a .MET file in the Landsat files directory. These are the
; files associated with the raw L1 Landsat scenes (have DN values only)


is_error = 0


multi_scene_list = FILE_SEARCH(multi_scene_dir + '/*', COUNT=n_files)

IF (n_files LE 0) THEN BEGIN 
  PRINT, ' '
  PRINT, '***************EXITING!******************'
  PRINT, 'from check_for_tiff_and_met_files_v1'
  PRINT, 'no Landsat files found in this directory:'
  PRINT, multi_scene_dir
  PRINT, 'maybe the pathname is invalid?'
  PRINTF, loglun, ' '
  PRINTF, loglun, '***************EXITING!******************'
  PRINTF, loglun, 'no Landsat files found in this directory:'
  PRINTF, loglun, multi_scene_dir
  PRINTF, loglun, 'maybe the pathname is invalid?'
  is_error = 1
  RETURN
ENDIF

multi_scene_list = FILE_SEARCH(multi_scene_dir + '/*.TIF', COUNT=n_files)
IF (n_files NE 0) THEN BEGIN
  PRINT, ' '
  PRINT, '***************WARNING!******************'
  PRINT, 'Expected at least 8 .TIF files in this directory:'
  PRINT, multi_scene_dir
  PRINT, 'but found ' + STRCOMPRESS(STRING(n_files))
  PRINT, ' '
  PRINTF, loglun, '***************WARNING!******************'
  PRINTF, loglun, 'Expected 10 .TIF files in this directory:'
  PRINTF, loglun, multi_scene_dir
  PRINTF, loglun, 'but found ' + STRCOMPRESS(STRING(n_files))
END

met_file_name = FILE_SEARCH(multi_scene_dir + '/*_MTL.txt', COUNT=n_files)
IF (n_files LE 0) THEN BEGIN
  PRINT, ' '
  PRINT, '***************EXITING!******************'
  PRINT, 'no Landsat MTL.txt files found in this directory:'
  PRINT, multi_scene_dir
  PRINT, 'maybe the pathname is invalid?'
  PRINTF, loglun, ' '
  PRINTF, loglun, '***************EXITING!******************'
  PRINTF, loglun, 'no Landsat MTL.txt files found in this directory:'
  PRINTF, loglun, multi_scene_dir
  PRINTF, loglun, 'maybe the pathname is invalid?'
  is_error = 1
  RETURN
ENDIF

IF (n_files GT 1) THEN BEGIN
  PRINT, ' '
  PRINT, '***************EXITING!******************'
  PRINT, 'too many Landsat MTL.txt files found in this directory:'
  PRINT, multi_scene_dir
  PRINT, 'found this many MTL.txt files ' + STRCOMPRESS(STRING(n_files), /REMOVE_ALL)
  PRINTF, loglun, ' '
  PRINTF, loglun, '***************EXITING!******************'
  PRINTF, loglun, 'too many Landsat MTL.txt files found in this directory:'
  PRINTF, loglun, multi_scene_dir
  PRINTF, loglun, 'found this many MTL.txt files ' + STRCOMPRESS(STRING(n_files), /REMOVE_ALL)
  is_error = 1
  RETURN
ENDIF


END
