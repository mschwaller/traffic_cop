PRO g_check_for_fmask_scenes_2016a, etm_multi_scene_dir, id_prefix, loglun, is_error

; this subroutine is called by write_lat_lon and checks to see if there are
; fmask.xxx.hdf Landsat scenes in the Landsat scene directory. If any are found
; they are moved out of the folder into another directory on the /Landsat_data
; directory. The reason the files are removed is that code later in write_latlon
; assumes that there is only 1 hdf file in any given scene directory


is_error = 0

chop_spot = STRPOS(etm_multi_scene_dir, '/', /REVERSE_SEARCH) 
landsat_directory = STRMID(etm_multi_scene_dir, 0, chop_spot)
fmask_dump_directory = landsat_directory + '/fmask_dump_directory'

multi_scene_list = FILE_SEARCH(etm_multi_scene_dir + '/*', COUNT=n_files)

IF (n_files LE 0) THEN BEGIN 
  PRINT, ' '
  PRINT, '***************EXITING!******************'
  PRINT, 'no Landsat files found in this directory:'
  PRINT, etm_multi_scene_dir
  PRINT, 'maybe the pathname is invalid?'
  is_error = 1
  RETURN
ENDIF

FOR is=0, n_files-1 DO BEGIN
  ;last_slash = STRPOS(multi_scene_list[is], '/', /REVERSE_SEARCH)
  ;obj_name = STRMID(multi_scene_list[is], last_slash+1, STRLEN(multi_scene_list[is]))
  ;IF (obj_name EQ '3') THEN CONTINUE ; can't belive I'm doing this!
  hdf_file_contents = FILE_SEARCH(multi_scene_list[is] + '/*.hdf', COUNT=n_hdf_files)
  CASE 1 OF
    n_hdf_files EQ 0: BEGIN ; CASE-1
      PRINT, '**********ERROR*********'
      PRINT, 'no hdf files found in this directory:'
      PRINT, multi_scene_list[is]
      PRINT, 'exiting'
      PRINTF, loglun, '**********ERROR*********'
      PRINTF, loglun, 'no hdf files found in this directory:'
      PRINTF, loglun, multi_scene_list[is]
      PRINTF, loglun, 'exiting'
      is_error = 1
      RETURN
    END ;  end CASE-1
    
    n_hdf_files EQ 1:  BEGIN; CASE-2
      is_error = 0 ; ALL IS RIGHT WITH THE WORLD! just keep looping
    END ; end CASE-2
    
    n_hdf_files EQ 2: BEGIN ; CASE-3 (probably has fmask files)
    FOR im=0,1 DO BEGIN
      IF (STRPOS(hdf_file_contents[im], 'fmask')) GE 0 THEN BEGIN
        check_directory = FILE_TEST(fmask_dump_directory, /DIRECTORY)
        ; does the dump directory already exist? if not, create it
        IF (NOT check_directory) THEN SPAWN,'mkdir ' + fmask_dump_directory
        ;now move the fmask hdf file into the dump directory
        SPAWN, 'mv ' + hdf_file_contents[im] + ' ' + fmask_dump_directory
        ; and move the fmask .hdr file too (were're assumin that one is in there...)
        SPAWN, 'mv ' + hdf_file_contents[im] + '.hdr' + ' ' + fmask_dump_directory
        fmask_check = 1
      ENDIF
    ENDFOR ; im loop
    IF (fmask_check EQ 0) THEN BEGIN ; found 2 hdf files but neither are fmask files
      PRINT, '**********ERROR*********'
      PRINT, 'too many hdf files found in this directory '
      PRINT, multi_scene_list[is]
      PRINT, 'exiting'
      PRINTF, loglun, '**********ERROR*********'
      PRINTF, loglun, 'too many hdf files found in this directory '
      PRINTF, loglun, multi_scene_list[is]
      PRINTF, loglun, 'exiting'
      is_error = 1
      RETURN
    ENDIF
   END ; end CASE-3
   
   n_hdf_files GE 3: BEGIN ; CASE-4
     PRINT, '**********ERROR*********'
     PRINT, 'expecting 1 hdf file in this directory '
     PRINT, multi_scene_list[is]
     PRINT, 'but found this many: ' + STRCOMPRESS(STRING(n_hdf_files, /REMOVE_ALL))
     PRINT, 'exiting'
     PRINTF, loglun, '**********ERROR*********'
     PRINTF, loglun, 'expecting 1 hdf file in this directory '
     PRINTF, loglun, multi_scene_list[is]
     PRINTF, loglun, 'but found this many: ' + STRCOMPRESS(STRING(n_hdf_files, /REMOVE_ALL))
     PRINTF, loglun, 'exiting'
     is_error = 1
    END ; end CASE-4
  ENDCASE
ENDFOR ; is=0,n_files loop
END