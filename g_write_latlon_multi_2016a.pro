PRO g_write_latlon_multi_2016a, config_file_name

 code_version_name = 'g_write_latlon_multi_2016a'

; April 1, 2016 cleaned up some of the data ingest due to a file-naming change by USGS
; USGS has removed the lndcal prefix from the ESPA filenames, the changes in here should
; now handle both possibilities. 

; December 1, 2015: removed cloud_calc code, this is now done in e_check_4_letter_code_status_v6

; October 22 this version is revised to handle the Landsat-8 exceptions
; there are only 3 variables that control the file naming and band selection:
; band_numbers_array : this variable is assigned in STEP-1 and stores the band numbers 
;    that will be used in the retrieval IN THE ORDER IN WHICH THEY will be converted to spherical 
;    coordinates. so for eacmple the band order could be B3+B5+B4+B2 and the band_numbers_array
;    would = [3,5,4,2]. WE ARE KEEPING THE CONVENTION that B1=blue, B2=green, B3=red,
;    B4=NIR, B5=SWIR-1, B6=SWIR-2 (no changes were actually made in the code related to this
;    variable)
; lndcal_fid_array : this variable is assigned in STEP-2 and stores the FIDs in sequential
;    order. For L4,5,7 these FIDs correspond to HDF files 0,1,2,3,4,5 (blue through SWIR-2)
;    For L8 these FIDs correspond to HDF files 1,2,3,4,5,6 (again blue through SWIR-2)
;    To get the indexing correct, a new variable was added to the exception section: dataset_exception_ctr.
;    At some points in the code lndcal_fid_array gets indexed by band_numbers_array
;    lndcal_fid_array[band_numbers_array[ictr]] ... so adding the dataset_exception_ctr should
;    ensure the correct indexing
; lndcal_file_name : the file name gets set in the exception section to handle the peculiarities
;    of L-4,5,7 and L-8


; October 14, 2015 this version can ingest Landsat-8 as well as L-4,5,7 data
; it does the nudging too.
; this version DOES NOT do take the ensemble approach
; this version acts onl ONLY 1 T-matrix

; June 22, 2015 This version applies the nudge factors to the trasnformed spherical coordinates
; data, with the intent of adjusting for cross-calibration differences between L7 and L4 and L5.
;   

; November 25, 2014 this version generates an ensemble of results using different band 
; combinations fed to it by its config file

; July 17 a new t_ version of the software, intended to account for the 4 possible
; outcomes of a colony search: 
; no data
; cloudy over a site
; clear data over a site, no colony records
; clear data over a site, has colony records
; 

; this routine calls e_mine_lndcal_subsets_multi_2015a and
; this routine calls e_calc_hdf_map_projection_2015a
; this routine calls e_checkdupes_multi_2015a
; this routine calls e_make_accounting_png_thumbnails_multi_201a

; July 1 this version saves colony pixels discovered in any 1 scene to a temp file
; and removes any duplicates, then saves the un-duplicated colony pixels to a
; class_summary file. The previous version did the dupe check AFTER all colony
; pixels were found, but that took hours and hours...

; June 25 this version ingests the colony name from the PLOS-1 paper Apendices B & C
; this version now passes the array adelie_name_line_array holds the colony names
; corresponding to the adelie_sample_line_array list of lat,lon coordinates associated
; line-by-line with the a colony name. This action actually takes place in the
; subroutine s_mine_lndcal_subsets_2014_b

; June 20-22, 2014 cleaned up some bugs in array indexing caused by the
; transition from whole scene processing to known-colony-area-only
; processing. The Dec 5 version of this code was writeen but not actually
; tested with real data, so the need for bug fixes.

; December 5, 2013 this version uses the "data mining" approach
; to finding the spatial extent of Adelie colonies. It assumes that
; we have a set of latitude and longitude points where we are searching
; for Adelie colonies. These lat/lon points could be known Adelie
; colonies or they could be the locations of known rock outcrops.
; 
; this routine calls s_mine_lndcal_subsets_2014b and
; this routine calls s_calc_hdf_map_projection_v1
; this routine calls s_checkdupes_v1_2014a
; 
; this routine uses a configuration file, the filename for the
; config file is specified just below the 1st line of IDL code
; 
; The sequence for generating rookery classifications and
; associated output is
; 
;     s_write_latlon_v1_2014.pro (this proc, calls 3 subroutines, see above)
;     s_find_rooks_9_2014a.pro (calls s_print_rooks7_1_2014.pro
;        and s_make_png_thumbnails_v1_2014.pro)
;     

COMPILE_OPT STRICTARR


;******************default values***********************
total_config_parms = 13 ; total numbers of variables & files to be read from the config file
;******************default values***********************

; generate a systime string to append to the output file
timedate =  SYSTIME(0)
timedate = STRMID(timedate,4,STRLEN(timedate)) ; time & date will be added to
STRPUT, timedate, 'h', 6                       ; the output file name
STRPUT, timedate, 'm', 9
STRPUT, timedate, 's', 12
STRPUT, timedate, 'y', 15
timedate = STRCOMPRESS(timedate, /remove_all)  ; remove all blanks

; and print the start time
PRINT, '--->start time: ', timedate

; Restore the core file and start ENVI in batch
ENVI, /RESTORE_BASE_SAVE_FILES
ENVI_BATCH_INIT, /NO_STATUS_WINDOW

FOR i=1,3 DO PRINT, '*****STARTING*****'


; open the config file 
OPENR, config_lun, config_file_name, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to open the config file!'
  PRINT, config_file_name
  PRINT, STRMESSAGE(err)
  PRINT, '--->quitting!'
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF

PRINT, '--->parsing the config file '
PRINT, config_file_name

aline = ' '
line_number = 0UL ; Unsigned Long
config_check = 0
num_config_lines = FILE_LINES(config_file_name)
config_text = STRARR(num_config_lines) ; save the config file to this array as it is read in, later we will write it out to a file
WHILE ~ EOF(config_lun) DO BEGIN ; adding the classification summary filename, which is used as input by r_find_rooks_9
  READF, config_lun, aline
  config_text[line_number] = aline
  line_number = line_number + 1UL
  CASE 1 OF
    ; ****CASE-1****
    STRPOS(aline, '<ID_prefix>') GE 0: BEGIN
      ; TYPE = STRING
      ; prefix to be used when assigning accession numbers to rookeries 
      first_mark = STRPOS(aline, '>') + 1
      last_mark  = STRPOS(aline, '<', /REVERSE_SEARCH)
      ID_prefix  = STRMID(aline, first_mark, last_mark-first_mark)
      config_check = config_check + 1
    END
    ; ****CASE-2****
    STRPOS(aline, '<etm_multi_scene_dir>') GE 0: BEGIN
      ; TYPE = STRING
      ; The input Landsat data directory to search on (input data) it contains many scenes 
      ; along the antarctic coast. these scenes have all been transformed to at-instrument reflectance
      first_mark = STRPOS(aline, '>') + 1
      last_mark  = STRPOS(aline, '<', /REVERSE_SEARCH)
      etm_multi_scene_dir = STRMID(aline, first_mark, last_mark-first_mark)
      config_check = config_check + 1
    END
    ; ****CASE-3****
    STRPOS(aline, '<T_file_name>') GE 0: BEGIN
      ; TYPE = STRIN
      ; the file containing the T-matrix
      ; THIS FILE WILL ALSO HAVE A LIST OF THE BANDS USED TO GENERATE THE T-MATRIX
      ; the band list will be in the form B1+B2+B3+B4 but may be in any order with
      ; band numbers 1 to 6 (sequentially numbered, not L7 band-name numbered)
      first_mark = STRPOS(aline, '>') + 1
      last_mark  = STRPOS(aline, '<', /REVERSE_SEARCH)
      T_file_name = STRMID(aline, first_mark, last_mark-first_mark)
      config_check = config_check + 1
    END
    ; ****CASE-4****
    STRPOS(aline, '<write_lat_lon_out_dir>') GE 0: BEGIN
      ; TYPE = STRING
      ; this is the output data directory to store the transformed/classified output pixels
      first_mark = STRPOS(aline, '>') + 1
      last_mark  = STRPOS(aline, '<', /REVERSE_SEARCH)
      write_lat_lon_out_dir = STRMID(aline, first_mark, last_mark-first_mark)
      config_check = config_check + 1
    END
    ; ****CASE-5****
    STRPOS(aline, '<write_lat_lon_out_log>') GE 0: BEGIN
      ; this is the output data directory to store the log file
      first_mark = STRPOS(aline, '>') + 1  
      last_mark  = STRPOS(aline, '<', /REVERSE_SEARCH)
      write_lat_lon_out_log = STRMID(aline, first_mark, last_mark-first_mark)
      config_check = config_check + 1
    END
      ; ****CASE-6****
      STRPOS(aline, '<adelie_latlon_filename>') GE 0: BEGIN
        ; TYPE = STRING
        ; filename of Adelie colony locations (longitude,latitude) based on Lynch's Appendix B
        ; and Schwaller's retrievals
        ; at present the file is located here:
        ; '/Volumes/Solid_State/re-analysis/lynch_and_schwaller_lonlat.txt'
        first_mark = STRPOS(aline, '>') + 1
        last_mark  = STRPOS(aline, '<', /REVERSE_SEARCH)
        adelie_latlon_filename  = STRMID(aline, first_mark, last_mark-first_mark)
        config_check = config_check + 1
      END
      ; ****CASE-7****
      STRPOS(aline, '<nudge_filename>') GE 0: BEGIN
        ; TYPE = STRING
        ; filename of nudge factors used to cross calibrate L7, L5, & L4 spherical coordinate data
        ; at present the file is located here:
        ; /Users/mschwall/~Essentials/penguin_stuff/cross_calibration/run_for_record/config_file
        first_mark = STRPOS(aline, '>') + 1
        last_mark  = STRPOS(aline, '<', /REVERSE_SEARCH)
        nudge_filename  = STRMID(aline, first_mark, last_mark-first_mark)
        config_check = config_check + 1
      END
      ; ****CASE-8****
      STRPOS(aline, '<satellite_ID>') GE 0: BEGIN
        ; TYPE = STRING
        ; two-letter code that identifies the Landsat satellite that generated the
        ; image data that will be ingested into this analysis
        ; valid values are: L4 L5 L7 L8
        first_mark = STRPOS(aline, '>') + 1
        last_mark  = STRPOS(aline, '<', /REVERSE_SEARCH)
        sat_check  = STRMID(aline, first_mark, last_mark-first_mark)
        config_check = config_check + 1
      END
      ; ****CASE-9****
      STRPOS(aline, '<this_chunk_number>') GE 0: BEGIN
        ; TYPE = integer
        ; the job number ID
        first_mark = STRPOS(aline, '>') + 1
        last_mark  = STRPOS(aline, '<', /REVERSE_SEARCH)
        this_chunk_number_string = STRMID(aline, first_mark, last_mark-first_mark)
        this_chunk_number  = FIX(this_chunk_number_string)
        config_check = config_check + 1
      END
      ; ****CASE-10****
      STRPOS(aline, '<number_of_scenes>') GE 0: BEGIN
        ; TYPE = integer
        ; the number of scenes to process in this job
        first_mark = STRPOS(aline, '>') + 1
        last_mark  = STRPOS(aline, '<', /REVERSE_SEARCH)
        number_of_scenes  = FIX(STRMID(aline, first_mark, last_mark-first_mark))
        config_check = config_check + 1
      END
      ; ****CASE-11****
      STRPOS(aline, '<job_name>') GE 0: BEGIN
        ; TYPE = integer
        ; the number of scenes to process in this job
        first_mark = STRPOS(aline, '>') + 1
        last_mark  = STRPOS(aline, '<', /REVERSE_SEARCH)
        job_name  = STRMID(aline, first_mark, last_mark-first_mark)
        config_check = config_check + 1
      END
  ELSE:
ENDCASE
ENDWHILE

IF (total_config_parms NE config_check) THEN BEGIN
  PRINT, '--->expected this many input parameters in the config file ', total_config_parms
  PRINT, '--->found this many ', config_check
  PRINT, '--->EXITING!'
  ENVI_BATCH_EXIT
  CLOSE, /ALL
  RETURN
ENDIF

IF NOT ((sat_check EQ 'L4') OR (sat_check EQ 'L5') OR (sat_check EQ 'L7') OR (sat_check EQ 'L8') OR (sat_check EQ 'L45')) THEN BEGIN
  PRINT, 'satellite ID not found to be L4, L5, L7, or L8'
  PRINT, '***QUITTING!***'
  ENVI_BATCH_EXIT
  CLOSE, /ALL
  RETURN
ENDIF

continent_pix_total = ULONG64(0) ; just how many pixels get processed anyhow?




; generate a log file
log_file =  write_lat_lon_out_log + '/' + ID_prefix + 'write_latlon_log_file_' + 'job' + this_chunk_number_string + '_' + timedate + '.txt'
OPENW, loglun, log_file, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to create a this log file'
  PRINT, log_file
  PRINT, STRMESSAGE(err)
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF
PRINT, '--->generated this log file: '
PRINT, log_file

; make an array with the scene names in it, this is a subset of the full Landsat directory of scenes
; the subset is specified by the config file for a given job
scene_name_array = STRARR(number_of_scenes) 
found_it = 0
FOR il = 0, num_config_lines-1 DO BEGIN ; loop through the config_text array (max_text lines long, a lot of blank lines)
  IF STRPOS(config_text[il], '<job_scenes>') GE 0 THEN BEGIN ; loop to where the scene subsets are defined in the config file
    found_it = 1
    i_start = il + 1 ; skip over the <job_scenes> line
    i_end = i_start + number_of_scenes - 1
    short_ctr = 0 ; index for the scene_name_array (a subset of the full directory of scenes)
    FOR iq = i_start, i_end DO BEGIN
      scene_name_array[short_ctr] = STRCOMPRESS(config_text[iq], /REMOVE_ALL)
      short_ctr = short_ctr + 1
    ENDFOR
    BREAK ; we have all of the scene names so break out of the outer loop
  ENDIF
ENDFOR
IF (found_it EQ 0) THEN BEGIN
  PRINT, '--->did not find any valid scene names in the config file ' + config_file_name
  PRINT, '--->EXITING!'
  PRINTF, loglun, '--->did not find any valid scene names in the config file ' + config_file_name
  PRINTF, loglun, '--->EXITING!'
  ENVI_BATCH_EXIT
  CLOSE, /ALL
  RETURN
ENDIF

PRINTF, loglun, 'these are the scenes that we are working on:'
FOR ip=0,number_of_scenes-1 DO PRINTF, loglun, scene_name_array[ip]

PRINTF, loglun, 'CODE VERSION: ' + code_version_name
PRINTF, loglun, '--->configurable parameters in config file'
PRINTF, loglun, 'job name: ' + job_name
PRINTF, loglun, 'chunk (job) number: ' + STRCOMPRESS(STRING(this_chunk_number), /REMOVE_ALL)
PRINTF, loglun, 'geographic region ID = ', ID_prefix
PRINTF, loglun, 'Landsat multi-file directory: '
PRINTF, loglun, etm_multi_scene_dir
PRINTF, loglun, 'T-matrix file name: '
PRINTF, loglun, T_file_name
PRINTF, loglun, 'output file directory for this procedure: '
PRINTF, loglun, write_lat_lon_out_dir
PRINTF, loglun, 'output log file for this procedure: '
PRINTF, loglun, write_lat_lon_out_log
PRINTF, loglun, 'Adelie lon,lat location filename: '
PRINTF, loglun, adelie_latlon_filename
PRINTF, loglun, 'operating on these Landsat files:'
FOR ip=0,number_of_scenes-1 DO PRINTF, loglun, scene_name_array[ip]
PRINTF, loglun, ' '

CLOSE, loglun
FREE_LUN, loglun
OPENW, loglun, log_file, /GET_LUN, ERROR=err, /APPEND
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to create a this log file'
  PRINT, log_file
  PRINT, STRMESSAGE(err)
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF


;*********************************************************************************************
;*******************************check for fmask.xxx.hdf files*********************************
;*********************************************************************************************
is_error = 0 ; no errors yet
g_check_for_fmask_scenes_2016a, etm_multi_scene_dir, sat_check, loglun, is_error
IF (is_error GT 0) THEN BEGIN
  PRINTF, loglun, '**********************************************************************'
  PRINTF, loglun, '*******FOUND PROBLEMS WITH HDF FILES IN THE LANDSAT DIRECTORY*********'
  PRINTF, loglun, '*************************QUITTING!************************************'
  PRINT, ' '
  PRINT,'**********************************************************************'
  PRINT,'*******FOUND PROBLEMS WITH HDF FILES IN THE LANDSAT DIRECTORY*********'
  PRINT,'*************************QUITTING!************************************'
  ENVI_BATCH_EXIT
  CLOSE, /ALL
  RETURN
ENDIF
;*********************************************************************************************
;*******************************check for fmask.xxx.hdf files*********************************
;*********************************************************************************************


;*********************************************************************************************
;******************************check for Landsat scene duplicates*****************************
;*********************************************************************************************
is_error = 0 ; no errors yet
g_check_for_dupe_scenes_2016a, etm_multi_scene_dir, sat_check, loglun, is_error
IF (is_error EQ 2) THEN BEGIN ; no files found in etm_multi_scene_dir, probably bad pathname
  ENVI_BATCH_EXIT
  CLOSE, /ALL
  RETURN
ENDIF
IF (is_error GT 0) THEN BEGIN
  PRINTF, loglun, '*********************************************************'
  PRINTF, loglun, '*******FOUND DUPLICATES IN THE LANDSAT DIRECTORY*********'
  PRINTF, loglun, '*******************QUITTING!*****************************' 
  PRINT, ' '
  PRINT, '*********************************************************'
  PRINT,'*******FOUND DUPLICATES IN THE LANDSAT DIRECTORY*********'
  PRINT,'*******************QUITTING!*****************************'
  ENVI_BATCH_EXIT
  CLOSE, /ALL
  RETURN
ENDIF
;*********************************************************************************************
;***************************end check for Landsat scene duplicates****************************
;*********************************************************************************************

;*********************************************************************************************
;****************create master list of colony names for accounting purposes*******************
;*********************************************************************************************
; create as master list of colony names from the filename stored in the variable adelie_latlon_filename
; this master list will be checked after each run of e_mine_lndcal_subsets. Each of the 
; colonies found within a a given scene by e_mine_lndcal_subsets will be checked off the list of colonies
; stored on the master list


; open the file of adelie locations for reading
OPENR, adelie_in_lun, adelie_latlon_filename, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to open file'
  PRINT, adelie_latlon_filename
  PRINTF, loglun, '--->a file error occurred when trying to open file'
  PRINTF, loglun,  adelie_latlon_filename
  PRINT, STRMESSAGE(err)
  is_error = 1
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF

num_adelie_lines = FILE_LINES(adelie_latlon_filename)

master_list_struct = {colony_name:STRARR(1,num_adelie_lines), $
  colony_lat_lon:FLTARR(2,num_adelie_lines), $
  found_in_scene:BYTARR(1,num_adelie_lines), $
  colony_4letter_id:STRARR(1,num_adelie_lines)}
; colony_name is the name of the colony
; colony_lat_lon is the geographic lat,lon of the colony
; found_in_scene is a logical 1,0 1=the colony was found within a landsat scene, 0=no data coverage of the colony

colony_in_counter = 0 ; count the number of colonies found within the scene

one_line = ' '
FOR ic=0L,num_adelie_lines-1L DO BEGIN
  READF, adelie_in_lun, one_line
  parsed = STRSPLIT(one_line, ',', /EXTRACT)
  colony_lat = DOUBLE(parsed[0])
  colony_lon = DOUBLE(parsed[1])
  master_list_struct.colony_name[0,ic] = parsed[2] ; add the name of the colony to the structure
  master_list_struct.colony_lat_lon[0,ic] = colony_lat ; enter the colony lat into the structure
  master_list_struct.colony_lat_lon[1,ic] = colony_lon ; enter the colony lon into the structure  
  master_list_struct.found_in_scene[0,ic] = 0B ; initialize the colony locations as "found in a landsat scene" to FALSE
  ; whether "found in scene" is TRUE will be tested after each call to e_mine_lndcal_subsets
  ; NOTE that a given colony may be covered by several scenes, this structure won't be able to tell you that
  master_list_struct.colony_4letter_id[0,ic] = parsed[3]
ENDFOR
CLOSE, adelie_in_lun
FREE_LUN, adelie_in_lun
;*********************************************************************************************
;***************************END colony master list creation***********************************
;*********************************************************************************************


;*********************************************************************************************
;********************************open and read T-matrix***************************************
;*********************************************************************************************


; open the T-matrix file and read the parsed T-matrix
OPENR, t_lun, T_file_name, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to open file'
  PRINT, T_file_name
  PRINT, STRMESSAGE(err)
  PRINT, '--->quitting!'
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF

PRINT, 'reading from file:'
PRINT, '   ', T_file_name

one_line = ' '
;READF, t_lun, one_line ; first line is an information header, has generation date, etc.
;READF, t_lun, one_line ; second line just says "number of iterations"
;READF, t_lun, one_line ; third line is the actual number of T-matrices in the file
;num_t_matrices = FIX(one_line)
num_t_matrices = 1
;PRINT, '--->number of T-matricies in the ensemble ', num_t_matrices

; t_matrix_array > is a bands x bands x num_t_matrices array that holds the t-matrices, bands assumed to = 6
; num_bands_per_matrix_array > actual number of bands in each T-matrix
; band_names_array > is an array with the bands names associated with each t-matrix in the form 
;                  for example: B1+B5+B3+B2+B5+B6  
t_matrix_struct = {t_matrix_array:DBLARR(6,6,num_t_matrices), $ ; assuming a max of 6 bands per T-matrix
                   num_bands_per_matrix_array:INTARR(num_t_matrices), $ ; actual # bands in each T-matrix
                   band_names_array:STRARR(num_t_matrices)}  ; an array of band names

; inm is the index of the T-mtrix in the array T_matrix_struct.T_matrix_array[col,row,inm]
; ALWAYS GO JUST 1X THROUGH THE LOOP (no longer doing ensembles)
FOR inm=0,num_t_matrices-1 DO BEGIN ; read a set of num_t_matrices
  ; READF, t_lun, one_line ; this line should be a set of ************s
  ;IF (STRPOS(one_line, '*') LT 0) THEN BEGIN ; there should be a * in the line...
  ;  PRINT, '--->an error occurred when reading the T-matrix file on this line:'
  ;  PRINT, one_line
  ;  PRINT, '--->expected a line of ************s
  ;  PRINT, '--->quitting!'
  ;  CLOSE, /ALL
  ;  ENVI_BATCH_EXIT
  ;  RETURN
  ;ENDIF
  READF, t_lun, one_line ; this should be the line with the band numbers, e.g., B1+B2+B3+B4
  t_matrix_struct.band_names_array[inm] = one_line
  my_bands_array = STRSPLIT(one_line, '+', /EXTRACT, COUNT=num_bands)
  t_matrix_struct.num_bands_per_matrix_array[inm] = num_bands ; 3 bands in this T-matrix
  PRINT, '--->number of bands found in the T-matrix ', num_bands
  IF (num_bands LT 4) THEN BEGIN
    PRINT, '--->too few bands were found in the T-matrix when reading this line:'
    PRINT, one_line
    PRINT, '--->quitting!'
    CLOSE, /ALL
    ENVI_BATCH_EXIT
    RETURN
  ENDIF
  ; now read the num_bands lines of the T-matrix
  FOR imat=0,num_bands-1 DO BEGIN
    READF, t_lun, one_line
    parsed = STRSPLIT(one_line, '|', /EXTRACT, COUNT=n_fields_check)
    FOR jmat=0,num_bands-1 DO T_matrix_struct.T_matrix_array[jmat,imat,inm]=DOUBLE(parsed[jmat])
  ENDFOR
  
  PRINT, ' '
  PRINT, '********************'
  PRINT, 'T-matrix number ' + STRCOMPRESS(STRING(inm), /REMOVE_ALL)
  PRINT, '--->these channels will be used in the write_latlon retrieval: '
  PRINT, t_matrix_struct.band_names_array[inm]
  PRINT, ' '
  PRINTF, loglun, ' '
  PRINTF, loglun, '********************'
  PRINTF, loglun, 'T-matrix number ' + STRCOMPRESS(STRING(inm), /REMOVE_ALL)
  PRINTF, loglun, '--->these channels will be used in the write_latlon retrieval: '
  PRINTF, loglun, t_matrix_struct.band_names_array[inm]
  PRINTF, loglun, ' '

ENDFOR ; the inm loop


;*********************************************************************************************
;****************************END open and read T-matrix***************************************
;*********************************************************************************************


;*********************************************************************************************
;********************************open and read nudge-matrix***********************************
;*********************************************************************************************

; OK, we're assuming that the nudge matrix is a 4-row, 6-column matrix and we will bail out
; if it is anything else!
; 1st column is the satellite ID, subsequent columns are the nudge factors

nudge_row = 4
nudge_col = 6
str_row = STRCOMPRESS(STRING(nudge_row), /REMOVE_ALL)
str_col = STRCOMPRESS(STRING(nudge_col), /REMOVE_ALL)

nudge_array = DBLARR(nudge_col,nudge_row) ; includes the satellite ID and the nudge values
; the satellite ID is stripped off in Step-8 of the big loop

n_lines = FILE_LINES(nudge_filename)

IF (n_lines LT nudge_row) THEN BEGIN
  PRINT, 'expecting ' + str_row + ' lines in nudge file, but only found ' + STRCOMPRESS(STRING(n_lines), /REMOVE_ALL)
  PRINT, 'in this nudge file:'
  PRINT, nudge_filename
  PRINT, ' *** EXITING!***'
  PRINTF, loglun, 'expecting ' + str_row + ' lines in nudge file, but only found ' + STRCOMPRESS(STRING(n_lines), /REMOVE_ALL)
  PRINTF, loglun, 'in this nudge file:'
  PRINTF, loglun, nudge_filename
  PRINTF, loglun, ' *** EXITING!***'
  ENVI_BATCH_EXIT
  CLOSE, /ALL
  RETURN
ENDIF

; open the nudge matrix file and read the parsed T-matrix
OPENR, nudge_lun, nudge_filename, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to open file'
  PRINT, nudge_filename
  PRINT, STRMESSAGE(err)
  PRINT, '--->quitting!'
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF

PRINT, 'reading from nudge file:'
PRINT, '   ', nudge_filename
PRINTF, loglun, 'reading from nudge file:'
PRINTF, loglun, '   ', nudge_filename

one_line = ' '
FOR inud=0,nudge_row-1 DO BEGIN ; read the 3 lines of the nudge matrix
  READF, nudge_lun, one_line ; this should be the line with 6 values: satellite number, then 5 phi-values
  one_line_array = STRSPLIT(one_line, '|', /EXTRACT, COUNT=num_items)
  IF (num_items NE nudge_col) THEN BEGIN
    PRINT, 'expecting ' + str_col + ' items in nudge file line' + STRCOMPRESS(STRING(inud + 1), /REMOVE_ALL) + ' but only found ' + STRCOMPRESS(STRING(num_items), /REMOVE_ALL)
    PRINT, 'in this nudge file:'
    PRINT, nudge_filename
    PRINT, 'this is the offending line:'
    PRINT, one_line
    PRINT, ' *** EXITING!***'
    PRINTF, loglun, 'expecting ' + str_col + ' items in nudge file line' + STRCOMPRESS(STRING(inud + 1), /REMOVE_ALL) + ' but only found ' + STRCOMPRESS(STRING(num_items), /REMOVE_ALL)
    PRINTF, loglun, 'in this nudge file:'
    PRINTF, loglun, nudge_filename
    PRINTF, loglun, ' *** EXITING!***'
    ENVI_BATCH_EXIT
    CLOSE, /ALL
    RETURN
  ENDIF
  FOR jnud=0,nudge_col-1 DO nudge_array[jnud,inud] = DOUBLE(one_line_array[jnud]) ; include the satellite ID when indexing one_line_array
ENDFOR

PRINT, ' '
PRINT, 'nudge_array'
PRINT, nudge_array
PRINT, ' '
PRINTF, loglun, ' '
PRINTF, loglun, 'nudge_array'
PRINTF, loglun, nudge_array
PRINTF, loglun, ' '

; close the log file here, and re-open it at the start of every ie loop
; and /APPEND more lines to it
PRINT, '--->temporarily closing the log file'
CLOSE, loglun
FREE_LUN, loglun
  
;*************************************************************************
;***************START LOOPING THROUGH ALL T-MATRICES HERE*****************
;* This version DOES NOT take the ensemble approach. This version acts   *
;* on only 1 T-matrix!                                                   *
;*************************************************************************

FOR ie=0,num_t_matrices-1 DO BEGIN ; num_t_matrices is fixed = 1
t_num_bands = t_matrix_struct.num_bands_per_matrix_array[ie]

; re-open the log file and /APPEND new info to it
OPENW, loglun, log_file, /APPEND, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to create a log file'
  PRINT, '--->in this directory ', write_lat_lon_out_dir
  PRINT, STRMESSAGE(err)
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF
PRINT, '--->re-opened this log file for /APPEND-ing: '
PRINT, log_file

; chunk out the ie-th T-matrix
T_matrix = DBLARR(t_num_bands,t_num_bands)
FOR it=0,t_num_bands-1 DO $
  FOR jt=0,t_num_bands-1 DO T_matrix[it,jt] = t_matrix_struct.T_matrix_array[it,jt,ie]
  
; and chunk the ie-th set of band names
this_loop_bands_array = STRSPLIT(t_matrix_struct.band_names_array[ie], '+', /EXTRACT, COUNT=num_bands)

; store the band numbers without the Bs (ASSSUMES NO MORE THAN 6 BANDS!) into band_numbers_arry
; this array is used when extracting FID band values and assigning them to the barr in the right order
; see notes from October 22, 2015 about this variable
band_numbers_array = INTARR(t_num_bands)
FOR ib=0,num_bands-1 DO BEGIN  ; test to be sure band numbers are within range expected
  the_num = FIX(STRMID(this_loop_bands_array[ib],1,1))
  IF (the_num GT 6)  THEN BEGIN
    PRINT, '--->Landsat band numbers are out of the range 1-6'
    PRINT, 'bogus band number encoundered = ', the_num
    PRINT, 'exiting!
    CLOSE, /ALL
    ENVI_BATCH_EXIT
    RETURN
  ENDIF ELSE BEGIN
    band_numbers_array[ib] = the_num
  ENDELSE
ENDFOR


T = T_matrix ; less to type            
T_invert  = INVERT(T)
T_dim = N_ELEMENTS(T[0,*])        ; there are T-1 phi bands
; note that if you are working with 5 bands T will have a dimension of 5x5
; so you  need to subtract 1 from the n_elements(T[0,*]) to get the right # of phi-bands

PRINTF, loglun, ' '
PRINTF, loglun, '--->iteration number ', STRCOMPRESS(STRING(ie), /REMOVE_ALL)
PRINTF, loglun, '--->started main loop at ', SYSTIME(0)
PRINTF, loglun, ' '
PRINT, ' '
PRINT, '--->iteration number ', STRCOMPRESS(STRING(ie), /REMOVE_ALL)
PRINT, '--->started main loop at ', SYSTIME(0)
PRINT, ' '

PRINTF, loglun, 'working on this t-matrix'
PRINTF, loglun, T
PRINTF, loglun, ' '
PRINTF, loglun, 'using these bands '
PRINTF, loglun, t_matrix_struct.band_names_array[ie]
PRINTF, loglun, 'band name check: ', band_numbers_array
PRINT, 'working on this t-matrix'
PRINT, T
PRINT, ' '
PRINT, 'using these bands '
PRINT, t_matrix_struct.band_names_array[ie]
PRINT, 'band name check: ', band_numbers_array

; generate an output file for each T-matrix loop
out_file = write_lat_lon_out_dir + '/' + ID_prefix + 'class_summary_' + t_matrix_struct.band_names_array[ie] + '_job' + this_chunk_number_string + '_' + timedate + '.txt'
PRINT, '--->creating this file for capturing colony location information'
PRINT, out_file
PRINTF, loglun, '--->creating this file for capturing colony location information'
PRINTF, loglun, out_file
OPENW, outlun, out_file, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred trying to open the output file ', err, STRMESSAGE(err)
  PRINTF, loglun, '--->a file error occurred tring to open the output file ', err, STRMESSAGE(err)
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF


; PREP FOR THE BIG LOOP!
; first, find the names of all landsat scenes in the Landsat_Data directory
; aka etm_multi_scene_dir ... then, loop through all scenes and extract 
; rookery pixels
    full_scene_list  = FILE_SEARCH(etm_multi_scene_dir + '/*', COUNT=num_all_files) 
    multi_scene_list = STRARR(number_of_scenes) ; number_of_scenes is specified in the config file
    
    ; loop through the full scene list and pick off just the scenes specified in the config file
    multi_scene_counter = 0
    FOR ia=0,num_all_files-1 DO BEGIN       ; loop through the full_scene_list
      FOR ib=0,number_of_scenes-1 DO BEGIN     ; loop though scene_name_array holds the names specified in the config file
        IF STRPOS(full_scene_list[ia], scene_name_array[ib]) GE 0 THEN BEGIN
          multi_scene_list[multi_scene_counter] = full_scene_list[ia]
          multi_scene_counter = multi_scene_counter + 1
        ENDIF
      ENDFOR
    ENDFOR
    
    IF (multi_scene_counter NE number_of_scenes) THEN BEGIN
      PRINT, 'did not find all of the scenes specified in the config file when looking through the Landsat file directory'
      PRINT, 'this config file: '
      PRINT, config_file_name
      PRINT, 'this Landsat file directory:'
      PRINT, etm_multi_scene_dir
      PRINT, '***EXITING!***'
      PRINTF, loglun, 'did not find all of the scenes specified in the config file when looking through the Landsat file directory'
      PRINTF, loglun, 'this config file: '
      PRINTF, loglun, config_file_name
      PRINTF, loglun, 'this Landsat file directory:'
      PRINTF, loglun, etm_multi_scene_dir
      PRINTF, loglun, '***EXITING!***'
      RETURN
    ENDIF
    
    n_files = number_of_scenes ; for backward compatibility with code that follows
       
    ; multi_scene_list = FILE_SEARCH(etm_multi_scene_dir + '/*', COUNT=n_files)
    
    PRINT, '--->using this scene directory '
    PRINT, etm_multi_scene_dir
    PRINTF, loglun, '--->using this scene directory '
    PRINTF, loglun, etm_multi_scene_dir
    PRINTF, loglun, ' '
    PRINTF, loglun, '--->preparing to read these files:'
    PRINTF, loglun, multi_scene_list
    PRINTF, loglun, ' '

    IF (n_files LE 0) THEN BEGIN
      PRINTF, loglun, '--->no files found in this directory '
      PRINTF, loglun, etm_multi_scene_dir
      PRINTF, loglun, '---->exiting!'
      PRINTF, loglun, ' '
      PRINT, '--->no files found in this directory '
      PRINT, etm_multi_scene_dir
      PRINT, '---->exiting!'
      CLOSE, /ALL
      ENVI_BATCH_EXIT
      RETURN
    ENDIF


; this procedure performs the following steps within the "BIG LOOP"
; STEP-1 opens a lndcal file and uses ENVI_OPEN_DATA_FILE to get a FID
; STEP-2 calls e_mine_lndcal_subsets_multi_2015a to get a list of colony locations
;        that fall within the lndcal file boundary, along with the dims of
;        those locations (in scene path,row coordinates)
; STEP-3 get projection info for the scene
; STEP-4 call e_mine_lndcal_subsets_multi_2015a and check that it ran successfully
;   then within a separate loop through all scene colony locations the following
;   steps are performed
;   STEP-5 ingest the T-dimension number of bands for each scene dims returned  from
;          e_mine_lndcal_subsets_multi_2015a. The T-dimensional array barr (with dimension
;          equal to dims) will hold the reflectance data
;   STEP-6 open the lndcal QA dataset with the same dimensions as barr
;   STEP-7 check that the reflectance and QA dataset dimensions are the same
;   STEP-8 calculate phi for barr, and repeat this loop for all colony locations
;
; ***********************START BIG LOOP HERE!*******************
; ***********************
; STEP-1 open the lndcal file and get a fid
; ***********************
total_colony_pixels = 0L
total_good_record_counter = 0L ; this counts the number of colony pixels actually written to the class summary file
                               ; so it is a counter of the "good" colony pixels found (i.e., no duplicates)
FOR i_case=0, n_files-1 DO BEGIN
  pix_count = 0L ; counts # of colony pixels  in a given file
  shore_reject_count = 0L ; # of pixels classified as colony 
  QA_reject_count = 0L ; number of pixels rejected because they were bogus data: part of a 
                       ; scene boundary, sturated pixels, or SLC-OFF missing data
  PRINT, ' '
  PRINT, ' '
  PRINTF, loglun, ' '
  PRINTF, loglun, ' '
  PRINT, 'starting file # ', STRTRIM(STRING(i_case+1),2), ' out of ', STRTRIM(STRING(n_files),2)
  
  ; create a temp filename in the same directory as the log file for the purpose of removing
  ; duplicate colony pixel records
  temp_file =  write_lat_lon_out_log + '/' + ID_prefix + 'write_latlon_temp_file_' + timedate + '.txt'
  
  ; create a temp filename in the same directory as the log file for the purpose of removing
  ; duplicate colony pixel records
  ; open this file for each lndcal image
  ; it is closed at the end of each iteration and it is removed at the end of this proc
  OPENW, templun, temp_file, /GET_LUN, ERROR=err
  IF err NE 0 THEN BEGIN
    PRINT, '--->a file error occurred when trying to create a temp file with this error ', err
    PRINT, '--->in this directory ', write_lat_lon_out_dir
    PRINT, STRMESSAGE(err)
    CLOSE, /ALL
    ENVI_BATCH_EXIT
    RETURN
  ENDIF
  ;PRINT, '--->generated this temp file: '
  ;PRINT, temp_file

;****************************************************************************
;******* handle L-8 (new) versus L-4/5/7 (old) filenaming exceptions ********
;****************************************************************************
sat_error = 0

; L8 and new versions of the other ESPA scenes have data files formatted like this
; with 1 file per reflective band:
;    LE70041122003047AGS00_toa_band1_hdf.img
;    LE70041122003047AGS00_toa_band2_hdf.img
;    LE70041122003047AGS00_toa_band3_hdf.img
;    LE70041122003047AGS00_toa_band4_hdf.img
;    LE70041122003047AGS00_toa_band5_hdf.img
;    LE70041122003047AGS00_toa_band7_hdf.img
;    LE70041122003047AGS00_toa_qa_hdf.img
;    LE70041122003047AGS00.hdf
;    LE70041122003047AGS00.hdf.hdr
;    LE70041122003047AGS00.xml
; Older versions are formatted like this with all 6 reflective bands in a single hdf file:
;    lndcal.LE70051122001032EDC00.hdf
;    lndcal.LE70051122001032EDC00.hdf.hdr
;    lndcal.LE70051122001032EDC00.txt
; the code below handles these exceptions (I hope)

hdf_file_name = FILE_SEARCH(multi_scene_list[i_case] + '/*.hdf', COUNT=n_hdf_files)
IF (n_hdf_files NE 1) THEN BEGIN
  PRINT, '***ERROR IN SEARCHING FOR HDF FILES***'
  PRINT, 'in this directory:
  PRINT, multi_scene_list[i_case]
  PRINT, 'expecting 1 hdf file, but found this many: ' + STRCOMPRESS(STRING(n_hdf_files), /REMOVE_ALL)
  PRINT, '*************EXITING!*****************'
  PRINTF, loglun, '***ERROR IN SEARCHING FOR HDF FILES***'
  PRINTF, loglun, 'in this directory:
  PRINTF, loglun, multi_scene_list[i_case]
  PRINTF, loglun, 'expecting 1 hdf file, but found this many: ' + STRCOMPRESS(STRING(n_hdf_files), /REMOVE_ALL)
  PRINTF, loglun, '*************EXITING!*****************'
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF

IF (STRPOS(hdf_file_name, 'lndcal.') GE 0) THEN BEGIN ; older version
  dataset_exception_ctr = 0
  lndcal_file_name = hdf_file_name
ENDIF ELSE BEGIN ; newer version
  dataset_exception_ctr = 0
  lndcal_file_name = hdf_file_name
ENDELSE

;****************************************************************************
;***************** handle L-8 versus L-4/5/7 exceptions *********************
;****************************************************************************
sat_error = 0
CASE 1 OF
  ; Landsat-8 case
  sat_check EQ 'L8': BEGIN
    ; dataset_exception_ctr is used in STEP-2 to index the FIDs. The L8 HDF_DATASET that we want
    ; starts at FID=1 (FID=1 is the blue band, adding 1 skips over the FID=0 water depth band)
    dataset_exception_ctr = 1
  END
  ; any other case
  (sat_check EQ 'L4') OR (sat_check EQ 'L5') OR (sat_check EQ 'L7') OR (sat_check EQ 'L45'): BEGIN
    ; dataset_exception_ctr is used in STEP-2 to index the FIDs. The L4/5/7 HDF_DATASET that we want
    ; starts at FID=0 (FID=0 is the blue band, there are not other bands that preceed it)
    dataset_exception_ctr = 0
  END
  ELSE: BEGIN
    PRINTF, loglun, '--->did not find a valid satellite ID'
    PRINTF, loglun, '--->this is the ID found ' + sat_check
    PRINTF, loglun, '--->***EXITING!***'
    PRINT,  '--->did not find avalid satellite ID'
    PRINT, '--->this is the ID found ' + sat_check
    PRINT, '--->***EXITING!***'
    CLOSE, /ALL
    RETURN
  ENDELSE
ENDCASE
;****************************************************************************
;***************end handle L-8 versus L-4/5/7 exceptions ********************
;****************************************************************************


;**********************BEGIN READ MASEK lndcal FILE***********************

PRINT, '--->working on TOA file ', lndcal_file_name
PRINTF, loglun, 'starting file # ' + STRTRIM(STRING(i_case+1),2), ' out of ' + STRTRIM(STRING(n_files),2)
PRINTF, loglun, '--->working on TOA file ', lndcal_file_name
PRINTF, loglun, '--->started working at ', SYSTIME(0)

; sequence is ENVI_OPEN_DATA_FILE -> ENVI_FILE_QUERY -> ENVI_GET_DATA
; NOTE that fid must be a variable, can't be a structure element (and maybe not an array?)

IF ((t_dim LT 4) OR (t_dim GT 6)) THEN BEGIN
  PRINT, '************************BUMMER!*************************;'
  PRINT, 'the code as written is expecting that 4 to 6 bands were used to'
  PRINT, 'generate the transition matrix ... but a t-matrix with a wrong'
  PRINT, 'dimension was found! We are quitting this run, a code change'
  PRINT, 'will need to be made to accomodate a t-matrix with more or'
  PRINT, 'fewer bands'
  PRINT, '*********************************************************'
  PRINT, 'this number of bands found ', t_dim
  PRINT, '*********************************************************'
  PRINTF, loglun, 'the t-matrix has this many coloums ', t_dim
  PRINTF, loglun, 'but 4-6 were expected!'
  PRINTF, loglun, 'EXITING!'
  GOTO, endit
ENDIF

lndcal_fid_array = LONARR(6) ; band 1,2,3,4,5,6 (=TM band7)

;*************************
; STEP-2 get the dims of the lndcal file (NEEDED?)
; ************************
PRINTF, loglun, '--->STARTING STEP 2'
CLOSE, loglun     ; close the file to force the buffer to write all the contents
FREE_LUN, loglun  ; free up the logical unit
; re-open the log file and /APPEND log info at then end of the file
PRINT, '--->closing and re-opening log file'
OPENW, loglun, log_file, /GET_LUN, ERROR=err, /APPEND
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to create a log file'
  PRINT, '--->in this directory ', write_lat_lon_out_dir
  PRINT, STRMESSAGE(err)
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF

; dataset_exception_ctr is set in the section that handles Landsat-8 exception,
; see also notes written above on October 22, 2015
FOR ip=0,5 DO BEGIN ; get the FIDs for the 6 ETM+ band images for the lndcal file
  ENVI_OPEN_DATA_FILE, lndcal_file_name, /HDF_SD, HDFSD_DATASET=ip+dataset_exception_ctr, R_FID=fid
  IF (fid EQ -1) THEN BEGIN
    PRINT, '--->ENVI_OPEN_FILE failed on ', lndcal_file_name, ' file-ID # , fid
    PRINTF, loglun, '--->ENVI_OPEN_FILE failed on ', lndcal_file_name, ' file-ID # , fid
    CLOSE, templun
    FREE_LUN, templun
    CONTINUE ; break from this iteration
    RETURN
  ENDIF
  lndcal_fid_array[ip] = fid ; can't pass an array to ENVI_OPEN_DATA_FILE!
ENDFOR

my_fid = lndcal_fid_array[0]
ENVI_FILE_QUERY, my_fid, DIMS=dims, DATA_TYPE=input_data_type ; find out the image scene dimensions

;***************************
; STEP-3 get the projection information for the lndcal scene
;***************************

PRINTF, loglun, '--->STARTING STEP 3'
CLOSE, loglun     ; close the file to force the buffer to write all the contents
FREE_LUN, loglun  ; free up the logical unit
; re-open the log file and /APPEND log info at then end of the file
PRINT, '--->closing and re-opening log file'
OPENW, loglun, log_file, /GET_LUN, ERROR=err, /APPEND
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to create a log file'
  PRINT, '--->in this directory ', write_lat_lon_out_dir
  PRINT, STRMESSAGE(err)
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF


; strangely, I can't get ENVI to append geolocation information to the hdf files
; when I use the sequence ENVI_OPEN_DATA_FILE / ENVI_FILE_QUERY / ENVI_GET_DATA
; so I need to use the messy sequence below:
; 1) open the .hdr file, 2) extract map and projection info using the proc
; r_calc_hdf_map_projection 3) create a geocoded image, in this case we use
; the QA_array and attach the map & projection info to it using ENVI_ENTER_DATA

; there is an ENVI header file in the same directory as the sr and cal files, it has the
; same filename as the sr and cal files, but with .hdr appended
; this file contains the projection info for the image
envi_hdr_file_name = lndcal_file_name + '.hdr'

; this proc parses the .hdr file and returns projection & map info
found_error = 0
g_calc_hdf_map_projection_2016a, found_error, envi_hdr_file_name, lndcal_proj, my_map_info

IF found_error EQ 1 THEN BEGIN
  PRINT, '--->ERROR FOUND WHILE TRYING TO EXTRACT SCENE MAP & PROJECTION INFO FOR THIS FILE'
  PRINT, lndcal_file_name
  PRINT, '--->SKIPPING THIS ITERATION!'
  PRINTF, loglun, '--->ERROR FOUND WHILE TRYING TO EXTRACT SCENE MAP & PROJECTION INFO FOR THIS FILE'
  PRINTF, loglun, lndcal_file_name
  PRINTF, loglun, '--->SKIPPING THIS ITERATION!'
  CLOSE, templun
  FREE_LUN, templun
CONTINUE
ENDIF

; attach the map & projection information to an dummy 2D image, we will
; use the FID of this image below in ENVI_CONVERT_FILE_COORDINATES to calculate
; ploar stereographic coordinates from the row,col values of the image

; buid a dummy array with the scene dimensions taken from the ENVI_FILE_QUERY just prior to STEP-3
dummy_array = BYTARR(dims[2] + 1, dims[4] + 1)

; create the dummy image, as noted above lndcal_geocoded_fid is used below
ENVI_ENTER_DATA, dummy_array, BNAMES='geocoded_QA_band', MAP_INFO=my_map_info, $
  R_FID=lndcal_geocoded_fid
  
;***************************
; STEP-4 call f_mine_lndcal_subsets_multi_2016a
;***************************

PRINTF, loglun, '--->STARTING STEP 4'
CLOSE, loglun     ; close the file to force the buffer to write all the contents
FREE_LUN, loglun  ; free up the logical unit
; re-open the log file and /APPEND log info at then end of the file
PRINT, '--->closing and re-opening log file'
OPENW, loglun, log_file, /GET_LUN, ERROR=err, /APPEND
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to create a log file'
  PRINT, '--->in this directory ', write_lat_lon_out_dir
  PRINT, STRMESSAGE(err)
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF

; adelie_latlon_filename =  filename with the lat,lon coordinates of each known Adelie colony,
;  these location data are from Schwaller et al and from Lynch and LaRue
; colony_in_counter = the number of colonies found within the scene
; adelie_sample_line_array = DBLARR(2, number of colony locations within the scene)
;  the 1st & 2nd elements in each rwo = sample,line of each colony location

; June 25 this version now ingests the colony name from the PLOS-1 paper Apendices B & C
; this version now passes the array adelie_name_line_array which holds the colony names
; corresponding to the adelie_sample_line_array list of lat,lon coordinates associated
; line-by-line with the a colony name

is_error = 0 ; e_mine_lndcal_subsets_multi_2015a will return is_error=1 if something goes wrong
; call e_mine_lndcal_subsets which returns
; colony_in_counter = the number of colonies in the scene lndcal_file_name
; adelie_sample_line_array = a [2, colony_in_counter] array with the scene
; sample,line coordinates corresponding to the lat,lon coordinates of the Lynch/LaRue and/or
; Schwaller Adelie colonies present within the scene
; adelie_name_line_array = the colony name associated with each adelie_sample_line_array record
g_mine_lndcal_subsets_multi_2016a, lndcal_file_name, loglun, is_error, adelie_latlon_filename, $
    colony_in_counter, adelie_sample_line_array, adelie_name_line_array

IF (is_error NE 0) THEN BEGIN
  PRINT, '*** ERROR RETURNED BY SUBROUTINE e_mine_lndcal_subsets_multi_2015a ***'
  PRINT, '*****************************EXITING****************************'
  RETURN
ENDIF

;****************************
; now iterate over all colony_in_counter colonies within a given lndcal scene
; BUT if colony_in_counter=0 then skip out of this big loop iteration, go back
; to the top of the big loop and pick a new scene
;****************************

;****************POSSIBLE SKIP OUT POINT HERE***********************
IF (colony_in_counter EQ 0) THEN BEGIN
  PRINT, 'no colonies found in this scene, skipping to the next scene'
  ; but first close some files...
  ;PRINTF, loglun, '--->closing ENVI files'
  ;PRINT, '--->closing ENVI files'
  FOR ik=0,T_dim-1 DO BEGIN
    a_fid = lndcal_fid_array[band_numbers_array[ik]-1]
    ENVI_FILE_MNG, ID=a_fid, /REMOVE ; remove the files from ENVI
  END
  ; haven't openend the QA fid yet!
  ; ENVI_FILE_MNG, ID=QA_fid, /REMOVE ; remove the QA DATA file from ENVI
  ENVI_FILE_MNG, ID=lndcal_geocoded_fid, /REMOVE ; remove the GEOCODED IMAGE that we created from ENVI
  CLOSE, templun
  FREE_LUN, templun
  CONTINUE
ENDIF
;*******************************************************************

subset_dims = LONARR(5) ; dimensions of the image subset
; colony_in_counter = number of colonies in the scene, see explanation above
FOR sub_loop_ctr=0,colony_in_counter-1 DO BEGIN 
  
  PRINT, 'working on colony subset number ' + STRTRIM(STRING(sub_loop_ctr+1),2), ' out of ' + STRTRIM(STRING(colony_in_counter),2)
  PRINTF, loglun, 'working on colony subset number ' + STRTRIM(STRING(sub_loop_ctr+1),2), ' out of ', STRTRIM(STRING(colony_in_counter),2)
  
;****************************
; STEP-5 injest T-dim bands of lndcal reflectance data into the array barr
;****************************

PRINTF, loglun, '--->STARTING STEP 5'
CLOSE, loglun     ; close the file to force the buffer to write all the contents
FREE_LUN, loglun  ; free up the logical unit
; re-open the log file and /APPEND log info at then end of the file
PRINT, '--->closing and re-opening log file'
OPENW, loglun, log_file, /GET_LUN, ERROR=err, /APPEND
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to create a log file'
  PRINT, '--->in this directory ', write_lat_lon_out_dir
  PRINT, STRMESSAGE(err)
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF

  colony_samp_line = adelie_sample_line_array[0:1,sub_loop_ctr]
  
  subset_dims[0] = -1L ; ENVI_GET_DATA documentation says to set this to -1L
  subset_dims[1] = colony_samp_line[0] - 100L ; min_samp, starting sample
  subset_dims[2] = colony_samp_line[0] + 100L ; max_samp, ending sample
  subset_dims[3] = colony_samp_line[1] - 100L ; min_line, starting line
  subset_dims[4] = colony_samp_line[1] + 100L ; max_line, ending line
  
  ; don't go past the borders!
  IF subset_dims[1] LT dims[1] THEN subset_dims[1] = dims[1] ; dims[1] is starting sample
  IF subset_dims[2] GT dims[2] THEN subset_dims[2] = dims[2] ; dims[2] is ending sample
  IF subset_dims[3] LT dims[3] THEN subset_dims[3] = dims[3] ; dims[3] is starting line
  IF subset_dims[4] GT dims[4] THEN subset_dims[4] = dims[4] ; dims[4] is ending line
  
  xsize = subset_dims[2] - subset_dims[1] + 1L
  ysize = subset_dims[4] - subset_dims[3] + 1L
 ; rook_image = BYTARR(xsize, ysize, 3) ; used to generate the rookery thumbnail

  ; build a byte array to hold the image data
  ; barr the b(and)-array stores the lndcal image data
  ; note that if you are working with 5 bands T will have a dimension of 5x5
  ; so you  need to subtract 1 from the n_elements(T[0,*]) to get the right # of phi-bands
  ; the LEDAPS TOA files are integer, we save them here as double precision floating point
  barr = MAKE_ARRAY(DIMENSION=[xsize,ysize,T_dim], TYPE=5)
  ; phi has 1 less dimension than barr, it is type double precision floating point
  phi  = MAKE_ARRAY(DIMENSION=[xsize,ysize,T_dim-1], TYPE=5)
  
  FOR iq=0,T_dim-1 DO BEGIN ; ingest the T_dim # of bands of lndcal TOA ETM+ data
    ; ingest the bands specified in band_numbers_array
    ; we subtract 1 from the band_numbers_array because Band1 = position-0
    my_fid = lndcal_fid_array[band_numbers_array[iq]-1]
    barr[*,*,iq] = ENVI_GET_DATA(DIMS=subset_dims, FID=my_fid, POS=0)
  ENDFOR

; do STEP-6 and STEP-7 for L4,5,7 ONLY!
;IF (sat_check EQ 'L4') OR (sat_check EQ 'L5') OR (sat_check EQ 'L7') OR (sat_check EQ 'L45') THEN BEGIN

;***************************
; STEP-6 open the lndcal QA dataset with the same dimensions as barr
;***************************

  ; no need to calculate phi if the QA byte > 0B in which case the data is either
  ; fill surronding the scene or it contains saturated data.
  ; get the QA data from lndcal_file_name
  ; the QA mask is DATASET #7 (POSITION 6 when starting from 0)
  ; sequence is ENVI_OPEN_DATA_FILE -> ENVI_FILE_QUERY -> ENVI_GET_DATA
;  ENVI_OPEN_DATA_FILE, lndcal_file_name, /HDF_SD, HDFSD_DATASET=6, R_FID=QA_fid
  
  ;PRINT, '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>OPENING QA_fid ', QA_fid
;  IF (QA_fid LT 0) THEN BEGIN
;    PRINT, '--->could not find the QA data in file: '
;    PRINT, lndcal_file_name
;    PRINT, 'Skipping this iteration!'
;    PRINTF, loglun, '--->could not find the QA data in file: '
;    PRINTF, loglun, lndcal_file_name
;    PRINTF, loglun, 'Skipping this iteration!'
    ; close files!
;    PRINTF, loglun, '--->closing ENVI files'
;    CONTINUE
;  ENDIF
  
;  ENVI_FILE_QUERY, QA_fid, DIMS=QA_dims ; find out the scene dimensions (QA_dims)

;***************************
; STEP-7 check to be sure that the scene dimensions of the QA and the lndcal file match
;***************************

 ; IF (dims[2] NE QA_dims[2]) OR (dims[4] NE QA_dims[4]) THEN BEGIN
 ;   PRINT, '--->dimensions of QA file and LEDAPS TOA file do not match! '
 ;   PRINT, '--->QA row,col = ', QA_dims[2], QA_dims[4]
 ;   PRINT, '--->LDPS ETM+ image row,col = ', dims[2], dims[4]
 ;   PRINT, '--->QA filename: '
 ;   PRINT, lndcal_file_name
 ;   PRINT, '--->LEDAPS TOA ETM+ filename: '
 ;   PRINT, lndcal_file_name
 ;   PRINT, 'EXITING!'
 ;   PRINTF, loglun, '--->dimensions of QA file and LEDAPS TOA file do not match! '
 ;   PRINTF, loglun, '--->QA row,col = ', QA_dims[2], QA_dims[4]
 ;   PRINTF, loglun, '--->LDPS ETM+ image row,col = ', dims[2], dims[4]
 ;   PRINTF, loglun, '--->QA in filename: '
 ;   PRINTF, loglun, lndcal_file_name
 ;   PRINTF, loglun, '--->LEDAPS TOA ETM+ filename: '
 ;   PRINTF, loglun, lndcal_file_name
 ;   CLOSE, /ALL
 ;   ENVI_BATCH_EXIT
 ;   RETURN
 ; ENDIF
  
 ; QA_array = MAKE_ARRAY(DIMENSION=[xsize,ysize], TYPE=1) ; build the BYTE array
 ; QA_array[*,*] = ENVI_GET_DATA(DIMS=subset_dims, FID=QA_fid, POS=0)
    
  ; **************finished reading the QA file
  
;ENDIF ; only do STEP-6 and STEP-7 for L4, L5 or L7

; create a dummy QA array when the satellite is Landsat-8
; because L8 does not have a native QA array
;IF (sat_check EQ 'L8') THEN BEGIN
  QA_array = MAKE_ARRAY(DIMENSION=[xsize,ysize], TYPE=1) ; build the BYTE array
  QA_array[*,*] = 0B
;ENDIF

; March 30, 2016 because of the complications in file naming conventions and
; mixed-type files we are skipping all of the QA checking
  
;***************************
; STEP-8 calculate phi for barr
;***************************

PRINTF, loglun, '--->STARTING STEP 8 (CALCULATING PHI)'
CLOSE, loglun     ; close the file to force the buffer to write all the contents
FREE_LUN, loglun  ; free up the logical unit
; re-open the log file and /APPEND log info at then end of the file
PRINT, '--->closing and re-opening log file'
OPENW, loglun, log_file, /GET_LUN, ERROR=err, /APPEND
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to create a log file'
  PRINT, '--->in this directory ', write_lat_lon_out_dir
  PRINT, STRMESSAGE(err)
  CLOSE, /ALL
  ENVI_BATCH_EXIT
  RETURN
ENDIF

  ;PRINT, '--->calculating phi'

  ; turn any zeros or pixels<0.0 into value 0.0001
  ; so that you don't get divide by 0
  ;PRINT, '--->turning all 0s into 0.0001s in image to avoid divide by 0 '
  barr[WHERE(barr LE 0.0, /NULL)] = 0.0001D

  ;PRINT, '--->starting to calculate phi'
  ; phi has dimensions row,col,T_dim-1 so for 4 bands phi has a z dimension of 0,1,2
  ; phi is calculated from the training data in the routine w_parse_roi_and_transform which
  ; itself is a sub-routine of ap_calc_eigen
  ; band values assigned by w_parse_roi_and_transform are of the form (for a 4-band set of training data):
  ; trans_ROI_array[0,num_events] = ATAN(b4/b3)                       ;PHI-1 = phi[*,*,0]
  ; trans_ROI_array[1,num_events] = ATAN(SQRT(b4^2 + b3^2)/b2)        ;PHI-2 = phi[*,*,1]
  ; trans_ROI_array[2,num_events] = ATAN(SQRT(b4^2 + b3^2 + b2^2)/b1) ;PHI-3 = phi[*,*,2]
  ; NOTE: we don't bother calculating RHO
  ; WE NEED TO CALCULATE PHIs IN THE SAME ORDER BELOW AS THEY WERE CALCULATED IN CALC_EIGEN
  ; THIS IS THE **EXACT*OPPOSITE** OF THE ORDER OF EQUATIONS 2,3 & 4 IN THE SCHWALLER ET AL PAPER
  ; Sorry about that! But it is the convention used by calc_eigen and it is too late to change it now!
  ; note that if there are T_dim bands there are T_dim-1 PHIs

  blast = T_dim-1 ; index of the last reflectance band, given that the index of the 1st band = 0
  phi[*,*,0] = ATAN(barr[*,*,blast]/barr[*,*,blast-1]) ; = PHI-3 in a 4-band set
  ;PRINT, 'computed phi ', blast
  ssqs = DBLARR(xsize,ysize) ; the sum of squares of each reflective band
  ssqs[*,*] = barr[*,*,blast]^2
  FOR ic=1,blast-1 DO BEGIN
    ssqs[*,*]   = ssqs[*,*] + barr[*,*,blast-ic]^2
    phi[*,*,ic] = ATAN(SQRT(ssqs[*,*])/barr[*,*,blast-ic-1])
  ;  PRINT, 'computed phi ', blast-ic
  ENDFOR
  ssqs = 0 ; don't carry this big array around
  
  ; ******************************do the nudging*************************************
  ; done calculating phi, now nudge the spherical coordinates according to the nudge array
  ; we're going to start by assuming that phi is based on a 6-band calc_eigen result
  IF (N_ELEMENTS(nudge_array[*,0]) NE 6) THEN BEGIN
    PRINT, '--->right now we are hard-wired to expect a nudge_array with 6 columns '
    PRINT, '--->only found this many ', N_ELEMENTS(nudge_array[*,0])
    PRINT, '--->EXITING!'
    ENVI_BATCH_EXIT
    CLOSE, /ALL
    RETURN
  ENDIF
  
  ; which set of nudge factors should we apply?
  nudge_vector = DBLARR(5)
  CASE 1 OF
    ; **** CASE-1: LT4 ****
    STRPOS(sat_check, '4'): BEGIN
      IF (FIX(nudge_array[0,0]) EQ 4) THEN BEGIN
        FOR inud=0,4 DO nudge_vector[inud] = nudge_array[inud+1,0]
      ENDIF ELSE BEGIN
        PRINT, '--->FOUND SOMETHING WRONG IN NUDGE_ARRAY! EXITING!'
        PRINTF, loglun, '--->FOUND SOMETHING WRONG IN NUDGE_ARRAY! EXITING!'
        ENVI_BATCH_EXIT
        CLOSE, /ALL
        RETURN
      ENDELSE
    END
      ; **** CASE-2: LT5 ****
    STRPOS(sat_check, '5'): BEGIN
        IF (FIX(nudge_array[0,1]) EQ 5) THEN BEGIN
          FOR inud=0,4 DO nudge_vector[inud] = nudge_array[inud+1,1]
        ENDIF ELSE BEGIN
          PRINT, '--->FOUND SOMETHING WRONG IN NUDGE_ARRAY! EXITING!'
          PRINTF, loglun, '--->FOUND SOMETHING WRONG IN NUDGE_ARRAY! EXITING!'
          ENVI_BATCH_EXIT
          CLOSE, /ALL
          RETURN
        ENDELSE
      END
        ; **** CASE-3: LT7 ****
    STRPOS(sat_check, '7'): BEGIN
          IF (FIX(nudge_array[0,2]) EQ 7) THEN BEGIN
            FOR inud=0,4 DO nudge_vector[inud] = 0.0
          ENDIF ELSE BEGIN
            PRINT, '--->FOUND SOMETHING WRONG IN NUDGE_ARRAY! EXITING!'
            PRINTF, loglun, '--->FOUND SOMETHING WRONG IN NUDGE_ARRAY! EXITING!'
            ENVI_BATCH_EXIT
            CLOSE, /ALL
            RETURN
          ENDELSE
        END
        ; **** CASE-4: LT8 ****
    STRPOS(sat_check, '8'): BEGIN
          IF (FIX(nudge_array[0,3]) EQ 8) THEN BEGIN
            FOR inud=0,4 DO nudge_vector[inud] = 0.0
          ENDIF ELSE BEGIN
            PRINT, '--->FOUND SOMETHING WRONG IN NUDGE_ARRAY! EXITING!'
            PRINTF, loglun, '--->FOUND SOMETHING WRONG IN NUDGE_ARRAY! EXITING!'
            ENVI_BATCH_EXIT
            CLOSE, /ALL
            RETURN
          ENDELSE
        END
    ELSE: BEGIN
      PRINT, '--->FOUND SOMETHING WRONG IN NUDGE_ARRAY! EXITING!'
      PRINTF, loglun, '--->FOUND SOMETHING WRONG IN NUDGE_ARRAY! EXITING!'
      ENVI_BATCH_EXIT
      CLOSE, /ALL
      RETURN
    END
  ENDCASE
  
  ; finally, nudge the array of spherical coordinates
  FOR ic=0,4 DO phi[*,*,ic] = phi[*,*,ic] - nudge_vector[ic]
 
  ; ******************************finished nudging*************************************



  ; determine the source projection of the image file
  ; this is the same structure as my_proj, it could be used here instead of
  ; calculating it from ENVI_GET_PROJECTION
  s_proj = ENVI_GET_PROJECTION(FID=lndcal_geocoded_fid)
  ; generate a handle for the target projection: geographic lat/lon
  t_proj = ENVI_PROJ_CREATE(/GEOGRAPHIC)

  pixel_array = DBLARR(T_dim)    ; pixel array has the same dimension as T
  pixel_array[T_dim-1] = 1.0D    ; make the last element = 1.0

  ; DIMS[0]: A pointer to an open ROI. Otherwise, set to -1L.
  ; DIMS[1]: The starting sample number. The first x pixel is 0.
  ; DIMS[2]: The ending sample number
  ; DIMS[3]: The starting line number. The first y pixel is 0.
  ; DIMS[4]: The ending line number
  i_start = subset_dims[1] ; starting column of the image subset covering a given colony
  j_start = subset_dims[3] ; starting line of the image subset covering a given colony
  i_end   = subset_dims[2] ; ending column of the image subset covering a given colony
  j_end   = subset_dims[4] ; ending line of the image subset covering a given colony 
                 
  scene_pix_total = (i_end - i_start + 1L) * (j_end - j_start + 1L)
  continent_pix_total = continent_pix_total + scene_pix_total

  ;PRINT, '   colony dimensions are col/row ', (i_end - i_start + 1L), (j_end - j_start + 1L)

  maxr = 0.0
  minr = 1000000.0
  
  ; what is the number of Landsat bands stored in barr?
  ; we will use this to check to see if a pixel falls into the fill area
  num_barr_bands = N_ELEMENTS(barr[0,0,*])

; we use in_image_data below to skip over fill data

  i_cnt = LONG(i_end) - LONG(i_start)
  j_cnt = LONG(j_end) - LONG(j_start)
  FOR i=0L,i_cnt DO BEGIN ; DO ALL ROWS OF THE IMAGE SUBSET
     FOR j=0L,j_cnt DO BEGIN ; DO ALL COLS OF THE IMAGE SUBSET
     ; QA array is set to 0 for Landsat-8
     IF (QA_array[i,j] GT 0B) THEN CONTINUE ; skip the fill pixels and saturated pixels
     ; check individual pixels to see if they are image fill in the L8 case
     FOR i_fill=0,num_barr_bands-1 DO IF (barr[i, j, i_fill] GT 0.01) THEN in_image_data = 1 ELSE in_image_data = 0
     IF (in_image_data EQ 0) THEN CONTINUE ; we're in the fill zone for L8, so skip this iteration
     pixel_array[0:T_dim-2] = phi[i,j,0:T_dim-2] 
     pixel_array[T_dim-1]   = 1.0D    
     result = TRANSPOSE(pixel_array) # T_invert
     r_dist = TRANSPOSE(result[0:T_dim-2]) # result[0:T_dim-2] ; calc sum of sqrs of all bands
     r_dist = SQRT(r_dist)
     IF (r_dist GT maxr) THEN maxr = r_dist
     IF (r_dist LT minr) THEN minr = r_dist
      
     IF (r_dist LE 1.0) THEN BEGIN ; a rookery pixel was found!
       ; now calculate the polar stereographic coordinates and lat/lon of the pixel
       ; *** THIS IS WHERE lndcal_geocoded_fid IS USED!
       scene_col = i + i_start
       scene_row = j + j_start
       ENVI_CONVERT_FILE_COORDINATES, lndcal_geocoded_fid, scene_col, scene_row, x_polarstereo, y_polarstereo, /to_map
       ; then convert those p-s coordinates to geographic lat/lon
       ENVI_CONVERT_PROJECTION_COORDINATES, x_polarstereo, y_polarstereo, s_proj, $
                                          x_latlon, y_latlon, t_proj                                                                                
       pix_count = pix_count + 1L   ; how many pixels did we find in this scene?
       ; PRINT filename, location, r-value, sample & line info to the file
       scene_id = STRSPLIT(lndcal_file_name, '/', /EXTRACT, COUNT=n_parts) ; scene_id is now an array
       ; the last element of the array is the scene id
       out_string = lndcal_file_name + '|' + scene_id[n_parts-1] + '|'  ; n_parts-1 is the original image fileID
       out_string = out_string + STRING(r_dist) + '|' + STRING(x_polarstereo) + '|' + STRING(y_polarstereo)
       out_string = out_string + '|' + STRING(x_latlon) + '|' + STRING(y_latlon)
       out_string = out_string + '|' + STRING(scene_col) + '|' + STRING(scene_row) ; add sample & line info
       out_string = out_string + '|' + adelie_name_line_array[sub_loop_ctr]
       out_string = out_string + '|' + t_matrix_struct.band_names_array[ie]
       out_string = STRCOMPRESS(out_string, /remove_all)
       ; PRINT, out_string
       PRINTF, templun, out_string ; print the colony/pixel string to a temp file
     ENDIF  ; check for r_dist LT 1
     ENDFOR ; j_start to j_end (all columns of the image subset)
   ENDFOR ; i_start to i_end (all rows of the image subset)

;*********************************************************************
; STEP-9 print off a png thumbnail for visual inspection of cloudiness
;*********************************************************************
; landcal_file_name = full pathname of the landsat scene
;e_make_accounting_png_thumbnails_multi_2015a, lndcal_file_name, subset_dims, colony_samp_line, $
;                                          adelie_name_line_array[sub_loop_ctr], is_error, master_list_struct, $
;                                          cloud_calc_thumbnail_directory
;
;IF (is_error) NE 0 THEN BEGIN
;  PRINT, '********ERROR OCCURRED IN ACCOUNTING_PNG_THUMBNAILS*******'
;  PRINT, '************************EXITING!**************************'
;ENDIF

; No longer doing the QA FID (April 18, 2016)
; get rid of the QA_fid ONLY if STEP-6 and STEP-7 were done for Landsat-4, 5, or 7 images
;IF (sat_check EQ 'L4') OR (sat_check EQ 'L5') OR (sat_check EQ '7') OR (sat_check EQ 'L45') THEN $
;    ENVI_FILE_MNG, ID=QA_fid, /REMOVE ; remove the QA DATA file from ENVI
    ; this QA file is generated for every image subset (each colony in the image)

 ENDFOR ; sub_loop_ctr=0,colony_in_counter-1 loop (loop over all colonies in a given scene)
 ;*********DONE ITERATING OVER ALL colony_in_counter COLONIES********
 ;*************FOR A GIVEN lndcal SCENE AT THIS ENDFOR***************
 ;*******************************************************************
 
  ; close templun for writing to empty the buffer and write all the records
  ; to the file becasue we are going to read from it in e_checkdupes
  CLOSE, templun
  FREE_LUN, templun
    
  ; CHECK FOR DUPES on a scene-by-scene basis once the summary pixels records
  ; for all the colonies in a given scene have been geerated (may be 0 records in a given scene)
  ; CHECK FOR DUPES in the temp_file list of colony/pixel summary records
  ; class_array is a string-array that contains the list of colony/pixel summary records
  ; for a given scene the temp_file and log_file filenames are passed to it
  ; class_array (dupe free array of class_summary records) and num_records 
  ; the number of records in class_array) are passed back to the calling routine
  ; e_checkdupes re-opens and re-closes the temp file
  found_error = 0
  g_checkdupes_multi_2016a, temp_file, loglun, class_array, num_records, found_error, good_record_counter
  total_good_record_counter = total_good_record_counter + good_record_counter
  
  IF (found_error EQ 1) THEN BEGIN
    PRINT, '--->s_write_latlon *** error found in e_checkdupes '
    PRINT, '--->s_write_latlon *** quitting '
    CLOSE, /ALL
    RETURN
  ENDIF
  
  ; close the tempfile and release the LUN, the temp file will be re-opened
  ; (cleared of all previous records) when the big loop starts the next iteration
  CLOSE, templun     ; close the file to force the buffer to write all the contents
  FREE_LUN, templun  ; free up the logical unit
  
  ; write the class_array records to the outlun file
  ; num_records = number of colony/pixel records for a given scene (may be multiple 
  ; colonies). There are num_records stored in the temp file (may be 0)
  dupe_count = 0L
  IF (num_records GT 0) THEN BEGIN
    FOR ir=0UL, num_records-1 DO BEGIN
      ; don't print if there is a ****dupe_record**** 
      ; if no *** is found STRPOS = -1
      IF (STRPOS(class_array[ir],'***') GE 0) THEN dupe_count = dupe_count + 1L $
         ; but do print otherwise
         ELSE PRINTF, outlun, class_array[ir]
    ENDFOR
  ENDIF
  ; but don't close outlun because we will continue to write to it throughout the big loop

  PRINT, '--->FOUND this many (duplicate free) penguin colony pixels ' + STRTRIM(STRING(num_records),1)
  PRINTF, loglun, '--->found this many (duplicate free) penguin colony pixels ' + STRTRIM(STRING(num_records),1)
  PRINT, '--->FOUND this many DUPLICATE penguin colony pixels ', STRTRIM(STRING(dupe_count),1)
  PRINTF, loglun, '--->FOUND this many DUPLICATE penguin colony pixels ', STRTRIM(STRING(dupe_count),1)
  total_colony_pixels = total_colony_pixels + num_records

  ;PRINTF, loglun, '--->closing ENVI files'
  ;PRINT, '--->closing ENVI files'
  FOR ik=0,T_dim-1 DO BEGIN
    a_fid = lndcal_fid_array[band_numbers_array[ik]-1]
    ENVI_FILE_MNG, ID=a_fid, /REMOVE ; remove the files from ENVI
  END

  ENVI_FILE_MNG, ID=lndcal_geocoded_fid, /REMOVE ; remove the GEOCODED IMAGE that we created from ENVI
  ; NOTE: the QA file is closed out in the scene subset loop because the QA dataset gets created
  ; for every thumbnail subset

  PRINTF, loglun, '--->finished working this iteration at ', SYSTIME(0)
  PRINTF, loglun, ' ' ; add a bit of white space before the next log entry

  CLOSE, loglun     ; close the file to force the buffer to write all the contents
  FREE_LUN, loglun  ; free up the logical unit
  ; re-open the log file and /APPEND log info at then end of the file
  PRINT, '--->closing and re-opening log file'
  OPENW, loglun, log_file, /GET_LUN, ERROR=err, /APPEND
  IF err NE 0 THEN BEGIN
    PRINT, '--->a file error occurred when trying to create a log file'
    PRINT, '--->in this directory ', write_lat_lon_out_dir
    PRINT, STRMESSAGE(err)
    CLOSE, /ALL
    ENVI_BATCH_EXIT
    RETURN
  ENDIF
  
 ; IF (i_case GE 130) THEN BEGIN
 ;   PRINT, '<*><*><*>skipping iteration ', i_case
 ;   CONTINUE
 ; ENDIF
  
ENDFOR ; end of the i_case=0,n_files-1 loop (loop over all scenes)

; ****************************BIG LOOP ENDS HERE********************************



; close the outlun file that contains the colony summary_file records
CLOSE, outlun
FREE_LUN, outlun

;delete the temp file after all of the scenes have been evaluated in the big loop
SPAWN, 'rm -r ' + temp_file

PRINTF, loglun, ' '
PRINTF, loglun, ' '
PRINTF, loglun, '--->FINISHED COLONY SEARCH OVER ALL INPUT IMAGES!'
PRINTF, loglun, '--->total number of pixels processed: ', STRTRIM(STRING(continent_pix_total))
PRINTF, loglun, '--->total number of colony pixels found: ', STRTRIM(STRING(total_colony_pixels))
PRINTF, loglun, '--->total number of non-duplicate colony pixels written to class summary: ', STRTRIM(STRING(total_good_record_counter))

;PRINT, '--->re-writing config file, including filename of classification file'
;PRINTF, loglun, '--->re-writing config file, including filename of classification file'

; ok, lets create a new config file with the same timedate tag as the classification file
; generate the output file
;config_out_dir =  STRMID(config_file_name,0,STRPOS(config_file_name, '/', /reverse_search))
;new_config_file = config_out_dir + '/' + new_config_file_name + timedate + '.txt'
;PRINT, '--->creating this file for capturing the revised config data'
;PRINT, new_config_file
;PRINTF, loglun, '--->creating this file for capturing the revised config data'
;PRINTF, loglun, new_config_file
;OPENW, config_lun, new_config_file, /GET_LUN, ERROR=err
;IF err NE 0 THEN BEGIN
;  PRINT, '--->a file error occurred trying to open the revised config file ', err, STRMESSAGE(err)
;  PRINTF, loglun, '--->a file error occurred trying to open the revised config file ', err, STRMESSAGE(err)
;  CLOSE, /ALL
;  ENVI_BATCH_EXIT
;  RETURN
;ENDIF

; out_file is the variable that holds the class_summary filename
; class_filename is also a variable that holds the class_sumary filename 
;new_config_outfile_name = '   <find_rooks_input_file_name>' + out_file + '</find_rooks_input_file_name>'

;PRINTF, loglun, ' '
; loop through the config file (a copy is stored in the string array config_text) 
;lmax = N_ELEMENTS(config_text)
;FOR il=0,line_number-1 DO BEGIN ; line_number is the number of lines in the config file
;  IF STRPOS(config_text[il], '<find_rooks_input_file_name>') GE 0 THEN $
;     config_text[il] = new_config_outfile_name
;  PRINTF, config_lun, config_text[il]
;ENDFOR
;CLOSE, config_lun
;FREE_LUN, config_lun

; this section commented out 12/1/2015
; this cloud-calc stuff is now done in e_check_4_letter_code_status_v6
; print the total list of known Adelie colonies, as identify whether they were
; actually covered by any Landsat scenes during this analysis
;FOR ick=0L,num_adelie_lines-1L DO BEGIN ; loop over the master_list_struct
;    a_name  = master_list_struct.colony_name[0,ick]
;    a_lat   = STRING(master_list_struct.colony_lat_lon[0,ick])
;    a_lon   = STRING(master_list_struct.colony_lat_lon[1,ick])
;    IF(master_list_struct.found_in_scene[0,ick] EQ 0B) THEN a_found='0' ELSE a_found='1'
;    a_line = a_name + '|' + a_lat + '|' + a_lon + '|' + a_found
;    ;PRINT, STRCOMPRESS(a_line, /REMOVE_ALL)
;    PRINTF, loglun, STRCOMPRESS(a_line, /REMOVE_ALL)
;ENDFOR


; how long did this whole thing take?
end_timedate =  SYSTIME(0)
end_timedate = STRMID(end_timedate,4,STRLEN(end_timedate)) 
STRPUT, end_timedate, 'h', 6                       
STRPUT, end_timedate, 'm', 9
STRPUT, end_timedate, 's', 12
STRPUT, end_timedate, 'y', 15
end_timedate = STRCOMPRESS(end_timedate, /REMOVE_ALL)  ; remove all blanks

PRINT, '---> start timedate ', timedate
PRINT, '---> end timedate ', end_timedate
PRINT, '--->total number of colony pixels found: ', STRTRIM(STRING(total_colony_pixels))

PRINTF, loglun, ' '
PRINTF, loglun, '---> start timedate ', timedate
PRINTF, loglun, '--->   end timedate ', end_timedate
PRINTF, loglun, '--->total number of colony pixels found: ', STRTRIM(STRING(total_colony_pixels))

IF (ie LT num_t_matrices-1) THEN PRINT, '--->temporarily closing the log file'
IF (ie GE num_t_matrices-1) THEN PRINT, '--->closing the log file for the last time'

ENDFOR ; the ie loop through all T-matrices

PRINT, ' '
FOR i=1,3 DO PRINT, '*******DONE*******'

PRINTF, loglun, ' '
FOR i=1,3 DO PRINTF, loglun, '*******DONE*******'

CLOSE, loglun
FREE_LUN, loglun

endit:
CLOSE, /ALL
ENVI_BATCH_EXIT
END