PRO nex_seab_nn_write_latlon_multi_2016_v8c, config_file_name

  IF NOT KEYWORD_SET(config_file_name) THEN BEGIN
    PRINT, 'missing the config_file_name parameter!'
    PRINT, 'call this procedure like this:
    PRINT, 'nex_seab_nn_write_latlon_multi_2016_v8c, config_file_name'
    PRINT, '***QUITTING!***
    RETURN
  ENDIF

  code_version_name = 'NEX_seab_write_latlon_multi_2016_v8d'
  COMPILE_OPT STRICTARR

  ;*****************config file name*********************
  ; config_directory = '/Users/mschwall/~Essentials/penguin_stuff/flying_seabirds/test_retrievals_Dec_2016/'
  ; config_file_name = config_directory + 'TEST_L8_config_file_B5B2B1B3B4B6_Jan9_Savarth_Biscoe_MtPat_seabird_ROIs.txt'
  ;*******************************************************

  ;******************default values***********************
  total_config_parms = 10 ; total numbers of variables & files to be read from the config file
  ;******************default values***********************
  
  ; January 21, 2017 -- this version handles USGS raw Level 1GT data DNs and converts the DNs
  ; to TOA reflectance
  
  ; December 20, 2016 this version of write_latlon DOES NOT have any dependencies on the QA file
  ; so it chunks through a larger number of pixels but it doesn't require a QA dataset in the scene
  
  ; October 26, 2016 removed all of the nudging, this just operates on Landsat-8 using the
  ; t-matrix for a single satellite

  ; April 1, 2016 cleaned up some of the data ingest due to a file-naming change by USGS
  ; USGS has removed the lndcal prefix from the ESPA filenames, the changes in here should
  ; now handle both possibilities.

  ; December 1, 2015: removed cloud_calc code, this is now done in e_check_4_letter_code_status_v6

  ; October 22 this version is revised to handle the Landsat-8 exceptions
  ; there are only 3 variables that control the file naming and band selection:
  ; band_numbers_array : this variable is assigned in STEP-1 and stores the band numbers
  ;    that will be used in the retrieval IN THE ORDER IN WHICH THEY will be converted to spherical
  ;    coordinates. so for example the band order could be B3+B5+B4+B2 and the band_numbers_array
  ;    would = [3,5,4,2]. WE ARE KEEPING THE CONVENTION that B1=blue, B2=green, B3=red,
  ;    B4=NIR, B5=SWIR-1, B6=SWIR-2 (no changes were actually made in the code related to this
  ;    variable)
  ; lndcal_fid_array : this variable is assigned in STEP-2 and stores the FIDs in sequential
  ;    order. For L8 these FIDs correspond to HDF files 1,2,3,4,5,6 (again blue through SWIR-2)


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
  ; e = ENVI(/HEADLESS)


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
        ; this stuff isn't used here anymore,
        ; this cloud-calc stuff is now done in e_check_4_letter_code_status_v6
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
      ; ****CASE-8****
      STRPOS(aline, '<this_chunk_number>') GE 0: BEGIN
        ; TYPE = integer
        ; the job number ID 
        first_mark = STRPOS(aline, '>') + 1
        last_mark  = STRPOS(aline, '<', /REVERSE_SEARCH)
        this_chunk_number_string = STRMID(aline, first_mark, last_mark-first_mark)
        this_chunk_number  = FIX(this_chunk_number_string)
        config_check = config_check + 1
      END
      ; ****CASE-9****
      STRPOS(aline, '<number_of_scenes>') GE 0: BEGIN
        ; TYPE = integer
        ; the number of scenes to process in this job
        first_mark = STRPOS(aline, '>') + 1
        last_mark  = STRPOS(aline, '<', /REVERSE_SEARCH)
        number_of_scenes  = FIX(STRMID(aline, first_mark, last_mark-first_mark))
        config_check = config_check + 1
      END
      ; ****CASE-10****
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

  IF (sat_check NE 'L8') THEN BEGIN
    PRINT, 'satellite ID is not L8!'
    PRINT, 'This seabird analysis code only works on Landsat-8!'
    PRINT, '***QUITTING!***'
    ENVI_BATCH_EXIT
    CLOSE, /ALL
    RETURN
  ENDIF
  

  continent_pix_total = ULONG64(0) ; just how many pixels get processed anyhow?

  ; generate a log file
  log_file =  write_lat_lon_out_log + '/' + ID_prefix + 'write_latlon_log_file_job' + this_chunk_number_string + '_' + timedate + '.txt'
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
  ;******************************check for Landsat scene duplicates*****************************
  ;*********************************************************************************************
  is_error = 0 ; no errors yet
  seab_check_for_RAW_dupe_scenes_2017a, etm_multi_scene_dir, sat_check, loglun, is_error
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
      PRINT, 'this Landat file directory:'
      PRINT, etm_multi_scene_dir
      PRINT, '***EXITING!***'
      PRINTF, loglun, 'did not find all of the scenes specified in the config file when looking through the Landsat file directory'
      PRINTF, loglun, 'this config file: '
      PRINTF, loglun, config_file_name
      PRINTF, loglun, 'this Landat file directory:'
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
    
    metadata_struct = {sun_elevation:DOUBLE(1), earth_sun_distance: DOUBLE(1), $
      reflectance_add_band1: DOUBLE(1), reflectance_add_band2: DOUBLE(1), reflectance_add_band3: DOUBLE(1), $
      reflectance_add_band4: DOUBLE(1), reflectance_add_band5: DOUBLE(1), reflectance_add_band6: DOUBLE(1), $
      reflectance_add_band7: DOUBLE(1), reflectance_add_band9: DOUBLE(1), $
      reflectance_mult_band1: DOUBLE(1), reflectance_mult_band2: DOUBLE(1), reflectance_mult_band3: DOUBLE(1), $
      reflectance_mult_band4: DOUBLE(1), reflectance_mult_band5: DOUBLE(1), reflectance_mult_band6: DOUBLE(1), $
      reflectance_mult_band7: DOUBLE(1), reflectance_mult_band9: DOUBLE(1)}
    
    ; close & re-open the log file
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
    total_pixels_analyzed = 0UL ; total number of pixels actually looked at
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
      PRINTF, loglun, 'starting file # ', STRTRIM(STRING(i_case+1),2), ' out of ', STRTRIM(STRING(n_files),2)

      ; create a temp filename in the same directory as the log file for the purpose of removing
      ; duplicate colony pixel records, and make it as unique as possible since there may be a lot
      ; of these at one time
      temp_file =  write_lat_lon_out_log + '/' + job_name + '_' + 'chunk' + STRING(this_chunk_number) + '_' + $
                  'write_latlon_temp_file_' + timedate + '.txt'
      temp_file = STRCOMPRESS(temp_file, /REMOVE_ALL) 

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
      
      ; geotiff_file_name is really an array!
      geotiff_file_name = FILE_SEARCH(multi_scene_list[i_case] + '/*.TIF', COUNT=n_geotiff_files)
      IF (n_geotiff_files LT 8) THEN BEGIN
        PRINT, '***ERROR IN SEARCHING FOR GEOTIFF FILES***'
        PRINT, 'in this scene directory:
        PRINT, multi_scene_list[i_case]
        PRINT, 'expecting at least 8 geotiff files, but found this many: ' + STRCOMPRESS(STRING(n_geotiff_files), /REMOVE_ALL)
        PRINT, '***SKIPPING THIS ITERATION***'
        PRINTF, loglun, '***ERROR IN SEARCHING FOR GEOTIFF FILES***'
        PRINTF, loglun, 'in this scene directory:
        PRINTF, loglun, multi_scene_list[i_case]
        PRINTF, loglun, 'expecting at least 8 geotiff files, but found this many: ' + STRCOMPRESS(STRING(n_geotiff_files), /REMOVE_ALL)
        PRINTF, loglun, '***SKIPPING THIS ITERATION***'
        CLOSE, templun
        FREE_LUN, templun
        CONTINUE
      ENDIF      
      
      
      ;**********************FIND & READ THE METADATA FILE***********************
      ; in this case, multi_scene_list = the list of scenes for this job only!
      tiff_and_met_exist = 0
      L8_scene_directory = multi_scene_list[i_case]
      check_for_tiff_and_met_files_v1, L8_scene_directory, loglun, met_file_name, tiff_and_met_exist     
      IF (tiff_and_met_exist GT 0) THEN BEGIN
        PRINTF, loglun, '*******************QUITTING!*****************************'
        PRINT, ' '
        PRINT,'*******************QUITTING!*****************************'
        ENVI_BATCH_EXIT
        CLOSE, /ALL
        RETURN
      ENDIF

      PRINT, 'opening & parsing metadata file ' + met_file_name
      PRINTF, loglun, 'opening & parsing metadata file ' + met_file_name
      parse_L8_metadata_v2, met_file_name, metadata_struct, is_error
      IF (is_error NE 0) THEN BEGIN
        PRINT, 'error found in parse_L8_metadata_v2'
        PRINT, 'skipping this iteration'
        CONTINUE
      ENDIF

      ;**********************BEGIN READ MASEK lndcal FILE***********************

      PRINT, '--->working on TOA file ', multi_scene_list[i_case]
      PRINTF, loglun, 'starting file # ' + STRTRIM(STRING(i_case+1),2), ' out of ' + STRTRIM(STRING(n_files),2)
      PRINTF, loglun, '--->working on TOA file ', multi_scene_list[i_case]
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

      ;*****************************************
      ; STEP-2 get the FIDs of the lndcal file
      ; ****************************************
      PRINT, '--->operating on this scene '
      PRINT, multi_scene_list[i_case]
      PRINTF, loglun, '--->operating on this scene '
      PRINTF, loglun, multi_scene_list[i_case]

      ; 0 means that the tiff & met files exist
      
      tif_directory_name = multi_scene_list[i_case]
      IF (tiff_and_met_exist EQ 0) THEN get_tiff_file_fids_v1, tif_directory_name, loglun, lndcal_fid_array, qa_fid, is_error
      
      a_dummy = 6
      

      ;**************************************************************************************************
      ; STEP-4 calculate the phi-bands of spherical coordinated transformed bands (dimension = #bands-1)
      ;**************************************************************************************************

      ; calculate the scene dimensions
      my_fid = lndcal_fid_array[0]
      ENVI_FILE_QUERY, my_fid, DIMS=scene_dims, DATA_TYPE=scene_data_type

      ; get the scene dimensions
      scene_num_cols = scene_dims[2] + 1L
      scene_num_rows = scene_dims[4] + 1L
      ; DIMS[0]: A pointer to an open ROI. Otherwise, set to -1L.
      ; DIMS[1]: The starting sample number. The first x pixel index is 0.
      ; DIMS[2]: The ending sample number
      ; DIMS[3]: The starting line number. The first y pixel index is 0.
      ; DIMS[4]: The ending line number


      ; enter the scene data into array full_scene
      ; band_numbers_array : this variable is assigned in STEP-1 and stores the band numbers
      ;    that will be used in the retrieval IN THE ORDER IN WHICH THEY will be converted to spherical
      ;    coordinates. so for example the band order could be B3+B5+B4+B2 and the band_numbers_array
      ;    would = [3,5,4,2]. WE ARE KEEPING THE CONVENTION that B1=blue, B2=green, B3=red,
      ;    B4=NIR, B5=SWIR-1, B6=SWIR-2 (no changes were actually made in the code related to this
      ;    variable)
      ; lndcal_fid_array : this variable is assigned in STEP-2 and stores the FIDs in sequential
      ;    order. For L8 these FIDs correspond to HDF files 1,2,3,4,5,6 (again blue through SWIR-2)
      ; ***VERY IMPORTANT***
      ; so lndcal_fid_array[0] has the fid for band2, lndcal_fid_array[1] has the fid for band3, and so on
      ; ***VERY IMPORTANT***
      ; FOR LANDSAT-8 BAND2 (blue) cooresponds to BAND1 in the band_numbers_array
      full_scene = MAKE_ARRAY(DIMENSION=[scene_num_cols, scene_num_rows, T_dim], TYPE=scene_data_type)
      FOR iq=0,T_dim-1 DO BEGIN
        ;PRINT, iq
        my_fid = lndcal_fid_array[band_numbers_array[iq]-1] ;band numbers go from 2 to 7 so subtract 1
        ;PRINT, my_fid
        full_scene[*,*,iq] = ENVI_GET_DATA(FID=my_fid, DIMS=scene_dims, POS=0)
      ENDFOR


      qa_scene = MAKE_ARRAY(DIMENSION=[scene_num_cols, scene_num_rows], TYPE=scene_data_type)
      qa_scene[*,*] = ENVI_GET_DATA(FID=qa_fid, DIMS=scene_dims, POS=0)
      ENVI_FILE_MNG, ID=qa_fid, /REMOVE ; remove the QA file from ENVI


      ; note that we need to index full_scene based on the band_numbers_array
      ; and this is really tricky!
      ; for band_numbers_array = 3       1       2       5       4       6
      ;                    dex = 1       2       0       4       3       5
      ; dex[0] = band1 = index#1 in band_numbers array = 1
      ; dex[1] = band2 = index#2 in band_numbers_array = 2
      ; dex[2] = band3 = index#0 in band_numbers_array = 3 and so on ...
      ; use dex below when doing the thresholding
      num_indices = N_ELEMENTS(lndcal_fid_array)
      dex = INTARR(num_indices)
      FOR ix=1,T_dim DO dex[ix-1] = WHERE(band_numbers_array EQ ix)
      
      ; returns an array of the good pixels that we want to process
      good_pixel_array = water_fill_mask1(qa_scene) 
      qa_scene = 0 ; no need to carry this around

      num_good_pix = N_ELEMENTS(good_pixel_array)
      continent_pix_total = continent_pix_total + num_good_pix ; how many pixels did we evaluated on this run?

      IF num_good_pix LE 0 THEN BEGIN
        PRINT, '--->main: calculating phi but ALL scene pixels classify water or fill for this scene:'
        PRINT, scene_id
        PRINTF, loglun,  '--->main: calculating phi but ALL scene pixels classify as water or fill for this scene:'
        PRINTF, loglun,  scene_id

        band_tot = N_ELEMENTS(band_numbers_array)
        FOR ik=0,band_tot-1 DO BEGIN
          a_fid = lndcal_fid_array[band_numbers_array[ik]-1]
          ENVI_FILE_MNG, ID=a_fid, /REMOVE ; remove the files from ENVI
        ENDFOR
        CONTINUE
      ENDIF

      total_pixels_analyzed = total_pixels_analyzed + num_good_pix
      PRINTF, loglun, '--->main: calculating phi and r-dist for ' + STRCOMPRESS(STRING(num_good_pix), /REMOVE_ALL) + ' pixels'
      PRINT, '--->main: calculating phi and r-dist for ' + STRCOMPRESS(STRING(num_good_pix), /REMOVE_ALL) + ' pixels'

      ; now actually calculate phi
      ;PRINT, '--->main: setting up the phi array'
      ; phi has T_dim-1 columns and num_good_pix rows
      phi = MAKE_ARRAY(DIMENSION=[T_dim-1, num_good_pix], TYPE=5) ; double precision
      ; scene index contains all of the (sample,line) values of the "good pixels"
      scene_index = INTARR(2, num_good_pix)
      good_scene_array = FLTARR(T_dim, num_good_pix)
      FOR ii=0L,num_good_pix-1L DO BEGIN
        col_index = good_pixel_array[ii] MOD scene_num_cols ; column index for this pixel
        row_index = good_pixel_array[ii] / FIX(scene_num_cols) ; row index for this pixel
        scene_index[0,ii] = col_index
        scene_index[1,ii] = row_index
        good_scene_array[*,ii] = full_scene[col_index, row_index,*] 
      ENDFOR
      
      ; get the multiplicative and additive factors needed to convert the DNs into TOA
      ; reflectance from the metadata_struct. We assume that all of the mult factors
      ; are the same, and the add factors are also the same
      ; Get the solar elevation too, and then convert from DN to TOA below
      ; note that we need to multiply TOA-reflectance by 10000 to scale it to
      ; be equivalent to the ESPA values
      sine_theta = SIN((metadata_struct.sun_elevation * !PI) / 180.0D)  ; convert decimal degrees to radians
      mult_fac = (metadata_struct.reflectance_mult_band1 * 10000.0D) / sine_theta
      add_fac  = (metadata_struct.reflectance_add_band1 * 10000.0D) / sine_theta

      good_scene_array = good_scene_array * mult_fac + add_fac
      ; not sure why but sometimes you get unrealistic values, so turnt hem into very black pixels
      bad_vals = WHERE(good_scene_array LE 0.0, bad_count)
      IF (bad_count GT 0) THEN BEGIN
        good_scene_array[bad_vals] = 0.0001
        PRINTF, loglun, '--->found 0.0 or negative reflectance values in this many pixels: ' + STRCOMPRESS(STRING(bad_count), /REMOVE_ALL)
        PRINTF, loglun, '--->changing their reflectance values to 0.0001'
      ENDIF
      
      ; no need to carry this around
      ; full_scene = 0

      ;PRINT, '--->main: now actually calculating phi'

      blast = T_dim - 1
      phi[0,*] = ATAN(good_scene_array[blast,*]/good_scene_array[blast-1,*])
      ssqs = DBLARR(num_good_pix)
      ssqs[*] = (good_scene_array[blast,*])^2
      FOR ic=1,blast-1 DO BEGIN ; ic counts the channels in the correct order
        ssqs[*] = ssqs[*] + good_scene_array[blast-ic,*]^2
        phi[ic,*] = ATAN(SQRT(ssqs[*]) / good_scene_array[blast-ic-1,*])
      ENDFOR
      ssqs = 0 ; no need to carry this around anymore
      
      total_pixels_analyzed = total_pixels_analyzed + num_good_pix
      PRINTF, loglun, '--->main: finished calculating phi! ' 
      PRINT, '--->main: finished calculating phi!' 

      ; close & re-open the log file
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


      ;**************************************************************************************************
      ; STEP-6 calculate r-dist for each good pixel
      ;**************************************************************************************************

      ; no problem getting the projection from the tiff files!
      s_proj = ENVI_GET_PROJECTION(FID=lndcal_fid_array[0])
      ; generate a handle for the target projection: geographic lat/lon
      t_proj = ENVI_PROJ_CREATE(/GEOGRAPHIC)

      pixel_array = DBLARR(T_dim)    ; pixel array has the same dimension as T
      pixel_array[T_dim-1] = 1.0D    ; make the last element = 1.0

      ; calculate r-dist
      FOR ip=0UL, num_good_pix-1UL DO BEGIN
        pixel_array[0:T_dim-2] = phi[0:T_dim-2,ip]
        pixel_array[T_dim-1] = 1.0D
        result = TRANSPOSE(pixel_array) # T_invert
        r_dist = TRANSPOSE(result[0:T_dim-2]) # result[0:T_dim-2] ; calc ssq of all spherical coord bands
        r_dist = SQRT(r_dist)
        IF (r_dist LE 1.0) THEN BEGIN ; a rookery pixel was found!
          ; now calculate the polar stereographic coordinates and lat/lon of the pixel
          scene_col = scene_index[0,ip]
          scene_row = scene_index[1,ip]
          lndcal_geocoded_fid = lndcal_fid_array[0] ; just pick one of the geotiff fids and pass it to ENVI_CONVERT_FILE_COORDINATES
          ENVI_CONVERT_FILE_COORDINATES, lndcal_geocoded_fid, scene_col, scene_row, x_polarstereo, y_polarstereo, /to_map
          ; then convert those p-s coordinates to geographic lat/lon
          ENVI_CONVERT_PROJECTION_COORDINATES, x_polarstereo, y_polarstereo, s_proj, $
            x_latlon, y_latlon, t_proj
          pix_count = pix_count + 1L   ; how many pixels did we find in this scene?
          ; PRINT filename, location, r-value, sample & line info to the file
          ; the last element of the array is the scene id
          out_string = multi_scene_list[i_case] + '|'  ; n_parts-1 is the original image fileID
          out_string = out_string + STRING(r_dist) + '|' + STRING(x_polarstereo) + '|' + STRING(y_polarstereo)
          out_string = out_string + '|' + STRING(x_latlon) + '|' + STRING(y_latlon)
          out_string = out_string + '|' + STRING(scene_col) + '|' + STRING(scene_row) ; add sample & line info
          out_string = out_string + '|' + '.' ; just for compatibility with earlier versions
          out_string = out_string + '|' + t_matrix_struct.band_names_array[ie]
          out_string = STRCOMPRESS(out_string, /remove_all)
          ; PRINT, out_string
          PRINTF, templun, out_string ; print the colony/pixel string to a temp file
        ENDIF  ; check for r_dist LT 1
      ENDFOR ; ip loop

      PRINTF, loglun, '--->finished calculating r_dist, found this many with r_dist < 1 ', STRCOMPRESS(STRING(pix_count), /REMOVE_ALL)
      

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
      ; June 11, 2016: we won't find any dupes but it is just easier to run with checkdupes
      ; in place than to revise the code
      found_error = 0
      seab_checkdupes_multi_2016a, temp_file, loglun, class_array, num_records, found_error, good_record_counter
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

      ; remove the geotiff files from ENVI
      band_tot = N_ELEMENTS(lndcal_fid_array)
      FOR ik=0,band_tot-1 DO BEGIN
        a_fid = lndcal_fid_array[ik]
        ENVI_FILE_MNG, ID=a_fid, /REMOVE ; remove the files from ENVI
      ENDFOR


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
    ;PRINTF, loglun, '--->total number of non-duplicate colony pixels written to class summary: ', STRTRIM(STRING(total_good_record_counter))



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
