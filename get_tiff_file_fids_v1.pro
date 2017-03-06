PRO get_tiff_file_fids_v1, lndcal_file_name, loglun, lndcal_fid_array, qa_fid, is_error

; this proc is called from nex_seab_nn_write_latlon_multi_2016_v8b
; it ingests Landsat data in the TIF format
; the general assumption is that these data are L1 DN values

; of the TIF bands, we want: 2,3,4,5,6,7 (6 bands)
; B1=coastal, B8=pan, B9=cirrus

is_error = 0

; check to make sure that enough TIF bands are present and that the QA band exists
tif_result = FILE_SEARCH(lndcal_file_name, '*.TIF', COUNT=tif_band_count)
qa_result = FILE_SEARCH(lndcal_file_name, '*_BQA.TIF', COUNT=qa_band_count)
IF (tif_band_count LT 8) THEN BEGIN
  PRINT, ' '
  PRINT, '***ERROR ingesting TOA files in this scene:'
  PRINT, lndcal_file_name
  PRINT, '***expecting at least 8 TOA bands, but found only ' + STRCOMPRESS(STRING(tif_band_count), /REMOVE_ALL)
  PRINT, '***SKIPPING THIS ITERATION!'
  PRINTF, loglun, ' '
  PRINTF, loglun, '***ERROR ingesting TOA files in this scene:'
  PRINTF, loglun, lndcal_file_name
  PRINTF, loglun, 'expecting at least 8 TOA bands, but found only ' + STRCOMPRESS(STRING(tif_band_count), /REMOVE_ALL)
  PRINTF, loglun, '***SKIPPING THIS ITERATION!'
  is_error = 1
  RETURN
ENDIF
IF (qa_band_count LE 0) THEN BEGIN
  PRINT, ' '
  PRINT, '***warning -- QA band not found for this scene:'
  PRINT, lndcal_file_name
  PRINT, '***continuing without QA band (not actually used in this version of write_latlon anyhow)!'
  PRINTF, loglun, ' '
  PRINTF, loglun, '***warning -- QA band not found for this scene:'
  PRINTF, loglun, lndcal_file_name
  PRINTF, loglun, '***continuing without QA band (not actually used in this version of write_latlon anyhow)!'
ENDIF

tif_file_name_array = STRARR(6) ; we want 6 bands
FOR ib=2,7 DO BEGIN
  search_str = STRCOMPRESS('*_B' + STRING(ib) + '.TIF', /REMOVE_ALL)
  band_filename = FILE_SEARCH(lndcal_file_name, search_str, COUNT=tif_band_count)
  IF (tif_band_count NE 1) THEN BEGIN
    PRINT, '***ERROR in ingest_tiff_files_v1'
    PRINT, 'could not find ' + search_str +  ' in:'
    PRINT, lndcal_file_name
    PRINT, 'returning with error!'
    is_error = 1
    RETURN
  ENDIF
  tif_file_name_array[ib-2] = band_filename
ENDFOR

lndcal_fid_array = LONARR(6) ;  fids for bands 2,3,4,5,6,7 PLUS BQA (no coastal, no cirrus!)
lndcal_fid_array[*] = -99L ; initialize the array
FOR ib=0,5 DO BEGIN ; 6 bands
  ENVI_OPEN_DATA_FILE, tif_file_name_array[ib], R_FID=fid
  ENVI_FILE_QUERY, fid, BNAMES=band_name ; what is the band name?
  ; PRINT, band_name
  CASE 1 OF
    STRPOS(tif_file_name_array[ib], '_B2') GE 0: lndcal_fid_array[ib] = fid ; blue
    STRPOS(tif_file_name_array[ib], '_B3') GE 0: lndcal_fid_array[ib] = fid ; green
    STRPOS(tif_file_name_array[ib], '_B4') GE 0: lndcal_fid_array[ib] = fid ; red
    STRPOS(tif_file_name_array[ib], '_B5') GE 0: lndcal_fid_array[ib] = fid ; IR
    STRPOS(tif_file_name_array[ib], '_B6') GE 0: lndcal_fid_array[ib] = fid ; SWIR-1
    STRPOS(tif_file_name_array[ib], '_B7') GE 0: lndcal_fid_array[ib] = fid ; SWIR-2  
    ELSE: BEGIN
      PRINT, '***ERROR in ingest_tiff_files_v1 while ingesting TOA files in this scene:'
      PRINT, lndcal_file_name
      PRINT, 'could not find a fid for this file:'
      PRINT, tif_file_name_array[ib]
      PRINT, '***RETURNING AN ERROR!'
      PRINTF, loglun, ' '
      PRINTF, loglun, '***ERROR in ingest_tiff_files_v1 while ingesting TOA files in this scene:'
      PRINTF, loglun, lndcal_file_name
      PRINTF, loglun, 'could not find a fid for this file:'
      PRINTF, loglun, tif_file_name_array[ib]
      PRINTF, loglun, '***RETURNING AN ERROR!'
      is_error = 1
    ENDELSE
  ENDCASE ;
    
ENDFOR

; get the BQA fid
ENVI_OPEN_DATA_FILE, qa_result, R_FID=qa_fid

; now do some checking
f_result = WHERE(lndcal_fid_array GE 0, fid_count)
IF (fid_count NE 6) THEN BEGIN
  error_index = WHERE(lndcal_fid_array LT 0, e_count)
  PRINT, ' '
  PRINT, '***ERROR ingesting TIF files in this scene:'
  PRINT, lndcal_file_name
  PRINTF, loglun, ' '
  PRINTF, loglun, '***ERROR ingesting TIF files in this scene:'
  PRINTF, loglun, lndcal_file_name
  FOR ier=0,e_count-1 DO BEGIN
    PRINT, 'did not find TIF file ' + STRCOMPRESS(STRING(error_index[ier] + 2), /REMOVE_ALL)
    PRINTF, loglun, 'did not find TIF file ' + STRCOMPRESS(STRING(error_index[ier] + 2), /REMOVE_ALL)
  ENDFOR
  PRINT, '***RETURNING AN ERROR!***
  PRINTF, loglun, '***RETURNING AN ERROR!***
  is_error = 1
ENDIF

END
