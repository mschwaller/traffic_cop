PRO parse_L8_metadata_v2, met_file_name, metadata_values, is_error

; ingest the reflectance additive and multiplicative factors from the _MTL.txt file
; return an error if they aren't all found or if they aren't uniform

CATCH, error_status
IF (error_status NE 0) THEN BEGIN
  PRINT, '***an error occurred in parse_L8_metadata_v2'
  PRINT, !ERROR_STATE.MSG
  is_error=1
  RETURN
ENDIF

ON_IOERROR, skip_here

is_error = 0

; open the metadata file
OPENR, met_lun, met_file_name, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to open the metadata file!'
  PRINT, met_file_name
  PRINT, STRMESSAGE(err)
  is_error = 1
  RETURN
ENDIF


aline = ' '
config_check = 0
WHILE ~ EOF(met_lun) DO BEGIN
  READF, met_lun, aline
  CASE 1 OF
    ; ****CASE-1****
    STRPOS(aline, 'SUN_ELEVATION') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.sun_elevation  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-2****
    STRPOS(aline, 'REFLECTANCE_ADD_BAND_1') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_add_band1  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-3****
    STRPOS(aline, 'REFLECTANCE_ADD_BAND_2') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_add_band2  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-4****
    STRPOS(aline, 'REFLECTANCE_ADD_BAND_3') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_add_band3  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
      ; ****CASE-5****
    END
    STRPOS(aline, 'REFLECTANCE_ADD_BAND_4') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_add_band4  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-6****
    STRPOS(aline, 'REFLECTANCE_ADD_BAND_5') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_add_band5  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-7****
    STRPOS(aline, 'REFLECTANCE_ADD_BAND_6') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_add_band6  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-8****
    STRPOS(aline, 'REFLECTANCE_ADD_BAND_7') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_add_band7  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-9****
    STRPOS(aline, 'REFLECTANCE_ADD_BAND_9') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_add_band9  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-10****
    STRPOS(aline, 'REFLECTANCE_MULT_BAND_1') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_mult_band1  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-11****
    STRPOS(aline, 'REFLECTANCE_MULT_BAND_2') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_mult_band2  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-12****
    STRPOS(aline, 'REFLECTANCE_MULT_BAND_3') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_mult_band3  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-13****
    STRPOS(aline, 'REFLECTANCE_MULT_BAND_4') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_mult_band4  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-14****
    STRPOS(aline, 'REFLECTANCE_MULT_BAND_5') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_mult_band5  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-15****
    STRPOS(aline, 'REFLECTANCE_MULT_BAND_6') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_mult_band6  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-16****
    STRPOS(aline, 'REFLECTANCE_MULT_BAND_7') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_mult_band7  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ; ****CASE-17****
    STRPOS(aline, 'REFLECTANCE_MULT_BAND_9') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.reflectance_mult_band9  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
      ; ****CASE-17****
    END
    STRPOS(aline, 'EARTH_SUN_DISTANCE') GE 0: BEGIN
      first_mark = STRPOS(aline, '=') + 1
      last_mark  = STRLEN(aline)
      metadata_values.earth_sun_distance  = DOUBLE(STRMID(aline, first_mark, last_mark-first_mark))
      config_check = config_check + 1
    END
    ELSE:
  ENDCASE
ENDWHILE


IF (config_check NE 18) THEN BEGIN
  PRINT, '***ERROR ingesting product metadata!***'
  PRINT, 'expected 17 metahdata parameters but found only ' + STRCOMPRESS(STRING(config_check), /REMOVE_ALL)
  PRINT, 'in this metadata file:'
  PRINT, met_file_name
  PRINT, metadata_values
  is_error = 1
  ; close the metadata file
  CLOSE, met_lun
  FREE_LUN, met_lun
  RETURN
ENDIF

; check to see that the additive factors are all equal (the same)
; and that the multiplicative factors are also all the same
result1 = (metadata_values.reflectance_mult_band1 EQ metadata_values.reflectance_mult_band2) $
  AND (metadata_values.reflectance_mult_band1 EQ metadata_values.reflectance_mult_band3) $
  AND (metadata_values.reflectance_mult_band1 EQ metadata_values.reflectance_mult_band4) $
  AND (metadata_values.reflectance_mult_band1 EQ metadata_values.reflectance_mult_band5) $
  AND (metadata_values.reflectance_mult_band1 EQ metadata_values.reflectance_mult_band6)
result2 = (metadata_values.reflectance_add_band1 EQ metadata_values.reflectance_add_band2) $
  AND (metadata_values.reflectance_add_band1 EQ metadata_values.reflectance_add_band3) $
  AND (metadata_values.reflectance_add_band1 EQ metadata_values.reflectance_add_band4) $
  AND (metadata_values.reflectance_add_band1 EQ metadata_values.reflectance_add_band5) $
  AND (metadata_values.reflectance_add_band1 EQ metadata_values.reflectance_add_band6)
  
IF (result1 NE 1) OR (result2 NE 1) THEN BEGIN
  PRINT, '***ERROR ingesting product metadata!***'
  IF (result1 NE 1) THEN PRINT, 'we assume that product metadata reflectance multiplicative factors will all be the same!'
  IF (result2 NE 1) THEN PRINT, 'we assume that product metadata reflectance additive factors will all be the same!'
  PRINT, 'in this metadata file:'
  PRINT, met_file_name
  PRINT, 'BUT THEY ARE DIFFERENT!'
  PRINT, 'returning from parse_L8_metadata_v2 with an error'
  is_error = 1
ENDIF

; close the metadata file
CLOSE, met_lun
FREE_LUN, met_lun

RETURN

; skip here is there is an IO error
skip_here:
HELP, /LAST_MESSAGE, OUTPUT=err_text
PRINT, err_test[0]
is_error = 1
RETURN

END