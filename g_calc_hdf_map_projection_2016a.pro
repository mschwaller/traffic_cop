PRO g_calc_hdf_map_projection_2016a, found_error, hdr_file_name, my_proj, my_map_info

COMPILE_OPT STRICTARR
FORWARD_FUNCTION ENVI_MAP_INFO_CREATE 

; re-named April 1, 2016
; called from f_write_latlon_multi_2016a.pro

; September 8, 2013
; Unfortunately ENVI doesn't have a built-in function to read LEDAPS sr & cal files
; when you read those files into ENVI using ENVI_OPEN_DATA_FILE & ENVI_GET_DATA you
; don't get the associated geocoded map info. So we need to jump through the hoops 
; below to parse the map & projection info out of the .hdr file, and then a few
; more hoops to generate map and projection info structures, and then even more
; hoops in the main program to associate the geocoded info with the image

; RETURNS A POLAR STEREOGRAPHIC PROJECTION!

found_error = 0 ; no errors yet!

;PRINT, '----->entering e_calc_hdf_map_projection_2015a'

; open the hdf header file
OPENR, hdr_lun, hdr_file_name, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  found_error = 1
  PRINT, '--->error found in e_calc_hdf_map_projection_2015a'
  PRINT, '--->a file error occurred when trying to open the header file!'
  PRINT, hdr_file_name
  PRINT, STRMESSAGE(err)
  ; CLOSE, hdr_lun
  RETURN
ENDIF

data_type = -1
aline = ' '
WHILE ~ EOF(hdr_lun) DO BEGIN
  READF, hdr_lun, aline
  IF STRPOS(aline, 'Polar Stereographic') GE 0 THEN BEGIN
    data_type = 31 ; ENVI data type associated with Polar Stereographic projections
  ENDIF
  IF STRPOS(aline, 'UTM') GE 0 THEN UTM_error_print = aline
  IF data_type GE 0 THEN CONTINUE
ENDWHILE

IF data_type LT 0 THEN BEGIN
  PRINT, '--->error found in e_calc_hdf_map_projection_2015a'
  PRINT, '--->no valid projection found in the hdf .hdr file!'
  PRINT, hdr_file_name
  PRINT, UTM_error_print
  found_error = 1
  CLOSE, hdr_lun
  FREE_LUN, hdr_lun
  RETURN
ENDIF

CLOSE, hdr_lun
FREE_LUN, hdr_lun

; re-open the header file
OPENR, hdr_lun, hdr_file_name, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  found_error = 1
  PRINT, '--->error found in e_calc_hdf_map_projection_2015a'
  PRINT, '--->a file error occurred when trying to open the header file!'
  PRINT, hdr_file_name
  PRINT, STRMESSAGE(err)
  ;CLOSE, hdr_lun
  RETURN
ENDIF


; parse the .hdr file to extract image projection & map info
aline = ' '
WHILE ~ EOF(hdr_lun) DO BEGIN
  READF, hdr_lun, aline
  ; PRINT, aline
  CASE 1 OF
    ; ****CASE-1****
    STRPOS(aline, 'map info') GE 0: BEGIN
      first_mark  = STRPOS(aline, '{') + 1
      last_mark   = STRPOS(aline, '}', /REVERSE_SEARCH)
      map_string = STRCOMPRESS(STRMID(aline, first_mark, last_mark-first_mark), /REMOVE_ALL)
      parsed = STRSPLIT(map_string, ',', /EXTRACT, COUNT=num_fields)
      mc_array = DOUBLE(parsed[1:4])
      ps_array = DOUBLE(parsed[5:6])
      my_datum = parsed[7]
      units_name_text = STRMID(parsed[8], STRPOS(parsed[8],'=')+1, STRLEN(parsed[8]))
      units_name = ENVI_TRANSLATE_PROJECTION_UNITS(units_name_text)
      IF units_name NE 0 THEN PRINT, '***WARNING: projection unit = ', units_name_text
      ;PRINT, parsed
      ; I'm not sure why you need to do this, but my image was 1 pixel off in the n & s
      ; directions compared to the raw Landsat and the LEDAPS data ingested by ENVI, so 
      ; I'm going with this tweaking even if I don't know why it works
      ; mc_array[2] = mc_array[2] + 30.0D ; shift 1 pixel east
      ; mc_array[3] = mc_array[3] - 30.0D ; shift 1 pixel south
      ; 9/9/2013 after more checking it seem that this fudge factor is NOT needed!
      ;PRINT, 'mc array ', mc_array
      ;PRINT, 'ps array ', ps_array
      ;PRINT, 'datum***', my_datum, '***'
      ;PRINT, 'units name***', units_name_text, '***      value ', units_name
    END
    ; ****CASE-2****
    STRPOS(aline, 'projection info') GE 0: BEGIN
      first_mark  = STRPOS(aline, '{') + 1
      last_mark   = STRPOS(aline, '}', /REVERSE_SEARCH)
      map_string = STRCOMPRESS(STRMID(aline, first_mark, last_mark-first_mark), /REMOVE_ALL)
      parsed = STRSPLIT(map_string, ',', /EXTRACT, COUNT=num_fields)
      param_array = DOUBLE(parsed[1:6])
      ps_proj_type = FIX(parsed[0])
      IF ps_proj_type NE 31 THEN BEGIN
        PRINT, '--->error found in e_calc_hdf_map_projection_2015a'
        PRINT, '************************************************************************************'
        PRINT, '***ERROR: Polar Stereographic projection type = 31, this projection type = ', ps_proj_type
        PRINT, '************************************************************************************'
        found_error = 1
        CLOSE, hdr_lun
        FREE_LUN, hdr_lun
        RETURN
      ENDIF
      ;PRINT, parsed
      ;PRINT, FORMAT = '( "param_array ", 2F20.10, 4F8.2 )', param_array
    END
    ELSE:
  ENDCASE
ENDWHILE

my_proj = ENVI_PROJ_CREATE(DATUM=my_datum, TYPE=ps_proj_type, PARAMS=param_array, UNITS=units_name)
; PRINT, my_proj

my_map_info = ENVI_MAP_INFO_CREATE(MC=mc_array, PS=ps_array, PROJ=my_proj, DATUM=my_datum, UNITS=units_name)
; PRINT, my_map_info 

CLOSE, hdr_lun
FREE_LUN, hdr_lun

;PRINT, '----->leaving e_calc_hdf_map_projection_2015a'

END