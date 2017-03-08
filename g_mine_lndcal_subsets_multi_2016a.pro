PRO g_mine_lndcal_subsets_multi_2016a, lndcal_file_name, loglun, is_error, adelie_latlon_filename, $
                                 colony_in_counter, adelie_sample_line_array, adelie_name_line_array

; April 1, 2016 re-named this

; June 25 this version ingests the colony name from the PLOS-1 paper Apendices B & C
; this version now passes the array adelie_name_line_array holds the colony names 
; corresponding to the adelie_sample_line_array list of lat,lon coordinates associated
; line-by-line with the a colony name

; returns colony_in_counter = the number of Adelie colonies found within the lndcal scene and
; returns adelie_sample_line_array = a [2, colony_in_counter] array with the scene 
; sample,line coordinates corresponding to the lat,lon coordinates of the Lynch/LaRue and/or
; Schwaller Adelie colonies present within the scene
; returns adelie_name_line_array = the colony name associated with each adelie_sample_line_array record


; December 4, 2013 this version turns the proc into a subroutine that is called
; by e_write_latlon. It is intended to be part of the "data mining" version of
; the code package.
; 
; September 21, 2013 this proc preprocesses lndcal data files to subset just those
; portions of the file that cover known Adelie penguin colonies
;


COMPILE_OPT STRICTARR
is_error = 0

PRINT, '---->entering g_mine_lndcal_subsets_multi_2016a'

; STEP-1 open the lndcal file with ENVI_OPEN_DATA_FILE
; then use ENVI_FILE_QUERY to get the dimensions of the file
; STEP-2 get the projection information for the file and
; determine the coordinates of the file boundaries
; STEP-3 read though the list of Adelie colony locations and 
; determine which of the colonies falls within the scene boundary
; STEP-4 close all files and return the reduced colony list

; *******************
; STEP-1 open the file and get the scene DIMS
; *******************

ENVI_OPEN_DATA_FILE, lndcal_file_name, /HDF_SD, HDFSD_DATASET=0, R_FID=my_fid
IF (my_fid EQ -1) THEN BEGIN
  PRINT, '--->ENVI_OPEN_FILE failed on ', lndcal_file_name, ' file-ID #' , my_fid
  PRINTF, loglun, '--->ENVI_OPEN_FILE failed on ', lndcal_file_name, ' file-ID #', my_fid
  is_error = 1
  RETURN
ENDIF

ENVI_FILE_QUERY, my_fid, DIMS=dims, DATA_TYPE=input_data_type ; find out the image scene dimensions

; build an array to hold the image data
; the array will have the following dimensions:
; cols = dims[2] (x-dimension)
; rows = dims[4] (y-dimension)
; number of bands = nb
; data type = input_data_type (from ENVI_FILE_QUERY)
; 1: Byte (8 bits)
; 2: Integer (16 bits)
; 3: Long integer (32 bits)
; 4: Floating-point (32 bits)
; 5: Double-precision floating-point (64 bits)
; 6: Complex (2x32 bits)
; 9: Double-precision complex (2x64 bits)
; 12: Unsigned integer (16 bits)
; 13: Unsigned long integer (32 bits)
; 14: Long 64-bit integer
; 15: Unsigned long 64-bit integer

scene_dims_pix_coords = LONARR(2,4) ; col,row coordinates: UL,UR,LR,LL dimensions of the image in pixels
scene_dims_pix_coords[*,0] = [dims[1], dims[3]] ; UL dims[1] = starting sample, dims[2] = ending sample
scene_dims_pix_coords[*,1] = [dims[2], dims[3]] ; UR dims[3] = starting line, dims[4] = ending line
scene_dims_pix_coords[*,2] = [dims[2], dims[4]] ; LR dims[0] = not used
scene_dims_pix_coords[*,3] = [dims[1], dims[4]] ; LL

; *******************
; STEP-2 get projecion info and determine scene boundary coordinates
; *******************

; strangely, I can't get ENVI to append geolocation information to the hdf files
; when I use the sequence ENVI_OPEN_DATA_FILE / ENVI_FILE_QUERY / ENVI_GET_DATA
; so I need to use the messy sequence below:
; 1) open the .hdr file, 2) extract map and projection info using the proc
; r_calc_hdf_map_projection 3) create a geocoded image, in this case we use
; the fill_QA_array and attach the map & projection info to it using ENVI_ENTER_DATA

; there is an ENVI header file in the same directory as the sr and cal files, it has the
; same filename as the sr and cal files, but with .hdr appended
; this file contains the projection info for the image
envi_hdr_file_name = lndcal_file_name + '.hdr'

; this proc parses the .hdr file and returns projection & map info
found_error = 0
g_calc_hdf_map_projection_2016a, found_error, envi_hdr_file_name, lndcal_proj, my_map_info

is_error=0
IF found_error EQ 1 THEN BEGIN
  PRINT, '--->*****ERROR FOUND WHILE TRYING TO EXTRACT SCENE MAP & PROJECTION INFO FOR THIS FILE'
  PRINT, lndcal_file_name
  PRINT, '--->*****SKIPPING THIS ITERATION!'
  is_error = 1
  ; close the image file and return with an error code
  ENVI_FILE_MNG, ID=my_fid, /REMOVE
  RETURN
ENDIF

; attach the map & projection information to an arbitrary 2D byte image (dummy_array), we will
; use the lndcal_geocoded_fid of the dummy_array below in ENVI_CONVERT_FILE_COORDINATES to calculate
; ploar stereographic coordinates from the row,col values of the image

dummy_array = BYTARR(dims[2]+1, dims[4]+1) ; build the array

ENVI_ENTER_DATA, dummy_array, BNAMES='geocoded_dummy_band', MAP_INFO=my_map_info, $
  R_FID=lndcal_geocoded_fid
  
; determine the source projection of the image file
s_proj = ENVI_GET_PROJECTION(FID=lndcal_geocoded_fid)
; generate a handle for the target projection: geographic lat/lon
t_geo_proj = ENVI_PROJ_CREATE(/GEOGRAPHIC)

; calculate the geographic coordinates of the scene boundaries
scene_dims_geo_coords = DBLARR(2,4) ; geographic LON,LAT coordinates: UL,UR,LR,LL 

FOR ig=0,3 DO BEGIN
   ; now convert scene coordinates to geographic lon/lat
   ENVI_CONVERT_FILE_COORDINATES, lndcal_geocoded_fid, scene_dims_pix_coords[0,ig], scene_dims_pix_coords[1,ig], $
     x_easting, y_northing, /to_map
     ; PRINT, 'easting & northing ', x_easting, y_northing
   ; convert northing and easting (may be UTM or polar stereographic) to geographic lat,lon
   ENVI_CONVERT_PROJECTION_COORDINATES, x_easting, y_northing, s_proj, $
     x_longitude, y_latitude, t_geo_proj 
     scene_dims_geo_coords[0,ig] = x_longitude ; [0,*]=longitude
     scene_dims_geo_coords[1,ig] = y_latitude  ; [1,*]=latitude 
ENDFOR

path_row_position = STRPOS(lndcal_file_name, 'LE7', /REVERSE_SEARCH) + 3
path_val = STRMID(lndcal_file_name, path_row_position, 3)
row_val  = STRMID(lndcal_file_name, path_row_position+3, 3)

;PRINT, '*****************'
;PRINT, 'col,row (x,y) dimensions'
;FOR ip=0,3 DO PRINT, scene_dims_pix_coords[*,ip]
;PRINT, path_val + ' ' + row_val
;FOR ip=0,3 DO PRINT, scene_dims_geo_coords[*,ip]
;PRINT, lndcal_proj
;PRINT, my_map_info
;PRINT, ' ' 

; *******************
; STEP-3 read through the list of known Adelie colonies and pick the ones that fall 
; into the bounds of the lndcal scene
; *******************

; read the adelie locations into an array
OPENR, adelie_in_lun, adelie_latlon_filename, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to open file'
  PRINT, adelie_latlon_filename
  PRINTF, loglun, '--->a file error occurred when trying to open file'
  PRINTF, loglun,  adelie_latlon_filename
  PRINT, STRMESSAGE(err)
  is_error = 1
  RETURN
ENDIF

one_line = ' '
num_adelie_lines = 0ULL
WHILE (EOF(adelie_in_lun) NE 1) DO BEGIN  ; *****count # of records*****
  READF, adelie_in_lun, one_line
  num_adelie_lines = num_adelie_lines + 1ULL
ENDWHILE
CLOSE, adelie_in_lun
FREE_LUN, adelie_in_lun

adelie_sample_line_array = DBLARR(2,num_adelie_lines) ; lat,lon of all the colonies in this image
adelie_name_line_array   = STRARR(1,num_adelie_lines) ; corresponsing colony names
colony_in_counter = 0 ; count the number of colonies found within the scene
; re-open the file of adelie locations for reading
OPENR, adelie_in_lun, adelie_latlon_filename, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to open file'
  PRINT, adelie_latlon_filename
  PRINTF, loglun, '--->a file error occurred when trying to open file'
  PRINTF, loglun,  adelie_latlon_filename
  PRINT, STRMESSAGE(err)
  is_error = 1
  RETURN
ENDIF
one_line = ' '
FOR ic=0L,num_adelie_lines-1L DO BEGIN
  READF, adelie_in_lun, one_line
  parsed = STRSPLIT(one_line, ',', /EXTRACT)
  colony_lat = DOUBLE(parsed[0])
  colony_lon = DOUBLE(parsed[1])
  colony_name = parsed[2] 
  ; convert from geographic lat,lon to easting,northing, may be UTM or polar stereographic
  ENVI_CONVERT_PROJECTION_COORDINATES, colony_lon, colony_lat, t_geo_proj, $
        col_easting, col_northing, s_proj 
  ; calculate the sample and line of the colony
  ENVI_CONVERT_FILE_COORDINATES, lndcal_geocoded_fid, colony_sample, colony_line, col_easting, col_northing 
  IF ((colony_sample GE dims[1]) AND (colony_sample LE dims[2]) AND $
      (colony_line GE dims[3]) AND (colony_line LE dims[4])) THEN BEGIN
        ; we have a hit, add it to the sample array
        adelie_sample_line_array[*,colony_in_counter] = [colony_sample, colony_line]
        adelie_name_line_array[*,colony_in_counter] = colony_name ; corresponds to the sample,line above
        colony_in_counter = colony_in_counter + 1L
  ENDIF
ENDFOR
CLOSE, adelie_in_lun
FREE_LUN, adelie_in_lun

;PRINT, ' '
PRINT, '        --->g_mine_lndcal_subsets_multi_2016a'
PRINT, colony_in_counter, ' colonies found within the bounds of this lndcal scene:'
PRINT, '       ' + lndcal_file_name

;PRINTF, loglun, ' '
PRINTF, loglun, '--->g_mine_lndcal_subsets_multi_2016a'
PRINTF, loglun, colony_in_counter, ' colonies found within the bounds of this lndcal scene:'
PRINTF, loglun, '       ' + lndcal_file_name


; *******************
; STEP-4 close files and return the list of colonies that fall within the 
; scene boundary
; *******************

ENVI_FILE_MNG, ID=my_fid, /REMOVE

ENVI_FILE_MNG, ID=lndcal_geocoded_fid, /REMOVE

PRINT, '---->leaving g_mine_lndcal_subsets_multi_2016a'


END
