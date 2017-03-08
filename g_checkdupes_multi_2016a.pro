
PRO g_checkdupes_multi_2016a, temp_filename, loglun, class_array, line_number, found_error, good_record_counter

; line_number = number of colony/pixel records

; this subroutine is called by e_write_latlon_v1_2014c and checks 
; the class_summary records generated for a single scene to see if there are any duplicate
; records, and if so, removes the dupes
; the array class_array stores the dupe-cleared classification_summary records

PRINT, '---->entering g_checkdupes_multi_2016a'

found_error = 0
good_record_counter = 0UL

; open the temp file for reading 
OPENR, templun, temp_filename, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to open the temp file!'
  PRINT, temp_filename
  PRINT, STRMESSAGE(err)
  CLOSE, templun
  FREE_LUN, templun
  found_error = 1
  RETURN
ENDIF

; and count the number of records in temp_filename
aline = ' '
line_number = 0UL ; Unsigned Long
WHILE ~ EOF(templun) DO BEGIN ; adding the classification summary filename, which is used as input by r_find_rooks_9
  READF, templun, aline
  line_number = line_number + 1UL
ENDWHILE

; close the file
CLOSE, templun
FREE_LUN, templun

IF line_number EQ 0 THEN BEGIN ; no colony pixels were found in this scene
  PRINT, 'e_checkdupes_multi_2015a--->no colony pixels found in this scene'
  PRINTF, loglun, 'e_checkdupes_multi_2015a--->no colony pixels found in this scene'
  RETURN
ENDIF

class_array = STRARR(line_number)

; re-open the temp file for reading
OPENR, templun, temp_filename, /GET_LUN, ERROR=err
IF err NE 0 THEN BEGIN
  PRINT, '--->a file error occurred when trying to open the temp file!'
  PRINT, temp_filename
  PRINT, STRMESSAGE(err)
  CLOSE, templun
  FREE_LUN, templun
  found_error = 1
  RETURN
ENDIF

; read the records into the array class_array
aline = ' '
FOR ix=0UL,line_number-1UL DO BEGIN
  READF, templun, aline
  class_array[ix] = aline
ENDFOR

; close the friggin file again
CLOSE, templun
FREE_LUN, templun

PRINT, 'checking through this many records ', line_number

total_iter = STRTRIM(STRING(ULONG(line_number) * (ULONG(line_number)/2)),1)

dupe_counter = 0L
total_iter_counter = 0UL
dupe_record_counter = 0UL
FOR i=0UL,line_number-1UL DO BEGIN
  IF ((i MOD 1000UL) EQ 0UL) THEN PRINT, 'finished iteration ' + STRTRIM(STRING(i),1) + ' out of ' + STRTRIM(STRING(line_number-1),1)
  parsed1 = STRSPLIT(class_array[i], '|', /EXTRACT, COUNT=num_fields)
  dupe_found = 0 ; no duplicates found yet for this record
  FOR j=i+1UL, line_number-1UL DO BEGIN
    total_iter_counter = total_iter_counter + 1UL
    parsed2 = STRSPLIT(class_array[j], '|', /EXTRACT, COUNT=num_fields)
    ; if you find a dupe then CONTINUE, that is, skip out of this iteration before printing the record
    IF ((parsed1[1] EQ parsed2[1]) AND (parsed1[7] EQ parsed2[7]) AND (parsed1[8] EQ parsed2[8])) THEN BEGIN
      dupe_found = 1 ; found a dupe for the class_array[i] record
      dupe_record_counter = dupe_record_counter + 1UL
      class_array[i] = '*****duplicate record*****'
      CONTINUE ; if a dupe is found, skip the rest of the j loop
    ENDIF
  ENDFOR ; j loop ends here
  IF (dupe_found EQ 0) THEN good_record_counter = good_record_counter + 1UL
ENDFOR ; i loop ends here

PRINTF, loglun, ' '
PRINTF, loglun, '--->finished checking for duplicate records in this file:'
PRINTF, loglun, temp_filename
PRINTF, loglun, '--->this many records were in class_summary file originally ' , STRTRIM(STRING(N_ELEMENTS(class_array[*])),1)
PRINTF, loglun, '--->this many records were duplicates ', STRTRIM(STRING(dupe_record_counter),1)
PRINTF, loglun, '--->this many records were written to the new class_summary file ', STRTRIM(STRING(good_record_counter),1)
PRINTF, loglun, '--->sum of dupes + good records ', STRTRIM(STRING(good_record_counter+dupe_record_counter),1)
PRINTF, loglun, ' '

PRINT, 'number of records in class_summary file originally ' , STRTRIM(STRING(N_ELEMENTS(class_array[*])),1)
PRINT, 'number of duplicates found ', STRTRIM(STRING(dupe_record_counter),1)
PRINT, 'number of records written to the new class_summary file ', STRTRIM(STRING(good_record_counter),1)
PRINT, 'sum of dupes + good records ', STRTRIM(STRING(good_record_counter+dupe_record_counter),1)
PRINT, '---->leaving g_checkdupes_multi_2016a'

END
