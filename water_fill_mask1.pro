FUNCTION water_fill_mask1, qaband

; this function uses the Landsat QA band to mask snow, cloud and water
; QA band bit values are:
; 0 = fill / integer value = 1
; 1 = dropped frame
; 2 = terrain occulsion
; 3 = reserved
; 4,5 = water confidence (1,1 = high confidence) / integer value = 48
; 6,7 = reserved for shadow
; 8,9 = vegetation confidence (1,1 = high confidence)
; 10,11 = snow/ice confidence (1,1 = high confidence) / integer value = 3072
; 12,13 = cirrus confidence (1,1 = high confidence) / integer value = 12288
; 14,15 = cloud confidence (1,1 = high confidence)  / integer value = 49152

;num_samples = N_ELEMENTS(qaband[*,0]) 
;num_lines   = N_ELEMENTS(qaband[0,*])

;the_mask = BYTARR(num_samples, num_lines)
;the_mask[*,*] = 0B

;dummy_index = WHERE(((qaband AND 49152L) EQ 49152L) OR $ ; = cloud
;              ((qaband AND 12288L) EQ 12288L) OR $  ; = cirrus
;              ((qaband AND 3072) EQ 3072) OR $    ; = snow/ice
;              ((qaband AND 48) EQ 48) OR $        ; = water
;              ((qaband AND 1) EQ 1), $            ; = fill
;              COMPLEMENT=good_index)
    
dummy_index = WHERE(((qaband AND 48) EQ 48) OR $   ; = water
              ((qaband AND 1) EQ 1), $             ; = fill
              COMPLEMENT=good_index) 

; the index of all the "good" pixels that we want to process
RETURN, good_index               

END
