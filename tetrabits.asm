.enc "petscii"  ;define an ascii->petscii encoding
.cdef " @", 32  ;characters
.cdef "AZ", $01
.cdef "az", $01

PLAYER1__CURRENT_TETROMINOE_TYPE = $0200
PLAYER1__CURRENT_TETROMINOE_FACE = $0201
PLAYER1__CURRENT_TETROMINOE_COLOR = $0202

PLAYER1__NEXT_TETROMINOE_TYPE = $0203
PLAYER1__NEXT_TETROMINOE_FACE = $0204
PLAYER1__NEXT_TETROMINOE_COLOR = $0205

PLAYER1__TETROMINOE_X = $0206
PLAYER1__TETROMINOE_Y = $0207

PLAYER1__CURRENT_TETROMINOE_CELLS = $0208

PLAYER1__FOUND_LINES = $0210

PLAYER1__FALL_COUNTER = $0218
PLAYER1__FALL_SPEED = $0219
PLAYER1__ACTUAL_FALL_SPEED = $021a

PLAYER1__MOVE_X = $021b
PLAYER1__MOVE_Y = $021c


PLAYER1__WELL_OFFSET = $021d
PLAYER1__ANIM_FRAME = $021e
PLAYER1__ANIM_COUNTER = $021f

PLAYER1__LINES_TO_CLEAR = $0220

PLAYER1__REPEAT_COUNTER = $0221

PLAYER2__CURRENT_TETROMINOE_CELLS = $0230





PREDICATE_TETROMINOE_CELLS = $0280
PREDICATE_TETROMINOE_X = $0288
PREDICATE_TETROMINOE_Y = $0289

LINE_CHECK_BUFFER = $028a0

ROTATE_BUFFER = $024a

DRAW_TETR__CHAR = $0300
DRAW_TETR__COLOR = $0301

RAND_MAX = $0304

SCREEN_RAM = $0400

ANIM_SPEED = #$05

*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $34, $30, $39, $36, $00, $00
.byte $00

;; +----------------------------------+
;; |                                  |
;; |    INIT ROUTINE                  |
;; |                                  |
;; +----------------------------------+
*=$1000
                                    lda #$00
                                    sta $d020
                                    sta $d021
                                    
                                    lda #$08
                                    sta PLAYER1__WELL_OFFSET
                                                                                                           
                                    lda #%00011110
                                    sta $d018
                                    
                                    lda $d016
                                    ora #%00010000
                                    sta $d016
                                    
                                    lda #$0b
                                    sta $d022
                                    lda #$0c
                                    sta $d023

                                    jsr clear_screen
                                    
                                    ldy PLAYER1__WELL_OFFSET
                                    jsr draw_well
                                    
                                    lda #$80
                                    sta $d412                       ; Turn off Voice #3 and set noise enabled
                                    sta $d40f                       ; Set Voice #3 frequency

                                    
                                    jsr plr1_prepare_next_tetrominoe
                                    
                                    lda PLAYER1__CURRENT_TETROMINOE_FACE
                                    sta DRAW_TETR__CHAR
                                    lda PLAYER1__CURRENT_TETROMINOE_COLOR
                                    sta DRAW_TETR__COLOR
                                    jsr draw_tetrominoe
       
                                    lda #$30
                                    sta PLAYER1__FALL_SPEED
                                    sta PLAYER1__ACTUAL_FALL_SPEED
                                    
                                    lda PLAYER1__FALL_SPEED
                                    sta PLAYER1__FALL_COUNTER
                                    
                                    lda #$00
                                    sta PLAYER1__MOVE_X
                                    sta PLAYER1__MOVE_Y
                                    
                                    lda #$ff
                                    sta PLAYER1__FOUND_LINES
                                    sta PLAYER1__FOUND_LINES+1
                                    sta PLAYER1__FOUND_LINES+2
                                    sta PLAYER1__FOUND_LINES+3
            
                                    jmp main_game_loop
                                    
; +---------------------------------------------------------------------------+
; | WAIT_UNTIL_NEXT_FRAME                                                     |
; | load_plr1_tetrominoe                                                      |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+                                           
wait_until_next_frame:
                                    lda $d011               
                                    and #%10000000
                                    bne wait_until_next_frame
                                    
                                    lda $d012
                                    bne wait_until_next_frame
                                    rts                                    

; +---------------------------------------------------------------------------+
; | MAIN GAME LOOP                                                            |
; | main_game_loop                                                            |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+               
main_game_loop:                                    
                                    ;lda #$00                                    
                                    ;sta $d020
                                    jsr wait_until_next_frame
                                    ;inc $d020
                                    
                                    jsr read_player_input                         
                                    jsr move_player_tetromonoes
                                    jsr advance_tetromonoes
                                    jsr animate_lines
                                    
                                    jmp main_game_loop

; +---------------------------------------------------------------------------+
; | ADVANCE TETROMONOES                                                       |
; | load_plr1_tetrominoe                                                      |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+                                           
advance_tetromonoes:
                                    lda PLAYER1__ANIM_FRAME
                                    bne no_player_1_y_move

                                    dec PLAYER1__FALL_COUNTER
                                    bne no_player_1_y_move
                                    
                                    lda PLAYER1__FALL_SPEED
                                    sta PLAYER1__FALL_COUNTER
                                    
                                    lda #$20
                                    sta DRAW_TETR__CHAR
                                    lda #$00
                                    sta DRAW_TETR__COLOR
                                    jsr draw_tetrominoe
                                    
                                    lda PLAYER1__TETROMINOE_X
                                    sta PREDICATE_TETROMINOE_X
                                    lda PLAYER1__TETROMINOE_Y
                                    sta PREDICATE_TETROMINOE_Y
                                    inc PREDICATE_TETROMINOE_Y
                                    ldx #$00
                                    jsr copy_buffer_to_predicate
                                    jsr evaluate_target_pos
                                    
                                    cpx #$00
                                    bpl place_tetrominoe 
                                    inc PLAYER1__TETROMINOE_Y                                    
                                    
draw_advanced_tetro                 lda PLAYER1__CURRENT_TETROMINOE_FACE
                                    sta DRAW_TETR__CHAR
                                    lda PLAYER1__CURRENT_TETROMINOE_COLOR
                                    sta DRAW_TETR__COLOR
                                    jsr draw_tetrominoe
                                    
no_player_1_y_move                  rts
      
; +---------------------------------------------------------------------------+
; | PLACE_TETROMINOE                                                          |
; | place_tetrominoe                                                          |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
place_tetrominoe        
                                    lda PLAYER1__CURRENT_TETROMINOE_FACE
                                    sta DRAW_TETR__CHAR
                                    lda PLAYER1__CURRENT_TETROMINOE_COLOR
                                    sta DRAW_TETR__COLOR                                    
                                    jsr draw_tetrominoe
                                    
                                    jsr check_for_lines
                                    beq no_lines
                                    
                                    lda #$05
                                    sta PLAYER1__ANIM_FRAME                                    
                                    lda ANIM_SPEED
                                    sta PLAYER1__ANIM_COUNTER
                                    rts                          
                                    
no_lines                            jsr plr1_prepare_next_tetrominoe
                                    rts


; +---------------------------------------------------------------------------+
; | APPLY PLAYER MOVEMENT                                                     |
; | move_player_tetrominoes                                                   |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+                                           
move_player_tetromonoes:
                                    lda PLAYER1__ANIM_FRAME
                                    bne move_player_exit

                                    lda PLAYER1__REPEAT_COUNTER           
                                    bne plr1_dec_repeat_counter
 
                                    lda #$20
                                    sta DRAW_TETR__CHAR
                                    lda #$00
                                    sta DRAW_TETR__COLOR
                                    jsr draw_tetrominoe
                                    
                                    lda PLAYER1__MOVE_X
                                    cpx #$00
                                    beq no_plr1_horizontal_move
                                    


                                    clc
                                    lda PLAYER1__TETROMINOE_X                                                                 
                                    adc PLAYER1__MOVE_X
                                    sta PREDICATE_TETROMINOE_X
                                    lda PLAYER1__TETROMINOE_Y
                                    sta PREDICATE_TETROMINOE_Y
                                    ldx #$00
                                    jsr copy_buffer_to_predicate
                                    jsr evaluate_target_pos
                                    
                                    bpl no_plr1_horizontal_move                       
                                    
                                    clc
                                    lda PLAYER1__TETROMINOE_X
                                    adc PLAYER1__MOVE_X
                                    sta PLAYER1__TETROMINOE_X
                                    
                                    lda #$05
                                    sta PLAYER1__REPEAT_COUNTER                                                                                                           
                                    
no_plr1_horizontal_move             lda PLAYER1__MOVE_Y
                                    cmp #$01
                                    bne no_plr1_speed_up
                                    
                                    lda #$05
                                    sta PLAYER1__FALL_SPEED
                                    sta PLAYER1__FALL_COUNTER
                                    sta PLAYER1__REPEAT_COUNTER
                                                                        
                                    jsr plr1_chk_rotate

no_plr1_speed_up                    lda PLAYER1__ACTUAL_FALL_SPEED
                                    sta PLAYER1__FALL_SPEED


plr1_chk_rotate                     lda PLAYER1__MOVE_Y                                    
                                    bpl no_plr1_move
                                    
                                    jsr plr1_rotate_tetrominoe
                                    
                                    lda #$08
                                    sta PLAYER1__REPEAT_COUNTER

no_plr1_move                        lda PLAYER1__CURRENT_TETROMINOE_FACE
                                    sta DRAW_TETR__CHAR
                                    lda PLAYER1__CURRENT_TETROMINOE_COLOR
                                    sta DRAW_TETR__COLOR                                    
                                    jsr draw_tetrominoe
                                    
                                    rts

plr1_dec_repeat_counter             dec PLAYER1__REPEAT_COUNTER
move_player_exit                    rts

; +---------------------------------------------------------------------------+
; | PLAYER 1 - ROTATE TETROMINOE                                              |
; | plr1_rotate_tetrominoe                                                    |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+       
plr1_rotate_tetrominoe:
                                    lda PLAYER1__CURRENT_TETROMINOE_CELLS
                                    sta ROTATE_BUFFER+1
                                    lda #$03
                                    sec
                                    sbc PLAYER1__CURRENT_TETROMINOE_CELLS+1
                                    sta ROTATE_BUFFER

                                    lda PLAYER1__CURRENT_TETROMINOE_CELLS+2
                                    sta ROTATE_BUFFER+3
                                    lda #$03
                                    sec
                                    sbc PLAYER1__CURRENT_TETROMINOE_CELLS+3
                                    sta ROTATE_BUFFER+2
                                    
                                    lda PLAYER1__CURRENT_TETROMINOE_CELLS+4
                                    sta ROTATE_BUFFER+5
                                    lda #$03
                                    sec
                                    sbc PLAYER1__CURRENT_TETROMINOE_CELLS+5
                                    sta ROTATE_BUFFER+4
                                    
                                    lda PLAYER1__CURRENT_TETROMINOE_CELLS+6
                                    sta ROTATE_BUFFER+7
                                    lda #$03
                                    sec
                                    sbc PLAYER1__CURRENT_TETROMINOE_CELLS+7
                                    sta ROTATE_BUFFER+6

                                    lda PLAYER1__TETROMINOE_X
                                    sta PREDICATE_TETROMINOE_X
                                    lda PLAYER1__TETROMINOE_Y
                                    sta PREDICATE_TETROMINOE_Y
                                    
                                    ldx #$02
                                    jsr copy_buffer_to_predicate
                                    jsr evaluate_target_pos
                                    
                                    bpl rotate_exit
                                    
                                    ldx #$07
plr1_write_result_loop              lda ROTATE_BUFFER,x
                                    sta PLAYER1__CURRENT_TETROMINOE_CELLS,x
                                    dex
                                    bpl plr1_write_result_loop 
                                    rts
                                    
rotate_exit                         rts
                                    
                                    

; +---------------------------------------------------------------------------+
; | CLEAR LINES                                                               |
; | clear_lines                                                               |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+                                           
clear_lines:                                    
                                    ldx #$00                                    
clear_lines_loop                    lda PLAYER1__FOUND_LINES,x
                                    bmi clear_next_line
                                    
                                    jsr set_plr1_line_pointer
                                        
                                    lda #$20
                                    ldy #$09                                    
clear_line_loop                     sta ($30),y
                                    dey
                                    bpl clear_line_loop                                    
                                    
clear_next_line                     inx
                                    cpx #$04
                                    bcc clear_lines_loop                                    
                                    
                                    rts
                                    
; +---------------------------------------------------------------------------+
; | ANIMATE_LINES                                                             |
; | animate_lines                                                             |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+                                         
animate_lines:                                             
                                    lda PLAYER1__ANIM_FRAME
                                    beq animation_done
                                    
                                    dec PLAYER1__ANIM_COUNTER
                                    bne animation_done
                                    
                                    lda ANIM_SPEED
                                    sta PLAYER1__ANIM_COUNTER
                                    
                                    dec PLAYER1__ANIM_FRAME
                                    bne do_animate_lines
                                    
                                    jsr clear_lines
                                    jsr shift_well_contents
                                    jsr plr1_prepare_next_tetrominoe
                                    
                                    jmp animation_done

do_animate_lines                                         
                                    ldx #$00                                       
animate_lines_loop                  lda PLAYER1__FOUND_LINES,x
                                    bmi animate_next_line
                                    
                                    jsr set_plr1_line_pointer
                                    
                                    clc
                                    
                                    ldy #$0a
animate_line_loop                   lda ($30),y
                                    adc #$01
                                    sta ($30),y
                                    dey
                                    bne animate_line_loop
                                    
animate_next_line                   inx
                                    cpx #$04
                                    bcc animate_lines_loop 
                                    
animation_done                      rts

set_plr1_line_pointer:                                   
                                    asl             
                                    tay
                                    clc
                                    lda PLAYER1__WELL_OFFSET
                                    adc table_scr_line,y
                                    sta $30
                                    lda table_scr_line+1,y
                                    adc #$00
                                    sta $31

                                    rts
       
; +---------------------------------------------------------------------------+
; | CHECK FOR LINES                                                           |
; | check_for_lines                                                           |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+                                           
check_for_lines:                                    
                                    lda #$00
                                    sta PLAYER1__LINES_TO_CLEAR
                                    
                                    ldx #$03
                                    lda #$ff
clear_lcheck_buf_loop               sta LINE_CHECK_BUFFER,x
                                    sta PLAYER1__FOUND_LINES,x
                                    ;sta $0650,x
                                    dex
                                    bpl clear_lcheck_buf_loop
                                    
                                    ldx #$03
populate_lcheck_buffer_loop         txa
                                    asl
                                    tay
                                    lda PLAYER1__CURRENT_TETROMINOE_CELLS+1,y
                                    ;sta $0628,x
                                    tay
                                    lda #$01
                                    sta LINE_CHECK_BUFFER,y                                    
                                    dex
                                    bpl populate_lcheck_buffer_loop
                
                                    ldx #$03                                      
check_for_lines_loop                lda LINE_CHECK_BUFFER, x
                                    ;sta $0600,x
                                    bmi not_a_line
                                    
                                    txa                                    
                                    clc
                                    adc PLAYER1__TETROMINOE_Y
                                    asl
                                    tay

                                    clc                                    
                                    lda PLAYER1__WELL_OFFSET
                                    adc table_scr_line,y                                    
                                    sta $30
                                    lda table_scr_line+1,y
                                    adc #$00
                                    sta $31
                                    
                                    ldy #$0a
check_line_loop                     lda ($30),y
                                    cmp #$20                                    
                                    beq not_a_line
                                    
                                    dey
                                    bne check_line_loop 
                                    
                                    txa                                    
                                    clc
                                    adc PLAYER1__TETROMINOE_Y
                                    sta PLAYER1__FOUND_LINES,x
                                    inc PLAYER1__LINES_TO_CLEAR
                                    ;; sta $0650,x                                    

not_a_line                          dex                                    
                                    bpl check_for_lines_loop                                    
                                    
                                    lda PLAYER1__LINES_TO_CLEAR
                                    ;sta $0402
                                    rts
                                    
                                    
                                    
; +---------------------------------------------------------------------------+
; | PREPARE NEXT TETROMINOE                                                   |
; | place_tetrominoe                                                          |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+       
plr1_prepare_next_tetrominoe                                    
                                    lda #$07                        
                                    jsr get_random_number
                                    tay
                                    jsr load_plr1_tetrominoe
                                    
                                    lda #$0c
                                    sta PLAYER1__TETROMINOE_X
                                    lda #$00
                                    sta PLAYER1__TETROMINOE_Y
                                    
                                    lda #$16
                                    jsr get_random_number
                                    asl
                                    tay
                                    lda table_tetrominoe_appearance, y
                                    sta PLAYER1__CURRENT_TETROMINOE_FACE
                                    lda table_tetrominoe_appearance+1, y
                                    sta PLAYER1__CURRENT_TETROMINOE_COLOR
                                    
                                    rts
                                    

                                    

; +---------------------------------------------------------------------------+
; | COPY_PLR1_tetr_to_predicate                                               |
; | load_plr1_tetrominoe                                                      |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
copy_buffer_to_predicate:
                                    txa
                                    asl
                                    tax
                                    lda table_tetro_buffers, x
                                    sta $30
                                    lda table_tetro_buffers+1, x
                                    sta $31

                                    ldx #$03                                    
                                    
copy_to_pred_loop                   txa
                                    asl
                                    tay
                                    lda ($30),y
                                    clc
                                    adc PREDICATE_TETROMINOE_X
                                    sta PREDICATE_TETROMINOE_CELLS,y
                                    
                                    iny
                                    lda ($30),y
                                    clc
                                    adc PREDICATE_TETROMINOE_Y
                                    sta PREDICATE_TETROMINOE_CELLS,y
                                    
                                    dex
                                    bpl copy_to_pred_loop
                                    rts

; +---------------------------------------------------------------------------+
; | LOAD PLAYER 1 TETROMINOE                                                  |
; | load_plr1_tetrominoe                                                      |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+                                                     
load_plr1_tetrominoe
                                    tya
                                    asl
                                    tay
                                    lda table_tetrominoe_1,y
                                    sta PLAYER1__CURRENT_TETROMINOE_CELLS
                                    lda table_tetrominoe_1+1,y
                                    sta PLAYER1__CURRENT_TETROMINOE_CELLS+1

                                    lda table_tetrominoe_2,y
                                    sta PLAYER1__CURRENT_TETROMINOE_CELLS+2
                                    lda table_tetrominoe_2+1,y
                                    sta PLAYER1__CURRENT_TETROMINOE_CELLS+3

                                    lda table_tetrominoe_3,y
                                    sta PLAYER1__CURRENT_TETROMINOE_CELLS+4
                                    lda table_tetrominoe_3+1,y
                                    sta PLAYER1__CURRENT_TETROMINOE_CELLS+5

                                    lda table_tetrominoe_4,y
                                    sta PLAYER1__CURRENT_TETROMINOE_CELLS+6
                                    lda table_tetrominoe_4+1,y
                                    sta PLAYER1__CURRENT_TETROMINOE_CELLS+7
                                    
                                    rts
                                    
; +---------------------------------------------------------------------------+
; | EVALUATE TARGET POSITION                                                  |
; | evaluate_target_position                                                  |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+                         
evaluate_target_pos:
                                    ldx #$03                                ; Start iteration at last cell
                                    
evaluate_target_pos_loop            txa                                     ; Multiply cell index by two
                                    asl                                     ; to get cell offset in memory
                                    
                                    pha                                     ; Store cell index for later
                                    
                                    tay                                     ; Get cell Y position on screen
                                    lda PREDICATE_TETROMINOE_CELLS+1,y
                                    
                                    asl
                                    tay                                     ; Load screen offset for row                                    
                                    lda table_scr_line,y                    ; and store to zero page pointer $30
                                    sta $30
                                    lda table_scr_line+1,y
                                    sta $31

                                    pla                                     ; Retrieve cell index

                                    tay
                                    lda PREDICATE_TETROMINOE_CELLS,y        ; Get cell X position on screen

                                    tay                                     ; Load from screen offset+x                                     
                                    lda ($30),y
                                    cmp #$20                                ; and verify that it is empty
                                    bne evaluate_target_pos_done           ; otherwise, bail out with X!=0 signifying failure
                                    
                                    dex
                                    bpl evaluate_target_pos_loop 
evaluate_target_pos_done            rts

; +---------------------------------------------------------------------------+
; | DRAW PLAYER TETROMINOE                                                    |
; | draw_tetrominoe                                                           |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+                                        
draw_tetrominoe:
                                    lda PLAYER1__TETROMINOE_Y                    
                                    asl
                                    tax
                                    lda table_scr_line, x
                                    sta $30                                    
                                    lda table_scr_line+1, x
                                    sta $31
                                    
                                    lda PLAYER1__TETROMINOE_X
                                    clc
                                    adc $30
                                    sta $30
                                    sta $24
                                    lda $31
                                    adc #$00
                                    sta $31

                                    clc                                    
                                    adc #$d4
                                    sta $25

                                    ldx #$03
                                    
draw_tetr_cell_loop                 txa
                                    asl
                                    tay
                                    lda PLAYER1__CURRENT_TETROMINOE_CELLS+1,y
                                    beq draw_tetrominoe_no_ymod
                                    
                                    tay
                                    lda #$00
                                    clc
draw_tetr_cell_add_y_offset_loop    adc #$28
                                    dey
                                    bne draw_tetr_cell_add_y_offset_loop
                                    
draw_tetrominoe_no_ymod:            pha
                                    txa
                                    asl
                                    tay
                                    pla
                                    clc                                    
                                    adc PLAYER1__CURRENT_TETROMINOE_CELLS,y
                                    
                                    tay
                                    lda DRAW_TETR__CHAR
                                    sta ($30),y
                                    lda DRAW_TETR__COLOR
                                    sta ($24),y
                                    
                                    dex
                                    bpl draw_tetr_cell_loop

                                    rts
                                   
draw_cell_base_addr                                   
                                    .byte $00, $00

; +---------------------------------------------------------------------------+
; | READ JOYSTICK INPUT                                                       |
; | read_joystick_input                                                       |
; +---------------------------------------------------------------------------+
; |                                                                           |         
; |                                                                           |
; +---------------------------------------------------------------------------+               
read_player_input:
                                    jsr read_joystick_port2
                                    stx PLAYER1__MOVE_X
                                    sty PLAYER1__MOVE_Y

                                    rts
                                                                        
shift_well_contents:                
                                    ldx #$00
                                        
shift_well_contents_loop            dec PLAYER1__FOUND_LINES,x
                                    lda PLAYER1__FOUND_LINES,x
                                    bmi shift_well_next_iter                                   
                                    asl
                                    tay
                                    clc
                                    lda PLAYER1__WELL_OFFSET                                    
                                    adc table_scr_line,y
                                    sta $30
                                    sta $24
                                    lda table_scr_line+1,y
                                    adc #$00
                                    sta $31
                                    clc
                                    adc #$d4
                                    sta $25
                                    
                                    lda $30
                                    clc
                                    adc #$28
                                    sta $fb
                                    sta $28
                                    lda $31
                                    adc #$00
                                    sta $fc
                                    clc
                                    adc #$d4
                                    sta $29
                                    
                                    
shift_line                          ldy #$0a                                                                        
shift_line_loop                     lda ($30),y
                                    sta ($fb),y
                                    lda ($24),y
                                    sta ($28),y
                                    dey
                                    bpl shift_line_loop
                                                                           
                                    jmp shift_well_contents_loop                                   
                                    
shift_well_next_iter                inx
                                    cpx #$04
                                    bne shift_well_contents_loop

                                    rts
                                    
; +---------------------------------------------------------------------------+
; | READ JOYSTICK INPUT                                                       |
; | read_joystick_input                                                       |
; +---------------------------------------------------------------------------+
; |                                                                           |         
; |                                                                           |
; +---------------------------------------------------------------------------+               
get_random_number:                                    
                                    sta RAND_MAX                  
                                    lda $d41b                                    
                                    and #%00111111
random_retry                        cmp RAND_MAX
                                    bcc random_number_ok
                                    sbc RAND_MAX
                                    jmp random_retry 
random_number_ok                    rts


; +---------------------------------------------------------------------------+
; | READ JOYSTICK - PORT 2                                                    |
; | read_joystick_port2                                                       |
; +---------------------------------------------------------------------------+
; |                                                                           |         
; |                                                                           |
; +---------------------------------------------------------------------------+               
read_joystick_port2:                                    
                                    lda $dc00
                                    ldx #$00
                                    ldy #$00
                                    
read_joystick_port2_chk_up          lsr
                                    bcs read_joystick_port2_chk_down
                                    dey
                                    
read_joystick_port2_chk_down        lsr     
                                    bcs read_joystick_port2_chk_left
                                    iny
                                    
read_joystick_port2_chk_left        lsr                                    
                                    bcs read_joystick_port2_chk_right
                                    dex
                                    
read_joystick_port2_chk_right       lsr                                    
                                    bcs read_joystick_done
                                    inx
                                    
read_joystick_done                  rts                                    
                                           
                                    
; +---------------------------------------------------------------------------+
; | DRAW WELL                                                                 |
; | draw_well                                                                 |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+                                           
draw_well:                                    
                                    sty well_tmp_var 
                                    sei       
                                    ldx #$18
draw_well_walls_loop                txa
                                    pha
                                    asl                                    
                                    tax
                                    lda table_scr_line, x
                                    sta $30

                                    lda table_scr_line+1, x
                                    sta $31                
                                    pla
                                    tax

                                    lda #$80
                                    ldy well_tmp_var
draw_well_left_wall                 sta ($30),y                                 
                                    tya
                                    clc
                                    adc #$0b
                                    tay
                                    lda #$80
                                    sta ($30),y
                                                                                                            
                                    dex
                                    bpl draw_well_walls_loop

draw_well_bottom                    dey
                                    lda #$83
draw_well_bottom_loop               sta SCREEN_RAM+960,y
                                    dey
                                    cpy well_tmp_var
                                    bne draw_well_bottom_loop
                                    
                                    ldy well_tmp_var
                                    lda #$82
                                    sta SCREEN_RAM+960,y
                                    tya
                                    clc
                                    adc #$0b
                                    tay
                                    lda #$84
                                    sta SCREEN_RAM+960,y
                                    
                                    cli
                                    rts
                                    
well_tmp_var                        .byte $00                                    


; +---------------------------------------------------------------------------+
; | CLEAR SCREEN                                                              |
; | clear_screen                                                              |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+               
clear_screen:
                                               
                                    ldy #$fb
clrscr_loop                         lda #$20
                                    sta SCREEN_RAM-1,y                  
                                    sta SCREEN_RAM+250,y
                                    sta SCREEN_RAM+500,y
                                    sta SCREEN_RAM+750,y
                                    lda #$01
                                    sta $d7ff,y
                                    sta $d8fa,y
                                    sta $d9f4,y
                                    sta $daee,y
                                    dey
                                    bne clrscr_loop
                                    rts
                                    

table_scr_line:                                      
                                    .word SCREEN_RAM                    
                                    .word SCREEN_RAM+40
                                    .word SCREEN_RAM+80
                                    .word SCREEN_RAM+120
                                    .word SCREEN_RAM+160
                                    .word SCREEN_RAM+200
                                    .word SCREEN_RAM+240
                                    .word SCREEN_RAM+280
                                    .word SCREEN_RAM+320
                                    .word SCREEN_RAM+360
                                    .word SCREEN_RAM+400
                                    .word SCREEN_RAM+440
                                    .word SCREEN_RAM+480
                                    .word SCREEN_RAM+520
                                    .word SCREEN_RAM+560
                                    .word SCREEN_RAM+600
                                    .word SCREEN_RAM+640
                                    .word SCREEN_RAM+680
                                    .word SCREEN_RAM+720
                                    .word SCREEN_RAM+760
                                    .word SCREEN_RAM+800
                                    .word SCREEN_RAM+840
                                    .word SCREEN_RAM+880
                                    .word SCREEN_RAM+920
                                    .word SCREEN_RAM+960
                                    
table_tetro_buffers                                    
                                    .byte <(PLAYER1__CURRENT_TETROMINOE_CELLS), >(PLAYER1__CURRENT_TETROMINOE_CELLS)                                    
                                    .byte <(PLAYER2__CURRENT_TETROMINOE_CELLS), >(PLAYER2__CURRENT_TETROMINOE_CELLS)
                                    .byte <(ROTATE_BUFFER), >(ROTATE_BUFFER)

table_tetrominoe_1:
                                    .byte $01, $01
                                    .byte $01, $01
                                    .byte $02, $01
                                    .byte $01, $00                                    
                                    .byte $01, $01
                                    .byte $02, $01
                                    .byte $02, $01
table_tetrominoe_2:
                                    .byte $02, $01
                                    .byte $01, $02
                                    .byte $02, $02
                                    .byte $01, $01
                                    .byte $02, $01
                                    .byte $03, $01
                                    .byte $01, $02
table_tetrominoe_3:
                                    .byte $01, $02
                                    .byte $01, $03
                                    .byte $02, $03
                                    .byte $01, $02
                                    .byte $02, $02
                                    .byte $01, $02
                                    .byte $02, $02
table_tetrominoe_4:
                                    .byte $02, $02
                                    .byte $02, $03
                                    .byte $01, $03
                                    .byte $01, $03
                                    .byte $03, $02
                                    .byte $02, $02
                                    .byte $03, $02
                                    
                                    
table_tetrominoe_appearance         
                                    .byte $40, $02                                    
                                    .byte $40, $04                                    
                                    .byte $40, $05
                                    .byte $40, $06
                                    .byte $40, $07
                                    .byte $45, $0a
                                    .byte $45, $0e
                                    .byte $45, $0d
                                    .byte $4a, $04                  
                                    .byte $4a, $05
                                    .byte $4a, $06
                                    .byte $4a, $07
                                    .byte $4f, $0c
                                    .byte $4f, $0d
                                    .byte $4f, $0e
                                    .byte $4f, $0f
                                    .byte $4f, $0a
                                    .byte $54, $0c
                                    .byte $54, $0d
                                    .byte $54, $0e
                                    .byte $54, $0f
                                    .byte $54, $0a
                                    

*=$3800
.binary "assets/chars.bin"