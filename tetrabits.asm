.enc "petscii"  ;define an ascii->petscii encoding
.cdef " @", 32  ;characters
.cdef "AZ", $01
.cdef "az", $01

PLAYER1__CURRENT_TETROMINOE_TYPE = $0200

PLAYER1__NEXT_TETROMINOE_TYPE = $0203
PLAYER1__NEXT_TETROMINOE_FACE = $0204
PLAYER1__NEXT_TETROMINOE_COLOR = $0205

PLAYER1__CURRENT_TETROMINOE_CELLS = $0208
PLAYER1__FOUND_LINES = $0210


PLAYER2__CURRENT_TETROMINOE_TYPE = $0230

PLAYER2__NEXT_TETROMINOE_TYPE = $0233
PLAYER2__NEXT_TETROMINOE_FACE = $0234
PLAYER2__NEXT_TETROMINOE_COLOR = $0235

PLAYER2__CURRENT_TETROMINOE_CELLS = $0238
PLAYER2__FOUND_LINES = $0240

PREDICATE_TETROMINOE_CELLS = $0280
PREDICATE_TETROMINOE_X = $0288
PREDICATE_TETROMINOE_Y = $0289

LINE_CHECK_BUFFER = $02a0

GAME_MODE = $02b0       ; Bit #0 - 0=1 Player Mode / 1=2 Player Mode,
                        ; Bit #1 - 0=CPU Player 2 / 1=Human Player 2

;; PLAYER VARIABLES, plr#-indexed

PLR__TETROMINOE_X = $02c0
PLAYER1__TETROMINOE_X = $02c0
PLAYER2__TETROMINOE_X = $02c1

PLR__TETROMINOE_Y = $02c2
PLAYER1__TETROMINOE_Y = $02c2
PLAYER2__TETROMINOE_Y = $02c3

PLR__WELL_OFFSET = $02c4
PLAYER1__WELL_OFFSET = $02c4
PLAYER2__WELL_OFFSET = $02c5

PLR__CURRENT_TETROMINOE_FACE = $02c6
PLAYER1__CURRENT_TETROMINOE_FACE = $02c6
PLAYER2__CURRENT_TETROMINOE_FACE = $02c7

PLR__CURRENT_TETROMINOE_COLOR = $02c8
PLAYER1__CURRENT_TETROMINOE_COLOR = $02c8
PLAYER2__CURRENT_TETROMINOE_COLOR = $02c9

PLR__ANIM_FRAME = $02ca
PLAYER1__ANIM_FRAME = $02ca
PLAYER2__ANIM_FRAME = $02cb

PLR__ANIM_COUNTER = $02cc
PLAYER1__ANIM_COUNTER = $02cc
PLAYER2__ANIM_COUNTER = $02cd

PLR__FALL_COUNTER = $02ce
PLAYER1__FALL_COUNTER = $02ce
PLAYER2__FALL_COUNTER = $02cf

PLR__FALL_SPEED = $02d0
PLAYER1__FALL_SPEED = $02d0
PLAYER2__FALL_SPEED = $02d1

PLR__ACTUAL_FALL_SPEED = $02d2
PLAYER1__ACTUAL_FALL_SPEED = $02d2
PLAYER2__ACTUAL_FALL_SPEED = $02d3

PLR__REPEAT_COUNTER = $02d4
PLAYER1__REPEAT_COUNTER = $02d4
PLAYER2__REPEAT_COUNTER = $02d5

PLR__MOVE_X = $02d6
PLAYER1__MOVE_X = $02d6
PLAYER2__MOVE_X = $02d7

PLR__MOVE_Y = $02d8
PLAYER1__MOVE_Y = $02d8
PLAYER2__MOVE_Y = $02d9

PLR__LINES_TO_CLEAR = $02da
PLAYER1__LINES_TO_CLEAR = $02da
PLAYER2__LINES_TO_CLEAR = $02db


ITERATOR = $fd

ROTATE_BUFFER = $024a

DRAW_TETR__CHAR = $0300
DRAW_TETR__COLOR = $0301

DRAW_TETR__X = $0302
DRAW_TETR__Y = $0303


RAND_MAX = $0304

CURRENT_PLAYER = $0305

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

                                    lda #%00000011
                                    sta GAME_MODE

                                    lda #%00011110
                                    sta $d018

                                    lda $d016
                                    ora #%00010000
                                    sta $d016

                                    lda #$0b
                                    sta $d022
                                    lda #$0c
                                    sta $d023


                                    lda #%01111111                  ; Disable CIA interrupts
                                    sta $dc0d
                                    sta $dd0d



                                    lda #$80
                                    sta $d412                       ; Turn off Voice #3 and set noise enabled
                                    sta $d40f                       ; Set Voice #3 frequency



; +---------------------------------------------------------------------------+
; | START GAME                                                                |
; | start_game                                                                |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
                                    jsr clear_screen

                                    lda #$30
                                    sta PLAYER1__FALL_SPEED
                                    sta PLAYER1__ACTUAL_FALL_SPEED
                                    sta PLAYER2__FALL_SPEED
                                    sta PLAYER2__ACTUAL_FALL_SPEEd

                                    lda PLAYER1__FALL_SPEED
                                    sta PLAYER1__FALL_COUNTER
                                    lda PLAYER2__FALL_SPEED
                                    sta PLAYER2__FALL_COUNTER

                                    lda #$00
                                    sta PLAYER1__MOVE_X
                                    sta PLAYER1__MOVE_Y
                                    sta PLAYER2__MOVE_X
                                    sta PLAYER2__MOVE_Y

                                    lda #$ff
                                    sta PLAYER1__FOUND_LINES
                                    sta PLAYER1__FOUND_LINES+1
                                    sta PLAYER1__FOUND_LINES+2
                                    sta PLAYER1__FOUND_LINES+3
                                    sta PLAYER2__FOUND_LINES
                                    sta PLAYER2__FOUND_LINES+1
                                    sta PLAYER2__FOUND_LINES+2
                                    sta PLAYER2__FOUND_LINES+3

                                    lda GAME_MODE
                                    and #%00000001
                                    asl
                                    tay

                                    lda well_offsets, y
                                    sta PLAYER1__WELL_OFFSET

                                    lda well_offsets+1, y
                                    sta PLAYER2__WELL_OFFSET

                                    ldy PLAYER1__WELL_OFFSET
                                    jsr draw_well

                                    ldx #$00
                                    jsr prepare_next_tetrominoe

                                    lda PLAYER1__CURRENT_TETROMINOE_FACE
                                    sta DRAW_TETR__CHAR
                                    lda PLAYER1__CURRENT_TETROMINOE_COLOR
                                    sta DRAW_TETR__COLOR
                                    ldx #$00
                                    jsr draw_player_tetromino

                                    ldy PLAYER2__WELL_OFFSET
                                    cpy #$00
                                    beq no_plr2_setup
                                    jsr draw_well

                                    ldx #$01
                                    jsr prepare_next_tetrominoe

                                    lda PLAYER2__CURRENT_TETROMINOE_FACE
                                    sta DRAW_TETR__CHAR
                                    lda PLAYER2__CURRENT_TETROMINOE_COLOR
                                    sta DRAW_TETR__COLOR
                                    ldx #$01
                                    jsr draw_player_tetromino

no_plr2_setup                       jmp main_game_loop



; +---------------------------------------------------------------------------+
; | WAIT UNTIL START OF NEXT FRAME                                            |
; | wait_until_next_frame                                                     |
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
                                    jsr wait_until_next_frame

                                    jsr read_player_input
                                    jsr move_tetrominoes
                                    jsr advance_tetrominoes
                                    jsr animate_lines

                                    jmp main_game_loop

; +---------------------------------------------------------------------------+
; | ADVANCE TETROMINOES                                                       |
; | advance_tetrominoes                                                       |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
advance_tetrominoes:
                                    ldx #$00                                ; Advance tetromino for player 1
                                    jsr advance_player_tetromino

                                    lda GAME_MODE                           ; Check game mode to see if there is
                                    and #%00000001                          ; a player 2
                                    beq advance_tetrominoes_end             ; If not, bail out

                                    ldx #$01                                ; Otherwise, advance tetromino for player 2
                                    jsr advance_player_tetromino

advance_tetrominoes_end             rts


; +---------------------------------------------------------------------------+
; | ADVANCE PLAYER TETROMINO                                                  |
; | advance_player_tetromino                                                  |
; +---------------------------------------------------------------------------+
; |                                                                           |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X           - Player # - $00=Player 1, $01=Player 2                       |
; |                                                                           |
; +---------------------------------------------------------------------------+
advance_player_tetromino:
                                    lda PLR__ANIM_FRAME,x               ; Check if an animation is in progress for player
                                    bne no_tetro_advancement            ; in which case, there will be no movement

                                    dec PLR__FALL_COUNTER,x             ; Decrease and check the frame counter for this players
                                    bne no_tetro_advancement            ; downwards movement. if not equal, then no movement this frame

                                    lda PLR__FALL_SPEED, x              ; Reset the frame counter for downwards movement for
                                    sta PLR__FALL_COUNTER, x            ; this player

                                    stx CURRENT_PLAYER                  ; Store away player number for later use

                                    lda #$20                            ; Set tetromino drawing to use black color and
                                    sta DRAW_TETR__CHAR                 ; whitespace to clear position of players tetromino
                                    lda #$00
                                    sta DRAW_TETR__COLOR
                                    jsr draw_player_tetromino           ; Clear tetromino from the screen

                                    ldx CURRENT_PLAYER                  ; Retrieve the player # for the player

                                    lda PLR__TETROMINOE_X,x             ; Load players X position and store it to the
                                    sta PREDICATE_TETROMINOE_X          ; test/predicate block X

                                    lda PLR__TETROMINOE_Y,x             ; Load players Y position and store it to the
                                    sta PREDICATE_TETROMINOE_Y          ; test/predicate block Y, and then increment it by one
                                    inc PREDICATE_TETROMINOE_Y          ; as it's the Y+1 position that we are to evaluate

                                    jsr copy_buffer_to_predicate        ; Copy players tetromino buffer to the test/predicate buffer
                                    jsr evaluate_target_pos             ; and evaluate if there is any collision at the test location

                                    bpl place_tetrominoe
                                    ldx CURRENT_PLAYER
                                    inc PLR__TETROMINOE_Y,x

draw_advanced_tetro                 lda PLR__CURRENT_TETROMINOE_FACE,x
                                    sta DRAW_TETR__CHAR
                                    lda PLR__CURRENT_TETROMINOE_COLOR,x
                                    sta DRAW_TETR__COLOR
                                    jsr draw_player_tetromino

no_tetro_advancement                rts

; +---------------------------------------------------------------------------+
; | PLACE_TETROMINOE                                                          |
; | place_tetrominoe                                                          |
; +---------------------------------------------------------------------------+
; | Place the tetromino at its current location and check for lines.          |
; | If placing this tile results in at least one full line, the player enters |
; | an animation state. If not, the next tile is moved to the current players |
; | tetromino buffer.                                                         |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | CURRENT_PLAYER -            $00=Player 1, $01=Player 2                    |
; +---------------------------------------------------------------------------+

place_tetrominoe
                                    ldx CURRENT_PLAYER
                                    lda PLR__CURRENT_TETROMINOE_FACE,x
                                    sta DRAW_TETR__CHAR
                                    lda PLR__CURRENT_TETROMINOE_COLOR,x
                                    sta DRAW_TETR__COLOR
                                    jsr draw_player_tetromino

                                    ldx CURRENT_PLAYER
                                    jsr check_for_lines
                                    beq place_tetr__no_lines

place_tetr__lines_found             ldx CURRENT_PLAYER
                                    lda #$05
                                    sta PLR__ANIM_FRAME,x
                                    lda ANIM_SPEED
                                    sta PLR__ANIM_COUNTER,x
                                    rts

place_tetr__no_lines                ldx CURRENT_PLAYER
                                    jsr prepare_next_tetrominoe
                                    rts


; +---------------------------------------------------------------------------+
; | APPLY PLAYER MOVEMENT                                                     |
; | move_tetrominoes                                                          |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
move_tetrominoes:
                                    ldx #$00
                                    jsr move_player_tetromino

                                    ldx #$01
                                    lda GAME_MODE
                                    and #%00000001
                                    bne move_player_tetromino

                                    rts

; +---------------------------------------------------------------------------+
; | MOVE PLAYER TETROMINO                                                     |
; | move_player_tetromino                                                     |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+

move_player_tetromino:
                                    lda PLR__ANIM_FRAME, x              ; Check if an animation is in progress for player X,
                                    beq eval_movement_repeat_counter    ; in which case movement is not allowed
                                    rts

eval_movement_repeat_counter        lda PLR__REPEAT_COUNTER,x           ; Check repeat counter, that works like a user input
                                    beq apply_player_movement           ; cooldown. If it is zero, then movement may be applied

                                    dec PLR__REPEAT_COUNTER,x           ; Otherwise, we'll decrease it by one.
                                    rts

apply_player_movement               stx CURRENT_PLAYER                  ; Keep track of the current player

                                    lda #$20                            ; Blank out the tetromino in its current location
                                    sta DRAW_TETR__CHAR                 ; on the screen
                                    lda #$00
                                    sta DRAW_TETR__COLOR
                                    jsr draw_player_tetromino

                                    ldx CURRENT_PLAYER                  ; Return current player # to X register
                                    lda PLR__MOVE_X,x                   ; Check horizontal movement input for this player
                                    beq no_horizontal_move              ; and skip adjustment of tetronimo X if there is none

                                    clc                                 ; Load the X position of the players tetromino and
                                    lda PLR__TETROMINOE_X,x             ; and apply the non-zero X-movement modifier ($01 or $ff)
                                    adc PLR__MOVE_X,x                   ; then store as predicate X
                                    sta PREDICATE_TETROMINOE_X

                                    lda PLR__TETROMINOE_Y,x             ; Copy X position of the players tetromino to
                                    sta PREDICATE_TETROMINOE_Y          ; predicate Y

                                    jsr copy_buffer_to_predicate        ; Check if the target position is blocked or not
                                    jsr evaluate_target_pos

                                    bpl no_horizontal_move              ; If the movement is not allowed, then skip movement

                                    ldx CURRENT_PLAYER
                                    clc                                 ; Otherwise apply the modified X coordinate to
                                    lda PLR__TETROMINOE_X,x             ; the players position
                                    adc PLR__MOVE_X,x
                                    sta PLR__TETROMINOE_X,x

                                    lda #$05                            ; Set the repeat counter after successful movement
                                    sta PLR__REPEAT_COUNTER,x

no_horizontal_move                  ldx CURRENT_PLAYER
                                    lda PLR__MOVE_Y,x                   ; Check if a vertical movement command has been applied
                                    cmp #$01                            ; If Y-modifier is $01, it means down has been pressed
                                    bne reset_fall_speed

                                    lda #$05                            ; As a result of holding down, well increase the rate
                                    sta PLR__FALL_SPEED,x               ; at which the tetromino is falling and reset the
                                    sta PLR__FALL_COUNTER,x             ; repeat counter
                                    sta PLR__REPEAT_COUNTER,x

                                    jmp check_rotate_command

reset_fall_speed                    lda PLR__ACTUAL_FALL_SPEED,x        ; Return teh rate at which the tetromino is falling
                                    sta PLR__FALL_SPEED,x               ; to the normal fall speed, since down is not being held

check_rotate_command                lda PLR__MOVE_Y,x                   ; A position Y-movement modifier means up has not
                                    bpl move_tetr__restore_tetromino    ; been pressed, in which case we are done

                                    jsr rotate_tetromino                ; Otherwise perform rotation of tetrominoe

                                    ldx CURRENT_PLAYER                  ; Set repeat counter to a high value to make sure
                                    lda #$08                            ; the rotation command doesn't spill over to next frame
                                    sta PLR__REPEAT_COUNTER,x

move_tetr__restore_tetromino        lda PLR__CURRENT_TETROMINOE_FACE,x
                                    sta DRAW_TETR__CHAR
                                    lda PLR__CURRENT_TETROMINOE_COLOR,x
                                    sta DRAW_TETR__COLOR
                                    jsr draw_player_tetromino

                                    rts



; +---------------------------------------------------------------------------+
; | ROTATE TETROMINOE                                                         |
; | plr1_rotate_tetrominoe                                                    |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
rotate_tetromino:
                                    jsr set_30_ptr_to_tetro_buffer

                                    ldy #$00
populate_rotate_buffer_loop         lda ($30),y
                                    iny
                                    sta ROTATE_BUFFER,y
                                    lda ($30),y
                                    sta tmp_var
                                    dey
                                    sec
                                    lda #$03
                                    sbc tmp_var
                                    sta ROTATE_BUFFER,y
                                    iny
                                    iny
                                    cpy #$08
                                    bne populate_rotate_buffer_loop

                                    lda PLR__TETROMINOE_X,x
                                    sta PREDICATE_TETROMINOE_X
                                    lda PLR__TETROMINOE_Y,x
                                    sta PREDICATE_TETROMINOE_Y

                                    ldx #$02
                                    jsr copy_buffer_to_predicate
                                    jsr evaluate_target_pos

                                    bpl rotate_exit

                                    ldx CURRENT_PLAYER
                                    jsr set_30_ptr_to_tetro_buffer

                                    ldy #$07
write_rotate_result_loop            lda ROTATE_BUFFER,y
                                    sta ($30),y
                                    dey
                                    bpl write_rotate_result_loop

rotate_exit                         rts

tmp_var                             .byte $00


; +---------------------------------------------------------------------------+
; | ANIMATE_LINES                                                             |
; | animate_lines                                                             |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
animate_lines:
                                    ldx #$00
                                    jsr animate_player_lines

                                    inx
                                    lda GAME_MODE
                                    and #%00000001
                                    bne animate_player_lines

                                    rts
; +---------------------------------------------------------------------------+
; | ANIMATE_PLAYER_LINES                                                      |
; | animate_player_lines                                                      |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
animate_player_lines:
                                    lda PLR__ANIM_FRAME,x               ; Check if animation frame counter is zero.
                                    beq animation_done                  ; If so, there is nothing to animate

                                    dec PLR__ANIM_COUNTER,x             ; Decrease animation delay counter. If it is now
                                    bne animation_done                  ; zero, it is time to animate

                                    lda ANIM_SPEED                      ; Reset animation delay counter
                                    sta PLR__ANIM_COUNTER,x

                                    stx CURRENT_PLAYER

                                    dec PLR__ANIM_FRAME,x               ; Decrease animation frame counter
                                    bne do_animate_lines                ; If it is not zero, move to animate lines

                                    jsr shift_well_contents             ;
                                    ldx CURRENT_PLAYER
                                    jsr prepare_next_tetrominoe

                                    jmp animation_done

do_animate_lines
                                    lda table_found_lines_buffers,x
                                    sta $24
                                    lda table_found_lines_buffers+1,x
                                    sta $25

                                    lda #$03
                                    sta ITERATOR

animate_lines_loop                  ldy ITERATOR

                                    lda ($24),y                       ; Check if row index contains a line to animate
                                    bmi animate_next_line

                                    ldx CURRENT_PLAYER
                                    jsr set_player_line_pointer

                                    clc

                                    ldy #$0a
animate_line_loop                   lda ($30),y
                                    adc #$01
                                    sta ($30),y
                                    dey
                                    bne animate_line_loop

animate_next_line                   dec ITERATOR
                                    bpl animate_lines_loop

animation_done                      rts

set_player_line_pointer:
                                    asl
                                    tay
                                    clc
                                    lda PLR__WELL_OFFSET,x
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
; | Check a players well for new lines. Must be called after a tetromino has  |
; | been placed, and before the players next tetronimo is activated, as this  |
; | routine uses the shape and coordinates of the players current tetronimo   |
; | to determine which rows to check.                                         |
; |                                                                           |
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X               - Player. $00=Player 1, $01=Player 2                      |
; +---------------------------------------------------------------------------+
; | Affects:                                                                  |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
; | Returns:                                                                  |
; +---------------------------------------------------------------------------+
; | PLR_LINES_TO_CLEAR,x    - Number of lines found                           |
; |                                                                           |
; | PLAYERx__LINES_FOUND    - 4 byte buffer, where each byte contains the     |
; |                           row index of a found line, or $ff for no line.  |
; |                           This buffer is not ordered, so all 4 bytes must |
; |                           be evaluated.                                   |
; +---------------------------------------------------------------------------+
check_for_lines:
                                    lda #$00                                    ; Set number of found lines to zero.
                                    sta PLR__LINES_TO_CLEAR,x

                                    lda PLR__WELL_OFFSET,x                      ; Store well column offset on screen
                                    sta tmp_well_offset                         ; to temporary variable

                                    lda table_found_lines_buffers,x
                                    sta $24
                                    lda table_found_lines_buffers+1,x
                                    sta $25

                                    ldy #$03
                                    lda #$ff                                    ; Reset temporary line check buffer
clear_lcheck_buf_loop               sta LINE_CHECK_BUFFER,y                     ; and player line info
                                    sta ($24),y
                                    dey
                                    bpl clear_lcheck_buf_loop

                                    jsr set_30_ptr_to_tetro_buffer              ; Set $30-$31 pointer to player tetro buffer

                                    stx CURRENT_PLAYER

                                    ldx #$03
populate_lcheck_buffer_loop         txa                                         ; Pull Y coordinates from the just placed
                                    asl                                         ; tetromino - new lines can only have been
                                    tay                                         ; formed there.

                                    iny
                                    lda ($30),y                                 ; Load Y-coordinate from tetromino cell
                                    tay
                                    lda #$01                                    ; Mark Y-coordinate in line check buffer
                                    sta LINE_CHECK_BUFFER,y

                                    dex
                                    bpl populate_lcheck_buffer_loop

                                    ldx #$03
check_for_lines_loop                lda LINE_CHECK_BUFFER,x                     ; First check that this row is included in
                                    bmi not_a_line                              ; tetronimo shape. If not, skip

                                    txa                                         ; Add tetronimo block Y to cell Y
                                    clc                                         ; to get actual screen row
                                    ldy CURRENT_PLAYER
                                    adc PLR__TETROMINOE_Y,y
                                    sta tmp_cell_y
                                    asl                                         ; Multiply screen row with 2 to get index
                                    tay                                         ; in screen line table

                                    clc
                                    lda tmp_well_offset                         ; Load screen RAM offset from table
                                    adc table_scr_line,y                        ; and add well offset to make pointer $30
                                    sta $30                                     ; contain the address of the left edge of
                                    lda table_scr_line+1,y                      ; the well
                                    adc #$00
                                    sta $31

                                    ldy #$0a                                    ; Load offset to right edge of well

check_line_loop                     lda ($30),y                                 ; Check if screen position is empty
                                    cmp #$20
                                    beq not_a_line                              ; if so, then this line does not contain a line

                                    dey                                         ; otherwise, keep checking the rest of the line
                                    bne check_line_loop

                                    txa
                                    tay
                                    lda tmp_cell_y
                                    sta ($24), y
                                    ldx CURRENT_PLAYER
                                    inc PLR__LINES_TO_CLEAR, x

                                    tya
                                    tax

not_a_line                          dex
                                    bpl check_for_lines_loop

                                    ldx CURRENT_PLAYER
                                    lda PLR__LINES_TO_CLEAR, x
                                    rts

tmp_well_offset                     .byte $00
tmp_cell_y                          .byte $00

; +---------------------------------------------------------------------------+
; | PREPARE NEXT TETROMINOE                                                   |
; | place_tetrominoe                                                          |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X           - Player index. $00=Player 1, $01=Player 2                    |
; +---------------------------------------------------------------------------+
prepare_next_tetrominoe
                                    lda #$07                    ; Get a random tetromino index
                                    jsr get_random_number
                                    tay
                                    jsr set_player_tetrominoe   ; Copy tetromino shape to player #X buffer

                                    lda #$04                    ; Set tetromino coordinates to top of players well
                                    adc PLR__WELL_OFFSET,x
                                    sta PLR__TETROMINOE_X,x
                                    lda #$00
                                    sta PLR__TETROMINOE_Y,x

                                    lda #$16                    ; Randomize appearance (tile and color)
                                    jsr get_random_number
                                    asl
                                    tay
                                    lda table_tetrominoe_appearance, y
                                    sta PLR__CURRENT_TETROMINOE_FACE,x
                                    lda table_tetrominoe_appearance+1, y
                                    sta PLR__CURRENT_TETROMINOE_COLOR,x

                                    rts




; +---------------------------------------------------------------------------+
; | COPY TETROMINO BUFFER TO PREDICATE BUFFER                                 |
; | copy_buffer_to_predicate                                                  |
; +---------------------------------------------------------------------------+
; |                                                                           |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X               - Buffer index. $00=Player 1 buffer                       |
; |                                 $01=Player 2 buffer                       |
; +---------------------------------------------------------------------------+
; | Affects:                                                                  |
; +---------------------------------------------------------------------------+
; | X               - Will be overwritten by loop counter                     |
; +---------------------------------------------------------------------------+
copy_buffer_to_predicate:
                                    txa                             ; Multiply X/buffer index by two to get offset
                                    asl                             ; in 16-bit buffer table
                                    tax
                                    lda table_tetro_buffers, x      ; Store buffer pointer to $30-$31
                                    sta $30
                                    lda table_tetro_buffers+1, x
                                    sta $31

                                    ldx #$03

copy_to_pred_loop                   txa                             ; Copy bytes from buffer to predicate buffer and
                                    asl                             ; add the predicate coordinates to the offset while
                                    tay                             ; doing so
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
; | SET PLAYER TETROMINOE                                                     |
; | set_player_tetrominoe                                                     |
; +---------------------------------------------------------------------------+
; | Loads tetrominoe shape data and stores it to a players current tetromino  |
; | shape buffer, effectively setting the shape/layout of that players        |
; | current block/tetrominoe                                                  |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X           - Player to set tetrominoe shape for, 0=plr1 / 1=plr2         |
; | Y           - Tetromino tile index/ID. Valid values are 00-06             |
; +---------------------------------------------------------------------------+
; | Affects:                                                                  |
; +---------------------------------------------------------------------------+
; | $24-$25     - Will point to the first byte of tetrominoe Y in tetrominoe  |
; |               data area.                                                  |
; | $30-31      - Will point to the first byte of tetrominoe buffer of player |
; |               X.                                                          |
; +---------------------------------------------------------------------------+
set_player_tetrominoe
                                    tya                                 ; Temporarily store tile index to the stack
                                    pha

                                    txa                                 ; Multiply player # by 2 to get index in 16-bit
                                    asl                                 ; tetrominoe buffer table
                                    tay
                                    lda table_tetro_buffers,y           ; Store pointer to players tetrominoe buffer to
                                    sta $30                             ; $30-$31
                                    lda table_tetro_buffers+1,y
                                    sta $31

                                    pla                                 ; Return tile index to Y register
                                    tay

                                    lda #<tetromino_data                ; Store beginning of tetrominoe data are
                                    sta $24                             ; to pointer $24-25
                                    lda #>tetromino_data
                                    sta $25

                                    tya                                 ; Multiply X by eight to get offset of
                                    asl                                 ; tetromino data for the argument tetromino
                                    asl                                 ; and add it to the pointer
                                    asl
                                    clc
                                    adc $24
                                    sta $24
                                    lda $25
                                    adc #$00                            ; Make sure to add carry to the pointer msb
                                    sta $25                             ; in case of page wrap

                                    ldy #$07

set_tetromino_loop                  lda ($24),y                         ; Copy data to buffer
                                    sta ($30),y
                                    dey
                                    bpl set_tetromino_loop

                                    rts

; +---------------------------------------------------------------------------+
; | EVALUATE TARGET POSITION                                                  |
; | evaluate_target_position                                                  |
; +---------------------------------------------------------------------------+
; | Examines the screen contents at the four positions that make up a         |
; | tetromino block and determines if all four are empty or not. Evaluation   |
; | stops immediately if a non-empty screen position is found.                |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | PREDICATE_TETROMINO_CELLS - 8 bytes large buffer containing four absolute |
; |                             X and Y coordinates.                          |
; +---------------------------------------------------------------------------+
; | Affects:                                                                  |
; +---------------------------------------------------------------------------+
; | $30-$31                   - Used as pointer to Screen RAM row             |
; +---------------------------------------------------------------------------+
; | Returns:                                                                  |
; +---------------------------------------------------------------------------+
; | X                         - $ff     = all positions are empty.            |
; |                             $00-$03 = blocking content found at           |
; |                                       coordinate                          |
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
; | DRAW PLAYER TETROMINO                                                     |
; | draw_player_tetromino                                                     |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
; | Arguments                                                                 |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
draw_player_tetromino:
                                    lda PLR__TETROMINOE_X,x
                                    sta DRAW_TETR__X

                                    lda PLR__TETROMINOE_Y,x
                                    sta DRAW_TETR__Y

                                    txa
                                    asl
                                    tay
                                    lda table_tetro_buffers,y
                                    sta draw_tetr_buf_read_x+1
                                    sta draw_tetr_buf_read_y+1
                                    inc draw_tetr_buf_read_y+1
                                    lda table_tetro_buffers+1,y
                                    sta draw_tetr_buf_read_x+2
                                    sta draw_tetr_buf_read_y+2

                                    jmp draw_tetrominoe


; +---------------------------------------------------------------------------+
; | DRAW TETROMINOE                                                           |
; | draw_tetrominoe                                                           |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
; | Arguments                                                                 |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
draw_tetrominoe:
                                    lda DRAW_TETR__Y                            ; Set up pointer to row in Screen RAM for
                                    asl                                         ; draw coordinate Y
                                    tax
                                    lda table_scr_line, x
                                    sta $30
                                    lda table_scr_line+1, x
                                    sta $31

                                    lda DRAW_TETR__X                            ; Adjust for draw coordinate X and
                                    clc                                         ; set up Color RAM pointer
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
draw_tetr_buf_read_y                lda PLAYER1__CURRENT_TETROMINOE_CELLS+1,y
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
draw_tetr_buf_read_x                adc PLAYER1__CURRENT_TETROMINOE_CELLS,y

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
; | SHIFT WELL CONTENTS                                                       |
; | shift_well_contents                                                       |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
shift_well_contents:
                                    lda table_found_lines_buffers,x
                                    sta $2c
                                    lda table_found_lines_buffers+1,x
                                    sta $2d

                                    lda PLR__WELL_OFFSET,x
                                    sta tmp_well_offset

                                    stx CURRENT_PLAYER

                                    ldx #$00
shift_well_contents_loop            txa
                                    tay
                                    lda ($2c),y
                                    sec
                                    sbc #$01
                                    sta ($2c),y

;                                    dec PLAYER1__FOUND_LINES,x
;                                    lda PLAYER1__FOUND_LINES,x
                                    bmi shift_well_next_iter

                                    asl
                                    tay
                                    clc
                                    lda tmp_well_offset
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
; | DRAW WELL / PLAYFIELD                                                     |
; | draw_well                                                                 |
; +---------------------------------------------------------------------------+
; | Draws one well / playfield to Screen RAM.                                 |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | Y          - The screen column to begin drawing at, ie the X value of     |
; |              of the left border.                                          |
; +---------------------------------------------------------------------------+
; | Affects:                                                                  |
; +---------------------------------------------------------------------------+
; | $30, $31                                                                  |
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


set_30_ptr_to_tetro_buffer:
                                    txa
                                    asl
                                    tay
                                    lda table_tetro_buffers,y
                                    sta $30
                                    lda table_tetro_buffers+1,y
                                    sta $31

                                    rts

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

; +---------------------------------------------------------------------------+
; | GET A RANDOM NUMBER                                                       |
; | get_random_number                                                         |
; +---------------------------------------------------------------------------+
; | Produces a random number between 00 and the argument A, using the noise   |
; | waveform of SID voice #3                                                  |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | A -           The max allowed random number                               |
; +---------------------------------------------------------------------------+
; | Returns:                                                                  |
; +---------------------------------------------------------------------------+
; | A -           The generated random number                                 |
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

                                    lda GAME_MODE
                                    and #%00000001
                                    beq read_player_input_exit

                                    lda GAME_MODE
                                    and #%00000010
                                    beq determine_cpu_move

                                    jsr read_joystick_port1
                                    stx PLAYER2__MOVE_X
                                    sty PLAYER2__MOVE_Y

read_player_input_exit              rts

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
; | READ JOYSTICK - PORT 1                                                    |
; | read_joystick_port1                                                       |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
read_joystick_port1:
                                    lda $dc01
                                    ldx #$00
                                    ldy #$00

read_joystick_port1_chk_up          lsr
                                    bcs read_joystick_port1_chk_down
                                    dey

read_joystick_port1_chk_down        lsr
                                    bcs read_joystick_port1_chk_left
                                    iny

read_joystick_port1_chk_left        lsr
                                    bcs read_joystick_port1_chk_right
                                    dex

read_joystick_port1_chk_right       lsr
                                    bcs read_joystick1_done
                                    inx

read_joystick1_done                 rts


; +---------------------------------------------------------------------------+
; | DETERMINE COMPUTER PLAYER MOVE                                            |
; | determine_cpu_move                                                        |
; +---------------------------------------------------------------------------+
; |                                                                           |
; |                                                                           |
; +---------------------------------------------------------------------------+
determine_cpu_move:
                                    rts

well_offsets
                                    .byte $0b, $00
                                    .byte $08, $17

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

table_found_lines_buffers           .byte <(PLAYER1__FOUND_LINES), >(PLAYER1__FOUND_LINES)
                                    .byte <(PLAYER2__FOUND_LINES), >(PLAYER2__FOUND_LINES)


tetromino_data:
tetromino_1                         .byte $01, $01
                                    .byte $02, $01
                                    .byte $01, $02
                                    .byte $02, $02

tetromino_2                         .byte $01, $01
                                    .byte $01, $02
                                    .byte $01, $03
                                    .byte $02, $03

tetromino_3                         .byte $02, $01
                                    .byte $02, $02
                                    .byte $02, $03
                                    .byte $01, $03

tetromino_4                         .byte $01, $00
                                    .byte $01, $01
                                    .byte $01, $02
                                    .byte $01, $03

tetromino_5                         .byte $01, $01
                                    .byte $02, $01
                                    .byte $02, $02
                                    .byte $03, $02

tetromino_6                         .byte $02, $01
                                    .byte $03, $01
                                    .byte $01, $02
                                    .byte $02, $02

tetromino_7                         .byte $02, $01
                                    .byte $01, $02
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