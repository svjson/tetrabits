.enc "petscii"  ;define an ascii->petscii encoding
.cdef " @", 32  ;characters
.cdef "AZ", $01
.cdef "az", $01


PLAYER1__CURRENT_TETROMINO_CELLS = $0200

PLAYER2__CURRENT_TETROMINO_CELLS = $0210

PLAYER1__FOUND_LINES = $0218
PLAYER2__FOUND_LINES = $021c

PREDICATE_TETROMINO_CELLS = $0220
ROTATE_BUFFER = $0228

LINE_CHECK_BUFFER = $0230

PREDICATE_TETROMINO_X = $0234
PREDICATE_TETROMINO_Y = $0235

DRAW_TETR__CHAR = $0236
DRAW_TETR__COLOR = $0237

DRAW_TETR__X = $0238
DRAW_TETR__Y = $0239

RAND_MAX = $023a

CURRENT_PLAYER = $023b

GAME_MODE = $023c       ; Bit #0 - 0=1 Player Mode / 1=2 Player Mode,
                        ; Bit #1 - 0=CPU Player 2 / 1=Human Player 2

;; PLAYER VARIABLES, plr#-indexed

PLR__TETROMINO_X = $0240
PLAYER1__TETROMINO_X = $0240
PLAYER2__TETROMINO_X = $0241

PLR__TETROMINO_Y = $0242
PLAYER1__TETROMINO_Y = $0242
PLAYER2__TETROMINO_Y = $0243

PLR__WELL_OFFSET = $0244
PLAYER1__WELL_OFFSET = $0244
PLAYER2__WELL_OFFSET = $0245

PLR__CURRENT_TETROMINO_FACE = $0246
PLAYER1__CURRENT_TETROMINO_FACE = $0246
PLAYER2__CURRENT_TETROMINO_FACE = $0247

PLR__CURRENT_TETROMINO_COLOR = $0248
PLAYER1__CURRENT_TETROMINO_COLOR = $0248
PLAYER2__CURRENT_TETROMINO_COLOR = $0249

PLR__ANIM_FRAME = $024a
PLAYER1__ANIM_FRAME = $024a
PLAYER2__ANIM_FRAME = $024b

PLR__ANIM_COUNTER = $024c
PLAYER1__ANIM_COUNTER = $024c
PLAYER2__ANIM_COUNTER = $024d

PLR__FALL_COUNTER = $024e
PLAYER1__FALL_COUNTER = $024e
PLAYER2__FALL_COUNTER = $024f

PLR__FALL_SPEED = $0250
PLAYER1__FALL_SPEED = $0250
PLAYER2__FALL_SPEED = $0251

PLR__ACTUAL_FALL_SPEED = $0252
PLAYER1__ACTUAL_FALL_SPEED = $0252
PLAYER2__ACTUAL_FALL_SPEED = $0253

PLR__REPEAT_COUNTER = $0254
PLAYER1__REPEAT_COUNTER = $0254
PLAYER2__REPEAT_COUNTER = $0255

PLR__MOVE_X = $0256
PLAYER1__MOVE_X = $0256
PLAYER2__MOVE_X = $0257

PLR__MOVE_Y = $0258
PLAYER1__MOVE_Y = $0258
PLAYER2__MOVE_Y = $0259

PLR__LINES_TO_CLEAR = $025a
PLAYER1__LINES_TO_CLEAR = $025a
PLAYER2__LINES_TO_CLEAR = $025b

PLR__NEXTBOX_OFFSET = $025c
PLAYER1__NEXTBOX_OFFSET = $025c
PLAYER2__NEXTBOX_OFFSET = $025d

PLR__NEXT_TETROMINO_TYPE = $025e
PLAYER1__NEXT_TETROMINO_TYPE = $025e
PLAYER2__NEXT_TETROMINO_TYPE = $025f

PLR__NEXT_TETROMINO_FACE = $0260
PLAYER1__NEXT_TETROMINO_FACE = $0260
PLAYER2__NEXT_TETROMINO_FACE = $0261

PLR__NEXT_TETROMINO_COLOR = $0262
PLAYER1__NEXT_TETROMINO_COLOR = $0262
PLAYER2__NEXT_TETROMINO_COLOR = $0263

PLR__CURRENT_TETROMINO_TYPE = $0264
PLAYER1__CURRENT_TETROMINO_TYPE = $0264
PLAYER2__CURRENT_TETROMINO_TYPE = $0265

PLR__TETROMINO_ROT_MOD = $0266
PLAYER1__CURRENT_TETROMINO_ROT_MOD = $0266
PLAYER2__CURRENT_TETROMINO_ROT_MOD = $0267

PLR__LINES_LO = $0270
PLAYER1__LINES_LO = $0270
PLAYER2__LINES_LO = $0271

PLR__LINES_HI = $0272
PLAYER1__LINES_HI = $0272
PLAYER2__LINES_HI = $0273

PLR__CURRENT_ROTATION = $0274
PLAYER1__CURRENT_ROTATION = $0274
PLAYER2__CURRENT_ROTATION = $0275

PLR__ACTION_STATUS = $0276
PLAYER1__ACTION_STATUS = $0276
PLAYER2__ACTION_STATUS = $0277

CPU__WELL_FLOOR = $0290

CPU__STATE = $02b0

CPU__TARGET_X = $02b1
CPU__TARGET_ROTATION = $02b2

CPU__COLUMN_ALT_ORDER = $02b3


CPU__COLUMN_ALT_INDEX = $02c0
CPU__COLUMN_ROT_INDEX = $02c1

CPU__DATA_BUFFER = $02d0


ITERATOR = $fd
SCREEN_RAM = $0400
ANIM_SPEED = #$03

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

                                    lda #%00000001
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
; | Initializes all player variables, draws wells and next-boxes.             |
; | Set up of player 2 variables depends on if the 2 player-mode bit is set   |
; | in GAME_MODE.                                                             |
; +---------------------------------------------------------------------------+
; | Depends on:                                                               |
; +---------------------------------------------------------------------------+
; | GAME_MODE                                                                 |
; +---------------------------------------------------------------------------+
                                    jsr clear_screen

                                    lda #$20
                                    sta PLAYER1__FALL_SPEED
                                    sta PLAYER1__ACTUAL_FALL_SPEED
                                    sta PLAYER2__FALL_SPEED
                                    sta PLAYER2__ACTUAL_FALL_SPEED

                                    lda PLAYER1__FALL_SPEED
                                    sta PLAYER1__FALL_COUNTER
                                    lda PLAYER2__FALL_SPEED
                                    sta PLAYER2__FALL_COUNTER

                                    lda #$00
                                    sta PLAYER1__MOVE_X
                                    sta PLAYER1__MOVE_Y
                                    sta PLAYER2__MOVE_X
                                    sta PLAYER2__MOVE_Y
                                    sta PLAYER1__LINES_HI
                                    sta PLAYER1__LINES_LO
                                    sta PLAYER2__LINES_HI
                                    sta PLAYER2__LINES_LO

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

                                    lda nextbox_offsets, y
                                    sta PLAYER1__NEXTBOX_OFFSET

                                    lda nextbox_offsets+1, y
                                    sta PLAYER2__NEXTBOX_OFFSET

                                    ldy PLAYER1__WELL_OFFSET
                                    jsr draw_well

                                    ldy PLAYER1__NEXTBOX_OFFSET
                                    jsr draw_nextbox

                                    ldx PLAYER1__NEXTBOX_OFFSET
                                    ldy #$09
                                    lda #<text_LINES
                                    sta $24
                                    lda #>text_LINES
                                    sta $25
                                    jsr print_text

                                    ldx #$00
                                    jsr print_player_line_count
                                    jsr prepare_next_tetromino
                                    jsr roll_new_tetromino

                                    lda PLAYER1__CURRENT_TETROMINO_FACE
                                    sta DRAW_TETR__CHAR
                                    lda PLAYER1__CURRENT_TETROMINO_COLOR
                                    sta DRAW_TETR__COLOR
                                    ldx #$00
                                    jsr draw_player_tetromino

                                    ldy PLAYER2__WELL_OFFSET
                                    cpy #$00
                                    beq game_init_done

                                    jsr draw_well

                                    ldy PLAYER2__NEXTBOX_OFFSET
                                    jsr draw_nextbox

                                    ldx PLAYER2__NEXTBOX_OFFSET
                                    ldy #$09
                                    lda #<text_LINES
                                    sta $24
                                    lda #>text_LINES
                                    sta $25
                                    jsr print_text

                                    ldx #$01
                                    jsr print_player_line_count
                                    jsr prepare_next_tetromino
                                    jsr roll_new_tetromino

                                    lda PLAYER2__CURRENT_TETROMINO_FACE
                                    sta DRAW_TETR__CHAR
                                    lda PLAYER2__CURRENT_TETROMINO_COLOR
                                    sta DRAW_TETR__COLOR
                                    ldx #$01
                                    jsr draw_player_tetromino

                                    lda GAME_MODE
                                    and #%00000010
                                    bne game_init_done

                                    ldy #$09
                                    lda #$18
init_cpu_well_tracking              sta CPU__WELL_FLOOR, y
                                    dey
                                    bpl init_cpu_well_tracking

game_init_done                      jmp main_game_loop



; +---------------------------------------------------------------------------+
; | WAIT UNTIL START OF NEXT FRAME                                            |
; | wait_until_next_frame                                                     |
; +---------------------------------------------------------------------------+
; | Keeps program in a loop until raster line $001 by polling the VIC raster  |
; | registers.                                                                |
; +---------------------------------------------------------------------------+
wait_until_next_frame:
                                    lda $d011                       ; Loop until the raster line hi-bit is cleared
                                    and #%10000000
                                    bne wait_until_next_frame

                                    lda $d012                       ; Loop until the raster line lo-byte is $01
                                    cmp #$01
                                    bne wait_until_next_frame
                                    rts

; +---------------------------------------------------------------------------+
; | MAIN GAME LOOP                                                            |
; | main_game_loop                                                            |
; +---------------------------------------------------------------------------+
; | Main loop that cycles through the four main steps of a game frame.        |
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
; | Calls advance_player_tetromino for each player in turn                    |
; +---------------------------------------------------------------------------+
; | Depends on:                                                               |
; +---------------------------------------------------------------------------+
; | GAME_MODE           - Controls if advance_player_tetromino is to be       |
; |                       for Player 2 in addition to for Player 1            |
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
; | Advances the tetromino of Player X downwards through the well, and        |
; | checks for collision. If a collision is detected during downwards         |
; | movement, then placement of tetromino and checking for lines is initiated |
; |                                                                           |
; | Collision detection is done by removing the tetromino from the screen,    |
; | copying the coordinates and contents of the players tetromino buffer and  |
; | coordinates(with Y+=1) to the predicate ditos. If any of the screen       |
; | positions that make up the new location of the tetromino, then a          |
; | collision is detected and execution branches out to place_tetromino.      |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X                   - Player # - $00=Player 1, $01=Player 2               |
; +---------------------------------------------------------------------------+
; | Depends on:                                                               |
; +---------------------------------------------------------------------------+
; | PLR__ANIM_FRAME,X   - Any non-zero value cancels execution                |
; |                                                                           |
; | PLR__FALL_SPEED,X   - Determines the value that PLR__FALL_COUNTER,X is    |
; |                       reset to after reaching zero. In other words, with  |
; |                       interval downwards advancement happens.             |
; |                                                                           |
; | PLR__ANIM_FRAME,X   - Any non-zero value cancels execution                |
; +---------------------------------------------------------------------------+
; | Affects:                                                                  |
; +---------------------------------------------------------------------------+
; | X                   - Is overwritten                                      |
; |                                                                           |
; | PLR__FALL_COUNTER,X - Is decreased by one (and reset if it reaches 0)     |
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

                                    lda PLR__TETROMINO_X,x              ; Load players X position and store it to the
                                    sta PREDICATE_TETROMINO_X           ; test/predicate block X

                                    lda PLR__TETROMINO_Y,x              ; Load players Y position and store it to the
                                    sta PREDICATE_TETROMINO_Y           ; test/predicate block Y, and then increment it by one
                                    inc PREDICATE_TETROMINO_Y           ; as it's the Y+1 position that we are to evaluate

                                    jsr copy_buffer_to_predicate        ; Copy players tetromino buffer to the test/predicate buffer
                                    jsr evaluate_target_pos             ; and evaluate if there is any collision at the test location
                                    bpl place_tetromino                 ; Branch out to place_tetromino if a collision has occurred

                                    ldx CURRENT_PLAYER                  ; Otherwise, increase the Y coordinate of the tetromino
                                    inc PLR__TETROMINO_Y,x

advance__draw_tetro_in_new_pos      lda PLR__CURRENT_TETROMINO_FACE,x   ; And draw it back to the screen
                                    sta DRAW_TETR__CHAR
                                    lda PLR__CURRENT_TETROMINO_COLOR,x
                                    sta DRAW_TETR__COLOR
                                    jsr draw_player_tetromino

no_tetro_advancement                rts

; +---------------------------------------------------------------------------+
; | PLACE_TETROMINO                                                           |
; | place_tetromino                                                           |
; +---------------------------------------------------------------------------+
; | Place the tetromino at its current location and check for lines.          |
; | If placing this tile results in at least one full line, the player enters |
; | an animation state. If not, the next tile is moved to the current players |
; | tetromino buffer.                                                         |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | CURRENT_PLAYER            - $00=Player 1, $01=Player 2                    |
; +---------------------------------------------------------------------------+
; | Affects:                                                                  |
; +---------------------------------------------------------------------------+
; | PLR__ANIM_FRAME           - Will be set if any lines are detected         |
; +---------------------------------------------------------------------------+
place_tetromino
                                    ldx CURRENT_PLAYER

                                    lda PLR__CURRENT_TETROMINO_FACE,x       ; Draw the tetromino in its new and now fixed
                                    sta DRAW_TETR__CHAR                     ; location
                                    lda PLR__CURRENT_TETROMINO_COLOR,x
                                    sta DRAW_TETR__COLOR
                                    jsr draw_player_tetromino

                                    ldx CURRENT_PLAYER                      ; Check if current player is Player 2
                                    beq place_tetro_check_lines             ; if not, proceed to check for lines

                                    lda GAME_MODE                           ; Check if player 2 is CPU
                                    and #%00000011
                                    cmp #$01
                                    bne place_tetro_check_lines             ; If not, proceed to check for lines

                                    jsr update_cpu_well_model               ; Player 2 is CPU, so update the memory model of the well bottom

                                    ldx CURRENT_PLAYER                      ; Restore Player index to X

place_tetro_check_lines             jsr check_for_lines                     ; Check if any lines have been formed after
                                    beq place_tetr__no_lines                ; placing the tetromino

place_tetr__lines_found             ldx CURRENT_PLAYER                      ; If so, initiate the animation that will
                                    lda #$05                                ; remove the lines from the screen.
                                    sta PLR__ANIM_FRAME,x
                                    lda ANIM_SPEED
                                    sta PLR__ANIM_COUNTER,x
                                    rts

place_tetr__no_lines                ldx CURRENT_PLAYER                      ; Otherwise, just roll the next tetromino into
                                    jsr roll_new_tetromino                  ; into the current buffers and randomize a new next.
                                    rts

; +---------------------------------------------------------------------------+
; | APPLY PLAYER MOVEMENT                                                     |
; | move_tetrominoes                                                          |
; +---------------------------------------------------------------------------+
; | Calls move_player_tetromino for each player in turn.                      |
; +---------------------------------------------------------------------------+
; | Depends on:                                                               |
; +---------------------------------------------------------------------------+
; | GAME_MODE           - Controls if move_player_tetromino is to be          |
; |                       for Player 2 in addition to for Player 1            |
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
; | Evaluates if any player actions have been signalled, and performs them.   |
; | The actions correspond to joystick movement, but may also have been set   |
; | by the CPU player routines                                                |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X                   - Player # - $00=Player 1, $01=Player 2               |
; +---------------------------------------------------------------------------+
; | Depends on:                                                               |
; +---------------------------------------------------------------------------+
; | PLR__MOVE_X,x       - Movement in horizontal direction                    |
; |                        $ff = Left                                         |
; |                        $00 = No horizontal movement                       |
; |                        $01 = Right                                        |
; |                                                                           |
; | PLR__MOVE_Y,x       - Rotation or speed up downwards advancement          |
; |                        $ff = Rotate tetromino                             |
; |                        $00 = No action                                    |
; |                        $01 = Speed up advancement                         |
; |                                                                           |
; | PLR__ANIM_FRAME,X   - Any non-zero value cancels execution                |
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
                                    beq no_horizontal_move              ; and skip adjustment of tetromino X if there is none

                                    clc                                 ; Load the X position of the players tetromino and
                                    lda PLR__TETROMINO_X,x              ; and apply the non-zero X-movement modifier ($01 or $ff)
                                    adc PLR__MOVE_X,x                   ; then store as predicate X
                                    sta PREDICATE_TETROMINO_X

                                    lda PLR__TETROMINO_Y,x              ; Copy X position of the players tetromino to
                                    sta PREDICATE_TETROMINO_Y           ; predicate Y

                                    jsr copy_buffer_to_predicate        ; Check if the target position is blocked or not
                                    jsr evaluate_target_pos

                                    bpl no_horizontal_move              ; If the movement is not allowed, then skip movement

                                    ldx CURRENT_PLAYER

                                    clc                                 ; Otherwise apply the modified X coordinate to
                                    lda PLR__TETROMINO_X,x              ; the players position
                                    adc PLR__MOVE_X,x
                                    sta PLR__TETROMINO_X,x

                                    lda #$03                            ; Set the repeat counter after successful movement
                                    sta PLR__REPEAT_COUNTER,x

                                    lda #$00
                                    sta PLR__ACTION_STATUS,x             ; Clear player action status

no_horizontal_move                  ldx CURRENT_PLAYER
                                    lda PLR__MOVE_Y,x                   ; Check if a vertical movement command has been applied
                                    cmp #$01                            ; If Y-modifier is $01, it means down has been pressed
                                    bne reset_fall_speed

                                    lda #$03                            ; As a result of holding down, well increase the rate
                                    sta PLR__FALL_SPEED,x               ; at which the tetromino is falling and reset the
                                    sta PLR__FALL_COUNTER,x             ; repeat counter
                                    lda #$03
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

move_tetr__restore_tetromino        lda PLR__CURRENT_TETROMINO_FACE,x   ; Draw the tetromino back to the screen.
                                    sta DRAW_TETR__CHAR
                                    lda PLR__CURRENT_TETROMINO_COLOR,x
                                    sta DRAW_TETR__COLOR
                                    jsr draw_player_tetromino

                                    rts

; +---------------------------------------------------------------------------+
; | ROTATE TETROMINO                                                          |
; | rotate_tetromino                                                          |
; +---------------------------------------------------------------------------+
; | Performs clockwise rotation of the current tetromino of Player X if       |
; | possible, depending on the surrounding screen area. This is done in much  |
; | the same way as checking if horizontal or vertical movement is possible   |
; | by first rotating the piece in a temporary buffer and then checking the   |
; | screen positions of the new shape.                                        |
; |                                                                           |
; | If any of the four tetromino cell positions are non-empty, the action is  |
; | cancelled.                                                                |
; |                                                                           |
; | Rotation is done by transforming each cell coordinate like so:            |
; | RotatedX = RotationMod-OriginalY                                          |
; | RotatedY = OriginalX                                                      |
; |                                                                           |
; | Where RotationMod is a value of #$03 or #$04 depending on the tetromino   |
; | type being rotated.                                                       |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X                   - Player # - $00=Player 1, $01=Player 2               |
; +---------------------------------------------------------------------------+
; | Depends On:                                                               |
; +---------------------------------------------------------------------------+
; | PLR__CURRENT_TETROMINO_CELLS      - the buffer containing the shape to    |
; |                                     rotate                                |
; +---------------------------------------------------------------------------+
rotate_tetromino:
                                    jsr set_30_ptr_to_tetro_buffer       ; Set $30 pointer to the tetromino buffer of Player X

                                    ldy #$00
populate_rotate_buffer_loop         lda ($30),y                          ; Rotate each cell and store to ROTATE_BUFFER
                                    iny
                                    sta ROTATE_BUFFER,y
                                    lda ($30),y
                                    sta tmp_var
                                    dey
                                    sec
                                    lda PLR__TETROMINO_ROT_MOD,x
                                    sbc tmp_var
                                    sta ROTATE_BUFFER,y
                                    iny
                                    iny
                                    cpy #$08
                                    bne populate_rotate_buffer_loop

                                    lda PLR__TETROMINO_X,x               ; Move player coordinates to predicate coordinate
                                    sta PREDICATE_TETROMINO_X            ; registers
                                    lda PLR__TETROMINO_Y,x
                                    sta PREDICATE_TETROMINO_Y

                                    ldx #$02                             ; Copy the rotate buffer into the predicate buffer
                                    jsr copy_buffer_to_predicate
                                    jsr evaluate_target_pos              ; Check if new cell coordinates are all empty

                                    bpl rotation_failed                  ; If not, then cancel/exit

                                    ldx CURRENT_PLAYER                   ; Restore Player index
                                    jsr set_30_ptr_to_tetro_buffer       ; Set $30 pointer to the tetromino buffer again

                                    ldy #$07                             ; Copy over the rotated cells to the player buffer
write_rotate_result_loop            lda ROTATE_BUFFER,y
                                    sta ($30),y
                                    dey
                                    bpl write_rotate_result_loop

                                    inc PLR__CURRENT_ROTATION,x
                                    lda PLR__CURRENT_ROTATION,x
                                    cmp #$04
                                    bne rotate_exit
                                    lda #$00
                                    sta PLR__CURRENT_ROTATION,x
                                    sta PLR__ACTION_STATUS,x             ; Clear player action status

rotate_exit                         rts

rotation_failed                     ldx CURRENT_PLAYER
                                    lda #%10000000
                                    sta PLR__ACTION_STATUS,x             ; Set player action status to failed rotation
                                    rts



tmp_var                             .byte $00


; +---------------------------------------------------------------------------+
; | ANIMATE_LINES                                                             |
; | animate_lines                                                             |
; +---------------------------------------------------------------------------+
; | Calls animate_player_lines for each player in turn.                       |
; +---------------------------------------------------------------------------+
; | Depends on:                                                               |
; +---------------------------------------------------------------------------+
; | GAME_MODE           - Controls if animate_player_lines is to be           |
; |                       for Player 2 in addition to for Player 1            |
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
; | Advances the animation of formed lines that occur before they are shifted |
; | out of the well.                                                          |
; |                                                                           |
; | The game character set is arranged so that the cells in Screen RAM can    |
; | simply be incremented by one to display their next animation graphic.     |
; |                                                                           |
; | The screen Y coordinates of the lines to animate are found in the buffer  |
; | PLR__FOUND_LINES, set up during the check for formed lines.               |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X                   - Player # - $00=Player 1, $01=Player 2               |
; +---------------------------------------------------------------------------+
; | Depends on:                                                               |
; +---------------------------------------------------------------------------+
; | PLR__ANIM_FRAME     - Determines if the animation is done. The value does |
; |                       does not otherwise influence the animation. Is      |
; |                       decreased by one before an animation step is        |
; |                       performed                                           |
; |                                                                           |
; | PLR__ANIM_COUNTER   - Frames/calls left until the next animation step is  |
; |                       is to be performed. Resets it reaches zero.         |
; |                                                                           |
; | PLR__FOUND_LINES    - Contains the Y coordinates of the lines that are to |
; |                       be animated.                                        |
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

                                    jsr add_to_player_line_count

                                    jsr shift_well_contents
                                    ldx CURRENT_PLAYER
                                    jsr roll_new_tetromino

                                    jmp animation_done

do_animate_lines
                                    txa
                                    asl
                                    tay

                                    lda table_found_lines_buffers,y
                                    sta $24
                                    lda table_found_lines_buffers+1,y
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
; | Checks a players well for new lines. Must be called after a tetromino has |
; | been placed, and before the players next tetromino is activated, as this  |
; | routine uses the shape and coordinates of the players current tetromino   |
; | to determine which rows to check.                                         |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X               - Player. $00=Player 1, $01=Player 2                      |
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

                                    txa
                                    asl
                                    tay

                                    lda table_found_lines_buffers,y
                                    sta $24
                                    lda table_found_lines_buffers+1,y
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
                                    bmi not_a_line                              ; tetromino shape. If not, skip

                                    txa                                         ; Add tetromino block Y to cell Y
                                    clc                                         ; to get actual screen row
                                    ldy CURRENT_PLAYER
                                    adc PLR__TETROMINO_Y,y
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
; | ADD NUMBER OF FORMED LINES TO PLAYERS LINE COUNT                          |
; | add_to_player_line_count                                                  |
; +---------------------------------------------------------------------------+
; | Adds to the line score of Player X using Binary-coded decimal mode, and   |
; | then prints the result to the screen                                      |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X               - Player. $00=Player 1, $01=Player 2                      |
; +---------------------------------------------------------------------------+
add_to_player_line_count:
                                    sei
                                    sed
                                    clc
                                    lda PLR__LINES_LO,x
                                    adc PLR__LINES_TO_CLEAR, x
                                    sta PLR__LINES_LO,x
                                    lda PLR__LINES_HI,x
                                    adc #$00
                                    sta PLR__LINES_HI,x
                                    cld
                                    cli

; +---------------------------------------------------------------------------+
; | PRINT PLAYER LINE COUNT TO SCREEN                                         |
; | print_player_line_count                                                   |
; +---------------------------------------------------------------------------+
; | Prints line count to screen                                               |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X               - Player. $00=Player 1, $01=Player 2                      |
; +---------------------------------------------------------------------------+
print_player_line_count
                                    ldy #$14
                                    lda table_scr_line, y
                                    sta $30
                                    lda table_scr_line+1, y
                                    sta $31

                                    clc
                                    lda #$02
                                    adc PLR__NEXTBOX_OFFSET,x
                                    adc $30
                                    sta $30
                                    lda $31
                                    adc #$00
                                    sta $31

                                    lda PLR__LINES_HI,x
                                    beq print_line_eval_lo_byte
                                    and #%11110000
                                    beq print_line_count_digit_2

print_line_count_digit_1            clc
                                    lda PLR__LINES_HI,x
                                    lsr
                                    lsr
                                    lsr
                                    lsr
                                    adc #$30
                                    ldy #$00
                                    sta ($30),y

print_line_count_digit_2            clc
                                    lda PLR__LINES_HI,x
                                    and #%00001111
                                    adc #$30
                                    ldy #$01
                                    sta ($30),y

                                    jmp print_line_count_digit_3

print_line_eval_lo_byte             lda PLR__LINES_LO,x
                                    and #%11110000
                                    beq print_line_count_digit_4

print_line_count_digit_3            lda PLR__LINES_LO,x
                                    lsr
                                    lsr
                                    lsr
                                    lsr
                                    clc
                                    adc #$30
                                    ldy #$02
                                    sta ($30),y

print_line_count_digit_4            lda PLR__LINES_LO,x
                                    and #%00001111
                                    adc #$30
                                    ldy #$03
                                    sta ($30),y
                                    rts


; +---------------------------------------------------------------------------+
; | PRINT TEXT STRING TO SCREEN WITH COLOR                                    |
; | print_text                                                                |
; +---------------------------------------------------------------------------+
; | Prints a text string to the screen and color to color RAM, reading from a |
; | data structure in memory at pointer $24                                   |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X           - Screen column to print at                                   |
; | Y           - Screen row to print at                                      |
; | $24-$25     - Pointer to text data structure in memory                    |
; |                 Byte 0 - Color                                            |
; |                 Byte 1 - String length                                    |
; |                 Byte 2.. Text data                                        |
; +---------------------------------------------------------------------------+
print_text:
                                    stx $30
                                    tya
                                    asl
                                    tay
                                    clc
                                    lda table_scr_line,y
                                    adc $30
                                    sta $30
                                    sta $28
                                    lda table_scr_line+1,y
                                    adc #$00
                                    sta $31
                                    adc #$d4
                                    sta $29

                                    ldy #$00
                                    lda ($24),y
                                    sta text_color
                                    iny
                                    lda ($24),y
                                    tax

                                    lda $24
                                    clc
                                    adc #$02
                                    sta $24
                                    lda $25
                                    adc #$00
                                    sta $25

                                    ldy #$00
print_text_loop                     lda ($24),y
                                    sta ($30),y
                                    lda text_color
                                    sta ($28),y
                                    iny
                                    dex
                                    bne print_text_loop

                                    rts


text_color                          .byte $00




; +---------------------------------------------------------------------------+
; | PREPARE NEXT TETROMINO                                                    |
; | place_tetromino                                                           |
; +---------------------------------------------------------------------------+
; | Randomizes a new tetromino shape and its appearance and stores to         |
; | the next-tetromino registers for Player X                                 |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X           - Player index. $00=Player 1, $01=Player 2                    |
; +---------------------------------------------------------------------------+
; | Affects:                                                                  |
; +---------------------------------------------------------------------------+
; | PLR__NEXT_TETROMINO_TYPE    - The new shape index                         |
; | PLR__NEXT_TETROMINO_FACE    - The char index of the tetromino cells       |
; | PLR__NEXT_TETROMINO_COLOR   - The color of the next tetromino.            |
; +---------------------------------------------------------------------------+
prepare_next_tetromino:

                                    lda #$07                                    ; Get a random tetromino index between
                                    jsr get_random_number                       ; $00 and $06
                                    sta PLR__NEXT_TETROMINO_TYPE,x              ; Store as next tetromino type for player X

                                    lda #$18                                    ; Get a random appearance index between
                                    jsr get_random_number                       ; $00 and $15
                                    asl
                                    tay                                         ; Multiply by two to get 16-bit table offset

                                    lda table_tetromino_appearance,y            ; Set next tetromino char
                                    sta PLR__NEXT_TETROMINO_FACE,x

                                    lda table_tetromino_appearance+1,y          ; Set next tetromino color
                                    sta PLR__NEXT_TETROMINO_COLOR,x

                                    rts

; +---------------------------------------------------------------------------+
; | ROLL NEW TETROMINO                                                        |
; | roll_new_tetromino                                                        |
; +---------------------------------------------------------------------------+
; | Rolls the queued "next tetromino" into the well and prepares a new next   |
; | piece.                                                                    |
; |                                                                           |
; | First, all properties of the tetromino currently in the "next-box" are    |
; | copied to the "current tetromino" registers, and then the shape of this   |
; | piece is loaded into the players teromino buffer and used to blank out    |
; | the next box.                                                             |
; |                                                                           |
; | Then a new tetromino is picked and loaded into the tetromino buffer to    |
; | be drawed in the "next-box".                                              |
; |                                                                           |
; | And, finally, the new current tetromino piece is loaded into the players  |
; | buffer and drawn to the well                                              |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X           - Player index. $00=Player 1, $01=Player 2                    |
; +---------------------------------------------------------------------------+
; | Affects:                                                                  |
; +---------------------------------------------------------------------------+
; | PLR__CURRENT_TETROMINO_TYPE    - Set to the type of the queued piece      |
; | PLR__CURRENT_TETROMINO_FACE    - Set to the face of the queued piece      |
; | PLR__CURRENT_TETROMINO_COLOR   - Set to the color of the queued piece     |
; | PLR__CURRENT_TETROMINO_ROT_MOD - Set tot he rotation mod of the queued pc |
; | PLR__CURRENT_TETROMINO_CELLS   - Loaded with the shape of the queued piece|
; | PLR__NEXT_TETROMINO_TYPE       - A new shape index                        |
; | PLR__NEXT_TETROMINO_FACE       - A new face                               |
; | PLR__NEXT_TETROMINO_COLOR      - A new color                              |
; +---------------------------------------------------------------------------+
roll_new_tetromino:
                                    lda PLR__NEXT_TETROMINO_FACE,x              ; Copy next tetromino char to current
                                    sta PLR__CURRENT_TETROMINO_FACE,x

                                    lda PLR__NEXT_TETROMINO_COLOR,x             ; Copy next tetromino color to current
                                    sta PLR__CURRENT_TETROMINO_COLOR,x

                                    ldy PLR__NEXT_TETROMINO_TYPE,x              ; Copy next tetromino type to current
                                    lda table_tetromino_rotation_mod,y
                                    sta PLR__TETROMINO_ROT_MOD,x

                                    tya
                                    sta PLR__CURRENT_TETROMINO_TYPE,x

                                    jsr set_player_tetromino                    ; to prepare for blanking out previous nextbox

                                    clc
                                    lda #$00                                    ; Set black draw color
                                    sta DRAW_TETR__COLOR

                                    lda #$20                                    ; Set empty draw char
                                    sta DRAW_TETR__CHAR

                                    clc
                                    lda PLR__NEXTBOX_OFFSET,x                   ; Load next box offset for player X
                                    adc #$01
                                    ldy PLR__CURRENT_TETROMINO_TYPE,x
                                    adc table_tetromino_well_x_mod,y
                                    sta PLR__TETROMINO_X,x                      ; and store as tetromino X coordinate

                                    lda #$03                                    ; Set 03 as tetromino Y
                                    sta PLR__TETROMINO_Y,x

                                    stx CURRENT_PLAYER                          ; Stash away player index
                                    jsr draw_player_tetromino                   ; Clear next tetromino box

                                    ldx CURRENT_PLAYER                          ; Restore player index
                                    jsr prepare_next_tetromino                  ; Set up a new random next tetromino

                                    lda PLR__NEXT_TETROMINO_FACE,x              ; Load new next tetromino properties to
                                    sta DRAW_TETR__CHAR                         ; draw variables
                                    lda PLR__NEXT_TETROMINO_COLOR,x
                                    sta DRAW_TETR__COLOR

                                    clc
                                    lda PLR__NEXTBOX_OFFSET,x                   ; Load next box offset for player X
                                    adc #$01
                                    ldy PLR__NEXT_TETROMINO_TYPE,x
                                    adc table_tetromino_well_x_mod,y
                                    sta PLR__TETROMINO_X,x                      ; and store as tetromino X coordinate

                                    ldy PLR__NEXT_TETROMINO_TYPE,x              ; Load new next tetromino type as argument
                                    jsr set_player_tetromino                    ; and load shape into players buffer
                                    jsr draw_player_tetromino                   ; And draw it to the next box

                                    ldx CURRENT_PLAYER                          ; Restore current player index

                                    lda PLR__CURRENT_TETROMINO_FACE,x           ; Load new current tetromino properties
                                    sta DRAW_TETR__CHAR                         ; to draw variables
                                    lda PLR__CURRENT_TETROMINO_COLOR,x
                                    sta DRAW_TETR__COLOR

                                    ldy PLR__CURRENT_TETROMINO_TYPE,x           ; Load new current tetromino type as argument
                                    jsr set_player_tetromino                    ; and load shape into players buffer

                                    lda #$04                                    ; Set tetromino coordinates to top of players well
                                    adc PLR__WELL_OFFSET,x
                                    sta PLR__TETROMINO_X,x
                                    lda #$00
                                    sta PLR__TETROMINO_Y,x

                                    jsr draw_player_tetromino                   ; Draw new tetromino to the screen

                                    ldx CURRENT_PLAYER
                                    beq end_roll_new

                                    lda GAME_MODE
                                    and #%00000011
                                    cmp #$01
                                    bne end_roll_new

                                    lda #$00
                                    sta CPU__STATE
                                    sta PLAYER2__CURRENT_ROTATION
                                    sta best_opt__weight

end_roll_new                        rts

; +---------------------------------------------------------------------------+
; | COPY TETROMINO BUFFER TO PREDICATE BUFFER                                 |
; | copy_buffer_to_predicate                                                  |
; +---------------------------------------------------------------------------+
; | Copies the contents of a source tetromino buffer into the predicate       |
; | buffer used to check if movement or rotation is valid.                    |
; |                                                                           |
; | During the copy, the Y coordinate of each cell is modified to include     |
; | the screen Y offset.                                                      |
; +---------------------------------------------------------------------------+
; | Arguments:                                                                |
; +---------------------------------------------------------------------------+
; | X               - Buffer index. $00=Player 1 buffer                       |
; |                                 $01=Player 2 buffer                       |
; |                                 $02=Rotation buffer                       |
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
                                    adc PREDICATE_TETROMINO_X
                                    sta PREDICATE_TETROMINO_CELLS,y

                                    iny
                                    lda ($30),y
                                    clc
                                    adc PREDICATE_TETROMINO_Y
                                    sta PREDICATE_TETROMINO_CELLS,y

                                    dex
                                    bpl copy_to_pred_loop
                                    rts

; +---------------------------------------------------------------------------+
; | SET PLAYER TETROMINO                                                      |
; | set_player_tetromino                                                      |
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
; | Y           - Used as loop iterator                                       |
; | $24-$25     - Will point to the first byte of tetrominoe Y in tetrominoe  |
; |               data area.                                                  |
; | $30-31      - Will point to the first byte of tetrominoe buffer of player |
; |               X.                                                          |
; +---------------------------------------------------------------------------+
set_player_tetromino
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
                                    lda PREDICATE_TETROMINO_CELLS+1,y

                                    asl
                                    tay                                     ; Load screen offset for row
                                    lda table_scr_line,y                    ; and store to zero page pointer $30
                                    sta $30
                                    lda table_scr_line+1,y
                                    sta $31

                                    pla                                     ; Retrieve cell index

                                    tay
                                    lda PREDICATE_TETROMINO_CELLS,y         ; Get cell X position on screen

                                    tay                                     ; Load from screen offset+x
                                    lda ($30),y
                                    cmp #$20                                ; and verify that it is empty
                                    bne evaluate_target_pos_done            ; otherwise, bail out with X!=0 signifying failure

                                    dex
                                    bpl evaluate_target_pos_loop
evaluate_target_pos_done            rts

; +---------------------------------------------------------------------------+
; | DRAW PLAYER TETROMINO                                                     |
; | draw_player_tetromino                                                     |
; +---------------------------------------------------------------------------+
; | Draws a tetromino to the screen using the shape buffer and coordinate     |
; | registers belonging to Player X.                                          |
; |                                                                           |
; | Color and char/face is not read from the players registers, but taken as  |
; | arguments as this routine is used to remove/draw over tetrominos with     |
; | empty black chars as well.                                                |
; |                                                                           |
; | This routine sets up the necessary registers and overwrites the source    |
; | buffer addresses in the draw_tetromino routine that performs the actual   |
; | drawing                                                                   |
; +---------------------------------------------------------------------------+
; | Arguments                                                                 |
; +---------------------------------------------------------------------------+
; | X                   - Player index. $00=Player 1, $01=Player 2            |
; |                                                                           |
; | DRAW_TETR__CHAR     - The char value to write to screen ram at each cell  |
; |                       coordinate                                          |
; |                                                                           |
; | DRAW_TETR__CHAR     - The value to write to color ram at for each cell    |
; |                       coordinate                                          |
; +---------------------------------------------------------------------------+
draw_player_tetromino:
                                    lda PLR__TETROMINO_X,x
                                    sta DRAW_TETR__X

                                    lda PLR__TETROMINO_Y,x
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

                                    jmp draw_tetromino


; +---------------------------------------------------------------------------+
; | DRAW TETROMINO                                                            |
; | draw_tetromino                                                            |
; +---------------------------------------------------------------------------+
; | Draws a tetromino to the screen using supplised coordinates and           |
; | appearance. The source buffer address must be written to the arguments of |
; | the assembly instructions of draw_tetr_buf_read_y and draw_tetr_buf_read_x|
; | before this routine is called.                                            |
; +---------------------------------------------------------------------------+
; | Arguments                                                                 |
; +---------------------------------------------------------------------------+
; | DRAW_TETR__X                                                              |
; | DRAW_TETR__Y                                                              |
; | DRAW_TETR__FACE                                                           |
; | DRAW_TETR__COLOR                                                          |
; +---------------------------------------------------------------------------+
draw_tetromino:
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
draw_tetr_buf_read_y                lda $0000+1,y                               ; Address must be overwritten by caller
                                    beq draw_tetromino_no_ymod

                                    tay
                                    lda #$00
                                    clc
draw_tetr_cell_add_y_offset_loop    adc #$28
                                    dey
                                    bne draw_tetr_cell_add_y_offset_loop

draw_tetromino_no_ymod:             pha
                                    txa
                                    asl
                                    tay
                                    pla
                                    clc
draw_tetr_buf_read_x                adc $0000,y                                 ; Address must be overwritten by caller

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
; | Shifts all lines of the well above each formed line down one step.        |
; | Currently a naive and inefficient implementation that shifts in multiple  |
; | passes - one for each line.                                               |
; +---------------------------------------------------------------------------+
; | Arguments                                                                 |
; +---------------------------------------------------------------------------+
; | X                   - Player index. $00=Player 1, $01=Player 2            |
; +---------------------------------------------------------------------------+
shift_well_contents:
                                    txa                                         ;  Multiply Player # by two, to get index of
                                    asl                                         ;  pointer to player lines buffer
                                    tay

                                    lda table_found_lines_buffers,y             ; Store pointer to player lines buffer in $2c-$2d
                                    sta $2c
                                    lda table_found_lines_buffers+1,y
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
; | DRAW CONTAINER BOX FOR NEXT TETROMINO                                     |
; | draw_nextbox                                                              |
; +---------------------------------------------------------------------------+
; | Draws one 6x6 container box                                               |
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
draw_nextbox:
                                    tya
                                    clc
                                    adc table_scr_line+4
                                    sta $30
                                    lda table_scr_line+5
                                    adc #$00
                                    sta $31

                                    ldx #$00
                                    ldy #$00

draw_nextbox_loop                   tya
                                    clc
                                    adc nextbox_data,x
                                    tay
                                    inx
                                    lda nextbox_data,x
                                    sta ($30),y
                                    inx
                                    cpx #$28
                                    bne draw_nextbox_loop

                                    rts


; +---------------------------------------------------------------------------+
; | CLEAR SCREEN                                                              |
; | clear_screen                                                              |
; +---------------------------------------------------------------------------+
; | Clears the screen by setting all of Screen RAM to $20 and all of Color    |
; | RAM to $01                                                                |
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
; | Reads input from each player in turn. If GAME_MODE specifices player 2 to |
; | be the computer, determine_cpu_move instead of polling joystick input.    |
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
; | Reads joystick in port 2                                                  |
; +---------------------------------------------------------------------------+
; | Returns:                                                                  |
; +---------------------------------------------------------------------------+
; | X                   - $ff if joystick is pulled left                      |
; |                       $00 if joystick is centered horizontally            |
; |                       $01 if joystick is pulled right                     |
; |                                                                           |
; | Y                   - $ff if joystick is pulled up                        |
; |                       $00 if joystick is centered vertically              |
; |                       $01 if joystick is pulled down                      |
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
; | Reads joystick in port 1                                                  |
; +---------------------------------------------------------------------------+
; | Returns:                                                                  |
; +---------------------------------------------------------------------------+
; | X                   - $ff if joystick is pulled left                      |
; |                       $00 if joystick is centered horizontally            |
; |                       $01 if joystick is pulled right                     |
; |                                                                           |
; | Y                   - $ff if joystick is pulled up                        |
; |                       $00 if joystick is centered vertically              |
; |                       $01 if joystick is pulled down                      |
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
; | Determine the next move for the CPU controlled Player 2.                  |
; |                                                                           |
; | The output of the function is simply a case of storing the input to       |
; | PLAYER2__MOVE_X and PLAYER2__MOVE_Y, just as if Player 2 had been         |
; | controlled by a joystick                                                  |
; |                                                                           |
; | For every call to his subroutine until all alternatives have been         |
; | been explored, an X position and a rotation for the current tetromino     |
; | will be evaluated and scored.                                             |
; |                                                                           |
; | Movement will then be assigned to rotate and move the block to the        |
; | current best option found.                                                |
; |                                                                           |
; | Once all options are evaluated and the tetromino is in place over the     |
; | target column and the rotation is correct, the CPU will "press down" to   |
; | speed up the falling block.                                               |
; +---------------------------------------------------------------------------+
; | Returns:                                                                  |
; +---------------------------------------------------------------------------+
; | PLAYER2__MOVE_X - The horizontal "joystick" movement requested by the CPU |
; | PLAYER2__MOVE_Y - The vertical "joystick" movement requested by the CPU   |
; +---------------------------------------------------------------------------+

determine_cpu_move:
                                    lda #$00                                    ; Blank out any previous moves
                                    sta PLAYER2__MOVE_X
                                    sta PLAYER2__MOVE_Y

                                    lda CPU__STATE                              ; Check if the column order for evaluation has been
                                    and #%00000001                              ; created for the current tetromino
                                    bne cpu__alternatives_ordered

                                    jsr cpu__col_order_alternatives             ; Otherwise call subroutine to generate


cpu__alternatives_ordered           lda CPU__STATE                              ; Check if all positions and rotations have been
                                    and #%10000000                              ; evaluated.
                                    bne cpu__find_target_position               ; If so, go straight to setting the next move

                                    jsr cpu__evaluate_cpu_move_option           ; Else evaluate next move option


cpu__find_target_position:
                                    clc
                                    lda best_opt__col_index
                                    adc PLAYER2__WELL_OFFSET
                                    sec
                                    sbc #$01
                                    sta CPU__TARGET_X

                                    lda best_opt__rotation
                                    sta CPU__TARGET_ROTATION

cpu__move_towards_target:
                                    lda CPU__TARGET_ROTATION
                                    ;sta $0428
                                    lda PLAYER2__CURRENT_ROTATION
                                    ;sta $0429

                                    lda CPU__TARGET_ROTATION
                                    cmp PLAYER2__CURRENT_ROTATION
                                    bne cpu__rotate

cpu__horizontal_move:
                                    lda CPU__TARGET_X
                                    ;sta $0400
                                    lda PLAYER2__TETROMINO_X
                                    ;sta $0401

                                    lda CPU__TARGET_X

                                    cmp PLAYER2__TETROMINO_X
                                    beq cpu__speed_up
                                    bcs cpu__move_right
                                    bcc cpu__move_left

                                    rts

; +---------------------------------------------------------------------------+
; | CPU OPPONENT - SPEED UP                                                   |
; | cpu__speed_up                                                             |
; +---------------------------------------------------------------------------+
; |                                                                           |
; +---------------------------------------------------------------------------+
cpu__speed_up:
                                    lda CPU__STATE
                                    and #%10000000
                                    beq no_speed_up

                                    lda #$01
                                    sta PLAYER2__MOVE_Y
no_speed_up                         rts

; +---------------------------------------------------------------------------+
; | CPU OPPONENT - MOVE LEFT                                                  |
; | cpu__move_left                                                            |
; +---------------------------------------------------------------------------+
; |                                                                           |
; +---------------------------------------------------------------------------+
cpu__move_left:
                                    lda #$ff
                                    sta PLAYER2__MOVE_X
                                    rts

; +---------------------------------------------------------------------------+
; | CPU OPPONENT - MOVE RIGHT                                                 |
; | cpu__move_right                                                           |
; +---------------------------------------------------------------------------+
; |                                                                           |
; +---------------------------------------------------------------------------+
cpu__move_right:
                                    lda #$01
                                    sta PLAYER2__MOVE_X
                                    rts

; +---------------------------------------------------------------------------+
; | CPU OPPONENT - ROTATE                                                     |
; | cpu__rotate                                                               |
; +---------------------------------------------------------------------------+
; |                                                                           |
; +---------------------------------------------------------------------------+
cpu__rotate:
                                    lda PLAYER2__ACTION_STATUS
                                    cmp #%10000000
                                    beq cpu__move_towards_center

                                    lda #$ff
                                    sta PLAYER2__MOVE_Y
                                    rts

cpu__move_towards_center:
                                    lda PLAYER2__WELL_OFFSET
                                    clc
                                    adc #$04
                                    cmp PLAYER2__TETROMINO_X
                                    bcs cpu__move_right
                                    jmp cpu__move_left

; +---------------------------------------------------------------------------+
; | CPU OPPONENT - RANDOMIZE COLUMN EVALUATION ORDER                          |
; | cpu__col_order_alternatives                                               |
; +---------------------------------------------------------------------------+
; |                                                                           |
; +---------------------------------------------------------------------------+
cpu__col_order_alternatives:
                                    ldy #$0a
fill_alt_loop                       tya
                                    sta CPU__DATA_BUFFER,y
                                    dey
                                    bpl fill_alt_loop

                                    ldy #$0a
                                    sty CPU__COLUMN_ALT_INDEX
fill_alt_order_loop                 iny
                                    tya
                                    jsr get_random_number
                                    dey
                                    tax

                                    lda CPU__DATA_BUFFER,x
                                    sta CPU__COLUMN_ALT_ORDER,y

shift_options_loop                  inx
                                    lda CPU__DATA_BUFFER,x
                                    dex
                                    sta CPU__DATA_BUFFER,x
                                    inx
                                    cpx CPU__COLUMN_ALT_INDEX
                                    bcc shift_options_loop

                                    dec CPU__COLUMN_ALT_INDEX
                                    ldy CPU__COLUMN_ALT_INDEX

                                    bne fill_alt_order_loop

                                    lda CPU__DATA_BUFFER,y
                                    sta CPU__COLUMN_ALT_ORDER,y

                                    lda CPU__STATE
                                    ora #%00000001
                                    sta CPU__STATE

                                    lda #$00                    ; Set rotation to 0 as first iteration
                                    sta CPU__COLUMN_ROT_INDEX

                                    rts

; +---------------------------------------------------------------------------+
; | CPU OPPONENT - UPDATE WELL MODEL                                          |
; | update_cpu_well_model                                                     |
; +---------------------------------------------------------------------------+
; |                                                                           |
; +---------------------------------------------------------------------------+

;; This routine is going to assume that a tetro has just been placed, and we can safely use the tetro Y as a starting point
update_cpu_well_model:
                                    ldx #$09                                    ; Iterate through the columns of the previous snapshot of
                                    lda #$18                                    ; the well "floor" to find the highest row number that is occupied
                                    sta curr_row                                ; (lowest number as values are stored as distance from top)
clear_well_floor_loop               ldy CPU__WELL_FLOOR,x                       ; and set all values to $18 (absolute bottom) as we go
                                    lda #$18
                                    sta CPU__WELL_FLOOR,x

                                    tya                                         ; Compare row number to the lowest value ecountered
                                    cmp curr_row                                ; so far (stored in curr_row).
                                    bcs clear_well_floor_loop_next_iter         ; if row number is equal or greater, then set up next iteration

                                    sta curr_row                                ; else store value as lowest encountered in curr_row
clear_well_floor_loop_next_iter     dex
                                    bpl clear_well_floor_loop

                                    lda PLAYER2__TETROMINO_Y                    ; Compare last known value of player 2 tetromino Y
                                    cmp curr_row                                ; (which is where last block was placed) to previous highest row
                                    bcs clear_well_set_scr_pointer              ; Move on if Player 2 Tetromino Y is a higher value

                                    sta curr_row                                ; else, set curr_row to Player 2 Y


clear_well_set_scr_pointer          lda curr_row
                                    asl                                         ; Set $30 pointer to the location in Screen RAM where
                                    tax                                         ; this row begins in player 2 well
                                    clc
                                    lda PLAYER2__WELL_OFFSET
                                    adc #$01
                                    adc table_scr_line,x
                                    sta $30
                                    lda table_scr_line+1,x
                                    adc #$00
                                    sta $31

                                    ldx #$0a                                    ; Set X to 10 (number of columns left to determine height of)

upd_well_floor__begin_row_loop      ldy #$09                                    ; Set Y to column offset 9 (scan from right to left)
upd_well_floor__row_loop            lda ($30),y
                                    cmp #$20                                    ; Skip this column immediately if it is empty on this row
                                    beq upd_well__next_pos

                                    lda CPU__WELL_FLOOR,y                       ; Otherwise, check if we have previously found an occupied
                                    cmp #$18                                    ; cell in this column by comparing to #$18(empty/default)
                                    bne upd_well__next_pos                      ; if so, skip this column

                                    lda curr_row                                ; otherwise, set the current row as the height of this column
                                    sta CPU__WELL_FLOOR,y

                                    dex                                         ; Decrease columns left to determine height of
                                    beq well_floor_done                         ; if zero, we're done

upd_well__next_pos                  dey                                         ; Decrease y to move one column left
                                    bpl upd_well_floor__row_loop                ; Continue looping while >= 0

                                    inc curr_row                                ; Move to next row
                                    lda curr_row                                ; Compare row number to #$18 to see if we have reached the
                                    cmp #$18                                    ; bottom, in which case the remaining columns are all empty
                                    beq well_floor_done                         ; and we are done

                                    clc                                         ; Otherwise, move the screen RAM pointer one row down
                                    lda $30
                                    adc #$28
                                    sta $30
                                    lda $31
                                    adc #$00
                                    sta $31

                                    jmp upd_well_floor__begin_row_loop

well_floor_done                     rts

curr_row                           .byte $00

; +---------------------------------------------------------------------------+
; | CPU OPPONENT - EVALUATE MOVE OPTION                                       |
; | cpu__evaluate_cpu_move_option                                             |
; +---------------------------------------------------------------------------+
; | Evaluates a single move option (a rotation and X position) of the current |
; | CPU player tetromino block.                                               |
; | The column order of evaluation is determined by CPU__COLUMN_ALT_ORDER and |
; | CPU__COLUMN_ALT_INDEX. All rotations are tried for a column before moving |
; | to the next.                                                              |
; |                                                                           |
; | For each move option the following steps are taken:                       |
; | 1) Set pointer $30 to lookup table containing the bottom shapes of the    |
; |    current tetromino blocks and forward to the current rotation.          |
; |                                                                           |
; | 2) Determine the baseline where the block would be placed if dropped in   |
; |    this column, and set the actual block bottom values.                   |
; |                                                                           |
; | 3) Evaluate each bottom-facing segment of the tetromino and assign scores |
; |    depending on contact with bottom/previously placed block and any       |
; |    caverns formed                                                         |
; |                                                                           |
; | 4) Determine if the left edge (HEAD) removes any cliffs or forms a new    |
; |    cliff, and score according to cliff offset removed or added.           |
; |                                                                           |
; | 5) Determine if the right edge (TAIL) removes any cliffs or forms a new   |
; |    cliff, and score according to cliff offset removed or added.           |
; |                                                                           |
; | 6) Apply all accumulated negative score modifications                     |
; |                                                                           |
; | 7) Set this option as the best known option if the score is higher than   |
; |    the previous best option                                               |
; +---------------------------------------------------------------------------+

cpu__evaluate_cpu_move_option
; -------------------------------- DEBUG --------------------------------------
;                                   lda #$03                                        ;; DEBUG SET BLOCK TYPE
;                                   sta PLAYER2__CURRENT_TETROMINO_TYPE
; -----------------------------------------------------------------------------
                                    lda #$00                                        ; Reset variables used during evaluation
                                    sta eval_opt__weight                            ; - Weight/Score of the current option
                                    sta negative_mods                               ; - Accumulated negative score modifiers
                                    sta positive_mods                               ; - Accumulated positive score modifiers

                                    lda #$ff                                        ; Set the Y position of the previous columns contact
                                    sta prev_contact_height                         ; with the well floor to $ff(no contact)

                                    jsr cpu__set_tetromino_bottom_ptr               ; 1) Set pointer in tetro bottom table
                                    jsr cpu__calc_option_baseline                   ; 2) Calculate baseline

                                    jsr calculate_column_weights                    ; 3) Calculate score/weights for each tetro column
                                    jsr calculate_head_weight                       ; 4) Calculate fill/cliff weights for left edge/head
                                    jsr calculate_tail_weight                       ; 5) Calculate fill/cliff weights for right edge/head
                                    jsr apply_negative_mods                         ; 6) Apply accumulated negative weight modifiers

weight_calculation_done             jsr write_result_to_debug_area

compare_to_best_option              lda eval_opt__weight                            ; If the weight is $ff, this means the option move is
                                    cmp #$ff                                        ; not possible to make, so we are discarding it immediately
                                    beq prepare_next_opt_rot_iteration

                                    lda best_opt__weight                            ; Else, if the weight is higher the currently known
                                    cmp eval_opt__weight                            ; best move then set this option as the best.
                                    bcc set_new_best_option

                                    jmp prepare_next_opt_rot_iteration              ; Otherwise, move directly on to setting up the next iteration
set_new_best_option
                                    lda eval_opt__weight
                                    sta best_opt__weight

                                    lda eval_opt__col_index
                                    sta best_opt__col_index

                                    lda eval_opt__rotation
                                    sta best_opt__rotation

                                    lda eval_opt__lines
                                    sta best_opt__lines

prepare_next_opt_rot_iteration      inc CPU__COLUMN_ROT_INDEX
                                    lda CPU__COLUMN_ROT_INDEX
                                    cmp #$04
                                    bne exit_add_option

                                    lda #$00
                                    sta CPU__COLUMN_ROT_INDEX

                                    inc CPU__COLUMN_ALT_INDEX
                                    lda CPU__COLUMN_ALT_INDEX
                                    cmp #$0b
                                    bne exit_add_option

                                    lda CPU__STATE
                                    ora #%10000000
                                    sta CPU__STATE

exit_add_option                     rts



; +---------------------------------------------------------------------------+
; | CPU OPPONENT - CALCULATE COLUMN WEIGHTS                                   |
; | calculate_column_weights                                                  |
; +---------------------------------------------------------------------------+
; |                                                                           |
; +---------------------------------------------------------------------------+
calculate_column_weights:
                                    ldx eval_opt__col_index
                                    ldy #$00

opt__calc_weight_loop:
                                    sty tetr_cell_index
                                    lda opt__bottoms,y
                                    bmi calc_weight__column_done

                                    lda CPU__WELL_FLOOR-2,x
                                    sec
                                    sbc opt__bottoms,y

                                    beq opt__add_contact_bonus

opt__add_cavern_penalty             clc
                                    asl
                                    asl
                                    adc negative_mods
                                    sta negative_mods

                                    lda #$ff
                                    sta prev_contact_height

                                    jmp opt__calc_weight__mods

opt__add_contact_bonus              inc positive_mods
                                    lda prev_contact_height
                                    cmp #$ff
                                    beq opt__add_contact_done

                                    sbc opt__bottoms,y
                                    bpl opt__add_contact_fill_bonus
                                    eor #$ff
                                    clc
                                    adc #$01

opt__add_contact_fill_bonus         clc
                                    adc positive_mods
                                    sta positive_mods

opt__add_contact_done               lda opt__bottoms,y
                                    sta prev_contact_height

opt__calc_weight__mods              ldy positive_mods
                                    beq calc_weight__column_done

                                    jsr apply_positive_mods

calc_weight__column_done            ldy tetr_cell_index

                                    lda #$00
                                    sta positive_mods

                                    iny
                                    inx
                                    cpy #$04
                                    bne opt__calc_weight_loop

                                    rts

; +---------------------------------------------------------------------------+
; | CPU OPPONENT - CALCULATE WEIGHT - APPLY POSITIVE MODS                     |
; | apply_positive_mods                                                       |
; +---------------------------------------------------------------------------+
; |                                                                           |
; +---------------------------------------------------------------------------+
apply_positive_mods:
                                    lda CPU__WELL_FLOOR-2,x

                                    lda eval_opt__weight
apply_pos_mods_loop                 clc
                                    adc CPU__WELL_FLOOR-2,x
                                    dey
                                    bne apply_pos_mods_loop

pos_mods_applied                    sta eval_opt__weight

                                    rts

; +---------------------------------------------------------------------------+
; | CPU OPPONENT - CALCULATE HEAD WEIGHT                                      |
; | calculate_head_weight                                                     |
; +---------------------------------------------------------------------------+
; |                                                                           |
; +---------------------------------------------------------------------------+
calculate_head_weight:
                                    ldy #$00
                                    ldx eval_opt__col_index

find_head_pos_loop                  lda opt__bottoms,y
                                    bpl calc_head__calc_head_height
                                    iny
                                    inx
                                    jmp find_head_pos_loop

calc_head__calc_head_height         sec
                                    sbc opt__head_mod
                                    sta opt__head_height

                                    cpx #$02
                                    beq calc_head__fill

                                    sec
                                    sbc CPU__WELL_FLOOR-3,x
                                    bpl calc_head__fill

                                    eor #$ff
                                    clc
                                    adc #$01
                                    sta calc_weight__cliff

calc_head__add_cliff_penalty        adc negative_mods
                                    sta negative_mods

                                    lda opt__head_mod
                                    sec
                                    sbc calc_weight__cliff
                                    tax
                                    beq calc_head_done
                                    bpl apply_head_fill_bonus

                                    jmp calc_head_done


calc_head__fill                     ldx opt__head_mod

apply_head_fill_bonus               lda eval_opt__weight
apply_head_fill_bonus_loop          clc
                                    adc opt__bottoms,y
                                    dex
                                    bne apply_head_fill_bonus_loop

                                    sta eval_opt__weight

calc_head_done                      rts

; +---------------------------------------------------------------------------+
; | CPU OPPONENT - CALCULATE TAIL WEIGHT                                      |
; | calculate_head_weight                                                     |
; +---------------------------------------------------------------------------+
; |                                                                           |
; +---------------------------------------------------------------------------+
calculate_tail_weight:
                                    ldy #$03
                                    tya
                                    clc
                                    adc eval_opt__col_index
                                    tax

find_tail_pos_loop                  lda opt__bottoms,y
                                    bpl calc_tail__calc_tail_height
                                    dey
                                    dex
                                    jmp find_tail_pos_loop

calc_tail__calc_tail_height         sec
                                    sbc opt__tail_mod
                                    sta opt__tail_height

                                    cpx #$0b
                                    beq calc_tail__fill

                                    sec
                                    sbc CPU__WELL_FLOOR-1,x
                                    bpl calc_tail__fill

                                    eor #$ff
                                    clc
                                    adc #$01
                                    sta calc_weight__cliff


calc_tail__add_cliff_penalty        adc negative_mods
                                    sta negative_mods

                                    lda opt__tail_mod
                                    sec
                                    sbc calc_weight__cliff
                                    tax
                                    beq calc_tail_done
                                    bpl apply_tail_fill_bonus

                                    jmp calc_tail_done


calc_tail__fill                     ldx opt__tail_mod

apply_tail_fill_bonus               lda eval_opt__weight
apply_tail_fill_bonus_loop          clc
                                    adc opt__bottoms,y
                                    dex
                                    bne apply_tail_fill_bonus_loop

                                    sta eval_opt__weight

calc_tail_done                      rts

; +---------------------------------------------------------------------------+
; | CPU OPPONENT - APPLY NEGATIVE MODS                                        |
; | apply_negative_mods                                                       |
; +---------------------------------------------------------------------------+
; | Apply negative mods accumulated throughout the evaluation of a move by    |
; | shifting the weight right once for every negative modifier                |
; +---------------------------------------------------------------------------+
apply_negative_mods:
                                    lda eval_opt__weight
                                    ldy negative_mods
                                    beq apply_negative_mods_done
apply_negative_mods_loop            lsr
                                    dey
                                    bne apply_negative_mods_loop
                                    sta eval_opt__weight

apply_negative_mods_done            rts


; +---------------------------------------------------------------------------+
; | CPU OPPONENT - WRITE OPTION EVALUATION RESULT TO DEBUG AREA IN MEMORY     |
; | write_result_to_debug_area                                                |
; +---------------------------------------------------------------------------+
; |                                                                           |
; +---------------------------------------------------------------------------+
write_result_to_debug_area
                                    sta eval_opt__weight
                                    lda eval_opt__col_index
                                    asl
                                    asl
                                    asl
                                    asl
                                    sta tmp_var

                                    lda eval_opt__rotation
                                    asl
                                    asl
                                    adc tmp_var
                                    tay
                                    lda eval_opt__col_index
                                    sta tmp_eval_result_area,y
                                    lda eval_opt__rotation
                                    sta tmp_eval_result_area+1,y
                                    lda eval_opt__weight
                                    sta tmp_eval_result_area+2,y
                                    lda eval_opt__lines
                                    sta tmp_eval_result_area+3,y

                                    rts


; +---------------------------------------------------------------------------+
; | CPU OPPONENT - CALCULATE OPTION BASELINE AND COLUMN Y POSITIONS           |
; | cpu__calc_option_baseline                                                 |
; +---------------------------------------------------------------------------+
; | Finds the baseline height value for the current tetromino's rotation and  |
; | assigns the actual Y position to the tetro_bottoms buffer as if it would  |
; | have been placed at the bottom of the current column                      |
; +---------------------------------------------------------------------------+

cpu__calc_option_baseline:
                                    ldx CPU__COLUMN_ALT_INDEX
                                    lda CPU__COLUMN_ALT_ORDER,x
                                    tax
                                    stx eval_opt__col_index

; -------------------------------- DEBUG --------------------------------------
;                                    lda #$01                            ; Debug column #00
;                                    sta eval_opt__col_index
;                                    tax
; -----------------------------------------------------------------------------

                                    lda #$18
                                    sta opt__baseline

                                    ldy #$00
opt_bottom__find_baseline_loop      lda ($30),y                                 ; Load column bottom diff of rotated tetromino
                                    bmi opt_bottom__next_baseline_opt           ; if negative ($ff), this column is empty - keep as is

                                    cpx #$02                                    ; Check if column is out of bounds, if so this option
                                    bcc option_is_impossible                    ; is not a possible move and we can abort further calculation

                                    cpx #$0c
                                    bcs option_is_impossible

                                    clc                                         ; Add the current floor height of the corresponding well column
                                    adc CPU__WELL_FLOOR-2,x

                                    cmp opt__baseline                           ; Check to see if this is the lowest value encountered so far
                                    bcs opt_bottom__next_baseline_opt

                                    sta opt__baseline                           ; if so, store as new baseline

opt_bottom__next_baseline_opt       inx                                         ; Set up next iteration
                                    iny
                                    cpy #$04
                                    bne opt_bottom__find_baseline_loop


                                    lda ($30),y
                                    sta opt__head_mod

                                    iny
                                    lda ($30),y
                                    sta opt__tail_mod

opt_bottom_apply_baseline           ldy #$03
opt_bottom_apply_baseline_loop      lda ($30),y
                                    bmi store_bottom
                                    lda opt__baseline
                                    sec
                                    sbc ($30),y
store_bottom                        sta opt__bottoms,y
                                    dey
                                    bpl opt_bottom_apply_baseline_loop

                                    rts

; +---------------------------------------------------------------------------+
; | CPU OPPONENT - MOVE_OPTION_IS_IMPOSSIBLE                                  |
; | option_is_impossible                                                      |
; +---------------------------------------------------------------------------+
option_is_impossible                lda #$ff
                                    sta eval_opt__weight
                                    jmp weight_calculation_done


; +---------------------------------------------------------------------------+
; | CPU OPPONENT - SET TETROMINO BOTTOM POINTER                               |
; | cpu__set_tetromino_bottom_ptr                                             |
; +---------------------------------------------------------------------------+
; | Sets pointer $30-$31 to the row in the tetromino_bottoms table that       |
; | corresponds to the current values of PLAYER2__CURRENT_TETROMINO_TYPE and  |
; | CPU__COLUMN_ROT_INDEX                                                     |
; +---------------------------------------------------------------------------+
; | Depends on:                                                               |
; +---------------------------------------------------------------------------+
; | PLAYER2__CURRENT_TETROMINO_TYPE - Current tetromino type/shape            |
; |                                   ($00 - $07)                             |
; | CPU__COLUMN_ROT_INDEX           - Rotation currently being evaluated      |
; |                                   ($00 - $03)                             |
; +---------------------------------------------------------------------------+
; | Affects:                                                                  |
; +---------------------------------------------------------------------------+
; | $30-31                  - Forwarded to row in tetromino_bottoms table     |
; |                           current shape and rotation                      |
; | eval_opt__rotation      - Sets the rotation index of the currently        |
; |                           evaluated option                                |
; +---------------------------------------------------------------------------+
cpu__set_tetromino_bottom_ptr:
                                    lda #$00                                        ; Set pointer offset to #$00
                                    clc
                                    ldy PLAYER2__CURRENT_TETROMINO_TYPE             ; Load tetromino type for use as counter
forward_tetr_bottoms_ptr_loop       beq fwd_tetr_done                               ; if the counter is zero, there is no more looping to be done
                                    adc #$18                                        ; Add the size of a tetromino entry in the
                                    dey                                             ; lookup table to the offset value.
                                    jmp forward_tetr_bottoms_ptr_loop

fwd_tetr_done                       adc #<tetromino_bottoms                         ; Add the base address to the lookup table to the offset value
                                    sta $30
                                    lda #>tetromino_bottoms
                                    adc #$00                                        ; Add any carry from setting the lsb
                                    sta $31

evaluate_opt_rotation               lda CPU__COLUMN_ROT_INDEX                       ; Move Y index to rotation line in table
                                    sta eval_opt__rotation
                                    tay

; -------------------------------- DEBUG --------------------------------------
;                                    ldy #$00                                        ;; DEBUG SET ROTATION
;                                    sty eval_opt__rotation
; -----------------------------------------------------------------------------

                                    lda #$00
                                    cpy #$00
inc_rot_offset_loop                 beq add_rot_offset
                                    clc
                                    adc #$06
                                    dey
                                    jmp inc_rot_offset_loop

add_rot_offset                      clc
                                    adc $30
                                    sta $30
                                    lda $31
                                    adc #$00
                                    sta $31

                                    rts


tetr_cell_index                     .byte $00

opt__baseline                       .byte $00
opt__head_mod                       .byte $00
opt__tail_mod                       .byte $00
opt__head_height                    .byte $00
opt__tail_height                    .byte $00
opt__bottoms                        .byte $00, $00, $00, $00

positive_mods                       .byte $00
negative_mods                       .byte $00

calc_weight__fill                   .byte $00
calc_weight__cavern                 .byte $00
calc_weight__cliff                  .byte $00

prev_contact_height                 .byte $ff




eval_option:
eval_opt__col_index                 .byte $00   ; Column Index
eval_opt__rotation                  .byte $00   ; Rotation
eval_opt__weight                    .byte $00   ; Weight/Score
eval_opt__lines                     .byte $00   ; Resulting Lines

best_option:
best_opt__col_index                 .byte $00   ; Column Index
best_opt__rotation                  .byte $00   ; Rotation
best_opt__weight                    .byte $00   ; Weight/Score
best_opt__lines                     .byte $00   ; Resulting Lines


well_offsets
                                    .byte $0b, $00
                                    .byte $07, $15

nextbox_offsets                     .byte $04, $00
                                    .byte $00, $22

text_LINES                          .byte $04, $05
                                    .text "LINES"

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
                                    .byte <(PLAYER1__CURRENT_TETROMINO_CELLS), >(PLAYER1__CURRENT_TETROMINO_CELLS)
                                    .byte <(PLAYER2__CURRENT_TETROMINO_CELLS), >(PLAYER2__CURRENT_TETROMINO_CELLS)
                                    .byte <(ROTATE_BUFFER), >(ROTATE_BUFFER)

table_found_lines_buffers           .byte <(PLAYER1__FOUND_LINES), >(PLAYER1__FOUND_LINES)
                                    .byte <(PLAYER2__FOUND_LINES), >(PLAYER2__FOUND_LINES)

table_tetromino_rotation_mod
                                    .byte $03
                                    .byte $04
                                    .byte $04
                                    .byte $03
                                    .byte $04
                                    .byte $04
                                    .byte $04

table_tetromino_well_x_mod          .byte $00
                                    .byte $ff
                                    .byte $00
                                    .byte $01
                                    .byte $00
                                    .byte $00
                                    .byte $00

tetromino_data:
tetromino_1                         .byte $01, $01    ;
                                    .byte $02, $01    ; ##
                                    .byte $01, $02    ; ##
                                    .byte $02, $02    ;

tetromino_2                         .byte $02, $01    ;
                                    .byte $02, $02    ;  #
                                    .byte $02, $03    ;  #
                                    .byte $03, $03    ;  ##

tetromino_3                         .byte $02, $01    ;
                                    .byte $02, $02    ;  #
                                    .byte $02, $03    ;  #
                                    .byte $01, $03    ; ##

tetromino_4                         .byte $01, $00    ; #
                                    .byte $01, $01    ; #
                                    .byte $01, $02    ; #
                                    .byte $01, $03    ; #

tetromino_5                         .byte $01, $01    ;
                                    .byte $02, $01    ; ##
                                    .byte $02, $02    ;  ##
                                    .byte $03, $02    ;

tetromino_6                         .byte $02, $01    ;
                                    .byte $03, $01    ;  ##
                                    .byte $01, $02    ; ##
                                    .byte $02, $02    ;

tetromino_7                         .byte $02, $01    ;
                                    .byte $01, $02    ;  #
                                    .byte $02, $02    ; ###
                                    .byte $03, $02    ;

tetromino_bottoms:
                                    .byte $ff, $00, $00, $ff,   $02, $02
                                    .byte $ff, $00, $00, $ff,   $02, $02
                                    .byte $ff, $00, $00, $82,   $02, $02
                                    .byte $ff, $00, $00, $82,   $02, $02

                                    .byte $ff, $ff, $00, $00,   $03, $01
                                    .byte $ff, $00, $01, $01,   $02, $01
                                    .byte $ff, $02, $00, $ff,   $01, $03
                                    .byte $ff, $00, $00, $00,   $01, $02

                                    .byte $ff, $00, $00, $ff,   $01, $03
                                    .byte $ff, $00, $00, $00,   $02, $01
                                    .byte $ff, $ff, $00, $02,   $03, $01
                                    .byte $ff, $01, $01, $00,   $01, $02

                                    .byte $ff, $00, $ff, $ff,   $04, $04
                                    .byte $00, $00, $00, $00,   $01, $01
                                    .byte $ff, $ff, $00, $ff,   $04, $04
                                    .byte $00, $00, $00, $00,   $01, $01

                                    .byte $ff, $01, $00, $00,   $01, $01
                                    .byte $ff, $ff, $00, $01,   $02, $02
                                    .byte $ff, $01, $00, $00,   $01, $01
                                    .byte $ff, $00, $01, $ff,   $02, $02

                                    .byte $ff, $00, $00, $01,   $01, $01
                                    .byte $ff, $ff, $01, $00,   $02, $02
                                    .byte $ff, $00, $00, $01,   $01, $01
                                    .byte $ff, $01, $00, $ff,   $02, $02

                                    .byte $ff, $00, $00, $00,   $01, $01
                                    .byte $ff, $ff, $00, $01,   $03, $01
                                    .byte $ff, $01, $00, $01,   $01, $01
                                    .byte $ff, $01, $00, $ff,   $01, $03



nextbox_data                        .byte $00, $85
                                    .byte $01, $83
                                    .byte $01, $83
                                    .byte $01, $83
                                    .byte $01, $83
                                    .byte $01, $86
                                    .byte $23, $80
                                    .byte $05, $80
                                    .byte $23, $80
                                    .byte $05, $80
                                    .byte $23, $80
                                    .byte $05, $80
                                    .byte $23, $80
                                    .byte $05, $80
                                    .byte $23, $82
                                    .byte $01, $83
                                    .byte $01, $83
                                    .byte $01, $83
                                    .byte $01, $83
                                    .byte $01, $84

table_tetromino_appearance:
                                    .byte $40, $02
                                    .byte $40, $04
                                    .byte $40, $05
                                    .byte $40, $06
                                    .byte $40, $07
                                    .byte $45, $0a
                                    .byte $45, $0e
                                    .byte $45, $0f
                                    .byte $45, $0d
                                    .byte $4a, $04
                                    .byte $4a, $05
                                    .byte $4a, $03
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

*=$3000
tmp_eval_result_area:
                                    .byte $00

*=$3800
.binary "assets/chars.bin"