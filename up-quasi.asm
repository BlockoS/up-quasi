hblnk = 0xe008
vblnk = 0xe002

SCREEN_WIDTH = 40
SCREEN_HEIGHT = 25

BLOB_AREA_WIDTH = 32
BLOB_AREA_HEIGHT = SCREEN_HEIGHT

BLOB_MAX = 32

FRAME_COUNT = 520
ROTOZOOM_FRAMES = 560

org #1200

macro wait_vbl
    ; wait for vblank    
    ld hl, vblnk
    ld a, 0x7f
@wait0:
    cp (hl)
    jp nc, @wait0
@wait1:
    cp (hl)
    jp c, @wait1
endm

main:
    di
    im 1

    ld hl, buffer
    ld (hl), 0x00
    ld de, buffer+1
    ld bc, SCREEN_WIDTH*SCREEN_HEIGHT-1
    ldir

    ld hl, histogram.black
    ld (fill_color), hl
    call fill_screen

PRESS_SPACE_OFFSET = 11*SCREEN_WIDTH + SCREEN_WIDTH/2 - 6
    ld hl, press_space
    ld de, 0xd000+PRESS_SPACE_OFFSET
    ld bc, 12
    ldir

    ld hl, 0xd800+PRESS_SPACE_OFFSET
    ld (hl), 0x70
    ld de, 0xd801+PRESS_SPACE_OFFSET
    ld bc, 11
    ldir

wait_key:
    ld hl, 0xe000
    ld (hl), 0xf6 
    inc hl
    bit 4,(hl)
    jp nz, wait_key

    ld hl, song
    xor a
    call PLY_LW_Init

    ld hl, _irq_vector
    ld (0x1039),hl

	ld hl, 0xe007               ;Counter 2.
	ld (hl), 0xb0
	dec hl
	ld (hl),1
	ld (hl),0

	ld hl, 0xe007               ;100 Hz (plays the music at 50hz).
	ld (hl), 0x74
	ld hl, 0xe005
ifdef EMU
    ld (hl), 156
else
	ld (hl), 110
endif
	ld (hl), 0

	ld hl, 0xe008 ;sound on
	ld (hl), 0x01

    ei

    ld ix, groups
    ld iy, 0xd800 + 40*25 - 40 + 10
    call gfx_fill
    ld iy, 0xd000 + 40*25 - 40 + 10
    call gfx_fill
    
    ld bc, 398
wait0:
    wait_vbl
    dec bc
    ld a,b
    or c
    jp nz, wait0


    ld ix, quasi
    ld iy, 0xd800 + 40*25 - 40 + 10
    call gfx_fill
    ld iy, 0xd000 + 40*25 - 40 + 10
    call gfx_fill
    
    ld bc, 398
wait1:
    wait_vbl
    dec bc
    ld a,b
    or c
    jp nz, wait1

main_loop:

    call rotozoom

    call blobs_clear_screen
    xor a
    call blobs

    ld a,1
    call blobs

    ld hl, palette.1
    ld de, palette
    ld bc, 32
    ldir

    ld a, 2
    call blobs

    ld a, 3
    call blobs

    ld a, 4
    call blobs

    call rotozoom

    call blobs_clear_screen

    ld hl, palette.2
    ld de, palette
    ld bc, 32
    ldir

    ld a, 5
    call blobs

    ld a, 6
    call blobs

    ld hl, palette.3
    ld de, palette
    ld bc, 32
    ldir

    ld a, 7
    call blobs

    ld hl, palette.0
    ld de, palette
    ld bc, 32
    ldir

    ld a, 8
    call blobs

    ld a, 9
    call blobs

    ld ix, quasi
    ld iy, 0xd800 + 40*25 - 40 + 10
    call gfx_fill
    ld iy, 0xd000 + 40*25 - 40 + 10
    call gfx_fill
    
forever:
    jp forever

blobs_clear_screen:
    ld hl, 0xd800
    ld (hl), 0x7f
    ld de, 0xd801
    ld bc, SCREEN_WIDTH*SCREEN_HEIGHT - 1
    ldir

    ld hl, 0xd000
    ld (hl), 0xef
    ld de, 0xd001
    ld bc, SCREEN_WIDTH*SCREEN_HEIGHT - 1
    ldir
    ret

blobs:
    ld (hl), a
    add a, a
    add a, lo(gfx)
    ld l, a
    adc a, hi(gfx)
    sub l
    ld h, a
    ld a, (hl)
    inc hl
    ld b, (hl)
    ld iyl, a
    ld iyh, b
    
    ld ix, 0xd800 + 40*25 - 40 + 8
    call border8_fill

    ld ix, 0xd000 + 40*25 - 40 + 8    
    call border8_fill
    
    ld hl, FRAME_COUNT
    ld (frame+1), hl
loop:
    ld a, (frame)
    inc a
    and 63
    ld (frame), a

    ld b, 64

    ld h, hi(x_obj.0)
    ld l, a

macro SETCOORD coord, blob_id    
    ex af, af'
    ld a, (hl)
    ld (blob_{blob_id}.{coord}), a 
    ex af, af'
    add a, 64
    ld l, a
mend

    SETCOORD x, 1
    SETCOORD x, 2
    SETCOORD x, 3
    SETCOORD x, 4
    inc h
    SETCOORD x, 5
    SETCOORD x, 6
    SETCOORD y, 1
    SETCOORD y, 2
    inc h
    SETCOORD y, 3
    SETCOORD y, 4
    SETCOORD y, 5
    SETCOORD y, 6
    
repeat 6, blob_id

blob_{blob_id}.y equ $+1
    ld bc,0x08
    
    ld hl, y_offset.lo
    add hl, bc
    ld e,(hl)
    
    ld hl, y_offset.hi
    add hl, bc
    ld d,(hl)

blob_{blob_id}.x equ $+2
    ld iy,0x08
    add iy, de

repeat 8, cnt
    di
    ld (@sp_save+1), sp
   
    ld sp, blob+(cnt-1)*8
    pop de
    pop bc
    exx
    pop de
    pop bc
    
    ld a, BLOB_MAX-1    

    ld sp, iy
    
    exx

    pop hl
    add hl,de
    cp l
    jp nc, @l0
        ld l, BLOB_MAX-1
@l0    
    cp h
    jp nc, @l1
        ld h, BLOB_MAX-1
@l1
    ld e,l
    ld d,h

    pop hl
    add hl,bc
    cp l
    jp nc, @l2
        ld l, BLOB_MAX-1
@l2
    cp h
    jp nc, @l3
        ld h, BLOB_MAX-1
@l3

    exx

    pop hl
    add hl,de
    cp l
    jp nc, @l4
        ld l, BLOB_MAX-1
@l4
    cp h
    jp nc, @l5
        ld h, BLOB_MAX-1
@l5
    ld e,l
    ld d,h

    pop hl
    add hl,bc
    cp l
    jp nc, @l6
        ld l, BLOB_MAX-1
@l6
    cp h
    jp nc, @l7
        ld h, BLOB_MAX-1
@l7

    push hl
    push de
    exx
    push hl
    push de

    ld iy,BLOB_AREA_WIDTH
    add iy,sp

@sp_save:
    ld sp, 0x0000
    ei

rend 
rend

    ld a, hi(palette)
    ld h, a
    exx
    ld h, a
    exx

repeat SCREEN_HEIGHT, j
repeat BLOB_AREA_WIDTH/8, i
    di
    ld (@sp_save+1), sp

    ld sp, buffer+(i-1)*8+(j-1)*BLOB_AREA_WIDTH
    pop de
    pop bc
    exx
    pop de
    pop bc

    ld sp, 0xd800+SCREEN_WIDTH*SCREEN_HEIGHT-(i-1)*8-(j-1)*SCREEN_WIDTH

    exx
    
    ld l,e
    ld a,(hl)
    ld l,d
    ld e,(hl)
    ld d,a
    push de
    ld l,c
    ld a,(hl)
    ld l,b
    ld c,(hl)
    ld b,a
    push bc
    exx
    ld l,e
    ld a,(hl)
    ld l,d
    ld e,(hl)
    ld d,a
    push de
    ld l,c
    ld a,(hl)
    ld l,b
    ld c,(hl)
    ld b,a
    push bc
    
@sp_save:
    ld sp, 0x0000
    ei

    nop
    nop
    nop
    nop
rend
rend

    di
    ld (sp_save), sp
    
    ld sp,buffer+BLOB_AREA_WIDTH*SCREEN_HEIGHT
    ld hl,0x0000
repeat BLOB_AREA_WIDTH*SCREEN_HEIGHT/2
    push hl
rend

sp_save equ $+1
    ld sp, 0x0000
    ei

    wait_vblnk

    ld hl, (frame+1)
    dec hl
    ld (frame+1), hl
    ld a, l
    or h
    jp nz, loop

    ret

rotozoom:
    ld hl, 0xd000
    ld (hl), 0x43
    ld de, 0xd001
    ld bc, SCREEN_WIDTH*SCREEN_HEIGHT - 1
    ldir

    exx
    ld de,ROTOZOOM_FRAMES
    exx
rotoloop:
    wait_vbl
    
@zoom equ $+1
    ld a,0x00
    ld l,a
    add a,a
    ld e,a
   
    ld h,hi(cos_table)
    ld a,(hl)
    add a,16
    ld iyl,a                 ; iyl = 16 + cos (i)

    neg
    rlca
    rlca
    rlca
    rlca
    ld c,a
    or 0xf0
    ld b,a
    ld a,c
    and 0xf0
    ld c,a                  ; bc = -c * 16
    push bc
    push bc
    
    ld l,e
    ld e,(hl)               ; e = cos(2*i)
    inc h
    ld a,(hl)               ; d = sin(2*i)
    ld (@s0),a
    
    ld a,iyl
    ld l,e
    call mul
    
    srl b
    rr c
    srl b
    rr c                    ; bc = z * cos(2*i) >> 2
    
    pop hl
    add hl,bc               ; hl += bc
    ld (@du),hl
   
    ld a,iyl
@s0 equ $+1
    ld l,0x00
    call mul

    srl b
    rr c
    srl b
    rr c                    ; bc = z * sin(2*i) >> 2
    
    pop hl
    add hl,bc               ; hl += bc
    ex de,hl
@du equ $+1
    ld bc,0x0000

    ld hl,de
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    ld (@u0),hl
    ld hl,bc
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    ld (@v0),hl

    exx    
    ld hl, 0xd800+(SCREEN_WIDTH*SCREEN_HEIGHT)
    ld bc,-SCREEN_WIDTH
    exx
    
    ex af,af'
    ld a,SCREEN_HEIGHT
        
.loop_y:
@u0 equ $+2
    ld ix,0x0000
@v0 equ $+2
    ld iy,0x0000

    di

    exx
    ld  (@sp_backup),sp
    ld sp,hl
    exx

    ex af,af'
repeat SCREEN_WIDTH/2
        ld a, ixh
        xor iyh
        and $50
        ld h,a
        
        add ix, bc
        add iy, de
        
        ld a, ixh
        xor iyh
        and $50
        ld l,a
        
        push hl
        
        add ix, bc
        add iy, de
rend
    exx
@sp_backup equ $+1
    ld  sp,0x0000
    add hl,bc
    exx

    ei

    ld hl,(@u0)
    scf
    ccf
    sbc hl,de
    ld (@u0),hl

    ld hl,(@v0)
    add hl,bc
    ld (@v0),hl

    ex af,af'
    dec a
    jp nz,.loop_y

    ld hl,@zoom
    inc (hl)    

    exx
    dec de
    ld a,d
    or e
    exx
    jp nz,rotoloop
    ret


mul:
    ld d, sqr2_lo>>8
    ld e,l
         
    ld b,0
    ld c,a

    ld h, sqr1_lo>>8
    add hl,bc

    ex de, hl

    xor 0xff
    ld c,a
    add hl,bc

    ld a,(de)
    inc d
    inc d
    ld c,(hl)
    inc h
    inc h
    
    sub c
    ld c,a

    ld a,(de)
    ld b,(hl)
    
    sbc b
    ld b,a
        
    ret

frame:
    defb 0
    defw 0
 
y_offset.lo:
repeat SCREEN_HEIGHT, cnt
    defb lo(buffer + ((cnt-1)*BLOB_AREA_WIDTH))
rend

y_offset.hi:
repeat SCREEN_HEIGHT, cnt
    defb hi(buffer + ((cnt-1)*BLOB_AREA_WIDTH))
rend

blob:
j=-4
while j<4
    i=-4
    while i<4
        d=(i+0.5)*(i+0.5) + (j+0.5)*(j+0.5)
        if d<=16.0
            f=d/16.0
            f=1.0-f*f
            f=f*f*f*f
            defb int(f*(BLOB_MAX-1) + 0.5)
        else
            defb 0
        endif
        i=i+1
    wend
    j=j+1
wend

; clear screen animation -------------------------------------------------------
fill_screen:
    ld iy, SCREEN_HEIGHT
    ld ix, 0xd000+SCREEN_WIDTH

fill_screen.loop
    ld bc, 0
    
fill_line:

    ld hl, vblnk
    ld a, 0x7f
.wait_2:
    cp (hl)
    jp nc, .wait_2
.wait_3:
    cp (hl)
    jp c, .wait_3

    di

    ld (transition_sp_save), sp

    ld sp, ix
    
    ld hl, histogram.attr
    add hl, bc
    ld a, (hl)
    ld h, a
    ld l, a

    repeat SCREEN_WIDTH/2
    push hl
    rend
        
    ld hl, 0x800+SCREEN_WIDTH
    add hl, sp
    ld sp, hl
    
fill_color equ $+1
    ld hl, histogram.color
    add hl, bc
    ld a, (hl)
    ld h, a
    ld l, a
    repeat SCREEN_WIDTH/2
    push hl
    rend
    
transition_sp_save equ $+1
    ld sp, 0x0000

    ei
        
    inc c
    ld a, 7
    cp c
    jp nz, fill_line

    ld de,SCREEN_WIDTH
    add ix,de
    
    dec iyl
    jp nz, fill_screen.loop
    
    ret
    
histogram.color:
    defb 0x70,0x70,0x70,0x07,0x07,0x07,0x77
histogram.attr:
    defb 0x70,0x36,0x7a,0x7e,0x3e,0x3c,0x7a
histogram.white:
    defb 0x71,0x71,0x71,0x17,0x17,0x17,0x77
histogram.red:
    defb 0x21,0x21,0x21,0x12,0x12,0x12,0x22
histogram.blue:
    defb 0x01,0x01,0x01,0x10,0x10,0x10,0x00
histogram.black:
    defb 0x01,0x01,0x01,0x10,0x10,0x10,0x00

press_space:
    defb 0x10,0x12,0x05,0x13,0x13,0x00,0x00,0x13,0x10,0x01,0x03,0x05

gfx_fill:
    ld a, 25
.l0:
    ld l, 4
.l1:
    ld (@gfx_fill.save), sp
    di
    
    ld sp, ix
    ld bc, 10
    add ix, bc
    
    pop bc  
    pop de
    exx
    pop hl
    pop bc
    pop de
    
    ld sp, iy
    push de
    push bc
    push hl
    exx
    push de
    push bc

    ld bc,  10
    add iy, bc

@gfx_fill.save equ $+1
    ld sp, 0x0000
    ei
    
    dec l
    jp nz, .l1

    ld bc, -80
    add iy, bc
    
    dec a
    jp nz, .l0

    ret

border8_fill:
    
    ld a, 25
.loop:
    ld (@b8sp_save), sp
    di
    
    ld sp, iy
    ld bc, 8
    add iy, bc   

    pop bc
    pop de
    
    exx
    pop bc
    pop de
    
    ld sp, ix
    push de
    push bc
    
    exx
    push de
    push bc

@b8sp_save equ $+1
    ld sp, 0x0000
    ei

    ld bc, -40
    add ix, bc
        
    dec a
    jp nz, .loop

    ret

quasi:
    incbin "./data/quasi.bin"

groups:
    incbin "./data/groups.bin"

gfx00:
    incbin "./data/gfx00.bin"
gfx01:
    incbin "./data/gfx01.bin"
gfx02:
    incbin "./data/gfx02.bin"
gfx03:
    incbin "./data/gfx03.bin"
gfx04:
    incbin "./data/gfx04.bin"
gfx05:
    incbin "./data/gfx05.bin"
gfx06:
    incbin "./data/gfx06.bin"
gfx07:
    incbin "./data/gfx07.bin"
gfx08:
    incbin "./data/gfx08.bin"
gfx09:
    incbin "./data/gfx09.bin"

gfx:
    defw gfx00, gfx01, gfx02
    defw gfx03, gfx04, gfx05
    defw gfx06, gfx07, gfx08
    defw gfx09
    
player: include "PlayerLightweight_SHARPMZ700.asm"
song: include "data/music.asm"

_irq_vector:
    di

    push af
    push hl
    push bc
    push de
    push ix
    push iy
    exx
    push af
    push hl
    push bc
    push de
    push ix
    push iy
    
    ld hl, 0xe006
    ld a,1
    ld (hl), a
    xor a
    ld (hl), a
    
    call PLY_LW_Play        
    
    pop iy
    pop ix
    pop de
    pop bc
    pop hl
    pop af
    exx
    pop iy
    pop ix
    pop de
    pop bc
    pop hl
    pop af

    ei
    reti

align 256
include "./data/trajectory.inc"

cos_table:
repeat 256, i
    defb 63 * cos(i * 360 / 256) + 63
rend

sin_table:
repeat 256, i
    defb 63 * sin(i * 360 / 256) + 63
rend

sqr1_lo:
i = 0
while i<512
    m = (i*i) / 4
    defb lo(m)
    i = i+1
wend

sqr1_hi:
i = 0
while i<512
    m = (i*i) / 4
    defb hi(m)
    i = i+1
wend

sqr2_lo:
i = 0
while i<512
    m = ((255 - i) * (255 - i)) / 4;
    defb lo(m)
    i = i+1
wend

sqr2_hi:
i = 0
while i<512
    m = ((255 - i) * (255 - i)) / 4;
    defb hi(m)
    i = i+1
wend

align 256
palette:
    defb 0x00,0x00,0x10,0x10,0x10,0x10,0x10,0x10
    defb 0x10,0x10,0x11,0x11,0x11,0x11,0x11,0x11
    defb 0x11,0x15,0x15,0x15,0x15,0x55,0x55,0x55
    defb 0x55,0x55,0x57,0x57,0x57,0x57,0x77,0x77
palette.0:
    defb 0x00,0x00,0x10,0x10,0x10,0x10,0x10,0x10
    defb 0x10,0x10,0x11,0x11,0x11,0x11,0x11,0x11
    defb 0x11,0x15,0x15,0x15,0x15,0x55,0x55,0x55
    defb 0x55,0x55,0x57,0x57,0x57,0x57,0x77,0x77
palette.3:
    defb 0x00,0x00,0x10,0x10,0x10,0x10,0x10,0x10
    defb 0x10,0x10,0x11,0x11,0x11,0x11,0x11,0x11
    defb 0x11,0x15,0x15,0x15,0x15,0x55,0x55,0x55
    defb 0x55,0x55,0x57,0x57,0x57,0x57,0x77,0x77
palette.2:
    defb 0x00,0x00,0x40,0x40,0x40,0x40,0x40,0x40
    defb 0x40,0x40,0x44,0x44,0x44,0x44,0x44,0x44
    defb 0x44,0x46,0x46,0x46,0x46,0x66,0x66,0x66
    defb 0x66,0x66,0x67,0x67,0x67,0x67,0x77,0x77
palette.1:
    defb 0x00,0x00,0x20,0x20,0x20,0x20,0x20,0x20
    defb 0x20,0x20,0x22,0x22,0x22,0x22,0x22,0x22
    defb 0x22,0x23,0x23,0x23,0x23,0x33,0x33,0x33
    defb 0x33,0x33,0x37,0x37,0x37,0x37,0x77,0x77

; put buffer at the end (we don't need to transfer empty space)
buffer:
