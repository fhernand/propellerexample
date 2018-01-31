'' +--------------------------------------------------------------------------+
'' | Cluso's ILI9163 1.44in SPI 128*128 V1.1 LCD Driver                V0.51  |
'' +--------------------------------------------------------------------------+
'' |  Authors:       (c)2015-2016  "Cluso99" (Ray Rodrick)                    |
'' |  Modifications:                                                          |
'' |  License:       MIT License - See end of file for terms of use           |
'' +--------------------------------------------------------------------------+
''
'' RR20150101       Commence
''                  Screen is 128*128 offset by 32
'' RR20150121  023  drawLine, drawCircle, drawPixel, setWindow, fillWindow, fillRectangle, drawChar working
''             030  clock working (fast, no time set, etc)  
''             032  digital clock
''             033  double height clock
''             035  tripple height double width clock
''             036  now use black pcb version (upside down and no offset)
''             038  simplify support routines
'' RR20150130  040  demo release
'' RR20150219  050  power LCD from prop pins (~2.0mA) [DID NOT WORK - must want 5V not 3V3 else change params!!!]
'' RR20161011  051  P8XBlade2 (power from prop pins - v050 didn't enable PWR/GND DIRA pins) 
'' RR20161011  052  create lcdSetup routine in low level routines (ready for pasm)
''             053  some fast pasm routines wkg in separate cog (delays added for viewing)
''             054  code tidy
'' RR20161015  055  Release


''+-----------------------------------------------------+
''| Notes:                                              |
''+-----------------------------------------------------+
''  1. Supports both the RED and BLACK LCD boards 
''      (modify con "yoffset")
''  2. The Digital and Round Clocks are only demo versions



CON


' _XINFREQ      = 5_000_000         '\ set for 5.00MHz (default)
' _CLKMODE      = xtal1 + pll16x    '/

' _XINFREQ      = 6_000_000         '\ set for 6.00MHz
' _CLKMODE      = xtal1 + pll16x    '/

' _XINFREQ      = 6_500_000         '\ set for 6.50MHz (CpuBlade/RamBlade/RamBlade3/TriBlade#2)
' _CLKMODE      = xtal1 + pll16x    '/

 ' _XINFREQ      = 12_000_000        '\ set for 12.00MHz (P8XBlade2)
 ' _CLKMODE      = xtal1 + pll8x     '/

' LCD Connector J2
  PIN_3V3       = 0 '   '0         ' 3v3 pwr supplied by prop pin
  PIN_GND       = 1 '   '1         ' GND pwr supplied by prop pin
  PIN_CE        = 11 '0  '2         ' Serial clock enable pin
  PIN_RST       = 12 '1  '3         ' Reset pin
  PIN_DC        = 13 '2  '4         ' Data / Command selection pin
  PIN_SDA       = 10 '3  '5         ' Serial Data pin
  PIN_CLK       = 9 '4  '6         ' Serial Clock pin
  PIN_LED       = 14 '5  '7         ' LED backlight enable pin

' ILI9163C registers...
  LCD_NOP               = $00
  LCD_SW_RESET          = $01   
  LCD_SLEEP_ON          = $10
  LCD_SLEEP_OFF         = $11
  LCD_PARTIAL           = $12
  LCD_NORMAL            = $13
  LCD_INVERSION_OFF     = $20
  LCD_INVERSION_ON      = $21
  LCD_GAMMA_SET         = $26
  LCD_DISPLAY_OFF       = $28
  LCD_DISPLAY_ON        = $29
  LCD_COL_ADR           = $2A
  LCD_ROW_ADR           = $2B
  LCD_RAM_WRITE         = $2C
  LCD_COLOR_SPACE       = $2D   '4K/65K/262K
' LCD_RAMRD             = $2E
  LCD_MADCTR            = $36   'Mem Addr Contol (rotate screen)
  LCD_PIXEL_FORMAT      = $3A
  LCD_FRAME_CTL1        = $B1
  LCD_INVERSION_CTL     = $B4
  LCD_POWER_CTL1        = $C0
  LCD_POWER_CTL2        = $C1
  LCD_VCOM_CTL1         = $C5
  LCD_VCOM_OFFSET       = $C7
  LCD_POS_GAMMA         = $E0
  LCD_NEG_GAMMA         = $E1
  LCD_GAMMA_RS          = $F2

' LCD_ALL_ON            = $A4
' LCD_ALL_OFF           = $A5

' BGR 5+6+5 16bits/pixel (RGB reversed)
  black = $0000
  white = $FFFF
  blue  = $F800
  green = $07E0
  red   = $001F
  yellow= red+green
  purple= red+blue

  width = 132                   ' screen width  (cols)
  height= 132                  ' screen height (rows)
  yoffset= 0                    '\  black pcb = 0 (screen y offset)
' yoffset= 32                   '/  red   pcb = 32

'                 ' sin(x) * 1024    ' degrees
Z00     =    0    ' 0.0000 * 1024    '  0 
Z01     =  107    ' 0.1045 * 1024    '  6
Z02     =  213    ' 0.2079 * 1024    ' 12
Z03     =  316    ' 0.3090 * 1024    ' 18
Z04     =  416    ' 0.4067 * 1024    ' 24
Z05     =  512    ' 0.5000 * 1024    ' 30
Z06     =  602    ' 0.5878 * 1024    ' 36
Z07     =  685    ' 0.6691 * 1024    ' 42
Z08     =  761    ' 0.7431 * 1024    ' 48
Z09     =  828    ' 0.8090 * 1024    ' 54
Z10     =  887    ' 0.8660 * 1024    ' 60
Z11     =  935    ' 0.9135 * 1024    ' 66
Z12     =  974    ' 0.9511 * 1024    ' 72
Z13     = 1002    ' 0.9781 * 1024    ' 78
Z14     = 1018    ' 0.9945 * 1024    ' 84
Z15     = 1024    ' 1.0000 * 1024    ' 90
'180    =     ' 0.0000 * 1024    '    180
'270    =     '-1.0000 * 1024    '    270

VAR

  long  time, hh, mm, ss
  long  row, col, fgcolor, bgcolor                      ' for text
  long  left, right, top, bottom                        ' current screen window
  long  fontpixels                                      ' 8x8 font pixels (2 longs)

' mailbox for PASM cog                                  '\
  long  mailbox                                         '| command(8b), spare(8b), param(16/8b)
  long  mailbox1                                        '| params(32b)
  long  mailbox2                                        '| 
  long  mailbox3                                        '/ fgcolor<<16 | bgcolor

  long  cog                                             ' pasm cog+1

PUB start 

  col~                                                  ' 0..15
  row~                                                  ' 0..15
  fgcolor := white
  bgcolor := black 
  
  lcdInit                                               ' sw/hw initialise LCD

  clearScreen

  main_digitalclock



PRI main_digitalclock | s
' Digital Clock

  clearScreen
  fgcolor := green
  bgcolor := black
  
' preset time hh:mm:ss
  hh := 11
  mm := 30
  ss := 0

  time := cnt
  s    := clkfreq               ' 1s
  time += s                     ' +1s

  repeat
    waitcnt(time += s)          ' +1s    
    ss ++
    if ss => 60
      ss~
      mm++
      if mm => 60
        mm~
        hh++
        if hh => 24
          hh~
    ' draw time hh:mm:ss          
    row~
    col~
    drawWHChar(3, 6, hh /  10 + "0")
    drawWHChar(3, 6, hh // 10 + "0")
    drawWHChar(3, 6, ":")
    drawWHChar(3, 6, mm /  10 + "0")
    drawWHChar(3, 6, mm // 10 + "0")
    row := 64
    col := 64
    drawWHChar(2, 4, ":")
    drawWHChar(2, 4, ss /  10 + "0")
    drawWHChar(2, 4, ss // 10 + "0")


''+-----------------------------------------------------+
''| High Level Drivers                                  |
''+-----------------------------------------------------+
  
PRI clearScreen
' Clear Screen
  setWindow(0,0,width-1,height-1)                       ' full screen 128*128
  fillWindow(black)                                     ' clear


PRI drawLine(xs, ys, xe, ye, rgb) | i, x, y
' Draw Line - start co-ords, end co-ords, color    
  'plot incrementing x axis
  repeat i from 0 to xe-xs
    y := (ye-ys)*i/(xe-xs)
    drawPixel(xs+i, ys+y, rgb)
  'plot incrementing y axis  
  repeat i from 0 to ye-ys
    x := (xe-xs)*i/(ye-ys)
    drawPixel(xs+x, ys+i, rgb)    

PRI drawCircle(xc, yc, dia, rgb) | i, x, y, d
' Draw Circle d^2 = x^2 + y^2; y = ^^(d^2 - x^2) (Pythagoras theorum)
  repeat i from 0 to dia-1
    x := i
    y := ^^((dia*dia) - (x*x))
    'plot incrementing x axis
    drawPixel(x+xc, y+yc, rgb)
    drawPixel(xc-x, y+yc, rgb)
    drawPixel(x+xc, yc-y, rgb)
    drawPixel(xc-x, yc-y, rgb)
    'plot incrementing y axis
    drawPixel(y+yc, x+xc, rgb)
    drawPixel(yc-y, x+xc, rgb)
    drawPixel(y+yc, yc-x, rgb)
    drawPixel(yc-y, yc-x, rgb)

PRI drawPixel(x, y, rgb) | i
' Draw 1 pixel
  setWindow(x, y, x, y)                                 ' 1 pixel
  fillWindow(rgb)

PRI fillRectangle(xs, ys, xe, ye, rgb)
  setWindow(xs, ys, xe, ye)
  fillWindow(rgb)

PRI drawWHChar(w, h, char) | c, i, j
' Draw Wx width, Hx height, char (multi-width & multi-height 8*8 font)
  setWindow(col, row, col+(w*8-1), row+(h*8-1))
  lcdWriteCmd(LCD_RAM_WRITE)
  c := char & $7F

  repeat j from 0 to 1                                  ' 2 sets of 4 rows of pixels...
    fontpixels := long[@font][c+j*128]                  '   get first/second 4 rows of pixels
    ' draw 1st row h*
    repeat h
      repeat i from 0 to 7
        drawWPixels(w, fontpixels & (1<<i))
    ' draw 2nd row h*
    repeat h
      repeat i from 8 to 15
        drawWPixels(w, fontpixels & (1<<i))
    ' draw 3rd row h*
    repeat h
      repeat i from 16 to 23
        drawWPixels(w, fontpixels & (1<<i))
    ' draw 4th row h*
    repeat h
      repeat i from 24 to 31
        drawWPixels(w, fontpixels & (1<<i))

  col += w*8
  if col => width
    col~
    row += h*8
    if row => height
     row~

PRI drawWPixels(w, bool)
' Draw W* pixel(s)
  repeat w
    if (bool)      
      lcdWriteData16(fgcolor)
    else                          
      lcdWriteData16(bgcolor)


''+-----------------------------------------------------+
''| Initialise LCD                                      |
''+-----------------------------------------------------+

PRI lcdInit                     
' Initialise LCD 1.44' SPI 128*128 V1.1

  lcdSetup                                              ' preset output pins, then enable output pins

  waitcnt(clkfreq/10 + cnt)                             ' 100ms

' Reset the LCD (either HW or SW - both work)
  lcdReset                                              ' HW Reset
' lcdWriteCmd(LCD_SW_RESET)                             ' SW Reset
  waitcnt(clkfreq/2 + cnt)                              ' 500ms

' Sleep mode off
  lcdWriteCmd(LCD_SLEEP_OFF)                           
  waitcnt(clkfreq/1000*5 + cnt)                         ' 5ms

' Pixel Format 16bits/pixel
  lcdWriteCmd(LCD_PIXEL_FORMAT)
  lcdWriteData($05)                                     ' 16b/pixel


{{
Gamma modes not required
  lcdWriteCmd(LCD_GAMMA_SET)
  lcdWriteData($04)                                     ' curve 3
  lcdWriteCmd(LCD_GAMMA_RS)
  lcdWriteData($01)                                     ' gamma adj enabled
  waitcnt(clkfreq/1000 + cnt)                           ' 1ms
  lcdWriteCmd(LCD_POS_GAMMA)
  lcdWriteData($3F)                                     ' 15 params
  lcdWriteData($25)                                    
  lcdWriteData($1C)                                    
  lcdWriteData($1E)                                    
  lcdWriteData($20)                                    
  lcdWriteData($12)                                    
  lcdWriteData($2A)                                    
  lcdWriteData($90)                                    
  lcdWriteData($24)                                    
  lcdWriteData($11)                                    
  lcdWriteData($00)                                    
  lcdWriteData($00)                                    
  lcdWriteData($00)                                    
  lcdWriteData($00)                                    
  lcdWriteData($00)
                                  
  lcdWriteCmd(LCD_NEG_GAMMA)
  lcdWriteData($20)                                     ' 15 params
  lcdWriteData($20)                                    
  lcdWriteData($20)                                    
  lcdWriteData($20)                                    
  lcdWriteData($05)                                    
  lcdWriteData($00)                                    
  lcdWriteData($15)                                    
  lcdWriteData($A7)                                    
  lcdWriteData($3D)                                    
  lcdWriteData($18)                                    
  lcdWriteData($25)                                    
  lcdWriteData($2A)                                    
  lcdWriteData($2B)                                    
  lcdWriteData($2B)                                    
  lcdWriteData($3A)
}}


{{ 007
Frame and Power modes default correctly              
  lcdWriteCmd(LCD_FRAME_CTL1)                           ' normal/full
  lcdWriteData($08)                                     ' DIVA=8
  lcdWriteData($08)                                     ' VPA=8
  waitcnt(clkfreq/1000 + cnt)                           ' 1ms
  
  lcdWriteCmd(LCD_INVERSION_CTL)
  lcdWriteData($07)                                     ' NLA=NLB=NLC=1
  waitcnt(clkfreq/1000 + cnt)                           ' 1ms
  lcdWriteCmd(LCD_POWER_CTL1)
  lcdWriteData($0A)                                     ' VRH=10 GVDD=4.30
  lcdWriteData($02)                                     ' VC=2 VCI1=2.65
  waitcnt(clkfreq/1000 + cnt)                           ' 1ms
  lcdWriteCmd(LCD_POWER_CTL2)
  lcdWriteData($02)                                     ' BT=2 AVDD=2xVCI1 VCL=-1xVCI1 VGH=5xVCI1 VGL=-2xVCI1
  waitcnt(clkfreq/1000 + cnt)                           ' 1ms
  lcdWriteCmd(LCD_VCOM_CTL1)
  lcdWriteData($50)                                     ' VMH=80 VCOMH=4.5
  lcdWriteData($5B)                                     ' VML=91 VCOML=-0.225
  waitcnt(clkfreq/1000 + cnt)                           ' 1ms
  lcdWriteCmd(LCD_VCOM_OFFSET)
  lcdWriteData($40)                                     ' NVM=0 VMF=64 VCOMH=VMH VCOML=VML
  waitcnt(clkfreq/1000 + cnt)                           ' 1ms
}}

{{
Row & Cols default correctly 128*128
  lcdWriteCmd(LCD_COL_ADR)
  lcdWriteData($00)                                     ' XSH
  lcdWriteData($00)                                     ' XSL
  lcdWriteData($00)                                     ' XEH
  lcdWriteData($7F)                                     ' XEL (128 pixels wide)
  lcdWriteCmd(LCD_ROW_ADR)
  lcdWriteData($00)                                     ' YSH
  lcdWriteData($00)                                     ' YSL
  lcdWriteData($00)                                     ' YEH
  lcdWriteData($7F)                                     ' YEL (128 pixels high)
}}

' set display orientation
  lcdWriteCmd(LCD_MADCTR)                               ' mem addr ctrl
  if yoffset <> 0                                       ' black=32, red=0
    lcdWriteData($00)                                   ' orientation 0deg
  else
    lcdWriteData($C0)                                   ' orientation 180deg
'   lcdWriteData($A0)                                   ' orientation 90deg
'   lcdWriteData($60)                                   ' orientation 270deg

' Display On
  lcdWriteCmd(LCD_DISPLAY_ON)
  waitcnt(clkfreq/1000 + cnt)                           ' 1ms


''+-----------------------------------------------------+
''| Low Level Drivers - call fast PASM via mailbox(es)  |
''+-----------------------------------------------------+

PRI lcdSetup
  'start pasm cog and wait till running
  mailbox := $00FF                                      ' non-zero
  cog := cognew(@entry, @mailbox) ' +1                    ' start LCD Driver Cog
  repeat while mailbox <> 0                             ' wait until cog running

PRI lcdReset
  mailbox := $FF_FF_0000                                ' hw reset LCD
  repeat while mailbox <> 0
  
PRI lcdWriteData16(val)
  lcdWriteData(val >> 8)
  lcdWriteData(val & $FF)

PRI lcdWriteData(val)
  mailbox := $200 | val                                 ' dc=data=1
  repeat while mailbox <> 0

PRI lcdWriteCmd(val)
  mailbox := val                                        ' dc=cmd=0
  repeat while mailbox <> 0

PRI setWindow(xs, ys, xe, ye)
  left   := xs
  top    := ys
  right  := xe
  bottom := ye
  mailbox1 := (xs<<24) | (ys<<16) | (xe<<8) | ye        ' xs, ys, xe, ye 
  mailbox := $81_FF_0000
  repeat while mailbox <> 0

PRI fillWindow(rgb) | n
' Fill Window/Rectangle - calc pixels & write
  n := (right - left +1)*(bottom - top +1)              ' calc no. of pixels
  mailbox1 := (rgb << 16) | n
  mailbox := $82_FF_0000
  repeat while mailbox <> 0

PRI drawChar(char) 
' Draw a Char (8*8 font)
  setWindow(col, row, col+7, row+7)                     ' set 8*8 font pixel window

  mailbox3 := (fgcolor << 16) | bgcolor
  mailbox  := $83_FF_0000 | (char & $7F)
  repeat while mailbox <> 0

  col += 8
  if col => width
    col~
    row += 8
    if row => height
     row~


DAT
''+-----------------------------------------------------+
''| LCD Low Level Drivers                               |
''+-----------------------------------------------------+
                org     0
entry           mov     hubptr, par                     ' save hub ptr
                mov     hubptr1, hubptr
                add     hubptr1, #4
                mov     hubptr2, hubptr1
                add     hubptr2, #4
                mov     hubptr3, hubptr2
                add     hubptr3, #4

setup           mov     outa, outmask                   ' preset levels
                mov     dira, dirmask                   ' enable outputs
                call    #wait50
                call    #wait50

done            wrlong  zero, hubptr                    ' clear hub mailbox
wait            rdlong  data, hubptr      wz            ' wait for something to do
        if_z    jmp     #wait

                rdlong  data1, hubptr1                  ' get params(1)

                mov     command, data                   '\ extract command
                shr     command, #24                    '|
                cmp     command, #$00     wz            '| $00= write cmd/data from spin
        if_e    jmp     #writex                         '|
                cmp     command, #$FF     wz            '| $FF= HW Reset
        if_e    jmp     #reset                          '|
                cmp     command, #$81     wz            '| $81= Set window xs,ys,xe,ye
        if_e    jmp     #window                         '|
                cmp     command, #$82     wz            '| $82= Fill window rgb
        if_e    jmp     #fill                           '|
                cmp     command, #$83     wz            '| $83= paint char
        if_e    jmp     #paintchar                      '|
                jmp     #done                           '/ unknown command, ignore

                
''+-----------------------------------------------------+
''| LCD SetWindow(xs,ys,xe,ye)                          |
''+-----------------------------------------------------+
window          mov     data, #LCD_COL_ADR
                call    #writecmd
                mov     data, data1                     '\ xs
                shr     data, #24                       '|
               'and     data, #$FF                      '/                      (not reqd)
                call    #write16data

                mov     data, data1                     '\ xe
                shr     data, #8                        '|
                and     data, #$FF                      '/
                call    #write16data

                mov     data, #LCD_ROW_ADR
                call    #writecmd
                mov     data, data1                     '\ ys
                shr     data, #16                       '|
                add     data, #yoffset                  '| ys+offset
                and     data, #$FF                      '/
                call    #write16data
                
                mov     data, data1                     '\ ye
                add     data, #yoffset                  '| ye+offset
                and     data, #$FF                      '/
                add     data, #yoffset
                call    #write16data

                jmp     #done 

''+-----------------------------------------------------+
''| LCD FillWindow(rgb,pixels)                          |
''+-----------------------------------------------------+
fill            mov     data, #LCD_RAM_WRITE
                call    #writecmd
                mov     ctr, data1                      '\ pixels(16b)
                and     ctr, xFFFF                      '/ 
                shr     data1, #16                      ' rgb(16b)
:fill_loop      mov     data, data1                     ' rgb
                call    #write16data
                djnz    ctr, #:fill_loop
                jmp     #done

''+-----------------------------------------------------+
''| LCD PaintChar(char)   mailbox3=fgcolor/bgcolor      |
''+-----------------------------------------------------+
paintchar       rdlong  fg, hubptr3                     ' get fgcolor/bgcolor
                mov     bg, fg
                shr     fg, #16                         ' extract fgcolor
                and     bg, xFFFF                       ' extract bgcolor
                and     data, #$7F                      ' char =<$7F
                movs    paint4rows, #font               ' point to start of font
                add     paint4rows, data                ' point to first 4 rows of font pixels

                mov     data, #LCD_RAM_WRITE
                call    #writecmd
                
                call    #paint4rows
                add     paint4rows, #128                ' point to second 4 rows of font pixels
                call    #paint4rows
                jmp     #done

paint4rows      mov     pixels, 0-0                     ' get first/second 4 rows of font pixels 
                rev     pixels, #32                     ' reverse font pixels
                mov     ctr, #32                        ' 32 pixels
:paintpixel     rol     pixels, #1              wc
        if_c    mov     data, fg                        ' "1" = fgcolor
        if_nc   mov     data, bg                        ' "0" = bgcolor
                call    #write16data                    ' write fgcolor/bgcolor(16b) pixel(1)
                djnz    ctr, #:paintpixel
paint4rows_ret  ret


''+-----------------------------------------------------+
''| LCD Reset                                           |
''+-----------------------------------------------------+
reset           andn    outa, rstmask                   ' RST=0
                call    #wait50
                or      outa, rstmask                   ' RST=1
                call    #wait50
                jmp     #done

wait50          mov     ctr, cnt
                add     ctr, delay50ms
wait50_ret      ret


''+-----------------------------------------------------+
''| LCD Write 8bit CMD/DATA (b8:cmd=0/data=1)           |
''+-----------------------------------------------------+
writedata       or      data, #$100                     ' DC=1=data
writecmd
write           rol     data, #23                       ' prep 9b byte to write (msb first)
                rol     data, #1                wc      ' cmd/data ?
                muxc    outa, dcmask                    ' cmd=0 / data=1
                andn    outa, csmask                    ' CS=0
                mov     bits, #8
:nextbit        andn    outa, clkmask                   ' CLK=0
                rol     data, #1                wc      ' \ write data bit
                muxc    outa, sdamask                   ' /  SDA=0/1
                or      outa, clkmask                   ' CLK=1
                djnz    bits, #:nextbit
writedata_ret
writecmd_ret
write_ret       ret

writex          call    #write                          ' write cmd/data (from spin)
                jmp     #done

write16data     ror     data, #8                        ' upper8 in b7-0, lower8 in b31-24
                call    #writedata                      ' write upper 8 bits DC=1=data
                rol     data, #8                        ' lower8 in b7-0
                call    #writedata
write16data_ret ret


' font follows in here before "xxxx res 1" (reserved variables)

'' +--------------------------------------------------------------------------+
'' | Font section: 128 characters of 8x8 font                                 |
'' |                two blocks of 128 longs of 4x8 (4 rows of 8 pixels)       |
'' +--------------------------------------------------------------------------+
'' | Derived from: AiChip_SmallFont_Atari_lsb_001.spin                        |
'' |                LSB first version of Font_ATARI from AiGeneric Text Driver|
'' +--------------------------------------------------------------------------+

font          byte      %00000000               ' ........    $00             
              byte      %00000000               ' ........                     
              byte      %00000000               ' ........                     
              byte      %00011000               ' ...##...                     
                                                 
              byte      %00001111               ' ####....    $01
              byte      %00001111               ' ####....
              byte      %00001111               ' ####....
              byte      %00001111               ' ####....
                                                 
              byte      %00000000               ' ........    $02  USED FOR CURSOR
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %00000000               ' ........
                                                 
              byte      %11111111               ' ########    $03
              byte      %11111111               ' ########
              byte      %11111111               ' ########
              byte      %11111111               ' ########
                                                 
              byte      %00000000               ' ........    $04
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %00000000               ' ........
                                                 
              byte      %00001111               ' ####....    $05
              byte      %00001111               ' ####....
              byte      %00001111               ' ####....
              byte      %00001111               ' ####....
                                                 
              byte      %11110000               ' ....####    $06
              byte      %11110000               ' ....####
              byte      %11110000               ' ....####
              byte      %11110000               ' ....####
                                                 
              byte      %11111111               ' ########    $07
              byte      %11111111               ' ########
              byte      %11111111               ' ########
              byte      %11111111               ' ########
                                                 
              byte      %00000000               ' ........    $08
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %00000000               ' ........
                                                 
              byte      %00001111               ' ####....    $09
              byte      %00001111               ' ####....
              byte      %00001111               ' ####....
              byte      %00001111               ' ####....
                                                 
              byte      %11110000               ' ....####    $0A
              byte      %11110000               ' ....####
              byte      %11110000               ' ....####
              byte      %11110000               ' ....####
                                                 
              byte      %11111111               ' ########    $0B
              byte      %11111111               ' ########
              byte      %11111111               ' ########
              byte      %11111111               ' ########
                                                 
              byte      %00000000               ' ........    $0C
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %00000000               ' ........
                                                 
              byte      %00001111               ' ####....    $0D
              byte      %00001111               ' ####....
              byte      %00001111               ' ####....
              byte      %00001111               ' ####....
                                                 
              byte      %11110000               ' ....####    $0E
              byte      %11110000               ' ....####
              byte      %11110000               ' ....####
              byte      %11110000               ' ....####
                                                 
              byte      %11111111               ' ########    $0F
              byte      %11111111               ' ########
              byte      %11111111               ' ########
              byte      %11111111               ' ########
                                                 
              byte      %00000000               ' ........    $10
              byte      %10000000               ' .......#
              byte      %10000000               ' .......#
              byte      %10000000               ' .......#
                                                 
              byte      %00000000               ' ........    $11
              byte      %11111111               ' ########
              byte      %00000000               ' ........
              byte      %00000000               ' ........
                                                 
              byte      %00000000               ' ........    $12
              byte      %11111111               ' ########
              byte      %10000000               ' .......#
              byte      %10000000               ' .......#
                                                 
              byte      %00000000               ' ........    $13
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %00000000               ' ........
                                                 
              byte      %00000000               ' ........    $14
              byte      %11111111               ' ########
              byte      %11111111               ' ########
              byte      %11111111               ' ########
                                                 
              byte      %00000000               ' ........    $15
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %11111000               ' ...#####
                                                 
              byte      %00000000               ' ........    $16
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %11111111               ' ########
                                                 
              byte      %00000000               ' ........    $17
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %00011111               ' #####...
                                                 
              byte      %00011000               ' ...##...    $18
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
              byte      %11111000               ' ...#####
                                                 
              byte      %00110000               ' ....##..    $19
              byte      %00110000               ' ....##..
              byte      %00110000               ' ....##..
              byte      %00111111               ' ######..
                                                 
              byte      %00100000               ' .....#..    $1A
              byte      %00110000               ' ....##..
              byte      %00111000               ' ...###..
              byte      %00111100               ' ..####..
                                                 
              byte      %00000100               ' ..#.....    $1B
              byte      %00001100               ' ..##....
              byte      %00011100               ' ..###...
              byte      %00111100               ' ..####..
                                                 
              byte      %00000000               ' ........    $1C
              byte      %00011000               ' ...##...
              byte      %00111100               ' ..####..
              byte      %01111110               ' .######.
                                                 
              byte      %00000000               ' ........    $1D
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $1E
              byte      %00001000               ' ...#....
              byte      %00001100               ' ..##....
              byte      %01111110               ' .######.
                                                 
              byte      %00000000               ' ........    $1F
              byte      %00010000               ' ....#...
              byte      %00110000               ' ....##..
              byte      %01111110               ' .######.
                                                 
              byte      %00000000               ' ........    $20  Space
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %00000000               ' ........
                                                 
              byte      %00000000               ' ........    $21  !
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $22  "
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $23  #
              byte      %01100110               ' .##..##.
              byte      %11111111               ' ########
              byte      %01100110               ' .##..##.
                                                 
              byte      %00011000               ' ...##...    $24  $
              byte      %01111100               ' ..#####.
              byte      %00000110               ' .##.....
              byte      %00111100               ' ..####..
                                                 
              byte      %00000000               ' ........    $25  %
              byte      %01100110               ' .##..##.
              byte      %00110110               ' .##.##..
              byte      %00011000               ' ...##...
                                                 
              byte      %00111000               ' ...###..    $26  &
              byte      %01101100               ' ..##.##.
              byte      %00111000               ' ...###..
              byte      %00011100               ' ..###...
                                                 
              byte      %00000000               ' ........    $27  '
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $28  (
              byte      %01110000               ' ....###.
              byte      %00111000               ' ...###..
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $29  )
              byte      %00001110               ' .###....
              byte      %00011100               ' ..###...
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $2A  *
              byte      %01100110               ' .##..##.
              byte      %00111100               ' ..####..
              byte      %11111111               ' ########
                                                 
              byte      %00000000               ' ........    $2B  +
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
              byte      %01111110               ' .######.
                                                 
              byte      %00000000               ' ........    $2C  ,
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %00000000               ' ........
                                                 
              byte      %00000000               ' ........    $2D  -
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %01111110               ' .######.
                                                 
              byte      %00000000               ' ........    $2E  .
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %00000000               ' ........
                                                 
              byte      %00000000               ' ........    $2F  /
              byte      %01100000               ' .....##.
              byte      %00110000               ' ....##..
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $30  0
              byte      %00111100               ' ..####..
              byte      %01100110               ' .##..##.
              byte      %01110110               ' .##.###.
                                                 
              byte      %00000000               ' ........    $31  1
              byte      %00011000               ' ...##...
              byte      %00011100               ' ..###...
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $32  2
              byte      %00111100               ' ..####..
              byte      %01100110               ' .##..##.
              byte      %00110000               ' ....##..
                                                 
              byte      %00000000               ' ........    $33  3
              byte      %01111110               ' .######.
              byte      %00110000               ' ....##..
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $34  4
              byte      %00110000               ' ....##..
              byte      %00111000               ' ...###..
              byte      %00111100               ' ..####..
                                                 
              byte      %00000000               ' ........    $35  5
              byte      %01111110               ' .######.
              byte      %00000110               ' .##.....
              byte      %00111110               ' .#####..
                                                 
              byte      %00000000               ' ........    $36  6
              byte      %00111100               ' ..####..
              byte      %00000110               ' .##.....
              byte      %00111110               ' .#####..
                                                 
              byte      %00000000               ' ........    $37  7
              byte      %01111110               ' .######.
              byte      %01100000               ' .....##.
              byte      %00110000               ' ....##..
                                                 
              byte      %00000000               ' ........    $38  8
              byte      %00111100               ' ..####..
              byte      %01100110               ' .##..##.
              byte      %00111100               ' ..####..
                                                 
              byte      %00000000               ' ........    $39  9
              byte      %00111100               ' ..####..
              byte      %01100110               ' .##..##.
              byte      %01111100               ' ..#####.
                                                 
              byte      %00000000               ' ........    $3A  :
              byte      %00000000               ' ........
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $3B  ;
              byte      %00000000               ' ........
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
                                                 
              byte      %01100000               ' .....##.    $3C  <
              byte      %00110000               ' ....##..
              byte      %00011000               ' ...##...
              byte      %00001100               ' ..##....
                                                 
              byte      %00000000               ' ........    $3D  =
              byte      %00000000               ' ........
              byte      %01111110               ' .######.
              byte      %00000000               ' ........
                                                 
              byte      %00000110               ' .##.....    $3E  >
              byte      %00001100               ' ..##....
              byte      %00011000               ' ...##...
              byte      %00110000               ' ....##..
                                                 
              byte      %00000000               ' ........    $3F  ?
              byte      %00111100               ' ..####..
              byte      %01100110               ' .##..##.
              byte      %00110000               ' ....##..
                                                 
              byte      %00000000               ' ........    $40  @
              byte      %00111100               ' ..####..
              byte      %01100110               ' .##..##.
              byte      %01110110               ' .##.###.
                                                 
              byte      %00000000               ' ........    $41  A
              byte      %00011000               ' ...##...
              byte      %00111100               ' ..####..
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $42  B
              byte      %00111110               ' .#####..
              byte      %01100110               ' .##..##.
              byte      %00111110               ' .#####..
                                                 
              byte      %00000000               ' ........    $43  C
              byte      %00111100               ' ..####..
              byte      %01100110               ' .##..##.
              byte      %00000110               ' .##.....
                                                 
              byte      %00000000               ' ........    $44  D
              byte      %00011110               ' .####...
              byte      %00110110               ' .##.##..
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $45  E
              byte      %01111110               ' .######.
              byte      %00000110               ' .##.....
              byte      %00111110               ' .#####..
                                                 
              byte      %00000000               ' ........    $46  F
              byte      %01111110               ' .######.
              byte      %00000110               ' .##.....
              byte      %00111110               ' .#####..
                                                 
              byte      %00000000               ' ........    $47  G
              byte      %01111100               ' ..#####.
              byte      %00000110               ' .##.....
              byte      %00000110               ' .##.....
                                                 
              byte      %00000000               ' ........    $48  H
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
              byte      %01111110               ' .######.
                                                 
              byte      %00000000               ' ........    $49  I
              byte      %01111110               ' .######.
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $4A  J
              byte      %01100000               ' .....##.
              byte      %01100000               ' .....##.
              byte      %01100000               ' .....##.
                                                 
              byte      %00000000               ' ........    $4B  K
              byte      %01100110               ' .##..##.
              byte      %00110110               ' .##.##..
              byte      %00011110               ' .####...
                                                 
              byte      %00000000               ' ........    $4C  L
              byte      %00000110               ' .##.....
              byte      %00000110               ' .##.....
              byte      %00000110               ' .##.....
                                                 
              byte      %00000000               ' ........    $4D  M
              byte      %11000110               ' .##...##
              byte      %11101110               ' .###.###
              byte      %11111110               ' .#######
                                                 
              byte      %00000000               ' ........    $4E  N
              byte      %01100110               ' .##..##.
              byte      %01101110               ' .###.##.
              byte      %01111110               ' .######.
                                                 
              byte      %00000000               ' ........    $4F  O
              byte      %00111100               ' ..####..
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $50  P
              byte      %00111110               ' .#####..
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $51  Q
              byte      %00111100               ' ..####..
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $52  R
              byte      %00111110               ' .#####..
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $53  S
              byte      %00111100               ' ..####..
              byte      %00000110               ' .##.....
              byte      %00111100               ' ..####..
                                                 
              byte      %00000000               ' ........    $54  T
              byte      %01111110               ' .######.
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $55  U
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $56  V
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $57  W
              byte      %11000110               ' .##...##
              byte      %11000110               ' .##...##
              byte      %11010110               ' .##.#.##
                                                 
              byte      %00000000               ' ........    $58  X
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
              byte      %00111100               ' ..####..
                                                 
              byte      %00000000               ' ........    $59  Y
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
              byte      %00111100               ' ..####..
                                                 
              byte      %00000000               ' ........    $5A  Z
              byte      %01111110               ' .######.
              byte      %00110000               ' ....##..
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $5B  [
              byte      %01111000               ' ...####.
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $5C  \
              byte      %00000010               ' .#......
              byte      %00000110               ' .##.....
              byte      %00001100               ' ..##....
                                                 
              byte      %00000000               ' ........    $5D  ]
              byte      %00011110               ' .####...
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $5E  ^
              byte      %00010000               ' ....#...
              byte      %00111000               ' ...###..
              byte      %01101100               ' ..##.##.
                                                 
              byte      %00000000               ' ........    $5F  _
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %00000000               ' ........
                                                 
              byte      %00000000               ' ........    $60  `
              byte      %10000000               ' .......#
              byte      %11000000               ' ......##
              byte      %01100000               ' .....##.
                                                 
              byte      %00000000               ' ........    $61  a
              byte      %00000000               ' ........
              byte      %00111100               ' ..####..
              byte      %01100000               ' .....##.
                                                 
              byte      %00000000               ' ........    $62  b
              byte      %00000110               ' .##.....
              byte      %00000110               ' .##.....
              byte      %00111110               ' .#####..
                                                 
              byte      %00000000               ' ........    $63  c
              byte      %00000000               ' ........
              byte      %00111100               ' ..####..
              byte      %00000110               ' .##.....
                                                 
              byte      %00000000               ' ........    $64  d
              byte      %01100000               ' .....##.
              byte      %01100000               ' .....##.
              byte      %01111100               ' ..#####.
                                                 
              byte      %00000000               ' ........    $65  e
              byte      %00000000               ' ........
              byte      %00111100               ' ..####..
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $66  f
              byte      %01110000               ' ....###.
              byte      %00011000               ' ...##...
              byte      %01111100               ' ..#####.
                                                 
              byte      %00000000               ' ........    $67  g
              byte      %00000000               ' ........
              byte      %01111100               ' ..#####.
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $68  h
              byte      %00000110               ' .##.....
              byte      %00000110               ' .##.....
              byte      %00111110               ' .#####..
                                                 
              byte      %00000000               ' ........    $69  i
              byte      %00011000               ' ...##...
              byte      %00000000               ' ........
              byte      %00011100               ' ..###...
                                                 
              byte      %00000000               ' ........    $6A  j
              byte      %00110000               ' ....##..
              byte      %00000000               ' ........
              byte      %00110000               ' ....##..
                                                 
              byte      %00000000               ' ........    $6B  k
              byte      %00000110               ' .##.....
              byte      %00000110               ' .##.....
              byte      %00110110               ' .##.##..
                                                 
              byte      %00000000               ' ........    $6C  l
              byte      %00011100               ' ..###...
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $6D  m
              byte      %00000000               ' ........
              byte      %01100110               ' .##..##.
              byte      %11111110               ' .#######
                                                 
              byte      %00000000               ' ........    $6E  n
              byte      %00000000               ' ........
              byte      %00111110               ' .#####..
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $6F  o
              byte      %00000000               ' ........
              byte      %00111100               ' ..####..
              byte      %01100110               ' .##..##.
                                                  
              byte      %00000000               ' ........    $70  p
              byte      %00000000               ' ........
              byte      %00111110               ' .#####..
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $71  q
              byte      %00000000               ' ........
              byte      %01111100               ' ..#####.
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $72  r
              byte      %00000000               ' ........
              byte      %00111110               ' .#####..
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $73  s
              byte      %00000000               ' ........
              byte      %01111100               ' ..#####.
              byte      %00000110               ' .##.....
                                                 
              byte      %00000000               ' ........    $74  t
              byte      %00011000               ' ...##...
              byte      %01111110               ' .######.
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $75  u
              byte      %00000000               ' ........
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
                                                
              byte      %00000000               ' ........    $76  v
              byte      %00000000               ' ........
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $77  w
              byte      %00000000               ' ........
              byte      %11000110               ' .##...##
              byte      %11010110               ' .##.#.##
                                                 
              byte      %00000000               ' ........    $78  x
              byte      %00000000               ' ........
              byte      %01100110               ' .##..##.
              byte      %00111100               ' ..####..
                                                 
              byte      %00000000               ' ........    $79  y
              byte      %00000000               ' ........
              byte      %01100110               ' .##..##.
              byte      %01100110               ' .##..##.
                                                 
              byte      %00000000               ' ........    $7A  z
              byte      %00000000               ' ........
              byte      %01111110               ' .######.
              byte      %00110000               ' ....##..
                                                 
              byte      %00000000               ' ........    $7B  {
              byte      %00111000               ' ...###..
              byte      %00011000               ' ...##...
              byte      %00011110               ' .###....
                                                 
              byte      %00011000               ' ...##...    $7C  |
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
              byte      %00011000               ' ...##...
                                                 
              byte      %00000000               ' ........    $7D  }
              byte      %00011100               ' ..###...
              byte      %00011000               ' ...##...
              byte      %01111000               ' ....###.
                                                 
              byte      %00000000               ' ........    $7E  ~
              byte      %11001100               ' ..##..##
              byte      %01111110               ' .######.
              byte      %00110011               ' ##..##..
                                                 
              byte      %00000000               ' ........    $7F
              byte      %00011100               ' ..###...
              byte      %00100010               ' .#...#..
              byte      %00100010               ' .#...#..

' *******************************************************************************************************
' second half of font longs...

              byte      %00011000               ' ...##...    $00
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %00000000               ' ........
                                                 
              byte      %00000000               ' ........    $01
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %00000000               ' ........
                                                 
              byte      %00000000               ' ........    $02  USED FOR CURSOR
              byte      %00000000               ' ........
              byte      %11111110               ' .#######
              byte      %11111110               ' .####### 
                                                 
              byte      %00000000               ' ........    $03                          
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00001111               ' ####....    $04                          
              byte      %00001111               ' ####....                                 
              byte      %00001111               ' ####....                                 
              byte      %00001111               '.####....                                 
                                                                                           
              byte      %00001111               ' ####....    $05                          
              byte      %00001111               ' ####....                                 
              byte      %00001111               ' ####....                                 
              byte      %00001111               ' ####....                                 
                                                                                           
              byte      %00001111               ' ####....    $06                          
              byte      %00001111               ' ####....                                 
              byte      %00001111               ' ####....                                 
              byte      %00001111               ' ####....                                 
                                                                                           
              byte      %00001111               ' ####....    $07                          
              byte      %00001111               ' ####....                                 
              byte      %00001111               ' ####....                                 
              byte      %00001111               ' ####....                                 
                                                                                           
              byte      %11110000               ' ....####    $08                          
              byte      %11110000               ' ....####                                 
              byte      %11110000               ' ....####                                 
              byte      %11110000               ' ....####                                 
                                                                                           
              byte      %11110000               ' ....####    $09                          
              byte      %11110000               ' ....####                                 
              byte      %11110000               ' ....####                                 
              byte      %11110000               ' ....####                                 
                                                                                           
              byte      %11110000               ' ....####    $0A                          
              byte      %11110000               ' ....####                                 
              byte      %11110000               ' ....####                                 
              byte      %11110000               ' ....####                                 
                                                                                           
              byte      %11110000               ' ....####    $0B                          
              byte      %11110000               ' ....####                                 
              byte      %11110000               ' ....####                                 
              byte      %11110000               ' ....####                                 
                                                                                           
              byte      %11111111               ' ########    $0C                          
              byte      %11111111               ' ########                                 
              byte      %11111111               ' ########                                 
              byte      %11111111               '.########                                 
                                                                                           
              byte      %11111111               ' ########    $0D                          
              byte      %11111111               ' ########                                 
              byte      %11111111               ' ########                                 
              byte      %11111111               ' ########                                 
                                                                                           
              byte      %11111111               ' ########    $0E                          
              byte      %11111111               ' ########                                 
              byte      %11111111               ' ########                                 
              byte      %11111111               ' ########                                 
                                                                                           
              byte      %11111111               ' ########    $0F                          
              byte      %11111111               ' ########                                 
              byte      %11111111               ' ########                                 
              byte      %11111111               ' ########                                 
                                                                                           
              byte      %10000000               ' .......#    $10                          
              byte      %10000000               ' .......#                                 
              byte      %11111111               ' ########                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000000               ' ........    $11                          
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %10000000               ' .......#    $12                          
              byte      %10000000               ' .......#                                 
              byte      %10000000               ' .......#                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000000               ' ........    $13                          
              byte      %00000000               ' ........                                 
              byte      %11111111               ' ########                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %11111111               ' ########    $14                          
              byte      %11111111               ' ########                                 
              byte      %11111111               ' ########                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %11111000               ' ...#####    $15                          
              byte      %00011000               ' ...##...                                 
              byte      %00011000               ' ...##...                                 
              byte      %00011000               ' ...##...                                 
                                                                                           
              byte      %11111111               ' ########    $16                          
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011111               ' #####...    $17                          
              byte      %00011000               ' ...##...                                 
              byte      %00011000               ' ...##...                                 
              byte      %00011000               ' ...##...                                 
                                                                                           
              byte      %11111000               ' ...#####    $18                          
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00111111               ' ######..    $19                          
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00111000               ' ...###..    $1A                          
              byte      %00110000               ' ....##..                                 
              byte      %00100000               ' .....#..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011100               ' ..###...    $1B                          
              byte      %00001100               ' ..##....                                 
              byte      %00000100               ' ..#.....                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $1C                          
              byte      %00011000               ' ...##...                                 
              byte      %00011000               ' ...##...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01111110               ' .######.    $1D                          
              byte      %00111100               ' ..####..                                 
              byte      %00011000               ' ...##...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01111110               ' .######.    $1E                          
              byte      %00001100               ' ..##....                                 
              byte      %00001000               ' ...#....                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01111110               ' .######.    $1F                          
              byte      %00110000               ' ....##..                                 
              byte      %00010000               ' ....#...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000000               ' ........    $20  Space                   
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $21  !                       
              byte      %00000000               ' ........                                 
              byte      %00011000               ' ...##...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000000               ' ........    $22  "                       
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $23  #                       
              byte      %11111111               ' ########                                 
              byte      %01100110               ' .##..##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100000               ' .....##.    $24  $                       
              byte      %00111110               ' .#####..                                 
              byte      %00011000               ' ...##...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00001100               ' ..##....    $25  %                       
              byte      %01100110               ' .##..##.                                 
              byte      %01100010               ' .#...##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %11110110               ' .##.####    $26  &                       
              byte      %01100110               ' .##..##.                                 
              byte      %11011100               ' ..###.##                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000000               ' ........    $27  '                  
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $28  (                       
              byte      %00111000               ' ...###..                                 
              byte      %01110000               ' ....###.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $29  )                       
              byte      %00011100               ' ..###...                                 
              byte      %00001110               ' .###....                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00111100               ' ..####..    $2A  *                       
              byte      %01100110               ' .##..##.                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $2B  +                       
              byte      %00011000               ' ...##...                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000000               ' ........    $2C  ,                       
              byte      %00011000               ' ...##...                                 
              byte      %00011000               ' ...##...                                 
              byte      %00001100               ' ..##....                                 
                                                                                           
              byte      %00000000               ' ........    $2D  -                       
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000000               ' ........    $2E  .                       
              byte      %00011000               ' ...##...                                 
              byte      %00011000               ' ...##...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00001100               ' ..##....    $2F  /                       
              byte      %00000110               ' .##.....                                 
              byte      %00000010               ' .#......                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01101110               ' .###.##.    $30  0                       
              byte      %01100110               ' .##..##.                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $31  1                       
              byte      %00011000               ' ...##...                                 
              byte      %01111110               ' .######.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $32  2                       
              byte      %00001100               ' ..##....                                 
              byte      %01111110               ' .######.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00110000               ' ....##..    $33  3                       
              byte      %01100110               ' .##..##.                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00110110               ' .##.##..    $34  4                       
              byte      %01111110               ' .######.                                 
              byte      %00110000               ' ....##..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100000               ' .....##.    $35  5                       
              byte      %01100110               ' .##..##.                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $36  6                       
              byte      %01100110               ' .##..##.                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $37  7                       
              byte      %00001100               ' ..##....                                 
              byte      %00001100               ' ..##....                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $38  8                       
              byte      %01100110               ' .##..##.                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100000               ' .....##.    $39  9                       
              byte      %00110000               ' ....##..                                 
              byte      %00011100               ' ..###...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000000               ' ........    $3A  :                       
              byte      %00011000               ' ...##...                                 
              byte      %00011000               ' ...##...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000000               ' ........    $3B  ;                       
              byte      %00011000               ' ...##...                                 
              byte      %00001100               ' ..##....                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $3C  <                       
              byte      %00110000               ' ....##..                                 
              byte      %01100000               ' .....##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000000               ' ........    $3D  =                       
              byte      %01111110               ' .######.                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $3E  >                       
              byte      %00001100               ' ..##....                                 
              byte      %00000110               ' .##.....                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $3F  ?                       
              byte      %00000000               ' ........                                 
              byte      %00011000               ' ...##...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01110110               ' .##.###.    $40  @                       
              byte      %00000110               ' .##.....                                 
              byte      %01111100               ' ..#####.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $41  A                       
              byte      %01111110               ' .######.                                 
              byte      %01100110               ' .##..##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $42  B                       
              byte      %01100110               ' .##..##.                                 
              byte      %00111110               ' .#####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000110               ' .##.....    $43  C                       
              byte      %01100110               ' .##..##.                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $44  D                       
              byte      %00110110               ' .##.##..                                 
              byte      %00011110               ' .####...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000110               ' .##.....    $45  E                       
              byte      %00000110               ' .##.....                                 
              byte      %01111110               ' .######.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000110               ' .##.....    $46  F                       
              byte      %00000110               ' .##.....                                 
              byte      %00000110               ' .##.....                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01110110               ' .##.###.    $47  G                       
              byte      %01100110               ' .##..##.                                 
              byte      %01111100               ' ..#####.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $48  H                       
              byte      %01100110               ' .##..##.                                 
              byte      %01100110               ' .##..##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $49  I                       
              byte      %00011000               ' ...##...                                 
              byte      %01111110               ' .######.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100000               ' .....##.    $4A  J                       
              byte      %01100110               ' .##..##.                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011110               ' .####...    $4B  K                       
              byte      %00110110               ' .##.##..                                 
              byte      %01100110               ' .##..##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000110               ' .##.....    $4C  L                       
              byte      %00000110               ' .##.....                                 
              byte      %01111110               ' .######.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %11010110               ' .##.#.##    $4D  M                       
              byte      %11000110               ' .##...##                                 
              byte      %11000110               ' .##...##                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01111110               ' .######.    $4E  N                       
              byte      %01110110               ' .##.###.                                 
              byte      %01100110               ' .##..##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $4F  O                       
              byte      %01100110               ' .##..##.                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00111110               ' .#####..    $50  P                       
              byte      %00000110               ' .##.....                                 
              byte      %00000110               ' .##.....                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $51  Q                       
              byte      %00110110               ' .##.##..                                 
              byte      %01101100               ' ..##.##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00111110               ' .#####..    $52  R                       
              byte      %00110110               ' .##.##..                                 
              byte      %01100110               ' .##..##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100000               ' .....##.    $53  S                       
              byte      %01100000               ' .....##.                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $54  T                       
              byte      %00011000               ' ...##...                                 
              byte      %00011000               ' ...##...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $55  U                       
              byte      %01100110               ' .##..##.                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $56  V                       
              byte      %00111100               ' ..####..                                 
              byte      %00011000               ' ...##...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %11111110               ' .#######    $57  W                       
              byte      %11101110               ' .###.###                                 
              byte      %11000110               ' .##...##                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00111100               ' ..####..    $58  X                       
              byte      %01100110               ' .##..##.                                 
              byte      %01100110               ' .##..##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $59  Y                       
              byte      %00011000               ' ...##...                                 
              byte      %00011000               ' ...##...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00001100               ' ..##....    $5A  Z                       
              byte      %00000110               ' .##.....                                 
              byte      %01111110               ' .######.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $5B  [                       
              byte      %00011000               ' ...##...                                 
              byte      %01111000               ' ...####.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $5C  \                       
              byte      %00110000               ' ....##..                                 
              byte      %01100000               ' .....##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $5D  ]                       
              byte      %00011000               ' ...##...                                 
              byte      %00011110               ' .####...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %11000110               ' .##...##    $5E  ^                       
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000000               ' ........    $5F  _                       
              byte      %00000000               ' ........                                 
              byte      %01111110               ' .######.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00110011               ' ##..##..    $60  `                       
              byte      %00011110               ' .####...                                 
              byte      %00001100               ' ..##....                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01111100               ' ..#####.    $61  a                       
              byte      %01100110               ' .##..##.                                 
              byte      %01111100               ' ..#####.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $62  b                       
              byte      %01100110               ' .##..##.                                 
              byte      %00111110               ' .#####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000110               ' .##.....    $63  c                       
              byte      %00000110               ' .##.....                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $64  d                       
              byte      %01100110               ' .##..##.                                 
              byte      %01111100               ' ..#####.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01111110               ' .######.    $65  e                       
              byte      %00000110               ' .##.....                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $66  f                       
              byte      %00011000               ' ...##...                                 
              byte      %00011000               ' ...##...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $67  g                       
              byte      %01111100               ' ..#####.                                 
              byte      %01100000               ' .....##.                                 
              byte      %00111110               ' .#####..                                 
                                                                                           
              byte      %01100110               ' .##..##.    $68  h                       
              byte      %01100110               ' .##..##.                                 
              byte      %01100110               ' .##..##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $69  i                       
              byte      %00011000               ' ...##...                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00110000               ' ....##..    $6A  j                       
              byte      %00110000               ' ....##..                                 
              byte      %00110000               ' ....##..                                 
              byte      %00011110               ' .####...                                 
                                                                                           
              byte      %00011110               ' .####...    $6B  k                       
              byte      %00110110               ' .##.##..                                 
              byte      %01100110               ' .##..##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $6C  l                       
              byte      %00011000               ' ...##...                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %11111110               ' .#######    $6D  m                       
              byte      %11010110               ' .##.#.##                                 
              byte      %11000110               ' .##...##                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $6E  n                       
              byte      %01100110               ' .##..##.                                 
              byte      %01100110               ' .##..##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $6F  o                       
              byte      %01100110               ' .##..##.                                 
              byte      %00111100               ' ..####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $70  p                       
              byte      %00111110               ' .#####..                                 
              byte      %00000110               ' .##.....                                 
              byte      %00000110               ' .##.....                                 
                                                                                           
              byte      %01100110               ' .##..##.    $71  q                       
              byte      %01111100               ' ..#####.                                 
              byte      %01100000               ' .....##.                                 
              byte      %01100000               ' .....##.                                 
                                                                                           
              byte      %00000110               ' .##.....    $72  r                       
              byte      %00000110               ' .##.....                                 
              byte      %00000110               ' .##.....                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00111100               ' ..####..    $73  s                       
              byte      %01100000               ' .....##.                                 
              byte      %00111110               ' .#####..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $74  t                       
              byte      %00011000               ' ...##...                                 
              byte      %01110000               ' ....###.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $75  u                       
              byte      %01100110               ' .##..##.                                 
              byte      %01111100               ' ..#####.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $76  v                       
              byte      %00111100               ' ..####..                                 
              byte      %00011000               ' ...##...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %11111110               ' .#######    $77  w                       
              byte      %01111100               ' ..#####.                                 
              byte      %01101100               ' ..##.##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $78  x                       
              byte      %00111100               ' ..####..                                 
              byte      %01100110               ' .##..##.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %01100110               ' .##..##.    $79  y                       
              byte      %01111100               ' ..#####.                                 
              byte      %00110000               ' ....##..                                 
              byte      %00011110               ' .####...                                 
                                                                                           
              byte      %00011000               ' ...##...    $7A  z                       
              byte      %00001100               ' ..##....                                 
              byte      %01111110               ' .######.                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011110               ' .###....    $7B  {                       
              byte      %00011000               ' ...##...                                 
              byte      %00111000               ' ...###..                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011000               ' ...##...    $7C  |                       
              byte      %00011000               ' ...##...                                 
              byte      %00011000               ' ...##...                                 
              byte      %00011000               ' ...##...                                 
                                                                                           
              byte      %01111000               ' ....###.    $7D  }                       
              byte      %00011000               ' ...##...                                 
              byte      %00011100               ' ..###...                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00000000               ' ........    $7E  ~                       
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
              byte      %00000000               ' ........                                 
                                                                                           
              byte      %00011100               ' ..###...    $7F                          
              byte      %00000000               ' ........
              byte      %00000000               ' ........
              byte      %00000000               ' ........

' end of font
' *******************************************************************************************************

''+-----------------------------------------------------+
''| PASM Constants & Variables                          |
''+-----------------------------------------------------+

outmask         long    1<<PIN_3V3 | 0<<PIN_GND | 1<<PIN_CE | 1<<PIN_RST | 0<<PIN_DC | 0<<PIN_SDA | 1<<PIN_CLK | 1<<PIN_LED
dirmask         long    1<<PIN_3V3 | 1<<PIN_GND | 1<<PIN_CE | 1<<PIN_RST | 1<<PIN_DC | 1<<PIN_SDA | 1<<PIN_CLK | 1<<PIN_LED
csmask          long    1<<PIN_CE
rstmask         long    1<<PIN_RST
dcmask          long    1<<PIN_DC
sdamask         long    1<<PIN_SDA
clkmask         long    1<<PIN_CLK

delay50ms       long    104_000_000 / 1000 * 50         ' 50ms at 96MHz (will be a bit longer if <104MHz)
zero            long    0
xFFFF           long    $FFFF

data            res     1
data1           res     1
fg              res     1
bg              res     1
command         res     1
hubptr          res     1
hubptr1         res     1
hubptr2         res     1
hubptr3         res     1
pixels          res     1
bits            res     1
ctr             res     1





dat                           
{{
+------------------------------------------------------------------------------------------------------------------------------+
|                                                   TERMS OF USE: MIT License                                                  |                                                            
+------------------------------------------------------------------------------------------------------------------------------+
|Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    | 
|files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    |
|modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software|
|is furnished to do so, subject to the following conditions:                                                                   |
|                                                                                                                              |
|The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.|
|                                                                                                                              |
|THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          |
|WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         |
|COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   |
|ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         |
+------------------------------------------------------------------------------------------------------------------------------+
}}
