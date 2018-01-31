{{      
**************************************
* Adafruit OLED 128x32 or 128x64     *
* Display Demo                       *
* Author: Thomas P. Sullivan         *
* See end of file for terms of use.  *
* 12/16/2012                         *
**************************************

 -----------------REVISION HISTORY-----------------
 v1.00 - Original Version - 12/16/2012 
 v1.01 - Changes to support both the x32 and x64 displays - 12/27/2012 

}}

CON
  CS    = 7
  RST   = 6
  DC    = 5
  CLK   = 4
  DATA  = 3

{{
     ┌─────────────────────────┐         ┌─────────────────────────┐ 
     │                         │         │                         │ 
     │         Adafruit        │         │         Adafruit        │ 
     │          128x32         │         │          128x64         │ 
     │       OLED Display      │         │       OLED Display      │ 
     │                         │         │                         │ 
     │   RST   CLK   VIN   GND │         │   RST   CLK   VIN   GND │ 
     │ CS   D/C   DATA  3.3    │         │ CS   D/C   DATA  3.3    │ 
     └─┬──┬──┬──┬──┬──┬──┬──┬──┘         └─┬──┬──┬──┬──┬──┬──┬──┬──┘ 
       │  │  │  │  │  │  │  │              │  │  │  │  │  │  │  │    
     ┌─┴──┴──┴──┴──┴──┴──┴──┴──┐         ┌─┴──┴──┴──┴──┴──┴──┴──┴──┐ 
     │ P7 P6 P5 P4 P3 NC V GND │         │ P7 P6 P5 P4 P3 NC V GND │ 
     │                   D     │         │                   D     │ 
     │                   D     │         │                   D     │ 
     │                         │         │                         │ 
     │                         │         │                         │ 
     │        Parallax         │         │        Parallax         │ 
     │  Propeller Demo Board   │         │  Propeller Demo Board   │ 
     └─────────────────────────┘         └─────────────────────────┘ 
}}

OBJ
  OLED    :     "OLED_AsmFast"       ''OLED dedicated SPI engine in Assembly
  NUM     :     "Numbers"

VAR
  long qq
  long stack_space[32]
  byte tstr[32]

PUB SPI_DEMO

  ''****************
  ''128x32 OLED Demo
  ''****************

  ''*************
  ''Init the OLED
  ''*************

  cognew(random_stuff, @stack_space)

PRI random_stuff|count
  ''For random number generation
  OLED.Init(CS,DC,DATA,CLK,RST,OLED#SSD1306_SWITCHCAPVCC,OLED#TYPE_128X64)
  count := 1  'For display inversion control
  OLED.AutoUpdateOff
  repeat
    count++

    OLED.write4x16String(String("Engine Subsystem"),strsize(String("Engine Subsystem")),0,0)
    OLED.write4x16String(String("All OK"),strsize(String("All OK")),1,0)
    
    OLED.write16x32Char(count,1,2)
    OLED.write16x32Char(count + 1,1,3)
    OLED.write16x32Char(",",1,4)
    OLED.write16x32Char(count+2,1,5)
    OLED.write16x32Char(count+3,1,6)
    
    OLED.updateDisplay	
    waitcnt(clkfreq/5000+cnt)

    OLED.write4x16String(String("Engine Subsystem"),strsize(String("Engine Subsystem")),0,0)
    OLED.write4x16String(String("All OK"),strsize(String("All OK")),1,0)
    OLED.write16x32Char(count+4,1,2)
    OLED.write16x32Char(count+5,1,3)
    OLED.write16x32Char(",",1,4)
    OLED.write16x32Char(count+6,1,5)
    OLED.write16x32Char(count+7,1,6)
    OLED.updateDisplay
    
    waitcnt(clkfreq/5000+cnt)

