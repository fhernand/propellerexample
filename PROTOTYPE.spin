CON
  _clkmode = xtal1 + pll16x                           
  _xinfreq = 5_000_000

OBJ
  OLED_Test :     "OLED_Test"      ''OLED dedicated SPI engine in Assembly
  ILI9163_1   :     "ILI9163_1"
  ILI9163_2   :     "ILI9163_2"
  
PUB main
  ILI9163_2.start 
  ILI9163_1.start
  OLED_Test.SPI_DEMO
 
