CON
  _clkmode = xtal1 + pll16x                           
  _xinfreq = 5_000_000

OBJ
  OLED_Test :     "OLED_Test"      ''OLED dedicated SPI engine in Assembly
  ILI9163   :     "ILI9163"
  
PUB main
  OLED_Test.SPI_DEMO  ' Start a new COG
  ILI9163.start 
