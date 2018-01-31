CON
  _XINFREQ      = 12_000_000        '\ set for 12.00MHz (P8XBlade2)
  _CLKMODE      = xtal1 + pll8x     '/
  '_clkmode = xtal1 + pll16x                           
  '_xinfreq = 5_000_000

OBJ
  OLED_Test :     "OLED_Test"      ''OLED dedicated SPI engine in Assembly
  ILI9163_1   :     "ILI9163_1" ' Breadboard TFT
  ILI9163_2   :     "ILI9163_2"
  
PUB main
  OLED_Test.SPI_DEMO
  ILI9163_2.start
  ILI9163_1.start
