avr = ../../avr

programmer = dragon_isp
device = atmega328p
frequency = 20000000

main = main

# TODO: once bootloader works, figure out if this is actually the start address by placing instructions before it
offset = 0x7000

# TODO: give fuses another try, if not, use http://electronics.stackexchange.com/questions/66145/avr-how-to-program-an-avr-chip-in-linux

lock = 0x2F:m
lfuse = 0xF7:m
hfuse = 0xD8:m
efuse = 0x04:m

include $(avr)/build/Makefile
