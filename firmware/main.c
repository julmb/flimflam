#include <basic.h>
#include <gpio.h>
#include <usart.h>
#include <memory.h>

// TODO: remove display
// TODO: remove unused includes
// TODO: move flash/eeprom access stuff to extra module(s)
// TODO: enable reading/writing all the pieces of memory that avrdude supports as well
// #include <display.h>
// display display = display_initialize(pin_initialize(port_initialize(port_c), 5), pin_initialize(port_initialize(port_c), 4), pin_initialize(port_initialize(port_c), 3), port_initialize(port_d));
// display_printf(display, "BL: 0x%04X", reset_type);

typedef enum { information = 0x0001, read = 0x0002, write = 0x0003 } command;
typedef enum { flash = 0x0001, eeprom = 0x0002 } memory;

void loader()
{
	// enable USART with a divider of 64 * 16, giving about 1 kBd/MHz
	usart_initialize(1, 1, 0x003F, 0);

	while (1)
	{
		command command;
		if (usart_read(&command, sizeof(command))) continue;

		switch (command)
		{
			case information:
			{
				size_t flash_page_count = FLASH_PAGE_COUNT;
				usart_write(&flash_page_count, sizeof(flash_page_count));
				size_t flash_page_size = FLASH_PAGE_SIZE;
				usart_write(&flash_page_size, sizeof(flash_page_size));
				size_t eeprom_page_count = EEPROM_PAGE_COUNT;
				usart_write(&eeprom_page_count, sizeof(eeprom_page_count));
				size_t eeprom_page_size = EEPROM_PAGE_SIZE;
				usart_write(&eeprom_page_size, sizeof(eeprom_page_size));
				size_t ram_size = SRAM_SIZE;
				usart_write(&ram_size, sizeof(ram_size));
				break;
			}
			case read:
			{
				memory memory;
				if (usart_read(&memory, sizeof(memory))) break;
				uint16_t page_index;
				if (usart_read(&page_index, sizeof(page_index))) break;
				switch (memory)
				{
					case flash:
					{
						uint8_t data[FLASH_PAGE_SIZE];
						flash_read_page(page_index, &data);
						usart_write(&data, sizeof(data));
						break;
					}
				}
				break;
			}
			case write:
			{
				memory memory;
				if (usart_read(&memory, sizeof(memory))) break;
				uint16_t page_index;
				if (usart_read(&page_index, sizeof(page_index))) break;
				switch (memory)
				{
					case flash:
					{
						uint8_t data[FLASH_PAGE_SIZE];
						usart_read(&data, sizeof(data));
						flash_write_page(page_index, &data);
						break;
					}
				}
				break;
			}
		}
	}

	usart_dispose();
}

// TODO: remove debug stuff
void main()
{
//	display display = display_initialize(pin_initialize(port_initialize(port_c), 5), pin_initialize(port_initialize(port_c), 4), pin_initialize(port_initialize(port_c), 3), port_initialize(port_d));
//	display_printf(display, "0x%04X", FLASH_PAGE_COUNT);
//	return;

	reset_type reset_type = initialize();

	pin red = pin_output(port_initialize(port_d), 4, 0);
	pin green = pin_output(port_initialize(port_d), 2, 0);

	if (reset_type == external)
	{
		pin_set(red);
		loader();
	}

	pin_set(green);
	start();
}
