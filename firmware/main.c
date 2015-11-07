#include <basic.h>
#include <gpio.h>
#include <usart.h>
#include <memory.h>
#include <crc.h>

// TODO: remove display
// TODO: remove unused includes
// #include <display.h>
// display display = display_initialize(pin_initialize(port_initialize(port_c), 5), pin_initialize(port_initialize(port_c), 4), pin_initialize(port_initialize(port_c), 3), port_initialize(port_d));
// display_printf(display, "0x%04X", reset_type);

typedef enum { get_page_count = 0x0001, get_page_length = 0x0002, read_page = 0x0003, write_page = 0x0004 } command;
typedef enum { flash = 0x0001, eeprom = 0x0002, calibration = 0x0003, fuse = 0x0004, lock = 0x0005, signature = 0x0006 } memory;

void usart_write_checked(void* data, size_t length)
{
	uint16_t crc = crc16(data, length, 0);

	usart_write(data, length);
	usart_write(&crc, sizeof(crc));
}
void usart_write_checked_size(size_t size)
{
	usart_write_checked(&size, sizeof(size));
}

void loader()
{
	// enable USART with a divider of 64 * 16, giving about 1 kBd/MHz
	usart_initialize(1, 1, 0x003F, 0);

	while (1)
	{
		command command;
		if (usart_read(&command, sizeof(command))) continue;

		// TODO: review control flow (breaks, should we extract functions, etc.)
		// TODO: add proper error handling? or is that not neccessary since the loader is locked anyways? do some experiments!
		switch (command)
		{
			case get_page_count:
			{
				memory memory;
				if (usart_read(&memory, sizeof(memory))) break;

				switch (memory)
				{
					case flash: usart_write_checked_size(FLASH_PAGE_COUNT); break;
					case eeprom: usart_write_checked_size(EEPROM_PAGE_COUNT); break;
				}
				break;
			}
			case get_page_length:
			{
				memory memory;
				if (usart_read(&memory, sizeof(memory))) break;

				switch (memory)
				{
					case flash: usart_write_checked_size(FLASH_PAGE_LENGTH); break;
					case eeprom: usart_write_checked_size(EEPROM_PAGE_LENGTH); break;
				}
				break;
			}
			case read_page:
			{
				memory memory;
				if (usart_read(&memory, sizeof(memory))) break;
				uint8_t page_index;
				if (usart_read(&page_index, sizeof(page_index))) break;

				switch (memory)
				{
					case flash:
					{
						uint8_t data[FLASH_PAGE_LENGTH];
						flash_read_page(page_index, data);
						usart_write_checked(&data, sizeof(data));
						break;
					}
					case eeprom:
					{
						uint8_t data[EEPROM_PAGE_LENGTH];
						eeprom_read_page(page_index, data);
						usart_write_checked(&data, sizeof(data));
						break;
					}
				}
				break;
			}
			case write_page:
			{
				memory memory;
				if (usart_read(&memory, sizeof(memory))) break;
				uint8_t page_index;
				if (usart_read(&page_index, sizeof(page_index))) break;

				switch (memory)
				{
					case flash:
					{
						uint8_t data[FLASH_PAGE_LENGTH];
						usart_read(&data, sizeof(data));
						flash_write_page(page_index, &data);
						usart_write_checked(0, 0);
						break;
					}
					case eeprom:
					{
						uint8_t data[EEPROM_PAGE_LENGTH];
						usart_read(&data, sizeof(data));
						eeprom_write_page(page_index, &data);
						usart_write_checked(0, 0);
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
//#include <display.h>
void main()
{
//	display display = display_initialize(pin_initialize(port_initialize(port_c), 5), pin_initialize(port_initialize(port_c), 4), pin_initialize(port_initialize(port_c), 3), port_initialize(port_d));
//	display_printf(display, "0x%04X", FLASH_SIZE);
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
