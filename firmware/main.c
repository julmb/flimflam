#include <basic.h>
#include <usart.h>

#include <avr/pgmspace.h>
#include <avr/boot.h>

// TODO: remove display
// TODO: remove unused includes
// TODO: move flash/eeprom access stuff to extra module(s)
// TODO: enable reading/writing all the pieces of memory that avrdude supports as well
// #include <display.h>
// display display = display_initialize(pin_initialize(port_initialize(port_c), 5), pin_initialize(port_initialize(port_c), 4), pin_initialize(port_initialize(port_c), 3), port_initialize(port_d));
// display_printf(display, "BL: 0x%04X", reset_type);

typedef enum { info = 0x0001, read = 0x0002, write = 0x0003 } command;
typedef enum { flash = 0x0001, eeprom = 0x0002 } memory;

uint8_t read_byte(void* position)
{
	return pgm_read_byte(position);
}
void write_page(void* position, void* data)
{
	uint8_t* bytes = data;

	boot_page_erase(position);
	boot_spm_busy_wait();

	for (uint8_t byte_index = 0; byte_index < SPM_PAGESIZE; byte_index += 2)
		boot_page_fill(position + byte_index, (*bytes++ << 0) | (*bytes++ << 8));

	boot_page_write(position);
	boot_spm_busy_wait();

	boot_rww_enable();
}

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
			case info:
			{
				size_t page_size = SPM_PAGESIZE;
				usart_write(&page_size, sizeof(page_size));
				break;
			}
			case read:
			{
				memory memory;
				if (usart_read(&memory, sizeof(memory))) break;
				void* position;
				if (usart_read(&position, sizeof(position))) break;
				switch (memory)
				{
					case flash:
					{
						uint8_t byte = read_byte(position);
						usart_write(&byte, sizeof(byte));
						break;
					}
				}
				break;
			}
			case write:
			{
				memory memory;
				if (usart_read(&memory, sizeof(memory))) break;
				void* position;
				if (usart_read(&position, sizeof(position))) break;
				switch (memory)
				{
					case flash:
					{
						uint8_t data[SPM_PAGESIZE];
						usart_read(&data, sizeof(data));
						write_page(position, &data);

						uint8_t ack = 0x12;
						usart_write(&ack, sizeof(ack));
						break;
					}
				}
				break;
			}
		}
	}

	usart_dispose();
}

void main()
{
	reset_type reset_type = initialize();

	if (reset_type == external) loader();

	application();
}
