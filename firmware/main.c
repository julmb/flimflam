#include <basic.h>
#include <usart.h>
#include <memory.h>
#include <crc.h>

typedef enum { get_device_information = 0x0001, read_page = 0x0002, write_page = 0x0003 } command;
typedef enum { flash = 0x0001, eeprom = 0x0002 } memory;

void usart_write_checked(void* data, size_t length)
{
	uint16_t crc = crc16(data, length, 0);

	usart_write(data, length);
	usart_write(&crc, sizeof(crc));
}

void device_information()
{
	uint16_t crc = 0;

	size_t memory_information[] = { FLASH_PAGE_COUNT, FLASH_PAGE_LENGTH, EEPROM_PAGE_COUNT, EEPROM_PAGE_LENGTH };
	usart_write(memory_information, sizeof(memory_information));
	crc = crc16(memory_information, sizeof(memory_information), crc);

	uint16_t program_length = PROGRAM_LENGTH;
	usart_write(&program_length, sizeof(program_length));
	crc = crc16(&program_length, sizeof(program_length), crc);

	uint8_t signature[SIGNATURE_LENGTH];
	signature_read(signature);
	usart_write(signature, sizeof(signature));
	crc = crc16(signature, sizeof(signature), crc);

	uint8_t fuse[FUSE_LENGTH];
	fuse_read(fuse);
	usart_write(fuse, sizeof(fuse));
	crc = crc16(fuse, sizeof(fuse), crc);

	usart_write(&crc, sizeof(crc));
}
void read_flash(uint8_t page_index)
{
	uint8_t data[FLASH_PAGE_LENGTH];
	flash_read_page(page_index, data);
	usart_write_checked(&data, sizeof(data));
}
void read_eeprom(uint8_t page_index)
{
	uint8_t data[EEPROM_PAGE_LENGTH];
	eeprom_read_page(page_index, data);
	usart_write_checked(&data, sizeof(data));
}
void write_flash(uint8_t page_index)
{
	uint8_t data[FLASH_PAGE_LENGTH];
	usart_read(&data, sizeof(data));
	flash_write_page(page_index, &data);
	usart_write_checked(0, 0);
}
void write_eeprom(uint8_t page_index)
{
	uint8_t data[EEPROM_PAGE_LENGTH];
	usart_read(&data, sizeof(data));
	eeprom_write_page(page_index, &data);
	usart_write_checked(0, 0);
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
			case get_device_information: device_information(); break;
			case read_page:
			case write_page:
			{
				memory memory;
				if (usart_read(&memory, sizeof(memory))) break;

				uint8_t page_index;
				if (usart_read(&page_index, sizeof(page_index))) break;

				switch (command)
				{
					case read_page:
						switch (memory)
						{
							case flash: read_flash(page_index); break;
							case eeprom: read_eeprom(page_index); break;
						}
						break;
					case write_page:
						switch (memory)
						{
							case flash: write_flash(page_index); break;
							case eeprom: write_eeprom(page_index); break;
						}
						break;
				}
			}
		}
	}

	usart_dispose();
}

void main()
{
	reset_type reset_type = initialize();

	if (reset_type == external) loader();

	start();
}
