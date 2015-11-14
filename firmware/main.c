#include <basic.h>
#include <usart.h>
#include <memory.h>
#include <crc.h>

typedef enum { exit = 0x0000, read_device_information = 0x0001, read_memory_page = 0x0002, write_memory_page = 0x0003 } command;
typedef enum { flash = 0x0000, eeprom = 0x0001 } memory;

void usart_write_checked(void* data, size_t length)
{
	uint16_t crc = crc16(data, length, 0);

	usart_write(data, length);
	usart_write(&crc, sizeof(crc));
}

void read_flash(uint8_t page_index)
{
	uint8_t data[FLASH_PAGE_LENGTH];
	flash_read_page(page_index, data);
	usart_write_checked(&data, sizeof(data));
}
void write_flash(uint8_t page_index)
{
	uint8_t data[FLASH_PAGE_LENGTH];
	usart_read(&data, sizeof(data));
	flash_write_page(page_index, &data);
}

void read_eeprom(uint8_t page_index)
{
	uint8_t data[EEPROM_PAGE_LENGTH];
	eeprom_read_page(page_index, data);
	usart_write_checked(&data, sizeof(data));
}
void write_eeprom(uint8_t page_index)
{
	uint8_t data[EEPROM_PAGE_LENGTH];
	usart_read(&data, sizeof(data));
	eeprom_write_page(page_index, &data);
}

uint8_t do_exit()
{
	usart_write_checked(0, 0);

	return 1;
}
uint8_t do_read_device_information()
{
	uint16_t crc = 0;

	size_t memory_information[] = { FLASH_PAGE_COUNT, FLASH_PAGE_LENGTH, EEPROM_PAGE_COUNT, EEPROM_PAGE_LENGTH };
	usart_write(memory_information, sizeof(memory_information));
	crc = crc16(memory_information, sizeof(memory_information), crc);

	size_t application_length = APPLICATION_LENGTH;
	usart_write(&application_length, sizeof(application_length));
	crc = crc16(&application_length, sizeof(application_length), crc);

	uint8_t signature[SIGNATURE_LENGTH];
	signature_read(signature);
	usart_write(signature, sizeof(signature));
	crc = crc16(signature, sizeof(signature), crc);

	uint8_t fuse[FUSE_LENGTH];
	fuse_read(fuse);
	usart_write(fuse, sizeof(fuse));
	crc = crc16(fuse, sizeof(fuse), crc);

	usart_write(&crc, sizeof(crc));

	return 0;
}
uint8_t do_read_memory_page()
{
	memory memory;
	if (usart_read(&memory, sizeof(memory))) return 0;

	uint8_t page_index;
	if (usart_read(&page_index, sizeof(page_index))) return 0;

	switch (memory)
	{
		case flash: read_flash(page_index); break;
		case eeprom: read_eeprom(page_index); break;
	}

	return 0;
}
uint8_t do_write_memory_page()
{
	memory memory;
	if (usart_read(&memory, sizeof(memory))) return 0;

	uint8_t page_index;
	if (usart_read(&page_index, sizeof(page_index))) return 0;

	switch (memory)
	{
		case flash: write_flash(page_index); break;
		case eeprom: write_eeprom(page_index); break;
	}

	usart_write_checked(0, 0);

	return 0;
}

void boot_loader()
{
	// enable USART with a divider of 64 * 16, giving about 1 kBd/MHz
	usart_initialize(1, 1, 0x003F, 0);

	uint8_t status = 0;

	while (!status)
	{
		command command;
		if (usart_read(&command, sizeof(command))) continue;

		switch (command)
		{
			case exit: status = do_exit(); break;
			case read_device_information: status = do_read_device_information(); break;
			case read_memory_page: status = do_read_memory_page(); break;
			case write_memory_page: status = do_write_memory_page(); break;
		}

		usart_wait_send();
	}

	usart_dispose();
}

void main()
{
	reset_type reset_type = initialize();

	if (reset_type == external) boot_loader();

	application();
}
