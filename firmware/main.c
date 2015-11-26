#include <basic.h>
#include <usart.h>
#include <memory.h>
#include <crc.h>

typedef enum { flash = 0x0000, eeprom = 0x0001, sigcal = 0x0002, fuselock = 0x0003 } memory;
typedef enum { exit = 0x0000, read = 0x0001, write = 0x0002 } command;
typedef enum { error = 0x0000, exit_success = 0x0001, read_success = 0x0002, write_success = 0x0003 } response;

void response_error()
{
	response response = error;

	usart_write(&response, sizeof(response));
}
void response_exit_success()
{
	response response = exit_success;

	usart_write(&response, sizeof(response));
}
void response_read_success(void* data, size_t length)
{
	response response = read_success;
	uint16_t crc = crc16(data, length, 0);

	usart_write(&response, sizeof(response));
	usart_write(&crc, sizeof(crc));
	usart_write(&length, sizeof(length));
	usart_write(data, length);
}
void response_write_success()
{
	response response = write_success;

	usart_write(&response, sizeof(response));
}

void read_flash(uint8_t page_index)
{
	if (page_index < FLASH_PAGE_COUNT)
	{
		uint8_t data[FLASH_PAGE_LENGTH];
		flash_read_page(page_index, data);
		response_read_success(&data, sizeof(data));
	}
	else response_error();
}
void write_flash(uint8_t page_index)
{
	if (page_index < FLASH_PAGE_COUNT)
	{
		uint8_t data[FLASH_PAGE_LENGTH];
		usart_read(&data, sizeof(data));
		flash_write_page(page_index, &data);
		response_write_success();
	}
	else response_error();
}

void read_eeprom(uint8_t page_index)
{
	if (page_index < EEPROM_PAGE_COUNT)
	{
		uint8_t data[EEPROM_PAGE_LENGTH];
		eeprom_read_page(page_index, data);
		response_read_success(&data, sizeof(data));
	}
	else response_error();
}
void write_eeprom(uint8_t page_index)
{
	if (page_index < EEPROM_PAGE_COUNT)
	{
		uint8_t data[EEPROM_PAGE_LENGTH];
		usart_read(&data, sizeof(data));
		eeprom_write_page(page_index, &data);
		response_write_success();
	}
	else response_error();
}

void read_sigcal(uint8_t page_index)
{
	if (page_index == 0)
	{
		uint8_t data[SIGCAL_LENGTH];
		sigcal_read(data);
		response_read_success(&data, sizeof(data));
	}
	else response_error();
}

void read_fuselock(uint8_t page_index)
{
	if (page_index == 0)
	{
		uint8_t data[FUSELOCK_LENGTH];
		fuselock_read(data);
		response_read_success(&data, sizeof(data));
	}
	else response_error();
}

void do_read()
{
	memory memory;
	if (usart_read(&memory, sizeof(memory))) { response_error(); return; }

	uint8_t page_index;
	if (usart_read(&page_index, sizeof(page_index))) { response_error(); return; }

	switch (memory)
	{
		case flash: read_flash(page_index); break;
		case eeprom: read_eeprom(page_index); break;
		case sigcal: read_sigcal(page_index); break;
		case fuselock: read_fuselock(page_index); break;
		default: response_error();
	}
}
void do_write()
{
	memory memory;
	if (usart_read(&memory, sizeof(memory))) { response_error(); return; }

	uint8_t page_index;
	if (usart_read(&page_index, sizeof(page_index))) { response_error(); return; }

	switch (memory)
	{
		case flash: write_flash(page_index); break;
		case eeprom: write_eeprom(page_index); break;
		default: response_error();
	}
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
			case exit: response_exit_success(); status = 1; break;
			case read: do_read(); break;
			case write: do_write(); break;
			default: response_error();
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
