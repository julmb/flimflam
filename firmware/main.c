#include <basic.h>
#include <usart.h>
#include <memory.h>
#include <crc.h>

typedef enum { flash = 0x0000, eeprom = 0x0001, sigcal = 0x0002, fuselock = 0x0003 } memory;
typedef enum { exit = 0x0000, read = 0x0001, write = 0x0002 } command;
typedef enum { error = 0x0000, success_exit = 0x0001, success_read = 0x0002, success_write = 0x0003 } response;

void response_error()
{
	response response = error;

	usart_write(&response, sizeof(response));
}
void response_success_exit()
{
	response response = success_exit;

	usart_write(&response, sizeof(response));
}
void response_success_read(void* data, size_t length)
{
	response response = success_read;
	uint16_t crc = crc16(data, length, 0);

	usart_write(&response, sizeof(response));
	usart_write(&crc, sizeof(crc));
	usart_write(&length, sizeof(length));
	usart_write(data, length);
}
void response_success_write(void* data, size_t length)
{
	response response = success_write;
	uint16_t crc = crc16(data, length, 0);

	usart_write(&response, sizeof(response));
	usart_write(&crc, sizeof(crc));
}

inline void read_memory(void (*read_page)(uint8_t page_index, void* data), uint16_t page_count, size_t page_length, uint8_t page_index)
{
	if (page_index >= page_count) { response_error(); return; }

	uint8_t data[page_length];

	read_page(page_index, data);

	response_success_read(&data, sizeof(data));
}
inline void write_memory(void (*write_page)(uint8_t page_index, void* data), uint16_t page_count, size_t page_length, uint8_t page_index)
{
	if (page_index >= page_count) { response_error(); return; }

	uint8_t data[page_length];

	if (usart_read(&data, sizeof(data))) { response_error(); return; }

	write_page(page_index, &data);

	response_success_write(&data, sizeof(data));
}

inline void do_read()
{
	memory memory;
	if (usart_read(&memory, sizeof(memory))) { response_error(); return; }

	uint8_t page_index;
	if (usart_read(&page_index, sizeof(page_index))) { response_error(); return; }

	switch (memory)
	{
		case flash: read_memory(flash_read_page, FLASH_PAGE_COUNT, FLASH_PAGE_LENGTH, page_index); break;
		case eeprom: read_memory(eeprom_read_page, EEPROM_PAGE_COUNT, EEPROM_PAGE_LENGTH, page_index); break;
		case sigcal: read_memory(sigcal_read_page, SIGCAL_PAGE_COUNT, SIGCAL_PAGE_LENGTH, page_index); break;
		case fuselock: read_memory(fuselock_read_page, FUSELOCK_PAGE_COUNT, FUSELOCK_PAGE_LENGTH, page_index); break;
		default: response_error();
	}
}
inline void do_write()
{
	memory memory;
	if (usart_read(&memory, sizeof(memory))) { response_error(); return; }

	uint8_t page_index;
	if (usart_read(&page_index, sizeof(page_index))) { response_error(); return; }

	switch (memory)
	{
		case flash: write_memory(flash_write_page, FLASH_PAGE_COUNT, FLASH_PAGE_LENGTH, page_index); break;
		case eeprom: write_memory(eeprom_write_page, EEPROM_PAGE_COUNT, EEPROM_PAGE_LENGTH, page_index); break;
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
			case exit: response_success_exit(); status = 1; break;
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
