#ifndef FILE_MMIO_H
#define FILE_MMIO_H
#include <stdlib.h>
#include <stdint.h>

static inline void *
addr_offset(const void *base, size_t offset)
{
	return (((char *) base) + offset);
}

static inline void *
addr_offset_mask(const void *base, size_t offset, size_t mask)
{
	return (((char *) base) + (offset & mask));
}

static inline void
mmio_write32(const void *base, size_t offset, uint32_t data)
{
	*(volatile uint32_t *) addr_offset(base, offset) = data;
}

static inline uint32_t
mmio_read32(const void *base, size_t offset)
{
	return *(volatile uint32_t *) addr_offset(base, offset);
}

#endif /* ndef FILE_MMIO_H */
