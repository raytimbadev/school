#ifndef DISK_EMU_H
#define DISK_EMU_H

/**
 * Initializes a fresh disk image, entirely zeroed out, at a given location.
 *
 * Returns 0 if the initialization is successful.
 * If the file given by the path `filename` cannot be opened, then this
 * function returns -1.
 */
int init_fresh_disk(char *filename, int block_size, int num_blocks);

/**
 * Initializes an existing disk image.
 *
 * Returns 0 if the initialization is successful.
 * If the file given by the path `filename` cannot be opened, then this
 * function returns -1.
 */
int init_disk(char *filename, int block_size, int num_blocks);

/**
 * Reads a number of blocks from the disk into a given buffer.
 *
 * If the return value is positive, then it represents the number of blocks
 * read.
 * If the return value is negative, then it represents the number of failures.
 */
int read_blocks(int start_address, int nblocks, void *buffer);

/**
 * Writes sequential blocks to the disk from a given buffer.
 *
 * Returns -1 if the number of blocks to write would go past the end of the
 * disk.
 * The buffer must have a size no smaller than the number of requested blocks
 * multiplied by the disk block size.
 */
int write_blocks(int start_address, int nblocks, void *buffer);

/**
 * Closes the emulated disk.
 *
 * Returns 0 if and only if the disk is closed successfully.
 */
int close_disk();

#endif
