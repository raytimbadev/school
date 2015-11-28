malloc
======

The design is very simple. Here's the pseudocode for allocation:

* scan the doubly-linked list for a free block using the current scanning
  strategy.
* if the scanning strategy fails: 
    * bump up the program break by 128KiB and
      append the newly created free block to the list.
    * merge the newly created block with the preceding block if it is free.
* if the found block has enough space to be split:
    * the block is split; the first section will be allocated and the second
      section is the new free block. The pointers in the linked list are
      adjusted to reflect this change.
* else:
    * the entire block is consumed by the allocation, and is deleted from the
      free list entirely.

The scanning strategy is changed using the `jmallopt` function.

Here's the pseudocode for freeing:

* scan the doubly-linked list to determine which free blocks come immediately
  before and immediately after the given pointer.
* if there is no _before_ or _after_ block, then the block of pointer to free
  will be made the first or last block in the linked list.
* insert the block of the pointer to free between the _before_ and _after_
  blocks.
* try to merge the block of the pointer to free with the _after_ block.
* try to merge the _before_ block with the block of the pointer to free.
* try to shrink the last block.

A merge is possible between blocks A and B that are adjacent in the linked list
if the address of block A plus its size plus the size of the block metadata
equals the address of block B. Indeed, this occurs when there is no intervening
used block between block A and block B.

In order to perform the merge, we simply increase the size of block A by the
size of block B plus the size of the block metadata. Then we delete block B and
adjust the pointers in the linked list.

Testing
=======

We implement four testing routines.

`test_alloc_free`
-----------------

This routine allocates a small amount of memory and immediately frees it a
certain number of times. The amount of memory are taken circularly from an
array of five predetermined sizes.

This routine performs as expected.

`test_big_allocs`
-----------------

This routine allocates a certain number of large chunks of memory and then
frees them. The preceding testing routine performs the free immediately after
the allocation, whereas this routing first performs all the allocations, then
performs all the frees in the same order the allocations were performed.

This routine performs as expected.

`test_big_allocs_reverse`
-------------------------

This routine is the same as the preceding one, but the order in which the
blocks are freed is the reverse of the order in which they are allocation. The
purpose of this routine is to properly verify that the merging routine works.

This routine performs as expected.

`test_interleave`
-----------------

This routine allocates memory and on every iteration frees the pointer
allocated five iterations ago (for each iteration past number four).

This routine appears to have a bug. Once it completes, we expect that all
memory is freed, but instead the memory is in a fragmented state. After much
debugging, we are still unsure why this occurs.
