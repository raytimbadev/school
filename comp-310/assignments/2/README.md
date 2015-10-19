Building
========

Use the provided Makefile(s) to build the project. The main Makefile in the
project root directory will invoke the Makefiles in the subdirectories to do
the following.

1. Build the `libprintspool.a` library in the `common/lib` directory.
2. Build the `server` binary statically linked to `libprintspool.a` in the
   `server/bin` directory.
3. Build the `client` binary statically linked to `libprintspool.a` in the
   `client/bin` directory.

The binaries are also linked with librt and libpthread. No threading
functionality is used from libpthread or librt; it was necessary to link to it
in order to use POSIX semaphores and shared memory.

Running
=======

Run each server in its own terminal. Make sure to start the at least one server
before starting any clients.

1. `bin/server <spool size>`
2. `bin/client <duration> <message>`

For convenience, to test many clients at once, I have provided a script
`test-many-clients.sh` that simply runs though the script of the meme "All your
base are belong to us." The attached video shows me running this script to test
the programs.

Architechture
=============

I've extended the project a little bit to make it more fun. I have made it so
clients communicate a string along with their duration. When servers take a job
from the spool, they print the given message character by character, sleeping a
bit between each, so that the total printing time comes out to the duration
indicated by the client. The maximum message size is defined at compile-time
in `common/include/printspool.h`.

As for the locking and semaphores, I used the typical producer-consumer setup
on _n_ jobs. Three semaphores are used: _enqueue_ is initialized to _n_,
_dequeue_ is initialized to _0_, and _jobs_ is initialized to _1_. The first
two semaphores respectively represent the number of free and taken jobs,
whereas the last one is to ensure mutual exclusion on the print spool.

Specifically, when a server tries to take a job, the following happens:

    wait(dequeue_lock)
    wait(jobs_lock)

    copy the job pointed to by the print head index into local memory
    increment the print head index, mod n

    signal(jobs_lock)
    signal(enqueue_lock)

    return the copy

When a client tries to put a job, the following happens:

    wait(enqueue_lock)
    wait(jobs_lock)

    copy the job in local memory into the buffer at the print tail index
    increment the print tail index, mod n

    signal(jobs_lock)
    signal(dequeue_lock)

As for the shared memory, I use two shared memory objects: one holds the spool
metadata represented by the Spool struct, and the other holds the jobs buffer
represented as an array of PrintJob structs.

The reason for using two shared memory objects is simple: the metadata
structure has a fixed size, whereas the jobs buffer has a variable size
determined by a value in the metadata structure.

All the interesting code relating to locking and shared memory is in
`common/src/printspool.c`.
