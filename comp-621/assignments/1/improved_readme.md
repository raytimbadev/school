kernel_opt.m
============

We added a lookup table GM containing the product `G * mi * mj` for all `i` and
`j`. In the unoptimized program, this product was calculated upon each
invocation of the `gforce` function, i.e. almost once per iteration of the
inner loop.
