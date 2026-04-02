FILTER_FLAGS=-fdebug-prefix-map=$$HOME=
G_FLAGS=-g -O0 $(FILTER_FLAGS)
DEMO_FLAGS=-fno-builtin -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0 -fno-stack-protector

all: cfs_test cfg_test heap_overflow

cfs_test: cfs_test.c
	gcc $(G_FLAGS) -o $@ $<

cfg_test: cfg_test.c
	gcc $(G_FLAGS) -o $@ $<

heap_overflow: heap_overflow.c
	gcc $(G_FLAGS) $(DEMO_FLAGS) -o $@ $<

clean:
	rm -rf cfs_test cfg_test heap_overflow
