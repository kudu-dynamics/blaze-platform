build: shim_libc.so shim_main

shim_libc.so: shim_libc.c
	gcc -shared -fPIC -o shim_libc.so shim_libc.c

shim_main: shim_main.c
	gcc -o shim_main shim_main.c

clean:
	rm -f shim_libc.so shim_main
