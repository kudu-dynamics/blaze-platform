ROOT := $(realpath .)

all:
	gcc -ffile-prefix-map=$(ROOT)= -g -O0 -fno-stack-protector -no-pie -U_FORTIFY_SOURCE -z norelro -o demo1 demo1.c

clean:
	rm -f demo1
