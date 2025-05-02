all:
	gcc -g -O0 -fno-stack-protector -no-pie -U_FORTIFY_SOURCE -z norelro -o onion_test onion_test.c

clean:
	rm -f onion_test
