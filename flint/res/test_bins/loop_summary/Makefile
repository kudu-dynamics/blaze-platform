all:
	gcc -g -O0 -fno-stack-protector -no-pie -U_FORTIFY_SOURCE -z norelro -o loop_test loop_test.c

clean:
	rm -f loop_test
