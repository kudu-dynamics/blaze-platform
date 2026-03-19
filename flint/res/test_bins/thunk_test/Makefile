all:
	gcc -g -O0 -fdebug-prefix-map=$$HOME= -fno-stack-protector -no-pie -U_FORTIFY_SOURCE -z norelro -o thunk thunk.c

clean:
	rm -f thunk
