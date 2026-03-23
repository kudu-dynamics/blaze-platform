all:
	gcc -g -O0 -fdebug-prefix-map=$$HOME= -fno-stack-protector -no-pie -U_FORTIFY_SOURCE -z norelro -o input_genesis input_genesis.c
	gcc -g -O0 -fdebug-prefix-map=$$HOME= -fno-stack-protector -no-pie -U_FORTIFY_SOURCE -z norelro -o input_simple input_simple.c
clean:
	rm -f input_genesis input_simple
