JAVAPATH ?= /usr/lib/jvm/java-11-openjdk-amd64
# JAVA_LIB_PATH = /usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/amd64/server
# JAVA_INCLUDE_PATH = /usr/lib/jvm/java-1.8.0-openjdk-amd64/include

JAVA_INCLUDE_PATH = /usr/lib/jvm/java-11-openjdk-amd64/include
JAVA_LIB_PATH = /usr/lib/jvm/java-11-openjdk-amd64/lib/server

java:
	gcc -O -c -g \
		-I ${JAVA_INCLUDE_PATH}/ \
		-I ${JAVA_INCLUDE_PATH}/linux/ \
		java.c

testjava:
	gcc -o testjava \
		-I ${JAVA_INCLUDE_PATH}/ \
		-I ${JAVA_INCLUDE_PATH}/linux/ \
		-L ${JAVA_LIB_PATH}/ \
		java.c \
		-ljvm

haskell:
	ghc -O2 -Wall \
		-L${JAVA_LIB_PATH}/ \
		-ljvm \
		clojure.hs \
		java.o

all: java haskell

run:
	./clojure

clean:
	rm -f java.o 
	rm -f clojure clojure.o clojure.hi
