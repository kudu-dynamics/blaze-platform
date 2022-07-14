# JAVA_INCLUDE_PATH = /usr/lib/jvm/java-11-openjdk-amd64/include
# JAVA_LIB_PATH = /usr/lib/jvm/java-11-openjdk-amd64/lib/server

# java:
# 	gcc -O -c -g \
# 		-I ${JAVA_INCLUDE_PATH}/ \
# 		-I ${JAVA_INCLUDE_PATH}/linux/ \
# 		java.c

# testjava:
# 	gcc -o testjava \
# 		-I ${JAVA_INCLUDE_PATH}/ \
# 		-I ${JAVA_INCLUDE_PATH}/linux/ \
# 		-L ${JAVA_LIB_PATH}/ \
# 		java.c \
# 		-ljvm

# haskell:
# 	ghc -O2 -Wall \
# 		-L${JAVA_LIB_PATH}/ \
# 		-ljvm \
# 		clojure.hs \
# 		java.o

build:
	stack build

download-clojure-jar:
	wget https://repo1.maven.org/maven2/org/clojure/clojure/1.11.1/clojure-1.11.1.jar -P res/clojure

download-spec-jar:
	wget https://repo1.maven.org/maven2/org/clojure/spec.alpha/0.3.218/spec.alpha-0.3.218.jar -P res/clojure

download: download-clojure-jar download-spec-jar

clean:
	stack clean
