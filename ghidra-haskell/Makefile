res/ghidra.jar: scripts/getGhidraJar.sh
	scripts/getGhidraJar.sh

JAVAC ?= $(if $(JAVA_HOME),$(JAVA_HOME)/bin/javac,javac)
JAR   ?= $(if $(JAVA_HOME),$(JAVA_HOME)/bin/jar,jar)

res/pcode-helper.jar: java/PcodeHelper.java res/ghidra.jar
	$(JAVAC) -cp res/ghidra.jar -d java java/PcodeHelper.java
	cd java && $(JAR) cf ../res/pcode-helper.jar PcodeHelper.class
