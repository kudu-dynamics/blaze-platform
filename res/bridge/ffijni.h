#ifndef FFIJNI_H_
#define FFIJNI_H_

#include "jni.h"
#include <stdint.h>
#include <stdbool.h>

#if defined(FFIJNI_MACOSX) && defined(FFIJNI_OSX_GUI)
#include <objc/objc-runtime.h>
#endif


struct vm {
    JNIEnv* env;
    JavaVM* vm;
    bool attached;
};
typedef struct vm vm_t;

typedef jobject (*WrappedFun) (vm_t*, jobject, jobject, jobjectArray);

/* This is taken from HsFFI.h */
typedef void (*HsFunPtr)(void);

extern void freeFunPtr(WrappedFun);

void
setLibjvmPath (char*);

char*
getLibjvmPath ();

char*
getCompiledLibjvmPath();

jboolean
registerCallbacks (vm_t*, jclass);

vm_t*
createVM ();

vm_t*
createVM2 (unsigned int argc, char** argv);

void
destroyVM (vm_t*);

void
persistVM (vm_t*);

jclass
findClass (vm_t*, const char*);

jobject
newObject (vm_t*, jclass, jmethodID, jvalue*);

jmethodID
getConstructorID (vm_t*, jclass, const char*);

jmethodID
getStaticMethodID (vm_t*, jclass, const char*, const char*);

jmethodID
getMethodID (vm_t*, jclass, const char*, const char*);

jfieldID
getStaticFieldID (vm_t*, jclass, const char*, const char*);

jfieldID
getFieldID (vm_t*, jclass, const char*, const char*);

void
callStaticVoidMethod (vm_t*, jclass, jmethodID, jvalue*);

jint
callStaticIntMethod (vm_t*, jclass, jmethodID, jvalue*);

jlong
callStaticLongMethod (vm_t*, jclass, jmethodID, jvalue*);

jshort
callStaticShortMethod (vm_t*, jclass, jmethodID, jvalue*);

jbyte
callStaticByteMethod (vm_t*, jclass, jmethodID, jvalue*);

jfloat
callStaticFloatMethod (vm_t*, jclass, jmethodID, jvalue*);

jdouble
callStaticDoubleMethod (vm_t*, jclass, jmethodID, jvalue*);

jboolean
callStaticBooleanMethod (vm_t*, jclass, jmethodID, jvalue*);

jchar
callStaticCharMethod (vm_t*, jclass, jmethodID, jvalue*);

jobject
callStaticObjectMethod (vm_t*, jclass, jmethodID, jvalue*);

const char*
callStaticStringMethod (vm_t*, jclass, jmethodID, jvalue*);

void
callVoidMethod (vm_t*, jobject, jmethodID, jvalue*);

jlong
callLongMethod (vm_t*, jobject, jmethodID, jvalue*);

jint
callIntMethod (vm_t*, jobject, jmethodID, jvalue*);

jshort
callShortMethod (vm_t*, jobject, jmethodID, jvalue*);

jbyte
callByteMethod (vm_t*, jobject, jmethodID, jvalue*);

jfloat
callFloatMethod (vm_t*, jobject, jmethodID, jvalue*);

jdouble
callDoubleMethod (vm_t*, jobject, jmethodID, jvalue*);

jboolean
callBooleanMethod (vm_t*, jobject, jmethodID, jvalue*);

jchar
callCharMethod (vm_t*, jobject, jmethodID, jvalue*);

jobject
callObjectMethod (vm_t*, jobject, jmethodID, jvalue*);

const char*
callStringMethod(vm_t*, jobject, jmethodID, jvalue*);

jlong
getStaticLongField (vm_t*, jclass, jfieldID);

jint
getStaticIntField (vm_t*, jclass, jfieldID);

jshort
getStaticShortField (vm_t*, jclass, jfieldID);

jbyte
getStaticByteField (vm_t*, jclass, jfieldID);

jfloat
getStaticFloatField (vm_t*, jclass, jfieldID);

jdouble
getStaticDoubleField (vm_t*, jclass, jfieldID);

jboolean
getStaticBooleanField (vm_t*, jclass, jfieldID);

jchar
getStaticCharField (vm_t*, jclass, jfieldID);

jobject
getStaticObjectField (vm_t*, jclass, jfieldID);

const char*
getStaticStringField (vm_t*, jclass, jfieldID);

jlong
getLongField (vm_t*, jobject, jfieldID);

jint
getIntField (vm_t*, jobject, jfieldID);

jshort
getShortField (vm_t*, jobject, jfieldID);

jbyte
getByteField (vm_t*, jobject, jfieldID);

jfloat
getFloatField (vm_t*, jobject, jfieldID);

jdouble
getDoubleField (vm_t*, jobject, jfieldID);

jboolean
getBooleanField (vm_t*, jobject, jfieldID);

jchar
getCharField (vm_t*, jobject, jfieldID);

jobject
getObjectField (vm_t*, jobject, jfieldID);

const char*
getStringField (vm_t*, jobject, jfieldID);

void
setStaticLongField (vm_t*, jclass, jfieldID, jlong);

void
setStaticIntField (vm_t*, jclass, jfieldID, jint);

void
setStaticShortField (vm_t*, jclass, jfieldID, jshort);

void
setStaticByteField (vm_t*, jclass, jfieldID, jbyte);

void
setStaticFloatField (vm_t*, jclass, jfieldID, jfloat);

void
setStaticDoubleField (vm_t*, jclass, jfieldID, jdouble);

void
setStaticBooleanField (vm_t*, jclass, jfieldID, jboolean);

void
setStaticCharField (vm_t*, jclass, jfieldID, jchar);

void
setStaticObjectField (vm_t*, jclass, jfieldID, jobject);

void
setStaticStringField (vm_t*, jclass, jfieldID, const char*);

void
setLongField (vm_t*, jobject, jfieldID, jlong);

void
setIntField (vm_t*, jobject, jfieldID, jint);

void
setShortField (vm_t*, jobject, jfieldID, jshort);

void
setByteField (vm_t*, jobject, jfieldID, jbyte);

void
setFloatField (vm_t*, jobject, jfieldID, jfloat);

void
setDoubleField (vm_t*, jobject, jfieldID, jdouble);

void
setBooleanField (vm_t*, jobject, jfieldID, jboolean);

void
setCharField (vm_t*, jobject, jfieldID, jchar);

void
setObjectField (vm_t*, jobject, jfieldID, jobject);

void
setStringField (vm_t*, jclass, jfieldID, const char*);

jvalue*
newJValues (int);

void
setJValueBoolean (jvalue*, int, jboolean);

void
setJValueLong (jvalue*, int, jlong);

void
setJValueInt (jvalue*, int, jint);

void
setJValueShort (jvalue*, int, jshort);

void
setJValueByte (jvalue*, int, jbyte);

void
setJValueFloat (jvalue*, int, jfloat);

void
setJValueDouble (jvalue*, int, jdouble);

void
setJValueChar (jvalue*, int, jchar);

void
setJValueString (vm_t*, jvalue*, int, const char*);

void
setJValueObject (jvalue*, int, jobject);

jobject
newJString (vm_t*, const char*);

const jchar*
charsFromJString (vm_t*, jobject);

jint
getArrayLength(vm_t*, jobject);

jclass
getObjectClass(vm_t*, jobject);

jboolean
isInstanceOf(vm_t*, jobject, jclass);

const char*
bytesFromJString (vm_t*, jobject);

void
releaseJChars (vm_t*, jobject, const jchar*);

void
releaseJBytes (vm_t*, jobject, const char*);

const char*
jStringToCString (vm_t*, jobject);

jboolean
exceptionCheck (vm_t*);

void
exceptionClear (vm_t*);

void
exceptionDescribe (vm_t*);

jthrowable
exceptionOccurred (vm_t*);

jthrowable
exceptionOccurredClear (vm_t*);

void
releaseJObjectRef (vm_t*, jobject);

void
releaseJClassRef (vm_t*, jclass);

void
releaseJThrowableRef (vm_t*, jthrowable);

void
release (jobject);

void
runCocoaMain ();

#include "hfunction.h"

#endif /* FFIJNI_H_ */

