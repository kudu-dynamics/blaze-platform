#define FFIJNI_LINUX
#define FFIJNI_LIBJVM "./libjvm.so"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ffijni.h"
#include "jni.h"

#if defined(FFIJNI_LINUX) || defined(FFIJNI_MACOSX)
#include <dlfcn.h>
#else
#if defined(FFIJNI_WINDOWS)
#include <windows.h>
#endif
#endif

#define FFIJNI_VERSION JNI_VERSION_10

#define FFIJNI_DEBUG JNI_TRUE

#ifdef FFIJNI_DEBUG /* def FFIJNI_DEBUG */

static jboolean
_ffijni_debug = JNI_TRUE;

jboolean
getDebugStatus()
{
    return _ffijni_debug;
}

void
setDebugStatus(jboolean status)
{
    _ffijni_debug = status;
}
#define DEBUG(STR) \
    if (_ffijni_debug) { \
        printf("ffijni: %s\n", STR); \
    }
#define DEBUG1(STR, ARG1) \
    if (_ffijni_debug) { \
        printf("ffijni: " STR "\n", ARG1); \
        fflush(stdout); \
    }
#define DEBUG2(STR, ARG1, ARG2) \
    if (_ffijni_debug) { \
        printf("ffijni: " STR "\n", ARG1, ARG2); \
        fflush(stdout); \
    }
#define DEBUG3(STR, ARG1, ARG2, ARG3) \
    if (_ffijni_debug) { \
        printf("ffijni: " STR "\n", ARG1, ARG2, ARG3); \
        fflush(stdout); \
    }
#define DEBUG4(STR, ARG1, ARG2, ARG3, ARG4) \
    if (_ffijni_debug) { \
        printf("ffijni: " STR "\n", ARG1, ARG2, ARG3, ARG4); \
        fflush(stdout); \
    }
#else /* def FFIJNI_DEBUG */
jboolean
getDebugStatus() { return JNI_FALSE; }

void
setDebugStatus(jboolean _state)
{
    fprintf(stderr,
            "ffijni: setDebugStatus(%s) was called, but this is not a debug build.\n"
            "ffijni: Use the DEBUG flag to enable a debug build.\n",
            _state ? "true" : "false");
}
#define DEBUG(STR)
#define DEBUG1(STR, ARG1)
#define DEBUG2(STR, ARG1, ARG2)
#define DEBUG3(STR, ARG1, ARG2, ARG3)
#define DEBUG4(STR, ARG1, ARG2, ARG3, ARG4)
#endif /* def FFIJNI_DEBUG */

#define XSTR(s) STR(s)
#define STR(s) #s

#define GLOBAL_REF(VMI, LOCAL, GLOBAL) \
    jobject GLOBAL = (*VMI->env)->NewGlobalRef(VMI->env, LOCAL); \
    (*VMI->env)->DeleteLocalRef(VMI->env, LOCAL); \
    DEBUG2("-> new global ref from local: %p -> %p", LOCAL, GLOBAL);

#ifdef FFIJNI_OSX_FRAMEWORK /* def FFIJNI_OSX_FRAMEWORK */

#define GET_CREATED_JAVA_VMS(a, b, c) JNI_GetCreatedJavaVMs(a, b, c)
#define CREATE_JAVA_VM(a, b, c) JNI_CreateJavaVM(a, b, c)

void
setLibjvmPath(char* path) {}

char*
getLibjvmPath() { return "Linked with JavaVM Framework at compile time."; }

char*
getCompiledLibjvmPath() { return "Linked with JavaVM Framework at compile time."; }

#else /* def FFIJNI_OSX_FRAMEWORK */

#ifndef FFIJNI_LIBJVM
#error "Please specify -DFFIJNI_LIBJVM=<path-to-libjvm>."
#endif

#define GET_CREATED_JAVA_VMS(a, b, c) _GetCreatedJavaVMs(a, b, c)
#define CREATE_JAVA_VM(a, b, c) _CreateJavaVM(a, b, c)

static
jint (*_GetCreatedJavaVMs) (JavaVM** vmBuf, jsize bufLen, jsize* nVMs) = NULL;

static
jint (*_CreateJavaVM) (JavaVM** pvm, void** penv, void* vmargs) = NULL;

static
char* _libjvm_path = NULL;

void
setLibjvmPath(char* path)
{
    _libjvm_path = path;
}

char*
getLibjvmPath()
{
    return _libjvm_path;
}

char*
getCompiledLibjvmPath()
{
    return XSTR(FFIJNI_LIBJVM);
}

jint
_load_JNI()
{
  char *err;
    if (!_GetCreatedJavaVMs) {
        if (!_libjvm_path) {
            /* The location of libjvm can be overriden by the
             * environment variable FFIJNI_LIBJVM. */
            _libjvm_path = getenv("FFIJNI_LIBJVM");
        }
        if (!_libjvm_path) {
            /* If no such environment variable exists, use the built in
             * location (needs to be given at compile time) */
            _libjvm_path = XSTR(FFIJNI_LIBJVM);
        }
        DEBUG1("Loading libjvm from %s.", _libjvm_path)
        #if defined(FFIJNI_LINUX) || defined(FFIJNI_MACOSX)
        /* On Linux and MacOSX use dlopen(...) to manually
         * load the dynamic library. */
          printf("DLOPEN %s\n", _libjvm_path);
        void* libVM = dlopen(_libjvm_path, RTLD_LAZY);
        if (!libVM) {
          err = dlerror();
          printf("DLOPEN error %s\n", err);
          return -41;
        }
        _GetCreatedJavaVMs = (jint (*)(JavaVM**,jsize,jsize*)) dlsym(libVM, "JNI_GetCreatedJavaVMs");
        _CreateJavaVM = (jint (*)(JavaVM**,void**,void*)) dlsym(libVM, "JNI_CreateJavaVM");
        #else
        #if defined(FFIJNI_WINDOWS)
        /* On Windows use LoadLibrary(...) to manually
         * load the dynamic library. */
        HINSTANCE hVM = LoadLibrary(_libjvm_path);
        if (!hVM) {
            return -41;
        }
        _GetCreatedJavaVMs = (jint (*)(JavaVM**,jsize,jsize*)) GetProcAddress(hVM, "JNI_GetCreatedJavaVMs");
        _CreateJavaVM = (jint (*)(JavaVM**,void**,void*)) GetProcAddress(hVM, "JNI_CreateJavaVM");
        #else
            #error "Unsupported platform."
        #endif
        #endif
    }
    return 0;
}
#endif /* def FFIJNI_OSX_FRAMEWORK */


void
runCocoaMain()
{
    #if defined(FFIJNI_MACOSX) && defined(FFIJNI_OSX_GUI)
    DEBUG("Retrieving NSApp...")
    void* clazz = objc_getClass("NSApplication");
    void* app = objc_msgSend(clazz, sel_registerName("sharedApplication"));

    DEBUG1("-> %p", app)

    DEBUG("Starting cocoa main runloop")
    objc_msgSend(app, sel_registerName("run"));
    #endif
}


jobject JNICALL
_callCallback(JNIEnv* env, jobject self, jlong func, jobject method, jobjectArray args)
{
    WrappedFun funPtr = (WrappedFun) func;
    DEBUG3("Callback called (func: %d -> %p, method: %p)\n", func, funPtr, method)
    vm_t vmi;
    vmi.env = env;
    (*env)->GetJavaVM(env, &vmi.vm);
    return funPtr(&vmi, self, method, args);
}


void JNICALL
_releaseCallback(JNIEnv* env, jobject self, jlong func)
{
    WrappedFun funPtr = (WrappedFun) func;
    DEBUG2("Callback released (func: %d -> %p)", func, funPtr)
    freeFunPtr(funPtr);
}


void
_loadHFunctionClass(JNIEnv* env)
{
    /* boilerplate:
     * - find java.lang.ClassLoader
     * - get #getSystemClassLoader
     * - get #defineClass
     * - retrieve the systemClassLoader
     */
    jclass clazz = (*env)->FindClass(env, "java/lang/ClassLoader");
    DEBUG1("Loaded java.lang.ClassLoader: %p", clazz)

    jmethodID getSystemClassLoader =
        (*env)->GetStaticMethodID(env, clazz, "getSystemClassLoader", "()Ljava/lang/ClassLoader;");
    DEBUG1("Got #getSystemClassLoader: %p", getSystemClassLoader)

    jmethodID defineClass =
        (*env)->GetMethodID(env, clazz, "defineClass", "(Ljava/lang/String;[BII)Ljava/lang/Class;");
    DEBUG1("Got #defineClass: %p", defineClass)

    jobject systemClassLoader = (*env)->CallStaticObjectMethod(env, clazz, getSystemClassLoader);
    DEBUG1("Retrieved systemClassLoader: %p", systemClassLoader)

    /* define the class:
     * - create the string className ("HFunction")
     * - create the jbyteArray bytes from hFunctionClass (see hfunction.h)
     * - call #defineClass on systemClassLoader
     */
    jstring className = (*env)->NewStringUTF(env, "HFunction");
    DEBUG1("Allocated className: %p", className)

    jbyteArray classBytes = (*env)->NewByteArray(env, FFIJNI_HFUNCTION_LENGTH);
    (*env)->SetByteArrayRegion(env, classBytes, 0, FFIJNI_HFUNCTION_LENGTH, hFunctionClass);
    DEBUG1("Allocated classBytes: %p", classBytes)

    jclass hFunction =
        (*env)->CallObjectMethod(env, systemClassLoader, defineClass,
                                 className, classBytes, 0, FFIJNI_HFUNCTION_LENGTH);
    DEBUG1("Defined class HFunction: %p", hFunction)
    
    /* clean up */
    (*env)->DeleteLocalRef(env, systemClassLoader);
    (*env)->DeleteLocalRef(env, className);
    (*env)->DeleteLocalRef(env, classBytes);
    (*env)->DeleteLocalRef(env, hFunction);
}

jboolean
registerCallbacks(vm_t* vmi, jclass hFunction) {
    JNIEnv* env = vmi->env;

    JNINativeMethod method;
    jint result;

    /* register native call function */
    DEBUG1("Registering call(...): %p", _callCallback)

    method.name = "call";
    method.signature = "(JLjava/lang/reflect/Method;[Ljava/lang/Object;)Ljava/lang/Object;";
    method.fnPtr = _callCallback;

    result = (*env)->RegisterNatives(env, hFunction, &method, 1);
    if (result < 0) {
        (*env)->ExceptionDescribe(env);
        return JNI_FALSE;
    }
    
    /* register native release function */
    DEBUG1("Registering release(...): %p", _releaseCallback)

    method.name = "release";
    method.signature = "(J)V";
    method.fnPtr = _releaseCallback;

    result = (*env)->RegisterNatives(env, hFunction, &method, 1);
    if (result < 0) {
        (*env)->ExceptionDescribe(env);
        return JNI_FALSE;
    }

    return JNI_TRUE;
}


jint
_get_vm(JavaVM** jvm, unsigned int argc, char** argv)
{
    #ifndef FFIJNI_OSX_FRAMEWORK
    /* First things first: load the dynamic library
     * (this is not necessary if the library was linked with the
     * JavaVM framework on OSX, see OSX_FRAMEWORK flag / cabal file). */
    if (_load_JNI() < 0) {
        DEBUG("Could not load libjvm")
        return -41;
    }
    #endif

    jsize numCreatedVMs = 0;
    GET_CREATED_JAVA_VMS(jvm, 1, &numCreatedVMs);
    if (numCreatedVMs > 0) {
        DEBUG("JVM already created")
        return 1;
    }

    JNIEnv* env;
    JavaVMInitArgs vm_args;
    vm_args.version = FFIJNI_VERSION;
    vm_args.ignoreUnrecognized = JNI_FALSE;

    /* Create a new Virtual Machine: */
    JavaVMOption* options = calloc(argc, sizeof(JavaVMOption));
    if (argc == 0) {
        vm_args.nOptions = 0;
    } else {
        for (int i = 0; i < argc; i++) {
            options[i].optionString = argv[i];
        }
        vm_args.nOptions = argc;
        vm_args.options = options;
    }

    jint status;
    if ((status = CREATE_JAVA_VM(jvm, (void**) &env, &vm_args)) < 0) {
        /* Creation of Virtual Machine failed. */
        free(options);
        return status;
    }
    /* Creation of Virtual Machine was successfull. */
    free(options);

    _loadHFunctionClass(env);

    return 0;
}

vm_t*
createVM ()
{
    return createVM2(0, NULL);
}

vm_t*
createVM2 (unsigned int argc, char** argv)
{
    JavaVM* jvm;
    jint status = _get_vm(&jvm, argc, argv);

    if (status < 0) {
      printf("STATUS WAS < 0\n");
      return NULL;
    }
    printf("STATUS WAS NOT < 0\n");
    JNIEnv* env;

    vm_t* vmi = malloc(sizeof(vm_t));
    vmi->attached = false;

    (*jvm)->GetEnv(jvm, (void**) &env, FFIJNI_VERSION);
    if (env == NULL) {
        DEBUG("Attach current thread")
        if ((*jvm)->AttachCurrentThread(jvm, (void**) &env, NULL) < 0) {
            free(vmi);
            return NULL;
        } else {
            vmi->attached = true;
        }
    }
    vmi->env = env;
    vmi->vm = jvm;

    return vmi;
}

void
destroyVM (vm_t* vmi)
{
    if (vmi->attached) {
        DEBUG("Detach current thread")
        (*vmi->vm)->DetachCurrentThread(vmi->vm);
    } else {
        DEBUG("Destroy Java VM")
        (*vmi->vm)->DestroyJavaVM(vmi->vm);
    }
    free(vmi);
}

void
persistVM (vm_t* vmi)
{
    DEBUG("persistVM()")

    vmi->attached = true;
}

jclass
findClass (vm_t* vmi, const char* className)
{
    DEBUG1("findClass(%s)", className)

    jclass clazzLocal = (*vmi->env)->FindClass(vmi->env, className);
    GLOBAL_REF(vmi, clazzLocal, clazz)
    return clazz;
}

jobject
newObject (vm_t* vmi, jclass clazz, jmethodID constructor, jvalue* args)
{
    DEBUG2("newObject(%p, %p)", clazz, constructor)
    jobject objLocal = (*vmi->env)->NewObjectA(vmi->env, clazz, constructor, args);
    GLOBAL_REF(vmi, objLocal, obj)
    return obj;
}

jmethodID
getConstructorID (vm_t* vmi, jclass clazz, const char* sig)
{
    DEBUG1("getConstructorID(%s)", sig)
    jmethodID methodID = (*vmi->env)->GetMethodID(vmi->env, clazz, "<init>", sig);
    DEBUG1("-> %p", methodID)
    return methodID;
}

jmethodID
getStaticMethodID(vm_t* vmi, jclass clazz, const char* methodName, const char* sig)
{
    DEBUG2("getStaticMethodID(%s, %s)", methodName, sig)
    jmethodID methodID = (*vmi->env)->GetStaticMethodID(vmi->env, clazz, methodName, sig);
    DEBUG1("-> %p", methodID)
    return methodID;
}

jmethodID
getMethodID(vm_t* vmi, jclass clazz, const char* methodName, const char* sig)
{
    DEBUG2("getMethodID(%s, %s)", methodName, sig)
    jmethodID methodID = (*vmi->env)->GetMethodID(vmi->env, clazz, methodName, sig);
    DEBUG1("-> %p", methodID)
    return methodID;
}

jfieldID
getStaticFieldID(vm_t* vmi, jclass clazz, const char* fieldName, const char* sig)
{
    DEBUG2("getStaticFieldID(%s, %s)", fieldName, sig)

    return (*vmi->env)->GetStaticFieldID(vmi->env, clazz, fieldName, sig);
}

jfieldID
getFieldID(vm_t* vmi, jclass clazz, const char* fieldName, const char* sig)
{
    DEBUG2("getFieldID(%s, %s)", fieldName, sig)

    return (*vmi->env)->GetFieldID(vmi->env, clazz, fieldName, sig);
}

void
callStaticVoidMethod (vm_t* vmi, jclass clazz, jmethodID method, jvalue* args)
{
    DEBUG2("callStaticVoidMethod(%p, %p)", clazz, method)

    (*vmi->env)->CallStaticVoidMethodA(vmi->env, clazz, method, args);
}

jint
callStaticIntMethod (vm_t* vmi, jclass clazz, jmethodID method, jvalue* args)
{
    DEBUG2("callStaticIntMethod(%p, %p)", clazz, method)

    return (*vmi->env)->CallStaticIntMethodA(vmi->env, clazz, method, args);
}

jlong
callStaticLongMethod (vm_t* vmi, jclass clazz, jmethodID method, jvalue* args)
{
    DEBUG2("callStaticLongMethod(%p, %p)", clazz, method)

    return (*vmi->env)->CallStaticLongMethodA(vmi->env, clazz, method, args);
}

jshort
callStaticShortMethod (vm_t* vmi, jclass clazz, jmethodID method, jvalue* args)
{
    DEBUG2("callStaticShortMethod(%p, %p)", clazz, method)

    return (*vmi->env)->CallStaticShortMethodA(vmi->env, clazz, method, args);
}

jbyte
callStaticByteMethod (vm_t* vmi, jclass clazz, jmethodID method, jvalue* args)
{
    DEBUG2("callStaticVoidMethod(%p, %p)", clazz, method)

    return (*vmi->env)->CallStaticByteMethodA(vmi->env, clazz, method, args);
}

jfloat
callStaticFloatMethod (vm_t* vmi, jclass clazz, jmethodID method, jvalue* args)
{
    DEBUG2("callStaticFloatMethod(%p, %p)", clazz, method)

    return (*vmi->env)->CallStaticFloatMethodA(vmi->env, clazz, method, args);
}

jdouble
callStaticDoubleMethod (vm_t* vmi, jclass clazz, jmethodID method, jvalue* args)
{
    DEBUG2("callStaticVoidMethod(%p, %p)", clazz, method)

    return (*vmi->env)->CallStaticDoubleMethodA(vmi->env, clazz, method, args);
}

jboolean
callStaticBooleanMethod (vm_t* vmi, jclass clazz, jmethodID method, jvalue* args)
{
    DEBUG2("callStaticBooleanMethod(%p, %p)", clazz, method)

    return (*vmi->env)->CallStaticBooleanMethodA(vmi->env, clazz, method, args);
}

jchar
callStaticCharMethod (vm_t* vmi, jclass clazz, jmethodID method, jvalue* args)
{
    DEBUG2("callStaticCharMethod(%p, %p)", clazz, method)

    return (*vmi->env)->CallStaticCharMethodA(vmi->env, clazz, method, args);
}

jobject
callStaticObjectMethod (vm_t* vmi, jclass clazz, jmethodID method, jvalue* args)
{
    DEBUG2("callStaticObjectMethod(%p, %p)", clazz, method)

    jobject objLocal = (*vmi->env)->CallStaticObjectMethodA(vmi->env, clazz, method, args);
    if (objLocal) {
        GLOBAL_REF(vmi, objLocal, obj)
        return obj;
    } else {
        return NULL;
    }
}

const char*
callStaticStringMethod (vm_t* vmi, jclass clazz, jmethodID method, jvalue* args)
{
    DEBUG2("callStaticStringMethod(%p, %p)", clazz, method)

    jobject string = (*vmi->env)->CallStaticObjectMethodA(vmi->env, clazz, method, args);
    if (string) {
        return jStringToCString(vmi, string);
    } else {
        return NULL;
    }
}

void
callVoidMethod (vm_t* vmi, jobject object, jmethodID method, jvalue* args)
{
    DEBUG2("callVoidMethod(%p, %p)", object, method)

    (*vmi->env)->CallVoidMethodA(vmi->env, object, method, args);
}

jlong
callLongMethod (vm_t* vmi, jobject object, jmethodID method, jvalue* args)
{
    DEBUG2("callLongMethod(%p, %p)", object, method)

    return (*vmi->env)->CallLongMethodA(vmi->env, object, method, args);
}

jint
callIntMethod (vm_t* vmi, jobject object, jmethodID method, jvalue* args)
{
    DEBUG2("callIntMethod(%p, %p)", object, method)
        
    return (*vmi->env)->CallIntMethodA(vmi->env, object, method, args);
}

jshort
callShortMethod (vm_t* vmi, jobject object, jmethodID method, jvalue* args)
{
    DEBUG2("callShortMethod(%p, %p)", object, method)

    return (*vmi->env)->CallShortMethodA(vmi->env, object, method, args);
}

jbyte
callByteMethod (vm_t* vmi, jobject object, jmethodID method, jvalue* args)
{
    DEBUG2("callByteMethod(%p, %p)", object, method)

    return (*vmi->env)->CallByteMethodA(vmi->env, object, method, args);
}

jfloat
callFloatMethod (vm_t* vmi, jobject object, jmethodID method, jvalue* args)
{
    DEBUG2("callFloatMethod(%p, %p)", object, method)

    return (*vmi->env)->CallFloatMethodA(vmi->env, object, method, args);
}

jdouble
callDoubleMethod (vm_t* vmi, jobject object, jmethodID method, jvalue* args)
{
    DEBUG2("callDoubleMethod(%p, %p)", object, method)

    return (*vmi->env)->CallDoubleMethodA(vmi->env, object, method, args);
}

jboolean
callBooleanMethod (vm_t* vmi, jobject object, jmethodID method, jvalue* args)
{
    DEBUG2("callBooleanMethod(%p, %p)", object, method)

    return (*vmi->env)->CallBooleanMethodA(vmi->env, object, method, args);
}

jchar
callCharMethod (vm_t* vmi, jobject object, jmethodID method, jvalue* args)
{
    DEBUG2("callCharMethod(%p, %p)", object, method)

    return (*vmi->env)->CallCharMethodA(vmi->env, object, method, args);
}

jobject
callObjectMethod (vm_t* vmi, jobject object, jmethodID method, jvalue* args)
{
    DEBUG2("callObjectMethod(%p, %p)", object, method)

    jobject objLocal = (*vmi->env)->CallObjectMethodA(vmi->env, object, method, args);
    if (objLocal) {
        GLOBAL_REF(vmi, objLocal, obj)
        return obj;
    } else {
        return NULL;
    }
}

const char*
callStringMethod (vm_t* vmi, jobject object, jmethodID method, jvalue* args)
{
    DEBUG2("callStringMethod(%p, %p)", object, method)

    jobject string = (*vmi->env)->CallObjectMethodA(vmi->env, object, method, args);
    if (string) {
        return jStringToCString(vmi, string);
    } else {
        return NULL;
    }
}

jlong
getStaticLongField (vm_t* vmi, jclass clazz, jfieldID field)
{
    return (*vmi->env)->GetStaticLongField(vmi->env, clazz, field);
}

jint
getStaticIntField (vm_t* vmi, jclass clazz, jfieldID field)
{
    return (*vmi->env)->GetStaticIntField(vmi->env, clazz, field);
}

jshort
getStaticShortField (vm_t* vmi, jclass clazz, jfieldID field)
{
    return (*vmi->env)->GetStaticShortField(vmi->env, clazz, field);
}

jbyte
getStaticByteField (vm_t* vmi, jclass clazz, jfieldID field)
{
    return (*vmi->env)->GetStaticByteField(vmi->env, clazz, field);
}

jfloat
getStaticFloatField (vm_t* vmi, jclass clazz, jfieldID field)
{
    return (*vmi->env)->GetStaticFloatField(vmi->env, clazz, field);
}

jdouble
getStaticDoubleField (vm_t* vmi, jclass clazz, jfieldID field)
{
    return (*vmi->env)->GetStaticDoubleField(vmi->env, clazz, field);
}

jboolean
getStaticBooleanField (vm_t* vmi, jclass clazz, jfieldID field)
{
    return (*vmi->env)->GetStaticBooleanField(vmi->env, clazz, field);
}

jchar
getStaticCharField (vm_t* vmi, jclass clazz, jfieldID field)
{
    return (*vmi->env)->GetStaticCharField(vmi->env, clazz, field);
}

jobject
getStaticObjectField (vm_t* vmi, jclass clazz, jfieldID field)
{
    jobject objLocal = (*vmi->env)->GetStaticObjectField(vmi->env, clazz, field);
    GLOBAL_REF(vmi, objLocal, obj)
    return obj;
}

const char*
getStaticStringField (vm_t* vmi, jclass clazz, jfieldID field)
{
    jobject string = (*vmi->env)->GetStaticObjectField(vmi->env, clazz, field);
    return jStringToCString(vmi, string);
}

jlong
getLongField (vm_t* vmi, jobject object, jfieldID field)
{
    return (*vmi->env)->GetLongField(vmi->env, object, field);
}

jint
getIntField (vm_t* vmi, jobject object, jfieldID field)
{
    return (*vmi->env)->GetIntField(vmi->env, object, field);
}

jshort
getShortField (vm_t* vmi, jobject object, jfieldID field)
{
    return (*vmi->env)->GetShortField(vmi->env, object, field);
}

jbyte
getByteField (vm_t* vmi, jobject object, jfieldID field)
{
    return (*vmi->env)->GetByteField(vmi->env, object, field);
}

jfloat
getFloatField (vm_t* vmi, jobject object, jfieldID field)
{
    return (*vmi->env)->GetFloatField(vmi->env, object, field);
}

jdouble
getDoubleField (vm_t* vmi, jobject object, jfieldID field)
{
    return (*vmi->env)->GetDoubleField(vmi->env, object, field);
}

jboolean
getBooleanField (vm_t* vmi, jobject object, jfieldID field)
{
    return (*vmi->env)->GetBooleanField(vmi->env, object, field);
}

jchar
getCharField (vm_t* vmi, jobject object, jfieldID field)
{
    return (*vmi->env)->GetCharField(vmi->env, object, field);
}

jobject
getObjectField (vm_t* vmi, jobject object, jfieldID field)
{
    jobject objLocal = (*vmi->env)->GetObjectField(vmi->env, object, field);
    GLOBAL_REF(vmi, objLocal, obj)
    return obj;
}

const char*
getStringField (vm_t* vmi, jobject object, jfieldID field)
{
    jobject string = (*vmi->env)->GetObjectField(vmi->env, object, field);
    return jStringToCString(vmi, string);
}

void
setStaticLongField (vm_t* vmi, jclass clazz, jfieldID field, jlong value)
{
    (*vmi->env)->SetStaticLongField(vmi->env, clazz, field, value);
}

void
setStaticIntField (vm_t* vmi, jclass clazz, jfieldID field, jint value)
{
    (*vmi->env)->SetStaticIntField(vmi->env, clazz, field, value);
}

void
setStaticShortField (vm_t* vmi, jclass clazz, jfieldID field, jshort value)
{
    (*vmi->env)->SetStaticShortField(vmi->env, clazz, field, value);
}

void
setStaticByteField (vm_t* vmi, jclass clazz, jfieldID field, jbyte value)
{
    (*vmi->env)->SetStaticByteField(vmi->env, clazz, field, value);
}

void
setStaticFloatField (vm_t* vmi, jclass clazz, jfieldID field, jfloat value)
{
    (*vmi->env)->SetStaticFloatField(vmi->env, clazz, field, value);
}

void
setStaticDoubleField (vm_t* vmi, jclass clazz, jfieldID field, jdouble value)
{
    (*vmi->env)->SetStaticDoubleField(vmi->env, clazz, field, value);
}

void
setStaticBooleanField (vm_t* vmi, jclass clazz, jfieldID field, jboolean value)
{
    (*vmi->env)->SetStaticBooleanField(vmi->env, clazz, field, value);
}

void
setStaticCharField (vm_t* vmi, jclass clazz, jfieldID field, jchar value)
{
    (*vmi->env)->SetStaticCharField(vmi->env, clazz, field, value);
}

void
setStaticObjectField (vm_t* vmi, jclass clazz, jfieldID field, jobject value)
{
    (*vmi->env)->SetStaticObjectField(vmi->env, clazz, field, value);
}

void
setStaticStringField (vm_t* vmi, jclass clazz, jfieldID field, const char* value)
{
    jstring string = (*vmi->env)->NewStringUTF(vmi->env, value);
    (*vmi->env)->SetStaticObjectField(vmi->env, clazz, field, string);
}

void
setLongField (vm_t* vmi, jobject object, jfieldID field, jlong value)
{
    (*vmi->env)->SetLongField(vmi->env, object, field, value);
}

void
setIntField (vm_t* vmi, jobject object, jfieldID field, jint value)
{
    (*vmi->env)->SetIntField(vmi->env, object, field, value);
}

void
setShortField (vm_t* vmi, jobject object, jfieldID field, jshort value)
{
    (*vmi->env)->SetShortField(vmi->env, object, field, value);
}

void
setByteField (vm_t* vmi, jobject object, jfieldID field, jbyte value)
{
    (*vmi->env)->SetByteField(vmi->env, object, field, value);
}

void
setFloatField (vm_t* vmi, jobject object, jfieldID field, jfloat value)
{
    (*vmi->env)->SetFloatField(vmi->env, object, field, value);
}

void
setDoubleField (vm_t* vmi, jobject object, jfieldID field, jdouble value)
{
    (*vmi->env)->SetDoubleField(vmi->env, object, field, value);
}

void
setBooleanField (vm_t* vmi, jobject object, jfieldID field, jboolean value)
{
    (*vmi->env)->SetBooleanField(vmi->env, object, field, value);
}

void
setCharField (vm_t* vmi, jobject object, jfieldID field, jchar value)
{
    (*vmi->env)->SetCharField(vmi->env, object, field, value);
}

void
setObjectField (vm_t* vmi, jobject object, jfieldID field, jobject value)
{
    (*vmi->env)->SetObjectField(vmi->env, object, field, value);
}

void
setStringField (vm_t* vmi, jobject object, jfieldID field, const char* value)
{
    jstring string = (*vmi->env)->NewStringUTF(vmi->env, value);
    (*vmi->env)->SetObjectField(vmi->env, object, field, string);
}

jvalue*
newJValues (int size)
{
    jvalue* arrayPointer = calloc (size, sizeof(jvalue));
    return arrayPointer;
}

void
setJValueBoolean (jvalue* array, int ix, jboolean val)
{
    (array + ix)->z = val;
}

void
setJValueInt (jvalue* array, int ix, jint val)
{
    (array + ix)->i = val;
}

void
setJValueLong (jvalue* array, int ix, jlong val)
{
    (array + ix)->j = val;
}

void
setJValueShort (jvalue* array, int ix, jshort val)
{
    (array + ix)->s = val;
}

void
setJValueByte (jvalue* array, int ix, jbyte val)
{
    (array + ix)->b = val;
}

void
setJValueFloat (jvalue* array, int ix, jfloat val)
{
    (array + ix)->f = val;
}

void
setJValueDouble (jvalue* array, int ix, jdouble val)
{
    (array + ix)->d = val;
}

void
setJValueChar (jvalue* array, int ix, jchar val)
{
    (array + ix)->c = val;
}

void
setJValueString (vm_t* vmi, jvalue* array, int ix, const char* string)
{
    jstring jstr = (*vmi->env)->NewStringUTF(vmi->env, string);
    (array + ix)->l = jstr;
}

void
setJValueObject (jvalue* array, int ix, jobject object)
{
    (array + ix)->l = object;
}

jobject
newJString (vm_t* vmi, const char* string)
{
    DEBUG1("newJString(\"%s\")", string)

    return (*vmi->env)->NewStringUTF(vmi->env, string);
}

jint
getArrayLength (vm_t* vmi, jobject array)
{
    jint length = (*vmi->env)->GetArrayLength(vmi->env, array);
    DEBUG2("getArrayLength(%p) -> %i", array, length)
    return length;
}

jclass
getObjectClass (vm_t* vmi, jobject object)
{
    DEBUG1("getObjectClass(%p)", object)

    jobject objLocal = (*vmi->env)->GetObjectClass(vmi->env, object);
    GLOBAL_REF(vmi, objLocal, obj)
    return obj;
}

jboolean
isInstanceOf (vm_t* vmi, jobject object, jclass clazz)
{
    DEBUG2("isInstanceOf(%p, %p)", object, clazz)

    return (*vmi->env)->IsInstanceOf(vmi->env, object, clazz);
}

const jchar*
charsFromJString (vm_t* vmi, jobject string)
{
    DEBUG1("charsFromString(%p)", string)
    const jchar* chars = (*vmi->env)->GetStringChars(vmi->env, string, NULL);
    DEBUG1("-> %p", chars)
    return chars;
}

const char*
bytesFromJString (vm_t* vmi, jobject string)
{
    DEBUG1("bytesFromString(%p)", string)
    const char* bytes = (*vmi->env)->GetStringUTFChars(vmi->env, string, NULL);
    DEBUG1("-> %p", bytes)
    return bytes;
}

void
releaseJChars (vm_t* vmi, jobject string, const jchar* chars)
{
    DEBUG2("releaseJChars(%p, %p)", string, chars)
    (*vmi->env)->ReleaseStringChars(vmi->env, string, chars);
}

void
releaseJBytes (vm_t* vmi, jobject string, const char* bytes)
{
    DEBUG2("releaseJBytes(%p, %p)", string, bytes)
    (*vmi->env)->ReleaseStringUTFChars(vmi->env, string, bytes);
}

const char*
jStringToCString (vm_t* vmi, jobject string)
{
    if (string == NULL) {
        return NULL;
    }
    jsize length = (*vmi->env)->GetStringUTFLength(vmi->env, string);
    const char* bytes = (*vmi->env)->GetStringUTFChars(vmi->env, string, NULL);
    char* cstring = malloc(length + 1);
    memcpy(cstring, bytes, length);
    cstring[length] = '\0';
    (*vmi->env)->ReleaseStringUTFChars(vmi->env, string, bytes);
    return cstring;
}

jboolean
exceptionCheck (vm_t* vmi)
{
    return (*vmi->env)->ExceptionCheck(vmi->env);
}

void
exceptionClear (vm_t* vmi)
{
    (*vmi->env)->ExceptionClear(vmi->env);
}

void
exceptionDescribe (vm_t* vmi)
{
    (*vmi->env)->ExceptionDescribe(vmi->env);
}

jthrowable
exceptionOccurred (vm_t* vmi)
{
    jthrowable exc = (*vmi->env)->ExceptionOccurred(vmi->env);
    DEBUG1("exceptionOccurred() -> %p", exc)
    return exc;
}

jthrowable
exceptionOccurredClear (vm_t* vmi)
{
    jthrowable excLocal = (*vmi->env)->ExceptionOccurred(vmi->env);
    DEBUG1("exceptionOccurredClear() -> %p", excLocal)
    if (excLocal) {
        (*vmi->env)->ExceptionClear(vmi->env);
        GLOBAL_REF(vmi, excLocal, exc)
        return exc;
    } else {
        return NULL;
    }
}

void
releaseJObjectRef (vm_t* vmi, jobject obj)
{
    (*vmi->env)->DeleteGlobalRef(vmi->env, obj);
}

void
releaseJClassRef (vm_t* vmi, jclass obj)
{
    (*vmi->env)->DeleteGlobalRef(vmi->env, obj);
}

void
releaseJThrowableRef (vm_t* vmi, jthrowable obj)
{
    (*vmi->env)->DeleteGlobalRef(vmi->env, obj);
}

void
release (jobject obj)
{
    JavaVM* jvm;
    jsize numCreatedVMs = 0;

    GET_CREATED_JAVA_VMS(&jvm, 1, &numCreatedVMs);
    if (numCreatedVMs == 1) {
        JNIEnv* env;
        if ((*jvm)->GetEnv(jvm, (void**) &env, FFIJNI_VERSION) == JNI_OK) {
            DEBUG1("release(%p)\n", (void*) obj)
            (*env)->DeleteGlobalRef(env, obj);
        }
    } else if (numCreatedVMs > 1) {
        fprintf(stderr, "THE IMPOSSIBLE HAPPENED - This should not have happend.\n"
                        "Somehow more than one JVM was created.\n"
                        "Please report this as a bug.\n");
        fflush(stderr);
    } else {
        DEBUG1("release(%p) - already shut down", (void*) obj)
        /* This is not an error, since the virtual machine
         * has already been teared down and no objects are
         * alive anymore - therefor nothing has to be freed
         * anymore (nor can it be).
         */
    }
}




