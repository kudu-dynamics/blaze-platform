// java.c
#include <jni.h>
#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>

#define JNI_VERSION JNI_VERSION_10

// Uninitialized Java natural interface
JavaVM *jvm = NULL;

// JClass for Clojure
jclass clojureClass, ifnClass, longClass, intClass, threadClass, objectClass, classLoaderClass;
jmethodID readM, varM, varQualM, // defined on 'clojure.java.api.Clojure'
  invoke[20],                   // defined on 'closure.lang.IFn'
  longValueM, longC,           // defined on 'java.lang.Long'
  intValueM, intC,
  toStringM,             // Object Class
  getThreadNameM, currentThreadM, getContextClassLoaderM, setContextClassLoaderM,
  getSystemClassLoaderM;


// Initialize the JVM with the Clojure JAR on classpath.
// returns True if new vm is created
bool create_vm() {
  JNIEnv *env;

  if (jvm == NULL) {
    // Configuration options for the JVM
    JavaVMOption opts[] = {
      {

        // CHANGEME link to Clojure and Spec JAR
        .optionString = "-Djava.class.path=res/clojure/clojure-1.11.1.jar:res/clojure/spec.alpha-0.3.218.jar",
      },
      {
        .optionString = "-Dclojure.spec.skip-macros=true"
      },
    };

    JavaVMInitArgs args = {
      .version = JNI_VERSION,
      .nOptions = sizeof(opts) / sizeof(opts[0]),
      .options = opts,
      .ignoreUnrecognized = false,
    };

    // Make the VM
    int rv = JNI_CreateJavaVM(&jvm, (void **)&env, &args);
    if (rv < 0 || !env) {
      printf("Unable to Launch JVM %d\n", rv);
      return false;
    }
    return true;
  }
  else {
    return false;
  }
}

void _print_version(JNIEnv *env) {
  printf("Env version: %d\n", (*env)->GetVersion(env));
}
  

/* creates global ref and deletes local ref */
jobject _newGlobalRef(JNIEnv *env, jobject jobj) {
  jobject r = (*env)->NewGlobalRef(env, jobj);
  (*env)->DeleteLocalRef(env, jobj);
  return r;
}

jboolean check_exception() {
  JNIEnv *env;
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  jboolean r = (*env)->ExceptionCheck(env);
  (*jvm)->DetachCurrentThread(jvm);
  return r;
}

void print_exception() {
  JNIEnv *env;
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  (*env)->ExceptionDescribe(env);
  (*jvm)->DetachCurrentThread(jvm);
}

void _check_exception(JNIEnv *env) {
  if ((*env)->ExceptionCheck(env)) {
    printf("There was an exception\n");
    (*env)->ExceptionDescribe(env);
  }
  return;
}

bool _is_null(JNIEnv *env, jobject obj) {
  return (*env)->IsSameObject(env, obj, NULL);
}

void _check_null(JNIEnv *env, char *objName, jobject obj) {
  if ((*env)->IsSameObject(env, obj, NULL)) {
    printf("%s is NULL\n", objName);
    _check_exception(env);
  } else {
    /* printf("Found %s\n", objName); */
  }
  return;
}

void _check_null_method(JNIEnv *env, char *objName, jmethodID mid) {
  if (mid == NULL) {
    printf("%s is NULL\n", objName);
    _check_exception(env);
  } else {
    printf("Found methodId %s (%d)\n", objName, mid);
  }
  return;
}


jobject _get_current_classloader (JNIEnv *env) {
  jobject current_thread = (*env)->CallStaticObjectMethod(env, threadClass, currentThreadM);
  
  jobject current_class_loader = (*env)->CallObjectMethod(env, current_thread, getContextClassLoaderM);

  return current_class_loader;
}

void _set_classloader(JNIEnv *env, jobject classLoader) {
  jobject current_thread = (*env)->CallStaticObjectMethod(env, threadClass, currentThreadM);
  _check_null(env, "set_classloader: current thread", current_thread);
  _check_null(env, "global classloader", classLoader);
  (*env)->CallObjectMethod(env, current_thread, setContextClassLoaderM, classLoader);
}


void _print_current_classloader(JNIEnv *env) {
  jobject thread = (*env)->CallStaticObjectMethod(env, threadClass, currentThreadM);
  jobject cloader = (*env)->CallObjectMethod(env, thread, getContextClassLoaderM);
  _check_null(env, "cloader", cloader);

  if ((*env)->IsSameObject(env, cloader, NULL)) {
    printf("Classloader was NULL. Setting to global classloader.\n");
    cloader = (*env)->CallStaticObjectMethod(env, classLoaderClass, getSystemClassLoaderM);
    printf("Got system loader\n");
    _set_classloader(env, cloader);
    printf("Set it\n");
  }

  jobject cname = (*env)->CallObjectMethod(env, cloader, toStringM);
  printf("momo 3\n");
  
  const char *c_str;
  c_str = (*env)->GetStringUTFChars(env, cname, NULL);
  if(c_str == NULL) {
    return;
  }
  printf("--------- Classloader: %s ----------\n", c_str);

  (*env)->ReleaseStringUTFChars(env, cname, c_str);
  return;
}

void _print_current_thread_name(JNIEnv *env) {
  jobject thread = (*env)->CallStaticObjectMethod(env, threadClass, currentThreadM);
  jobject tname = (*env)->CallObjectMethod(env, thread, getThreadNameM);
  
  const char *c_str;
  c_str = (*env)->GetStringUTFChars(env, tname, NULL);
  if(c_str == NULL) {
    return;
  }
  printf("+++++++++++++++++ Current Thread: %s ++++++++++++++++\n", c_str);

  (*env)->ReleaseStringUTFChars(env, tname, c_str);
  /* _print_current_classloader(env); */
  return;
}


// Lookup the classes and objects we need to interact with Clojure.
void load_methods() {
  JNIEnv *env;
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  jclass localClojureClass, localIfnClass, localLongClass, localIntClass;

  localClojureClass = (*env)->FindClass(env, "clojure/java/api/Clojure");
  clojureClass = (*env)->NewGlobalRef(env, localClojureClass);
  (*env)->DeleteLocalRef(env, localClojureClass);

  _check_null(env, "clojureClass", clojureClass);

  readM      = (*env)->GetStaticMethodID(env, clojureClass, "read", "(Ljava/lang/String;)Ljava/lang/Object;");
  _check_exception(env);
  
  varM       = (*env)->GetStaticMethodID(env, clojureClass, "var", "(Ljava/lang/Object;)Lclojure/lang/IFn;");

  // "(Ljava/lang/Object;Ljava/lang/Object;)Lclojure/lang/IFn;"
  varQualM   = (*env)->GetStaticMethodID(env, clojureClass, "var", "(Ljava/lang/Object;Ljava/lang/Object;)Lclojure/lang/IFn;");
  
  _check_null_method(env, "varQualM", varQualM);
  
  localIfnClass = (*env)->FindClass(env, "clojure/lang/IFn");
  ifnClass = (*env)->NewGlobalRef(env, localIfnClass);
  (*env)->DeleteLocalRef(env, localIfnClass);

  _check_null(env, "ifn", ifnClass);

  invoke[0]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "()Ljava/lang/Object;");
  invoke[1]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[2]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[3]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[4]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[5]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[6]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[7]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[8]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[9]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[10]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[11]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[12]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[13]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[14]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[15]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[16]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[17]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[18]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[19]  = (*env)->GetMethodID(env, ifnClass, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
  

  localLongClass = (*env)->FindClass(env, "java/lang/Long");
  longClass = (*env)->NewGlobalRef(env, localLongClass);
  (*env)->DeleteLocalRef(env, localLongClass);

  _check_null(env, "longClass", longClass);

  longValueM = (*env)->GetMethodID(env, longClass, "longValue", "()J");
  longC = (*env)->GetMethodID(env, longClass, "<init>", "(J)V");

  /* THREADS */
  jclass localThreadClass = (*env)->FindClass(env, "java/lang/Thread");
  threadClass = (*env)->NewGlobalRef(env, localThreadClass);
  (*env)->DeleteLocalRef(env, localThreadClass);

  _check_null(env, "threadClass", threadClass);

  currentThreadM = (*env)->GetStaticMethodID(env, threadClass, "currentThread", "()Ljava/lang/Thread;");
  
  _check_null_method(env, "currentThread", currentThreadM);

  getThreadNameM = (*env)->GetMethodID(env, threadClass, "getName", "()Ljava/lang/String;");
  getContextClassLoaderM = (*env)->GetMethodID(env, threadClass, "getContextClassLoader", "()Ljava/lang/ClassLoader;");
  _check_null_method(env, "getContextClassLoaderM", getContextClassLoaderM);

  setContextClassLoaderM = (*env)->GetMethodID(env, threadClass, "setContextClassLoader", "(Ljava/lang/ClassLoader;)V");
  _check_null_method(env, "setContextClassLoaderM", setContextClassLoaderM);
  
  jclass localObjectClass = (*env)->FindClass(env, "java/lang/Object");
  objectClass = (*env)->NewGlobalRef(env, localObjectClass);
  (*env)->DeleteLocalRef(env, localObjectClass);
  _check_null(env, "object", objectClass);
  
  toStringM = (*env)->GetMethodID(env, objectClass, "toString", "()Ljava/lang/String;");
  _check_null_method(env, "toString", toStringM);

  jclass localClassLoaderClass = (*env)->FindClass(env, "java/lang/ClassLoader");
  _check_null(env, "ClassLoader", localClassLoaderClass);
  classLoaderClass = (*env)->NewGlobalRef(env, localClassLoaderClass);
  (*env)->DeleteLocalRef(env, localClassLoaderClass);

  getSystemClassLoaderM = (*env)->GetStaticMethodID(env, classLoaderClass, "getSystemClassLoader", "()Ljava/lang/ClassLoader;");
  _check_null_method(env, "getSystemClassLoader", getSystemClassLoaderM);

  _print_current_thread_name(env);  
  (*jvm)->DetachCurrentThread(jvm);
}

/* use this to get the Env and prep the thread */
/* Call this at the start of any user-api facing function */
JNIEnv *getThreadEnv() {
  JNIEnv *env;

  if (create_vm()) {
    load_methods();
  }
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);

  printf("getThreadEnv: \n");
  _print_current_thread_name(env);

  jobject thread = (*env)->CallStaticObjectMethod(env, threadClass, currentThreadM);
  _check_null(env, "getThreadEnv thread", thread);
  
  jobject cloader = (*env)->CallObjectMethod(env, thread, getContextClassLoaderM);
  if ((*env)->IsSameObject(env, cloader, NULL)) {
    cloader = (*env)->CallStaticObjectMethod(env, classLoaderClass, getSystemClassLoaderM);
    _set_classloader(env, cloader);
  }
  return env;
}

/* call this at the end of a user facing api function */
void _cleanup() {
  jint r = (*jvm)->DetachCurrentThread(jvm);
  // TODO: do something with r
  return;
}

void deleteGlobalRefE(JNIEnv *env, jobject obj) {
  (*env)->DeleteGlobalRef(env, obj);
}

void deleteGlobalRef(jobject obj) {
  printf("DELETE global ref\n");
  deleteGlobalRefE(getThreadEnv(), obj);
}

// call the 'invoke' function of the right arity on 'IFn'.
jobject invokeFnE(JNIEnv *env, jobject obj, unsigned n, jobject *args) {
  return _newGlobalRef(env, (*env)->CallObjectMethodA(env, obj, invoke[n], (jvalue *)args));
}

jobject invokeFn(jobject obj, unsigned n, jobject *args) {
  return invokeFnE(getThreadEnv(), obj, n, args);
}

// 'read' static method from 'Clojure' object.
jobject readObjE(JNIEnv *env, const char *cStr) {
  jstring str = (*env)->NewStringUTF(env, cStr); // need to release later?
  return _newGlobalRef(env, (*env)->CallStaticObjectMethod(env, clojureClass, readM, str));
}

jobject readObj(const char *cStr) {
  return readObjE(getThreadEnv(), cStr);
}

// 'var' static method from 'Clojure' object.
jobject varObjE(JNIEnv *env, const char *fnCStr) {
  jstring fn = (*env)->NewStringUTF(env, fnCStr);
  return _newGlobalRef(env, (*env)->CallStaticObjectMethod(env, clojureClass, varM, fn));
}

jobject varObj(const char *fnCStr) {
  return varObjE(getThreadEnv(), fnCStr);
}

// qualified 'var' static method from 'Clojure' object.
jobject varObjQualifiedE(JNIEnv *env, const char *nsCStr, const char *fnCStr) {
  jstring ns = (*env)->NewStringUTF(env, nsCStr);
  jstring fn = (*env)->NewStringUTF(env, fnCStr);

  return _newGlobalRef(env, (*env)->CallStaticObjectMethod(env, clojureClass, varQualM, ns, fn));
}

jobject varObjQualified(const char *nsCStr, const char *fnCStr) {
  return varObjQualifiedE(getThreadEnv(), nsCStr, fnCStr); 
}
  
jobject newLongE(JNIEnv *env, long n) {
  return _newGlobalRef(env, (*env)->NewObject(env, longClass, longC, n));
}

jobject newLong(long n) {
  newLongE(getThreadEnv(), n);
}
  

long longValueE(JNIEnv *env, jobject n) {
  return (*env)->CallLongMethod(env, n, longValueM);
}

long longValue(jobject n) {
  return longValueE(getThreadEnv(), n);
}

const char *getStringUTFCharsE(JNIEnv *env, jstring str) {
  return (*env)->GetStringUTFChars(env, str, JNI_FALSE);
}

const char *getStringUTFChars(jstring str) {
  return getStringUTFCharsE(getThreadEnv(), str);
}
  
void releaseStringUTFCharsE(JNIEnv *env, jstring str, const char* chars) {
  (*env)->ReleaseStringUTFChars(env, str, chars);
}

void releaseStringUTFChars(jstring str, const char* chars) {
  return releaseStringUTFCharsE(getThreadEnv(), str, chars);
}

jstring toStringE(JNIEnv *env, jobject obj) {
  _print_current_thread_name(getThreadEnv());
  printf("Maybe the env is bad\n");
  _check_null(env, "toStringE obj", obj);
  return _newGlobalRef(env, (*env)->CallObjectMethod(env, obj, toStringM));
}

jstring toString(jobject obj) {
  return toStringE(getThreadEnv(), obj);
}

/* void main() { */
/*   long n; */
/*   jobject j; */
/*   jobject plusFunc; */
/*   jobject args[2]; */
/*   create_vm(); */
/*   printf("Created VM\n"); */
/*   /\* (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL); *\/ */
/*   load_methods(); */
/*   check_exception(); */
/*   printf("Loaded Methods VM\n"); */

/*   plusFunc = varObjQualified("clojure.core", "+"); */
/*   check_exception(); */
/*   /\* args[0] = newLong(88); *\/ */
/*   /\* args[1] = newLong(77); *\/ */
/*   /\* j = invokeFn(plusFunc, 2, args); *\/ */
/*   /\* check_exception(); *\/ */
/*   /\* n = longValue(j); *\/ */
/*   /\* printf("Long is %d\n", n); *\/ */
/* } */

