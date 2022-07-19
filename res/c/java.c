// java.c
#include <jni.h>
#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>

#define JNI_VERSION JNI_VERSION_10

// Uninitialized Java natural interface
JavaVM *jvm = NULL;

// JClass for Clojure
jclass clojure, ifn, longClass, intClass, threadClass, objectClass;
jmethodID readM, varM, varQualM, // defined on 'clojure.java.api.Clojure'
  invoke[20],                   // defined on 'closure.lang.IFn'
  longValueM, longC,           // defined on 'java.lang.Long'
  intValueM, intC,
  toStringM,             // Object Class
  getThreadNameM, currentThreadM, getContextClassLoaderM, setContextClassLoaderM;

jobject clojureClassLoader = NULL;

/* #define GLOBAL_REF(LOCAL, GLOBAL) \ */
/*     jobject GLOBAL = (*env)->NewGlobalRef(env, LOCAL); \ */
/*     (*env)->DeleteLocalRef(env, LOCAL); \ */

// Initialize the JVM with the Clojure JAR on classpath.
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

void check_exception() {
  JNIEnv *env;
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */

  if ((*env)->ExceptionCheck(env)) {
    printf("There was an exception\n");
    (*env)->ExceptionDescribe(env);
    return;
  }
  /* (*jvm)->DetachCurrentThread(jvm); */
  return;
}

void check_null(char *objName, jobject obj) {
  JNIEnv *env;
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  if ((*env)->IsSameObject(env, obj, NULL)) {
    printf("%s is NULL\n", objName);
    check_exception();
  } else {
    printf("Found %s\n", objName);
  }
  /* (*jvm)->DetachCurrentThread(jvm); */
  return;
}

void check_null2(char *objName, jmethodID mid) {
  if (mid == NULL) {
    printf("%s is NULL\n", objName);
    check_exception();
  } else {
    printf("Found methodId %s (%d)\n", objName, mid);
  }
  return;
}

void check_globals() {
  check_null("clojure", clojure);
  check_null("ifn", ifn);
  check_null("longClass", longClass);
}

jobject get_current_classloader (JNIEnv *env) {
  jobject current_thread = (*env)->CallStaticObjectMethod(env, threadClass, currentThreadM);
  
  jobject current_class_loader = (*env)->CallObjectMethod(env, current_thread, getContextClassLoaderM);

  return current_class_loader;
}

void set_classloader(JNIEnv *env, jobject classLoader) {
  jobject current_thread = (*env)->CallStaticObjectMethod(env, threadClass, currentThreadM);
  (*env)->CallObjectMethod(env, current_thread, setContextClassLoaderM, classLoader);
}


void print_current_classloader(JNIEnv *env) {
  jobject thread = (*env)->CallStaticObjectMethod(env, threadClass, currentThreadM);
  printf("momo 1\n");
  jobject cloader = (*env)->CallObjectMethod(env, thread, getContextClassLoaderM);
  check_null("cloader", cloader);
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

void print_current_thread_name(JNIEnv *env) {
  jobject thread = (*env)->CallStaticObjectMethod(env, threadClass, currentThreadM);
  jobject tname = (*env)->CallObjectMethod(env, thread, getThreadNameM);

  const char *c_str;
  c_str = (*env)->GetStringUTFChars(env, tname, NULL);
  if(c_str == NULL) {
    return;
  }
  printf("+++++++++++++++++ Current Thread: %s ++++++++++++++++\n", c_str);

  (*env)->ReleaseStringUTFChars(env, tname, c_str);
  print_current_classloader(env);
  return;
}


// Lookup the classes and objects we need to interact with Clojure.
void load_methods() {
  JNIEnv *env;
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  jclass localClojure, localIfn, localLongClass, localIntClass;

  localClojure = (*env)->FindClass(env, "clojure/java/api/Clojure");
  clojure = (*env)->NewGlobalRef(env, localClojure);
  (*env)->DeleteLocalRef(env, localClojure);

  check_null("clojure", clojure);

  readM      = (*env)->GetStaticMethodID(env, clojure, "read",
  "(Ljava/lang/String;)Ljava/lang/Object;");

  check_exception();
  
  varM       = (*env)->GetStaticMethodID(env, clojure, "var", "(Ljava/lang/Object;)Lclojure/lang/IFn;");

  // "(Ljava/lang/Object;Ljava/lang/Object;)Lclojure/lang/IFn;"
  varQualM   = (*env)->GetStaticMethodID(env, clojure, "var", "(Ljava/lang/Object;Ljava/lang/Object;)Lclojure/lang/IFn;");
  
  check_null2("varQualM", varQualM);
  
  localIfn = (*env)->FindClass(env, "clojure/lang/IFn");
  ifn = (*env)->NewGlobalRef(env, localIfn);
  (*env)->DeleteLocalRef(env, localIfn);

  check_null("ifn", ifn);

  invoke[0]  = (*env)->GetMethodID(env, ifn, "invoke",
  "()Ljava/lang/Object;");
  invoke[1]  = (*env)->GetMethodID(env, ifn, "invoke",
  "(Ljava/lang/Object;)Ljava/lang/Object;");
  invoke[2]  = (*env)->GetMethodID(env, ifn, "invoke",
  "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");

  // Obviously we could keep going here. The Clojure API has 'invoke' for up to
  // 20 arguments...

  localLongClass = (*env)->FindClass(env, "java/lang/Long");
  longClass = (*env)->NewGlobalRef(env, localLongClass);
  (*env)->DeleteLocalRef(env, localLongClass);

  check_null("longClass", longClass);

  longValueM = (*env)->GetMethodID(env, longClass, "longValue", "()J");
  longC = (*env)->GetMethodID(env, longClass, "<init>", "(J)V");
  /* longDecode = (*env)->GetMethodID(env, longClass, "decode", "(S)V"); */
  /* (*jvm)->DetachCurrentThread(jvm); */

  /* THREADS */
  jclass localThreadClass = (*env)->FindClass(env, "java/lang/Thread");
  threadClass = (*env)->NewGlobalRef(env, localThreadClass);
  (*env)->DeleteLocalRef(env, localThreadClass);

  check_null("threadClass", threadClass);

  currentThreadM = (*env)->GetStaticMethodID(env, threadClass, "currentThread", "()Ljava/lang/Thread;");
  
  check_null2("currentThread", currentThreadM);

  getThreadNameM = (*env)->GetMethodID(env, threadClass, "getName", "()Ljava/lang/String;");
  getContextClassLoaderM = (*env)->GetMethodID(env, threadClass, "getContextClassLoader", "()Ljava/lang/ClassLoader;");
  check_null2("getContextClassLoaderM", getContextClassLoaderM);

  setContextClassLoaderM = (*env)->GetMethodID(env, threadClass, "setContextClassLoader", "(Ljava/lang/ClassLoader;)");

  jclass localObjectClass = (*env)->FindClass(env, "java/lang/Object");
  objectClass = (*env)->NewGlobalRef(env, localObjectClass);

  check_null("object", objectClass);
  
  toStringM = (*env)->GetMethodID(env, objectClass, "toString", "()Ljava/lang/String;");
  check_null2("toString", toStringM);
  
  print_current_thread_name(env);  
}


jobject newGlobalRef(jobject jobj) {
  JNIEnv *env;
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  jobject r = (*env)->NewGlobalRef(env, jobj);
  (*env)->DeleteLocalRef(env, jobj);
  /* (*jvm)->DetachCurrentThread(jvm); */
  return r;
}

void deleteGlobalRef(jobject obj) {
  JNIEnv *env;
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  (*env)->DeleteGlobalRef(env, obj);
  /* (*jvm)->DetachCurrentThread(jvm); */
}

// call the 'invoke' function of the right arity on 'IFn'.
jobject invokeFn(jobject obj, unsigned n, jobject *args) {
  JNIEnv *env;
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  print_current_thread_name(env);
  printf("Call invoke %d\n", n);
  jobject r = newGlobalRef((*env)->CallObjectMethodA(env, obj, invoke[n], (jvalue *)args));
  /* (*jvm)->DetachCurrentThread(jvm); */
  return r;
}

// 'read' static method from 'Clojure' object.
jobject readObj(const char *cStr) {
  JNIEnv *env;
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  print_current_thread_name(env);
  jstring str = (*env)->NewStringUTF(env, cStr);
  jmethodID localReadM = (*env)->GetStaticMethodID(env, clojure, "read",
  "(Ljava/lang/String;)Ljava/lang/Object;");
  printf("Call localReadM\n");
  jobject r = newGlobalRef((*env)->CallStaticObjectMethod(env, clojure, localReadM, str));
  /* (*jvm)->DetachCurrentThread(jvm); */
  return r;
}

void print_version() {
  JNIEnv *env;
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  printf("Env version: %d\n", (*env)->GetVersion(env));
  /* (*jvm)->DetachCurrentThread(jvm); */
}
  

// 'var' static method from 'Clojure' object.
jobject varObj(const char *fnCStr) {
  JNIEnv *env;
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  print_current_thread_name(env);
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  print_version();
  check_globals();
  /* (*env)->EnsureLocalCapacity(env,1); */
  check_exception();
  jstring fn = (*env)->NewStringUTF(env, fnCStr);
  jmethodID localVarM = (*env)->GetStaticMethodID(env, clojure, "var", "(Ljava/lang/Object;)Lclojure/lang/IFn;");
  
  printf("localVarM: %d, varM: %d\n", localVarM, varM);

  /* localVarQualM   = (*env)->GetStaticMethodID(env, clojure, "var", "(Ljava/lang/Object;Ljava/lang/Object;)Lclojure/lang/IFn;"); */

  printf("Call localVarM\n");
  jobject r = newGlobalRef((*env)->CallStaticObjectMethod(env, clojure, varM, fn));
  /* (*jvm)->DetachCurrentThread(jvm); */
  return r;
}

// qualified 'var' static method from 'Clojure' object.
jobject varObjQualified(const char *nsCStr, const char *fnCStr) {
  jobject r;
  JNIEnv *env;
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  print_current_thread_name(env);
  /* print_current_thread_name(env); */
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  printf("new strings\n");

  jstring ns = (*env)->NewStringUTF(env, nsCStr);
  jstring fn = (*env)->NewStringUTF(env, fnCStr);
  /* jstring ns = newGlobalRef((*env)->NewStringUTF(env, "clojure.core")); */
  /* jstring fn = newGlobalRef((*env)->NewStringUTF(env, "+")); */
  jmethodID localVarQualM;

  printf("strings: %s %s\n", nsCStr, fnCStr);
  printf("strings': %s %s\n", ns, fn);

  check_exception();
  /* (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL); */
  /* printf("attached thread\n"); */

  /* check_null("clojure2", clojure); */
  /* check_null2("varQualM2", varQualM); */
  /* check_null2("varM", varM); */
  localVarQualM   = (*env)->GetStaticMethodID(env, clojure, "var", "(Ljava/lang/Object;Ljava/lang/Object;)Lclojure/lang/IFn;");
  printf("nah uh\n");
  check_exception();
  printf("not me\n");

  printf("localVarQualM: 0x%x, varQualM: 0x%x\n", localVarQualM, varQualM);

  printf("Call varQualM\n");
  r = (*env)->CallStaticObjectMethod(env, clojure, localVarQualM, ns, fn);
  /* r = (*env)->CallStaticObjectMethod(env, clojure, varM, fn); */
  printf("Oh well, charlie\n");
  check_exception();
  jobject r2 = newGlobalRef(r);
  /* (*jvm)->DetachCurrentThread(jvm); */
  return r2;
}

jobject newLong(long n) {
  JNIEnv *env;
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  jobject r = newGlobalRef((*env)->NewObject(env, longClass, longC, n));
  /* (*jvm)->DetachCurrentThread(jvm); */
  return r;
}

long longValue(jobject n) {
  JNIEnv *env;
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  printf("Call longValueM\n");
  long r = (*env)->CallLongMethod(env, n, longValueM);
  /* (*jvm)->DetachCurrentThread(jvm); */
  return r;
}

const char *getStringUTFChars(jstring str) {
  JNIEnv *env;
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  const char *r = (*env)->GetStringUTFChars(env, str, JNI_FALSE);
  /* (*jvm)->DetachCurrentThread(jvm); */
  return r;
}

void releaseStringUTFChars(jstring str, const char* chars) {
  JNIEnv *env;
  /* (*jvm)->GetEnv(jvm, (void **)&env, JNI_VERSION); */
  (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
  // why does this take two args instead of just chars?
  (*env)->ReleaseStringUTFChars(env, str, chars);
  /* (*jvm)->DetachCurrentThread(jvm); */
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

