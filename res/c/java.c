// java.c
#include <jni.h>
#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>

// Uninitialized Java natural interface
JNIEnv *env;
JavaVM *jvm;

// JClass for Clojure
jclass clojure, ifn, longClass, intClass;
jmethodID readM, varM, varQualM, // defined on 'clojure.java.api.Clojure'
    invoke[2],                   // defined on 'closure.lang.IFn'
    longValueM, longC,           // defined on 'java.lang.Long'
    intValueM, intC;

/* #define GLOBAL_REF(LOCAL, GLOBAL) \ */
/*     jobject GLOBAL = (*env)->NewGlobalRef(env, LOCAL); \ */
/*     (*env)->DeleteLocalRef(env, LOCAL); \ */

// Initialize the JVM with the Clojure JAR on classpath.
bool create_vm() {
  if (env == NULL) {
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
      .version = JNI_VERSION_10,
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
  if ((*env)->ExceptionCheck(env)) {
    printf("There was an exception\n");
    (*env)->ExceptionDescribe(env);
    return;
  }
  return;
}

void check_null(char *objName, jobject obj) {
  if ((*env)->IsSameObject(env, obj, NULL)) {
    printf("%s is NULL\n", objName);
    check_exception();
  } else {
    printf("Found %s\n", objName);
    return;
  }
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



// Lookup the classes and objects we need to interact with Clojure.
void load_methods() {
  jclass localClojure, localIfn, localLongClass, localIntClass;
  
  localClojure = (*env)->FindClass(env, "clojure/java/api/Clojure");
  clojure = (*env)->NewGlobalRef(env, localClojure);
  
  check_null("clojure", clojure);

  readM      = (*env)->GetStaticMethodID(env, clojure, "read",
  "(Ljava/lang/String;)Ljava/lang/Object;");

  check_exception();
  
  varM       = (*env)->GetStaticMethodID(env, clojure, "var",
  "(Ljava/lang/Object;)Lclojure/lang/IFn;");

  // "(Ljava/lang/Object;Ljava/lang/Object;)Lclojure/lang/IFn;"
  varQualM   = (*env)->GetStaticMethodID(env, clojure, "var",
  "(Ljava/lang/Object;Ljava/lang/Object;)Lclojure/lang/IFn;");

  check_null2("varQualM", varQualM);
  
  localIfn = (*env)->FindClass(env, "clojure/lang/IFn");
  ifn = (*env)->NewGlobalRef(env, localIfn);
  
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
  
  check_null("longClass", longClass);

  longValueM = (*env)->GetMethodID(env, longClass, "longValue", "()J");
  longC = (*env)->GetMethodID(env, longClass, "<init>", "(J)V");
  /* longDecode = (*env)->GetMethodID(env, longClass, "decode", "(S)V"); */
}

jobject newGlobalRef(jobject jobj) {
  jobject r = (*env)->NewGlobalRef(env, jobj);
  (*env)->DeleteLocalRef(env, jobj);
  return r;
}

// call the 'invoke' function of the right arity on 'IFn'.
jobject invokeFn(jobject obj, unsigned n, jobject *args) {
  return newGlobalRef((*env)->CallObjectMethodA(env, obj, invoke[n], (jvalue *)args));
}

// 'read' static method from 'Clojure' object.
jobject readObj(const char *cStr) {
  jstring str = (*env)->NewStringUTF(env, cStr);
  return newGlobalRef((*env)->CallStaticObjectMethod(env, clojure, readM, str));
}

// 'var' static method from 'Clojure' object.
jobject varObj(const char *fnCStr) {
  jstring fn = (*env)->NewStringUTF(env, fnCStr);
  return newGlobalRef((*env)->CallStaticObjectMethod(env, clojure, varM, fn));
}

// qualified 'var' static method from 'Clojure' object.
jobject varObjQualified(const char *nsCStr, const char *fnCStr) {
  jobject r;
  /* jstring ns = (*env)->NewStringUTF(env, nsCStr); */
  /* jstring fn = (*env)->NewStringUTF(env, fnCStr); */
  jstring ns = (*env)->NewStringUTF(env, "clojure.core");
  jstring fn = (*env)->NewStringUTF(env, "+");

  printf("strings: %s %s\n", nsCStr, fnCStr);
  printf("strings': %s %s\n", ns, fn);


  check_exception();
  /* (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL); */
  /* printf("attached thread\n"); */

  check_null("clojure2", clojure);
  check_null2("varQualM2", varQualM);
  check_null2("varM", varM);

  r = (*env)->CallStaticObjectMethod(env, clojure, varQualM, ns, fn);
  /* r = (*env)->CallStaticObjectMethod(env, clojure, varM, fn); */
  printf("Oh well, charlie\n");
  check_exception();
  return newGlobalRef(r);
}

jobject newLong(long n) {
  return newGlobalRef((*env)->NewObject(env, longClass, longC, n));
}

long longValue(jobject n) {
  return (*env)->CallLongMethod(env, n, longValueM);
}

const char *getStringUTFChars(jstring str) {
  return (*env)->GetStringUTFChars(env, str, JNI_FALSE);
}

void releaseStringUTFChars(jstring str, const char* chars) {
  // why does this take two args instead of just chars?
  (*env)->ReleaseStringUTFChars(env, str, chars);
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

