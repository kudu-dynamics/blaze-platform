#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// FILLER FUNCTIONS
// -------------------------------------------------------

// Function to calculate the sum of an array
int array_sum(int *array, int size) {
    int sum = 0;
    for (int i = 0; i < size; i++) {
        sum += array[i];
    }
    return sum;
}

// Function to reverse a string
void reverse_string(char *str) {
    int n = strlen(str);
    for (int i = 0; i < n / 2; i++) {
        char temp = str[i];
        str[i] = str[n - i - 1];
        str[n - i - 1] = temp;
    }
}

// Function to swap two integers
void swap_ints(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

// Function to find the factorial of a number
unsigned long long factorial(int n) {
    if (n == 0) return 1;
    return n * factorial(n - 1);
}

// Function to check if a number is prime
int is_prime(int n) {
    if (n <= 1) return 0;
    for (int i = 2; i <= n / 2; i++) {
        if (n % i == 0) return 0;
    }
    return 1;
}

// Function to find the greatest common divisor (GCD)
int gcd(int a, int b) {
    if (b == 0) return a;
    return gcd(b, a % b);
}

// Function to convert a character to uppercase
char to_uppercase(char c) {
    if (c >= 'a' && c <= 'z') {
        return c - 'a' + 'A';
    }
    return c;
}

// Function to convert a character to lowercase
char to_lowercase(char c) {
    if (c >= 'A' && c <= 'Z') {
        return c - 'A' + 'a';
    }
    return c;
}

// Function to find the maximum element in an array
int max_element(int *array, int size) {
    int max = array[0];
    for (int i = 1; i < size; i++) {
        if (array[i] > max) {
            max = array[i];
        }
    }
    return max;
}

// Function to find the minimum element in an array
int min_element(int *array, int size) {
    int min = array[0];
    for (int i = 1; i < size; i++) {
        if (array[i] < min) {
            min = array[i];
        }
    }
    return min;
}

// Function to calculate the length of a string
int string_length(const char *str) {
    int length = 0;
    while (str[length] != '\0') {
        length++;
    }
    return length;
}

// Function to concatenate two strings
void string_concat(char *dest, const char *src) {
    while (*dest) dest++;
    while ((*dest++ = *src++));
}

// Function to copy a string
void string_copy(char *dest, const char *src) {
    while ((*dest++ = *src++));
}

// Function to compare two strings
int string_compare(const char *str1, const char *str2) {
    while (*str1 && (*str1 == *str2)) {
        str1++;
        str2++;
    }
    return *(unsigned char *) str1 - *(unsigned char *) str2;
}

// Function to initialize an array with a value
void array_init(int *array, int size, int value) {
    for (int i = 0; i < size; i++) {
        array[i] = value;
    }
}

// Function to calculate the power of a number
double power(double base, int exponent) {
    double result = 1.0;
    for (int i = 0; i < exponent; i++) {
        result *= base;
    }
    return result;
}

// Function to find the maximum of two numbers
int max(int a, int b) {
    return (a > b) ? a : b;
}

// Function to find the minimum of two numbers
int min(int a, int b) {
    return (a < b) ? a : b;
}

// Function to check if a character is a digit
int is_digit(char c) {
    return (c >= '0' && c <= '9');
}

// Function to check if a character is an alphabetic letter
int is_alpha(char c) {
    return ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'));
}





// Input FUNCTIONS
// -------------------------------------------------------


char *read_file(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        perror("Error opening file");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    char *buffer = (char *) malloc(sizeof(char) * (file_size + 1));
    if (buffer == NULL) {
        perror("Error allocating memory");
        fclose(file);
        return NULL;
    }

    size_t read_size = fread(buffer, sizeof(char), file_size, file);
    if (read_size != file_size) {
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return NULL;
    }
    buffer[file_size] = '\0';

    fclose(file);
    return buffer;
}


char *read_stdin() {
    size_t buffer_size = 1024;
    char *buffer = (char *) malloc(buffer_size);
    if (buffer == NULL) {
        perror("Error allocating memory");
        return NULL;
    }

    size_t position = 0;
    int ch;
    printf("Enter input: ");
    while ((ch = getchar()) != EOF && ch != '\n') {
        if (position >= buffer_size - 1) {
            buffer_size *= 2;
            buffer = (char *) realloc(buffer, buffer_size);
            if (buffer == NULL) {
                perror("Error reallocating memory");
                return NULL;
            }
        }
        buffer[position++] = (char) ch;
    }
    buffer[position] = '\0';

    return buffer;
}


void read_stdin_into_buffer(char *buffer, size_t buffer_size) {
    if (buffer == NULL) {
        perror("Error: buffer is NULL");
        return;
    }

    size_t position = 0;
    int ch;
    printf("Enter input: ");

    while ((ch = getchar()) != EOF && ch != '\n') {
        if (position >= buffer_size - 1) {
            fprintf(stderr, "Error: buffer overflow\n");
            return;
        }
        buffer[position++] = (char) ch;
    }
    buffer[position] = '\0';
}


void read_file_into_buffer(const char *filename, char *buffer, size_t buffer_size) {
    if (buffer == NULL) {
        perror("Error: buffer is NULL");
        return;
    }

    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        perror("Error opening file");
        return;
    }

    size_t position = 0;
    int ch;
    while ((ch = fgetc(file)) != EOF) {
        if (position >= buffer_size - 1) {
            fprintf(stderr, "Error: buffer overflow\n");
            fclose(file);
            return;
        }
        buffer[position++] = (char) ch;
    }
    buffer[position] = '\0';

    fclose(file);
}


// Vulnerable FUNCTIONS
// -------------------------------------------------------

void buffer_overflow(char *buffer) {
    char local_buffer[10];
    strcpy(local_buffer, buffer); // Unsafe copy, can overflow
}

void format_string_vulnerability(char *buffer) {
    printf(buffer); // Uncontrolled format string
}

void integer_overflow(char *buffer) {
    int len = atoi(buffer); // Read length from buffer
    int total_size = len + 2147483647; // Potential overflow if len is positive
    char *large_buffer = (char *) malloc(total_size);
    if (large_buffer != NULL) {
        strncpy(large_buffer, buffer, total_size);
        free(large_buffer);
    }
}

void use_after_free(char *buffer) {
    char *temp = strdup(buffer);
    free(temp);
    strcpy(temp, buffer); // Using memory after it's freed
}



void null_pointer_dereference(char *buffer) {
    char *temp = NULL;
    strcpy(temp, buffer); // Dereferencing a NULL pointer
}

void stack_based_buffer_overflow(char *buffer) {
    char local_buffer[10];
    memcpy(local_buffer, buffer, strlen(buffer)); // Unsafe copy, can overflow
}

void heap_based_buffer_overflow(char *buffer) {
    char *heap_buffer = (char *) malloc(10);
    if (heap_buffer != NULL) {
        strcpy(heap_buffer, buffer); // Unsafe copy, can overflow
        free(heap_buffer);
    }
}


void buffer_overread(char *buffer) {
    int len = strlen(buffer) + 10; // Intentional over-read
    printf("%s", buffer + len); // Reading beyond buffer
}

void double_free(char *buffer) {
    char *temp = strdup(buffer);
    free(temp);
    free(temp); // Double free
}

void uninitialized_variable(char *buffer) {
    char local_buffer[100];
    printf("%s", local_buffer); // Uninitialized read
}

void memory_leak(char *buffer) {
    char *temp = (char *) malloc(10);
    strcpy(temp, buffer); // Memory is never freed
}


void off_by_one_error(char *buffer) {
    char local_buffer[10];
    for (int i = 0; i <= 10; i++) { // Off-by-one error
        local_buffer[i] = buffer[i];
    }
}

void command_injection(char *buffer) {
    char command[100];
    sprintf(command, "ping -c 3 %s", buffer); // Command injection vulnerability
    system(command);
}


void integer_truncation(char *buffer) {
    long large_number = atol(buffer);
    int truncated = (int) large_number; // Potential data loss
    printf("%d\n", truncated);
}

void improper_null_termination(char *buffer) {
    char local_buffer[10];
    strncpy(local_buffer, buffer, 10); // Potentially missing null termination
    printf("%s\n", local_buffer);
}

void path_traversal(char *buffer) {
    char path[100] = "/safe/dir/";
    strcat(path, buffer); // Path traversal if buffer contains ../
    FILE *file = fopen(path, "r");
    if (file != NULL) {
        fclose(file);
    }
}


void out_of_bounds_access(char *buffer) {
    char array[5] = {0, 1, 2, 3, 4};
    int index = atoi(buffer); // User controls index
    int value = array[index]; // Out-of-bounds access possible
    printf("Value: %d\n", value);
}

//void untrusted_pointers(char *buffer) {
//    int (*func_ptr)();
//    func_ptr = (int (*)()) buffer; // Untrusted pointer from user input
//    func_ptr(); // Could lead to arbitrary code execution
//}




//read_file
//
//read_stdin
//
//read_stdin_into_buffer
//
//read_file_into_buffer



void call_buffer_overflows(char *a_buff){
    buffer_overflow("Words");
    buffer_overflow(a_buff);
    buffer_overflow("Hi");
    char *a_buff2 = read_stdin();
    buffer_overflow(a_buff2);
    buffer_overflow("Hello");
}

void call_call_buffer_overflows(){
    char *a_buff2 = read_stdin() ;
    call_buffer_overflows(a_buff2);
}

void call_format_string_vulnerability(char *a_buff){
    format_string_vulnerability("Words");
    format_string_vulnerability(a_buff);
    format_string_vulnerability("Hi");
    char *a_buff2 = read_stdin();
    format_string_vulnerability(a_buff2);
    format_string_vulnerability("Hello");
}

void call_call_format_string_vulnerability(){
    char *a_buff2 = read_stdin() ;
    call_format_string_vulnerability(a_buff2);
}

void call_integer_overflow(char *a_buff){
    integer_overflow("0");
    integer_overflow(a_buff);
    integer_overflow("-1");
    printf("Enter Filename: ");
    char *a_buff2 = read_stdin();
    char *a_buff3 = read_file(a_buff2);
    integer_overflow(a_buff3);
}

void call_call_integer_overflow() {
    char *a_buff2 = read_stdin();
    call_integer_overflow(a_buff2);
}

void call_use_after_free(char *a_buff){
    use_after_free("Words");
    use_after_free(a_buff);
    char *a_buff2 = read_stdin();
    use_after_free(a_buff2);
}

void call_call_use_after_free(){
    char *a_buff2 = read_stdin() ;
    call_use_after_free(a_buff2);
}

void call_null_pointer_dereference(char *a_buff){
    null_pointer_dereference("Words");
    null_pointer_dereference(a_buff);
    char *a_buff2 = read_stdin();
    null_pointer_dereference(a_buff2);
}

void call_call_null_pointer_dereference(){
    char *a_buff2 = read_stdin() ;
    call_null_pointer_dereference(a_buff2);
}


void call_stack_based_buffer_overflow(char *a_buff){
    stack_based_buffer_overflow("Words");
    stack_based_buffer_overflow(a_buff);
    stack_based_buffer_overflow("Hi");
    char *a_buff2 = read_stdin();
    stack_based_buffer_overflow(a_buff2);
    stack_based_buffer_overflow("Hello");
}

void call_call_stack_based_buffer_overflow(){
    char *a_buff2 = read_stdin() ;
    call_stack_based_buffer_overflow(a_buff2);
}

void call_heap_based_buffer_overflow(char *a_buff){
    heap_based_buffer_overflow("Words");
    heap_based_buffer_overflow(a_buff);
    heap_based_buffer_overflow("Hi");
    char *a_buff2 = read_stdin();
    heap_based_buffer_overflow(a_buff2);
    heap_based_buffer_overflow("Hello");
}

void call_call_heap_based_buffer_overflow(){
    char *a_buff2 = read_stdin() ;
    call_heap_based_buffer_overflow(a_buff2);
}

void call_buffer_overread(char *a_buff){
    buffer_overread("Words");
    buffer_overread(a_buff);
    buffer_overread("Hi");
    char *a_buff2 = read_stdin();
    buffer_overread(a_buff2);
    buffer_overread("Hello");
}

void call_call_buffer_overread(){
    char *a_buff2 = read_stdin() ;
    call_buffer_overread(a_buff2);
}

void call_double_free(char *a_buff){
    double_free("Words");
    double_free(a_buff);
    char *a_buff2 = read_stdin();
    double_free(a_buff2);
}

void call_call_double_free(){
    char *a_buff2 = read_stdin() ;
    call_double_free(a_buff2);
}

void call_uninitialized_variable(char *a_buff){
    uninitialized_variable("Words");
    uninitialized_variable(a_buff);
    char *a_buff2 = read_stdin();
    uninitialized_variable(a_buff2);
}

void call_call_uninitialized_variable(){
    char *a_buff2 = read_stdin() ;
    call_uninitialized_variable(a_buff2);
}

void call_memory_leak(char *a_buff){
    memory_leak("Words");
    memory_leak(a_buff);
    char *a_buff2 = read_stdin();
    memory_leak(a_buff2);
}

void call_call_memory_leak(){
    char *a_buff2 = read_stdin() ;
    call_memory_leak(a_buff2);
}

void call_off_by_one_error(char *a_buff){
    off_by_one_error("Words");
    off_by_one_error(a_buff);
    off_by_one_error("Hi");
    char *a_buff2 = read_stdin();
    off_by_one_error(a_buff2);
    off_by_one_error("Hello");
}

void call_call_off_by_one_error(){
    char *a_buff2 = read_stdin() ;
    off_by_one_error(a_buff2);
}

void call_command_injection(char *a_buff){
    command_injection("Words");
    command_injection(a_buff);
    char *a_buff2 = read_stdin();
    command_injection(a_buff2);
}

void call_call_command_injection(){
    char *a_buff2 = read_stdin() ;
    call_command_injection(a_buff2);
}

void call_integer_truncation(char *a_buff){
    integer_truncation("Words");
    integer_truncation(a_buff);
    char *a_buff2 = read_stdin();
    integer_truncation(a_buff2);
}

void call_call_integer_truncation(){
    char *a_buff2 = read_stdin() ;
    call_integer_truncation(a_buff2);
}

void call_improper_null_termination(char *a_buff){
    improper_null_termination("Words");
    improper_null_termination(a_buff);
    char *a_buff2 = read_stdin();
    improper_null_termination(a_buff2);
}

void call_call_improper_null_termination(){
    char *a_buff2 = read_stdin() ;
    call_improper_null_termination(a_buff2);
}

void call_path_traversal(char *a_buff){
    path_traversal("Words");
    path_traversal(a_buff);
    char *a_buff2 = read_stdin();
    path_traversal(a_buff2);
}

void call_call_path_traversal(){
    char *a_buff2 = read_stdin() ;
    call_path_traversal(a_buff2);
}

void call_out_of_bounds_access(char *a_buff){
    out_of_bounds_access("Words");
    out_of_bounds_access(a_buff);
    char *a_buff2 = read_stdin();
    out_of_bounds_access(a_buff2);
}

void call_call_out_of_bounds_access(){
    char *a_buff2 = read_stdin() ;
    call_out_of_bounds_access(a_buff2);
}


int main(void) {
    printf("Hello, World!\n");
    call_call_buffer_overflows();
    call_call_format_string_vulnerability();
    call_call_integer_overflow();
    call_call_use_after_free();
    call_call_null_pointer_dereference();
    call_call_stack_based_buffer_overflow();
    call_call_heap_based_buffer_overflow();
    call_call_buffer_overread();
    call_call_double_free();
    call_call_uninitialized_variable();
    call_call_memory_leak();
    call_call_off_by_one_error();
    call_call_integer_truncation();
    call_call_improper_null_termination();
    call_call_path_traversal();
    call_call_out_of_bounds_access();
    return 0;
}
