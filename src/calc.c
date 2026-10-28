/*
 * Calculator program
 * Tasks:
 * 1. Memory and values
 * 2. Lexer (tokens)
 * 3. Parser and calculations
 * 4. File input/output
 * 5. Directory processing
 * 6. Command-line arguments
 */
#define _POSIX_C_SOURCE 200809L
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
/* ============================================================
   TASK 0: STUDENT INFORMATION
   ============================================================ */
#define STUDENT_NAME     "azizjon"
#define STUDENT_LASTNAME "rakhimjonov"
#define STUDENT_ID       "251ADB090"
#define INT_MAX_VALUE  9223372036854775807LL
#define INT_MIN_VALUE  (-INT_MAX_VALUE - 1)
/* ============================================================
   TASK 1: MEMORY, BUFFER AND VALUES
   ============================================================ */
typedef struct {
    char *data;
    size_t len, cap;
} Buffer;

static void *allocate(size_t n){
    void *p = malloc(n ? n : 1);
    if(!p) {
        fprintf(stderr, "calc: out of memory\n");
        exit(1);
    }
    return p;
}

static void append(Buffer *b, const char *s, size_t n){
    if(b->len + n + 1 > b->cap) {
        size_t cap = b->cap ? b->cap : 64;
        while (cap < b->len + n + 1){
            cap *= 2;
        }
        char *p = allocate(cap);
        if(b->len){
            memcpy(p, b->data, b->len);
        }
        free(b->data);
        b->data = p;
        b->cap = cap;
    }
    memcpy(b->data + b->len, s, n);
    b->len += n;
    b->data[b->len] = '\0';
}
typedef struct {
    int is_float;
    long long i;
    double f;
} Value;
static Value integer(long long n){
    return (Value){0, n, 0};
}
static Value decimal(double n){
    return (Value){1, 0, n};
}
static double number(Value v){
    return v.is_float ? v.f : (double)v.i;
}

/* ============================================================
   TASK 2: LEXER
   Recognizes numbers, operators and parentheses.
   ============================================================ */

enum {
    END, INT, FLOAT, PLUS, MINUS, MUL, DIV, POWER, LP, RP
};

typedef struct {
    const char *s;
    size_t pos, end, token_pos, error;
    int token;
    long long iv;
    double fv;
} Parser;

static void fail(Parser *p, size_t pos){
    if(!p->error){
        p->error = pos + 1;}
}

static void next(Parser *p){
    while (p->pos < p->end &&
           (p->s[p->pos] == ' ' || p->s[p->pos] == '\t'))
        p->pos++;

    p->token_pos = p->pos;
    p->token = END;

    if(p->pos >= p->end){
        return;}

    size_t start = p->pos;
    char c = p->s[p->pos];

    if(isdigit((unsigned char)c)) {
        while (p->pos < p->end && isdigit((unsigned char)p->s[p->pos])){
            p->pos++;}

        if(p->pos < p->end && p->s[p->pos] == '.') {
            p->pos++;

            if(p->pos >= p->end || !isdigit((unsigned char)p->s[p->pos])) {
                fail(p, p->pos - 1);
                p->pos = p->end;
                return;
            }

            while (p->pos < p->end && isdigit((unsigned char)p->s[p->pos])){
                p->pos++;}

            char *endptr;
            p->fv = strtod(p->s + start, &endptr);
            p->token = FLOAT;
        } else {
            p->iv = 0;

            for (size_t i = start; i < p->pos; i++) {
                int digit = p->s[i] - '0';

                if(p->iv > (INT_MAX_VALUE - digit) / 10) {
                    fail(p, start);
                    p->pos = p->end;
                    return;
                }
                p->iv = p->iv * 10 + digit;
            }
            p->token = INT;
        }
        return;
    }
    p->pos++;
    switch (c) {
    case '+': p->token = PLUS; break;
    case '-': p->token = MINUS; break;
    case '/': p->token = DIV; break;
    case '(': p->token = LP; break;
    case ')': p->token = RP; break;
    case '*':
        if(p->pos < p->end && p->s[p->pos] == '*') {
            p->pos++;
            p->token = POWER;
        } else {
            p->token = MUL;
        }
        break;
    default:
        fail(p, start);
        p->pos = p->end;
    }
}

/* ============================================================
   TASK 3: PARSER AND CALCULATOR
   Priority: parentheses, power, multiplication/division,
   addition/subtraction.
   ============================================================ */

static Value expression(Parser *p);

static Value primary(Parser *p){
    Value v;
    if(p->token == INT) {
        v = integer(p->iv);
        next(p);
        return v;
    }
    if(p->token == FLOAT) {
        v = decimal(p->fv);
        next(p);
        return v;
    }
    if(p->token == LP) {
        next(p);
        v = expression(p);
        if(!p->error && p->token != RP){
            fail(p, p->token_pos);}
        else if(!p->error){
            next(p);}
        return v;
    }
    fail(p, p->token_pos);
    return integer(0);
}
static unsigned long long int_power(long long base, long long exponent){
    unsigned long long result = 1;
    unsigned long long factor = (unsigned long long)base;

    while (exponent > 0) {
        if(exponent % 2){
            result *= factor;}
        factor *= factor;
        exponent /= 2;
    }
    return result;
}

static Value calc(Parser *p, Value a, Value b, int op, size_t op_pos, size_t right_pos){
    if(!a.is_float && !b.is_float) {
        switch (op) {
        case PLUS: return integer(a.i + b.i);
        case MINUS: return integer(a.i - b.i);
        case MUL: return integer(a.i * b.i);

        case DIV:
            if(!b.i || (a.i == INT_MIN_VALUE && b.i == -1)) {
                fail(p, right_pos);
                return integer(0);
            }
            return integer(a.i / b.i);
        case POWER:
            if(b.i < 0) {
                fail(p, right_pos);
                return integer(0);
            }
            return integer((long long)int_power(a.i, b.i));
        }
    }

    double x = number(a), y = number(b), r = 0;

    switch (op) {
    case PLUS:  r = x + y; break;
    case MINUS: r = x - y; break;
    case MUL:   r = x * y; break;

    case DIV:
        if(y == 0) {
            fail(p, right_pos);
            return decimal(0);
        }
        r = x / y;
        break;

    case POWER:
        r = pow(x, y);
        break;
    }

    if(!isfinite(r)) {
        fail(p, op_pos);
        return decimal(0);
    }
    return decimal(r);
}

typedef struct {
    Value value;
    size_t op_pos;
    size_t right_pos;
} PowerItem;

static Value power(Parser *p){
    Value a = primary(p);

    if(p->error || p->token != POWER){
        return a;}
    size_t count = 1, cap = 8;
    PowerItem *items = allocate(cap * sizeof *items);

    items[0].value = a;
    items[0].op_pos = 0;
    items[0].right_pos = 0;

    while (!p->error && p->token == POWER) {
        size_t op_pos = p->token_pos;
        next(p);

        size_t right_pos = p->token_pos;
        Value b = primary(p);

        if(p->error){
            break;}

        if(count == cap) {
            cap *= 2;
            PowerItem *bigger = allocate(cap * sizeof *items);
            memcpy(bigger, items, count * sizeof *items);
            free(items);
            items = bigger;
        }

        items[count].value = b;
        items[count].op_pos = op_pos;
        items[count].right_pos = right_pos;
        count++;
    }

    Value result = items[count - 1].value;

    for (size_t i = count - 1; i > 0 && !p->error; i--){
        result = calc(p, items[i - 1].value, result, POWER, items[i].op_pos, items[i].right_pos);}

    free(items);
    if(p->error){
        return integer(0);}

    return result;
}

static Value term(Parser *p){
    Value a = power(p);

    while (!p->error && (p->token == MUL || p->token == DIV)) {
        int op = p->token;
        size_t op_pos = p->token_pos;

        next(p);
        size_t right_pos = p->token_pos;
        Value b = power(p);

        if(p->error){
            break;}

        a = calc(p, a, b, op, op_pos, right_pos);
    }

    return a;
}

static Value expression(Parser *p){
    Value a = term(p);

    while (!p->error && (p->token == PLUS || p->token == MINUS)) {
        int op = p->token;
        size_t op_pos = p->token_pos;

        next(p);
        size_t right_pos = p->token_pos;
        Value b = term(p);

        if(p->error){
            break;}

        a = calc(p, a, b, op, op_pos, right_pos);
    }
    return a;
}

static size_t evaluate(const char *s, size_t len, Buffer *out){
    size_t start = 0;

    while (start < len) {
        size_t end = start;
        while (end < len && s[end] != '\n'){
            end++;
        }
        size_t first = start;

        while (first < end && (s[first] == ' ' || s[first] == '\t')){
            first++;
        }
        if(first < end && s[first] != '#') {
            Parser p = {0};
            p.s = s;
            p.pos = first;
            p.end = end;

            next(&p);
            Value v = expression(&p);

            if(!p.error && p.token != END){
                fail(&p, p.token_pos);
            }
            if(p.error){
                return p.error;
            }
            char result[64];
            int n;

            if(v.is_float){
                n = snprintf(result, sizeof result, "%.15g\n", v.f);
            } else {
                n = snprintf(result, sizeof result, "%lld\n", v.i);
            }
            append(out, result, (size_t)n);
        }
        start = end + 1;
    }
    return 0;
}

/* ============================================================
   TASK 4: FILE INPUT AND OUTPUT
   ============================================================ */

static int is_dir(const char *path){
    struct stat st;
    return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}

static int is_file(const char *path){
    struct stat st;
    return stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

static char *join(const char *a, const char *b){
    size_t n = strlen(a) + strlen(b) + 2;
    char *s = allocate(n);
    snprintf(s, n, "%s/%s", a, b);
    return s;
}

static char *last_part(const char *path){
    size_t end = strlen(path);

    while (end > 1 && path[end - 1] == '/'){
        end--;}

    size_t start = end;

    while (start > 0 && path[start - 1] != '/'){
        start--;}

    char *name = allocate(end - start + 1);
    memcpy(name, path + start, end - start);
    name[end - start] = '\0';
    return name;
}

/* Name of an input file without folder and without ".txt". */
static char *base_name(const char *path){
    char *name = last_part(path);
    size_t n = strlen(name);

    if(n >= 4 && strcmp(name + n - 4, ".txt") == 0){
        name[n - 4] = '\0';}

    return name;
}

/* Name of a folder. For "." and ".." we ask the system for the real name. */
static char *folder_name(const char *path){
    char *name = last_part(path);

    if(!strcmp(name, ".") || !strcmp(name, "..")) {
        char *full = realpath(path, NULL);

        if(full) {
            free(name);
            name = last_part(full);
            free(full);
        }
    }
    return name;
}

static int read_file(const char *path, Buffer *b){
    FILE *f = fopen(path, "rb");

    if(!f) {
        fprintf(stderr, "calc: cannot open '%s': %s\n",
                path, strerror(errno));
        return 1;
    }

    char chunk[4096];
    size_t n;

    while ((n = fread(chunk, 1, sizeof chunk, f)) > 0)
        append(b, chunk, n);
    int error = ferror(f);
    fclose(f);
    if (error) {
        fprintf(stderr, "calc: error reading '%s'\n", path);
        return 1;
    }
    return 0;
}

static int write_file(const char *path, const Buffer *b){
    FILE *f = fopen(path, "wb");
    if(!f) {
        fprintf(stderr, "calc: cannot write '%s': %s\n",
                path, strerror(errno));
        return 1;
    }
    int error = b->len > 0 && fwrite(b->data, 1, b->len, f) != b->len;
    if(fclose(f) != 0){
        error = 1;}
    if(error){
        fprintf(stderr, "calc: error writing '%s'\n", path);}
    return error;
}

static int make_dir(const char *path){

    if(is_dir(path)){
        return 0;}

    if(mkdir(path, 0777) == 0){
        return 0;}

    fprintf(stderr, "calc: cannot create folder '%s': %s\n",
            path, strerror(errno));
    return 1;
}

static int process_file(const char *input, const char *output){

    Buffer in = {0}, out = {0};

    if(!is_file(input)) {
        fprintf(stderr, "calc: '%s' is not a file\n", input);
        return 1;
    }
    if(read_file(input, &in)) {
        free(in.data);
        return 1;
    }

    size_t error = evaluate(in.data, in.len, &out);
    if(error) {
        out.len = 0;
        char message[64];
        int n = snprintf(message, sizeof message, "ERROR:%zu\n", error);
        append(&out, message, (size_t)n);
    }

    char *base = base_name(input);
    size_t n = strlen(base) + strlen(STUDENT_NAME) + strlen(STUDENT_LASTNAME) + strlen(STUDENT_ID) + 8;

    char *name = allocate(n);
    snprintf(name, n, "%s_%s_%s_%s.txt", base, STUDENT_NAME, STUDENT_LASTNAME, STUDENT_ID);

    char *path = join(output, name);
    int failed = write_file(path, &out);

    free(path);
    free(name);
    free(base);
    free(in.data);
    free(out.data);

    return failed;
}

/* ============================================================
   TASK 5: DIRECTORY PROCESSING
   ============================================================ */

static int compare(const void *a, const void *b){
    return strcmp(*(char *const *)a, *(char *const *)b);
}

static int process_dir(const char *dir, const char *output){
    DIR *d = opendir(dir);

    if(!d) {
        fprintf(stderr, "calc: cannot open folder '%s': %s\n",
                dir, strerror(errno));
        return 1;
    }
    char **files = NULL;
    size_t count = 0, cap = 0;
    struct dirent *entry;

    while ((entry = readdir(d))) {
        size_t n = strlen(entry->d_name);

        if(n <= 4 || strcmp(entry->d_name + n - 4, ".txt")){
            continue;}
        char *path = join(dir, entry->d_name);

        if(!is_file(path)) {
            free(path);
            continue;
        }

        if(count == cap) {
            cap = cap ? cap * 2 : 16;
            char **tmp = allocate(cap * sizeof *files);

            if(count){
                memcpy(tmp, files, count * sizeof *files);}

            free(files);
            files = tmp;
        }
        files[count++] = path;
    }

    closedir(d);
    if(count > 1){
        qsort(files, count, sizeof *files, compare);}

    int failed = make_dir(output);

    if(!failed) {
        for (size_t i = 0; i < count; i++){
            if (process_file(files[i], output)){
                failed = 1;
            }
        }
    }

    for (size_t i = 0; i < count; i++){
        free(files[i]);}

    free(files);
    return failed;
}

/* ============================================================
   TASK 6: MAIN AND COMMAND-LINE ARGUMENTS
   ============================================================ */

static void usage(void){
    fprintf(stderr,
        "Usage: calc [-d DIR | --dir DIR] "
        "[-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
}

int main(int argc, char **argv){
    const char *input = NULL, *dir = NULL, *output = NULL;

    for (int i = 1; i < argc; i++) {
        if(!strcmp(argv[i], "-d") || !strcmp(argv[i], "--dir")) {
            if(++i >= argc || dir) {
                usage();
                return 2;
            }
            dir = argv[i];
        } else if(!strcmp(argv[i], "-o") || !strcmp(argv[i], "--output-dir")) {
            if(++i >= argc || output) {
                usage();
                return 2;
            }
            output = argv[i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "calc: unknown option '%s'\n", argv[i]);
            usage();
            return 2;
        } else if (!input) {
            input = argv[i];
        } else {
            usage();
            return 2;
        }
    }

    if((!dir && !input) || (dir && input)) {
        usage();
        return 2;
    }
    char *default_output = NULL;
    if(!output) {
        char *base = dir ? folder_name(dir) : base_name(input);

        size_t n = strlen(base) + strlen(STUDENT_NAME) +
                   strlen(STUDENT_ID) + 4;

        default_output = allocate(n);
        snprintf(default_output, n, "%s_%s_%s",
                 base, STUDENT_NAME, STUDENT_ID);

        free(base);
        output = default_output;
    }

    int failed;

    if (dir) {
        failed = process_dir(dir, output);
    } else if (!is_file(input)) {
        fprintf(stderr, "calc: '%s' is not an existing file\n", input);
        failed = 1;
    } else if (make_dir(output)) {
        failed = 1;
    } else {
        failed = process_file(input, output);
    }
    free(default_output);
    return failed ? 1 : 0;
}