#include <stdlib.h>
#include <stdbool.h>
#include <string.h>



typedef struct var_node var_node;
typedef struct var_node {
   void* var;
   var_node* next;
} var_node;

typedef struct environnement {
    var_node* head;
} environnement;

typedef struct {
    void* (*function) (void*);
    environnement *env;
} closure;

typedef union {
    int i;
    float f;
    char* str;
    bool b;
    closure c;
} prim_type;

closure* make_closure(environnement *env, void* fun) {
    closure *c = malloc(sizeof(closure));
    c->function = fun;
    c->env = env;
}

void call(closure *c, void* arg) {
    (c->function)(arg);    
}

void* get_var_at_index(environnement *env, int ind) {
    var_node* node = env->head;
    for (int i = 0; i < ind; i++)
        node = node->next;
    return node->var;
}

/* TODO env ne devrait pas changer l'environnement */
void add_var_in_env(environnement *env, void* var) {
    var_node* node = malloc(sizeof(var_node));
    node->var = var;
    node->next = env->head;
    env->head = node;
}

prim_type builtin_int_add(prim_type a, prim_type b) {
    prim_type res;
    res.i = a.i + b.i;
    return res;
}

prim_type builtin_int_sub(prim_type a, prim_type b) {
    prim_type res;
    res.i = a.i - b.i;
    return res;
}

prim_type builtin_int_mul(prim_type a, prim_type b) {
    prim_type res;
    res.i = a.i * b.i;
    return res;
}

prim_type builtin_int_div(prim_type a, prim_type b) {
    prim_type res;
    res.i = a.i / b.i;
    return res;
}

prim_type builtin_float_add(prim_type a, prim_type b) {
    prim_type res;
    res.f = a.f + b.f;
    return res;
}

prim_type builtin_float_sub(prim_type a, prim_type b) {
    prim_type res;
    res.f = a.f - b.f;
    return res;
}
prim_type builtin_float_mul(prim_type a, prim_type b) {
    prim_type res;
    res.f = a.f * b.f;
    return res;
}
prim_type builtin_float_div(prim_type a, prim_type b) {
    prim_type res;
    res.f = a.f / b.f;
    return res;
}

prim_type builtin_int_lt(prim_type a, prim_type b) {
    prim_type res;
    res.b = (a.i < b.i);
    return res;
}

prim_type builtin_int_gt(prim_type a, prim_type b) {
    prim_type res;
    res.b = (a.i > b.i);
    return res;
}

prim_type builtin_int_eq(prim_type a, prim_type b) {
    prim_type res;
    res.b = (a.i == b.i);
    return res;
}

prim_type builtin_int_leq(prim_type a, prim_type b) {
    prim_type res;
    res.b = (a.i <= b.i);
    return res;
}

prim_type builtin_int_geq(prim_type a, prim_type b) {
    prim_type res;
    res.b = (a.i >= b.i);
    return res;
}

prim_type builtin_str_eq(prim_type a, prim_type b) {
    prim_type res;
    res.b = (strcmp(a.str, b.str));
    return res;
}
