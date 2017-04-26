#include <stdlib.h>
#include <stdbool.h>



typedef struct var_node var_node;
typedef struct var_node {
   void* var;
   var_node* next;
} var_node;

typedef struct environnement {
    var_node* head;
} environnement;

typedef struct {
    void* function;
    environnement env;
} closure;

/*
typedef union {
    int i;
    float f;
    char* string;
    bool b;
    closure c;
} prim_type;
*/

void* get_var_at_index(environnement *env, int ind) {
    var_node* node = env->head;
    for (int i = 0; i < ind; i++)
        node = node->next;
    return node->var;
};

/* TODO env ne devrait pas changer l'environnement */
void add_var_in_env(environnement *env, void* var) {
    var_node* node = malloc(sizeof(var_node));
    node->next = env->head;
    env->head = node;
};

