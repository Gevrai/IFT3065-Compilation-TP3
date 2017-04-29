#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

union prim_type;

typedef struct closure {
	union prim_type (*fun)(union prim_type, union prim_type*);
	union prim_type *ctx;
} closure;

typedef union prim_type {
	int i;
	float f;
	char* str;
	bool b;
	closure c;
} prim_type;

prim_type mkInt(int i){
	prim_type res;
	res.i = i;
	return res;
}

prim_type mkFloat(float f){
	prim_type res;
	res.f = f;
	return res;
}

prim_type mkBool(bool b){
	prim_type res;
	res.b = b;
	return res;
}

prim_type mkString(char* s){
	prim_type res;
	res.str = strdup(s);
	if(res.str == NULL) {
		printf("Runtime fatal error: mkString can't allocate\n");
		exit(EXIT_FAILURE);
	}
	return res;
}

prim_type mkClosure(prim_type (*f)(prim_type, prim_type*), int ctx_size, prim_type *ctx) {
	prim_type res;
	prim_type *copied_ctx = malloc(ctx_size * sizeof(prim_type));
	if(copied_ctx == NULL) {
		printf("Runtime fatal error: mkClosure can't allocate\n");
		exit(EXIT_FAILURE);
	}

	for (int i=0;i<ctx_size;i++){
		copied_ctx[i] = ctx[i];
	}

	res.c.fun = f;
	res.c.ctx = copied_ctx;
	return res;
}

prim_type callclosure(prim_type closure, prim_type arg) {
	prim_type res;
	if(closure.c.fun == NULL) {
		printf("Runtime fatal error: callclosure on a non-closure\n");
		exit(EXIT_FAILURE);
	}
	res = ((closure.c.fun)(arg, closure.c.ctx));
	return res;
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
	res.f = a.f / b.f; return res;
}

// Source: http://stackoverflow.com/questions/7228438/
prim_type builtin_float_tostring(prim_type a) {
	prim_type res;
	char buf[48];
	snprintf(buf, 48, "%f", a.f);
	res.str = strdup(buf);
	return res;
}

prim_type builtin_str_eq(prim_type a, prim_type b) {
	prim_type res;
	res.b = (strcmp(a.str, b.str));
	return res;
}
