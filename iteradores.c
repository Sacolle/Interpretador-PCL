#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct vec{
	int size;
	int cap;
	int* list;
} Vec;

void vec_new(Vec* vec){
	#define VEC_INIT_CAP 4

	int* mem = (int*) malloc(VEC_INIT_CAP);

	vec->size = 0;
	vec->cap = VEC_INIT_CAP;
	vec->list = mem;
}

void vec_push(Vec* vec, int val){
	if(vec->size == vec->cap){
		vec->cap *= 2; //double the capacity
		// realocate
		int* mem = (int*) malloc(vec->cap * sizeof(int));
		memcpy(mem, vec->list, sizeof(int) * vec->size);
		free(vec->list);

		vec->list = mem;
	}
	vec->list[vec->size] = val;
	vec->size++;
}


int* vec_start(Vec* vec){
	return vec->list;
}

int* vec_end(Vec* vec){
	return vec->list + vec->size;
}

void vec_print(Vec* vec){
	for(int i = 0; i < vec->cap; i++){
		if(i < vec->size){
			printf("%d, ", vec->list[i]);
		}else{
			printf("/, ");
		}
	}
	printf("\n");
}


int main(){
	Vec vec;
	vec_new(&vec);

	int values[7] = {1, 2, 3, 4, 5, 6, 7};

	for(int i = 0; i < 7; i++){
		vec_push(&vec, values[i]);
	}

	vec_print(&vec);

	//double the vec
	// explodes
	for(int* ptr = vec_start(&vec); ptr < vec_end(&vec); ptr++){
		vec_push(&vec, *ptr);
	}

	vec_print(&vec);
	return 0;
}