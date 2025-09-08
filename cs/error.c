#include <stdlib.h>
#include <stdio.h>

#define SIZE 10
void stufff(){
	int err, abrt;
char* ptr = (char*) malloc(SIZE);
if (err){
	abrt = 1;
	free (ptr) ;
}
if (abrt) {
	logError (" operation aborted before commit ", ptr ) ;
}
}

#define STR_MAX 10
char* getName(){
	char name[STR_MAX];
	fillInName(name);
	return name;
}


int get_val(int* list, int len, int idx){
	if(idx < len) return list[idx];
	else return -1;
}

int main(){
	int* stuff = (int*) calloc(sizeof(int), 10);
	int inp = 0;
	do{
		scanf("%d", &inp);
		printf("%d\n", get_val(stuff, 3, inp));
	}while (inp < 10);
	return 0;
}