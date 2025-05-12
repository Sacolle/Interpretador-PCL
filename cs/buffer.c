#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAX_SIZE 100

char* sanitize_input(char *user_supplied_string){
	char *dst_buf = (char*) malloc(4*sizeof(char) * MAX_SIZE);  
	if ( MAX_SIZE <= strlen(user_supplied_string) ){
		printf("user string too long, die evil hacker!\n");
		exit(1);
	}  
	int dst_index = 0;  
	for (int i = 0; i < strlen(user_supplied_string); i++ ){
		if( '&' == user_supplied_string[i] ){
			dst_buf[dst_index++] = '&';  
			dst_buf[dst_index++] = 'a';  
			dst_buf[dst_index++] = 'm';  
			dst_buf[dst_index++] = 'p';  
			dst_buf[dst_index++] = ';';
		}
		/*  
		else if ('<' == user_supplied_string[i] ){
			//encode to &lt; 
		}*/  
		else dst_buf[dst_index++] = user_supplied_string[i];
	}  
	dst_buf[dst_index] = '\0';
	return dst_buf;
}

int main(int argc, char** argv){
	char* sanit = sanitize_input(argv[1]);
	for(int i = 0; i < 100 /*strlen(sanit)*/; i++){
		printf("%c\n", sanit[i]);
	}
	return 0;
}