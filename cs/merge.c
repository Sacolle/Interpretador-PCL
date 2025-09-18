#include <stdio.h>
#include <stdlib.h>


void mergestep(int* list, int* buff, int* below, int* above, int* i, const int pivot, const int high){
    if((*below > pivot) || (*above > high)) return; 

	if(list[*below] > list[*above]){
		buff[*i] = list[*above];
		*above = *above + 1;
	}else{
		buff[*i] = list[*below];
		*below = *below + 1;
	}
	*i = *i + 1;
	mergestep(list, buff, below, above, i, pivot, high);
}


void copy_over_i(int* from, int* to, int* i, int j, int top){
	if (j < top){
		to[*i] = from[j];
		*i = *i + 1;
		copy_over_i(from, to, i, j + 1, top);
	}
}

void copy(int* from, int* to, int i, int top){
	if (i < top){
		to[i] = from[i];
		copy(from, to, i + 1, top);
	}
}


void merge(int *list, int *buff, int low, int pivot, int high){
    int below = low;
    int i = low;
    int above = pivot + 1;

	mergestep(list, buff, &below, &above, &i, pivot, high);

    if(below > pivot){
		copy_over_i(list, buff, &i, above, high + 1);
    }else{
		copy_over_i(list, buff, &i, below, pivot + 1);
    }
	copy(buff, list, low, high + 1);
}

void mergesort(int *a, int*b, int low, int high){
    if(low < high){
        int pivot = (low + high)/2;
        mergesort(a,b,low,pivot);
        mergesort(a,b,pivot+1,high);
        merge(a,b,low,pivot,high);
    }
}

void plist(int* begin, int* end){
	while(begin++ < end) printf("%d, ", *(begin - 1));
	printf("\n");
}


int main(){
	int* list = (int*) malloc(sizeof(int) * 5);
	*(list + 0) = 5;
	*(list + 1) = 9;
	*(list + 2) = 8;
	*(list + 3) = 2;
	*(list + 4) = 6;

	plist(list, list + 5);

	int* buff = (int*) malloc(sizeof(int) * 5);

	mergesort(list, buff, 0, 4);

	plist(buff, buff + 5);
	plist(list, list + 5);

	return 0;
}