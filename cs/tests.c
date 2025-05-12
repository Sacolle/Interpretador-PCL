#include <stdio.h>
#include <stdlib.h>


struct Btree {
	int x;
	struct Btree* left;
	struct Btree* right;
};

struct Btree* newNode(int val){
	struct Btree* node = (struct Btree*) malloc(sizeof(struct Btree));
	node->x = val;
	node->left = NULL;
	node->right = NULL;

	return node;
}

struct Btree* insert(struct Btree* tree, int val){
	if(tree == NULL) return newNode(val);

	if(val > tree->x) tree->right = insert(tree->right, val);
	else tree->left = insert(tree->left, val);
	return tree;
}

struct Btree* get(struct Btree* tree, int val){
	if(tree == NULL) return NULL;

	if(val == tree->x) return tree;
    else if(val > tree->x) return get(tree->right, val);
	else return get(tree->left, val);
}

void printTree(struct Btree* tree){
	if(tree != NULL){
		printf("(%d,", tree->x);
		printTree(tree->left);
		printf(" ");
		printTree(tree->right);
		printf(")");
	}else{
		printf("NULL");
	}
}


int main(){
	struct Btree* tree = NULL;

	tree = insert(tree, 5);
	tree = insert(tree, 1);
	tree = insert(tree, 7);
	tree = insert(tree, 4);

	printTree(tree);
	printf("\n");

	struct Btree* tree5 = get(tree, 7);

	printTree(tree5);

	printf("\n");

	printTree(tree);

	printf("\n");
	return 0;
}