// Name: Dinh Luong
// Lab Assignment 2
// CECS 424 
// Professor Neal Terrell

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

struct Block {
	int block_size; // # of bytes in the data section
	struct Block *next_block; // in C, you have to use "struct Block" as the type
};

const int OVERHEAD_SIZE = sizeof(struct Block);		
const int POINTER_SIZE = sizeof(void*);				
struct Block *free_head;

void my_initialize_heap(int size){
	free_head = (struct Block*) malloc(size); // initialize a buffer given size
	free_head->block_size = size - OVERHEAD_SIZE; // [ Overhead | Data/Block size]
	free_head->next_block = NULL; // No next block yet
}

void* my_alloc(int size){
	// (a) Positive int value and data size is multiple of your pointer size
	if (size <= 0){
		printf("\n Size is a negative value or 0 \n");
		return 0;
	}
	if(size%POINTER_SIZE!=0){
		size = size + POINTER_SIZE - (size%POINTER_SIZE);
	}

	// (b) Walk free list starting at free_head using first fit 
	struct Block *previous = free_head;
	struct Block *current = free_head; 
	bool hasBlock = false;
	while (current != NULL) {// while still have the next block
		if(current->block_size >= size){ // check if it fits, if yes, exit the loop
			hasBlock = true;
			break;
		} else  { // move to next block
			current = current->next_block; 
			previous = current; 
		}
	}

	// (c) Found a block to fit data size, decide whether to split that block
	if (hasBlock){
		// Split into 4 cases/scenarios : 
		bool isHead = false;
		bool canSplit = false; 

		// Check if the block is head
		if(current == free_head){
			isHead = true;
		}
		// Needs at least the size being allocated AND the excess space in
		// data portion to fit another block with overhead & pointer size
		if( current->block_size >= (size + (OVERHEAD_SIZE+POINTER_SIZE))) { 
			canSplit = true;
		}

		// (i) Block is head and can split
		if (isHead && canSplit){
			// Initialize a new struct Block* pointer to that location and assign its new block_size.
			struct Block *newBlock = (struct Block*) ((char *) current + OVERHEAD_SIZE + size);
			newBlock->block_size = current->block_size - size - OVERHEAD_SIZE;
			
			// The new block's next_block pointer needs to point to the same block as the next pointer
			// of the block you are splitting
			newBlock->next_block = current->next_block;
			
			// Reduce the size of the original block to match the allocation request
			current->block_size = size;

			// Update the free_head
			free_head = newBlock;
		}

		// (ii) Block is not head and can split
		else if (!isHead && canSplit){
			// Initialize a new struct Block* pointer to that location and assign its new block_size.
			struct Block *newBlock = (struct Block*) ((char *) current + OVERHEAD_SIZE + size);
			newBlock->block_size = current->block_size - size - OVERHEAD_SIZE;
			
			// The new block's next_block pointer needs to point to the same block as the next pointer
			// of the block you are splitting
			newBlock->next_block = current->next_block;

			// Reduce the size of the original block to match the allocation request
			current->block_size = size;

			// Update the pointer linked list accordingly once the allocation done
			previous->next_block = newBlock;
			current->next_block = NULL;
		}

		// (iii) Block is head and cannot split
		else if (isHead && !canSplit){
			// Adjust pointer to the next free block
			free_head = current->next_block;
			current->next_block = NULL;
			// Update the size of allocation request
			current->block_size = size;
		}
		// (iv) Block is not head and cannot split
		else if (!isHead && !canSplit){
			// Adjust pointer to the next free block
			previous->next_block = current->next_block;
			current->next_block = NULL;
			// Update the size of allocation request
			current->block_size = size;
		}

		// (d) Return a poiter to the data region 
		return (void*) ((char*) current + OVERHEAD_SIZE);

	} else {
		printf("\n There is no spaces \n");
		return 0;
	}
}


void my_free(void *data){
	// Pointer is the DATA portion of a block, move backwards in memory to find block's
	// overhead information
	struct Block* freeBlock = (struct Block*) ((char*)data - OVERHEAD_SIZE);
	// Link it into the free list
	freeBlock->next_block = free_head;
	free_head = freeBlock;
}


int main() {
	// Initialize the heap
	my_initialize_heap(1000);
	
	void* case1a, * case1b; // Variables for case 1
	void* case2a, * case2b; // Variables for case 2
	void* case3a, * case3b, * case3c, * case3d, * case3e; // Variables for case 3
	void* case4a, * case4b; // Variables for case 4
	void* case5a; // Variables for case 5
	int* case5b; // Variables for case 5

	int choice = 1;

	switch(choice){
		case 1:  // Test Case 1
			case1a = my_alloc(sizeof(int));
			printf("Allocation: Address of case1a (size of an int) : %d \n", case1a);
			my_free(case1a);
			printf("Deallocation of case1a\n");
			case1b = my_alloc(sizeof(int));
			printf("Allocation: Address of case1b (size of an int) : %d \n", case1b);
			break;
		case 2: // Test Case 2
			case2a = my_alloc(sizeof(int));
			case2b = my_alloc(sizeof(int));
			printf("Allocation: Address of case2a (size of an int) : %d \n", case2a);
			printf("Allocation: Address of case2b (size of an int) : %d \n", case2b);
			printf("Size of overhead + size of int is %d apart \n", OVERHEAD_SIZE+sizeof(int));
			break;
		case 3: // Test Case 3
			case3a = my_alloc(sizeof(int));
			case3b = my_alloc(sizeof(int));
			case3c = my_alloc(sizeof(int));
			printf("Allocation: Address of case3a (size of an int) : %d \n", case3a);
			printf("Allocation: Address of case3b (size of an int) : %d \n", case3b);
			printf("Allocation: Address of case3c (size of an int) : %d \n", case3c);
			my_free(case3b);
			printf("Deallocation of case3b\n");
			case3d = my_alloc(sizeof(double)*2);
			printf("Allocation: Address of case3d (size of 2 doubles) : %d \n", case3d);
			case3e = my_alloc(sizeof(int));
			printf("Allocation: Address of case3e (size of an int) : %d \n", case3e);
			break;
		case 4: // Test Case 4
			case4a = my_alloc(sizeof(char));
			case4b = my_alloc(sizeof(int));
			printf("Allocation: Address of case4a (size of an char) : %d \n", case4a);
			printf("Allocation: Address of case4b (size of an int) : %d \n", case4b);
			printf("Size of overhead + minimum size is 12 apart \n");			
			break;
		case 5: // Test Case 5
			case5a = my_alloc(sizeof(int)*80);
			case5b = my_alloc(sizeof(int));
			*case5b = 424;
			printf("Address of case5b based on calculation : %d \n", case5a+(OVERHEAD_SIZE + sizeof(int)*80));
			printf("Allocation: Address of case5b (size of an int) : %d \n", case5b);
			printf("Value of case5b %d \n", *case5b);
			my_free(case5a);
			printf("Deallocation of case5a\n");
			printf("Address of case5b (size of an int) : %d \n", case5b);
			printf("Value of case5b %d \n", *case5b);
			break;
	}
	return 0;
};