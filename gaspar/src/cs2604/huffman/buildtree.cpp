#include "Huffman.cpp"

int main(int argc, char **argv) {
	if(argc != 4) {
		cout<<"buildtree needs three arguments: frequencyfile, treefile, codefile"<<endl;
		exit(0);
	}
	else {
		Huffman *temp = new Huffman();
	
		temp->buildTree(argv[1]);

		temp->buildTreeFile(argv[2]);
	
		temp->buildCodeFile(argv[3]);

		delete temp;
	}
}
