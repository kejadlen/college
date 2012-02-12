#include "Huffman.cpp"

int main(int argc, char **argv) {
	if(argc != 4) {
		cout<<"decode needs three arguments: treefile, encodedmessagefile, messagefile"<<endl;
		exit(0);
	}
	else {
		Huffman *temp = new Huffman();
	
		temp->decodeFile(argv[1], argv[2], argv[3]);

		delete temp;
	}
}
