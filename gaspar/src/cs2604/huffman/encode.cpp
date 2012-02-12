#include "Huffman.cpp"

int main(int argc, char **argv) {
	if(argc != 4) {
		cout<<"encode needs three arguments: codefile, messagefile, encodedmessagefile"<<endl;
		exit(0);
	}
	else {
		Huffman *temp = new Huffman();
	
		temp->encodeFile(argv[1], argv[2], argv[3]);

		delete temp;
	}
}
