#include <iostream>

#include "WebTree.cpp"

using namespace std;

int main(int argc, char **argv) {
	if(argc == 4) {
		WebTree a(argv[1], argv[3]);
		a.startSearch(argv[2]);
	}
	else {
		cout<<"webcrawl2 needs three arguments: webpagefile, searchfile, outputfile"<<endl;
		exit(0);
	}

	return 0;
}
