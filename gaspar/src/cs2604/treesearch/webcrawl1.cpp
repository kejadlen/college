#include <iostream>

#include "WebTree.cpp"

using namespace std;

int main(int argc, char **argv) {
	if(argc == 3) {
		WebTree a(argv[1], argv[2]);
		a.webcrawl();
	}
	else {
		cout<<"webcrawl1 needs two arguments: webpagefile, datafile"<<endl;
		exit(0);
	}

	return 0;
}
