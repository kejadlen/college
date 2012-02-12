#include <iostream>

#include "WebSearch.cpp"

using namespace std;

int main(int argc, char **argv) {
	if(argc==4)	{
		WebSearch *a = new WebSearch(argv[1], argv[2], argv[3]);
		a->runSearch();
		delete a;
	}
	else {
		cout<<".... needs three arguments: input, search, output.\n";
		exit(0);
	}

	return 0;
}
