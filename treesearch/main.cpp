#include <iostream>

#include "WebNode.cpp"
#include "TreeNode.cpp"

using namespace std;

int main(int argc, char **argv) {
/*	if(argc==4)	{
		WebSearch *a = new WebSearch(argv[1], argv[2], argv[3]);
		a->runSearch();
		delete a;
	}
	else {
		cout<<".... needs three arguments: input, search, output.\n";
		exit(0);
	}*/

	WebNode *a = new WebNode("www.vt.edu", 50, "virginia tech");
	a->print();

	WebNode *b = new WebNode("blarhg", 1, "hi there you");
	b->print();

	cout<<"hi.\n\n";
	
	TreeNode<WebNode> c(a, NULL);
	c.print();
	
	c.addChild(b);
	
	return 0;
}
