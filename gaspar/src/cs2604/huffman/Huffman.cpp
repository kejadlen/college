#ifndef _HUFFMAN_CPP_
#define _HUFFMAN_CPP_

#include "Huffman.h"

Huffman::Huffman() {
	// Constructor

	huffList = NULL;
	huffHead = NULL;
	
	for(int i=0; i<47; i++) charChange[i] = 0;
	
	charChange[45] = 26;
	charChange[46] = 27;
	charChange[33] = 28;
	charChange[43] = 29;
}

void Huffman::buildTree(string f) {
	// Builds the Huffman tree from the frequency file f,
	// storing it in huffList
	
	ifstream input(f.c_str());

	if(input.fail()) {
		cout<<"Unable to open frequency file "<<f<<endl;
		exit(0);
	}

	huffList = new HuffList();
	HuffNode *newNode = NULL, *tempNode = NULL, *headNode = NULL;
	string temp;
	char character;
	int frequency, space;
	
	// Read through the entire file
	while(!input.eof() && getline(input,temp,'\n')) {

		// Get the character and frequency from the line
		space = temp.find(' ');
		character = temp[0];
		frequency = atoi(temp.substr(space+1,temp.length()-space-1).c_str());
	
		newNode = new HuffNode(character, frequency);

		// Put the node into the list
		huffList->insert(newNode);
	}

	// Now, create the tree - this iterates until
	// there is only one node in the list (which
	// is the parent of the code tree)
	while(huffList->length-1) {
		// Grab the first two nodes (they have the lowest frequencies)
		headNode = huffList->head;
		tempNode = huffList->head->next;
		
		// Take into account that two nodes are "taken out"
		huffList->head = huffList->head->next->next; 
		huffList->length -= 2;
		
		// The nodes are not part of the list anymore, so
		// next shouldn't be pointing to anything
		headNode->next = NULL;
		tempNode->next = NULL;

		// Make the nodes the leaves of a new node
		newNode = new HuffNode(char(1), headNode->frequency + tempNode->frequency, headNode, tempNode);

		// Insert the new node into the list
		huffList->insert(newNode);
	}

	input.close();
}

void Huffman::buildTreeFile(string o) {
	// Write the treefile
	
	string temp;
	temp = buildTreeFile(huffList->head);

	ofstream output;
	output.open(o.c_str());
	output<<temp<<endl;
	output.close();
}

string Huffman::buildTreeFile(HuffNode *n) {
	// A helper file for writing the treefile
	
	string temp = "";
	
	// If the node is a character node, output the character
	if(n->character != 1) temp += n->character;

	// The node is a parent, so go through the left and
	// right branches.  Mark going down the left branch.
	else {
		if(n->left) {
			temp += "/";
			temp += buildTreeFile(n->left);
		}
		if(n->right) {
//			temp += "\\";
			temp += buildTreeFile(n->right);
		}
	}

	return temp;
}

void Huffman::buildCodeFile(string c) {
	// Builds the code file
	
	string array[30] = {""};
	
	// Populate the array
	getCodes(huffList->head, array, "");

	ofstream output(c.c_str());
	
	for(int i=0; i<26; i++)
		output<<char(i+'a')<<'\t'<<array[i]<<endl;

	// Handle the extra character cases
	output<<"-\t"<<array[26]<<endl;
	output<<".\t"<<array[27]<<endl;
	output<<"!\t"<<array[28]<<endl;
	output<<"+\t"<<array[29]<<endl;

	output.close();
}

void Huffman::getCodes(HuffNode *h, string a[30], string c) {
	// Get the codes of each character
	
	char character = h->character;
	
	// If this is not a leaf node	
	if(character == 1) {
		// Go down the left adding a 0
		// and down the right adding a 1
		getCodes(h->left, a, c+"0");
		getCodes(h->right, a, c+"1");
	}
	
	// If it is a leaf node, place the code into the array
	else if(character >= 'a' && character <= 'z')
		a[character-'a'] = c;
	else
		a[charChange[int(character)]] = c;
}

void Huffman::encodeFile(string c, string m, string e) {
	// Encodes file m to file e using codefile c
	
	string codeArray[30];

	buildCodeArray(c, codeArray);

	ifstream input(m.c_str());
	ofstream output(e.c_str());

	if(input.fail()) {
		cout<<"Unable to open message file "<<m<<endl;
		exit(0);
	}
	
	int i=0;
	char temp, outputChar = 0;
	string buffer;

	// Go through entire input file
	while(input.get(temp)) {

		// Convert the spaces, periods, and newlines
		// so that they will point to the correct
		// location in the codeArray
		switch(temp) {
			case ' ':
				temp = 'a' + 26; break;
			case '.':
				temp = 'a' + 27; break;
			case '\n':
				temp = 'a' + 28; break;
		}

		// The code string
		buffer = codeArray[int(temp-'a')];

		// Convert the code string into (a) character(s)
		for(; buffer.length() ;i++) {

			// Once the character is "filled up", it
			// can be written to the file
			if(i == 8) {
				output<<outputChar;
				i = 0;
			}
			
			// Using bitwise manipulation, add each
			// bit to the character
			outputChar *= 2;
			outputChar ^= buffer[0]-'0';

			// Delete the bit so that it is not used again
			buffer = buffer.substr(1,buffer.length()-1);
		}
	}

	// Now that the last character of the file is read in
	// and the buffer for it is finished, we can output
	// the EOF character
	buffer = codeArray[29];

	// Same mechanics as the above for loop
	for(; buffer.length() ;i++) {
		if(i == 8) {
			output<<outputChar;
			i = 0;
		}

		outputChar *= 2;
		outputChar ^= buffer[0]-'0';
		buffer = buffer.substr(1,buffer.length()-1);
	}

	// Fill in the extra bits with zeroes
	for(; i<8; i++) {
		outputChar *= 2;
	}

	// Output the last character
	output<<outputChar;
}

void Huffman::buildCodeArray(string c, string a[30]) {
	// Reads the code for each character from the code
	// file c and put it into array a
	
	ifstream input(c.c_str());

	if(input.fail()) {
		cout<<"Unable to open code file "<<c<<endl;
		exit(0);
	}

	string temp, code;
	char character;
	int space;
	
	// Go through the file one line at a time
	while(!input.eof() && getline(input,temp,'\n')) {

		// Get the character and code from the line
		space = temp.find('\t');
		character = temp[0];
		code = temp.substr(space+1,temp.length()-space-1).c_str();
	
		// Put the code into the proper place in the array
		if(character >= 'a' && character <= 'z')
			a[character-'a'] = code;
		else
			a[charChange[int(character)]] = code;
	}

	input.close();
}

void Huffman::buildTreeFromFile(string t) {
	// Build the huffman tree from treefile t
	
	ifstream input(t.c_str());

	if(input.fail()) {
		cout<<"Unable to open tree file "<<t<<endl;
		exit(0);
	}

	char temp;
	HuffNode *current = NULL;

	// Initialize huffHead
	if(huffHead) delete huffHead;
	huffHead = new HuffNode(char(1), 0, NULL, NULL, current);

	current = huffHead;

	// Go through entire treefile
	while(input.get(temp) && temp != '\n') {

		// If temp is not a legal character, create a new branch
		if(temp == '/') {

			// Place a branch to the left
			if(!current->left) {
				current->left = new HuffNode(char(1), 0, NULL, NULL, current);
				current = current->left;
			}
			// Branch to the right, and then to the left
			else {
				while(current->right) current = current->parent;
				current->right = new HuffNode(char(1), 0, NULL, NULL, current);
				current = current->right;
				current->left = new HuffNode(char(1), 0, NULL, NULL, current);
				current = current->left;
			}
		}
		else {
			
			// Change the character to the correct character
			switch(temp) {
				case '-':
					temp = ' '; break;
				case '!':
					temp = '\n'; break;
			}

			// Place character into node (which has
			// already been created)
			if(!current->left) {
				current->character = temp;
				current = current->parent;
			}
			// Find a right branch to put the character in
			else {
				while(current->right) current = current->parent;
				current->right = new HuffNode(temp, 0, NULL, NULL, current);
				current = current->parent;
			}

		}
	}
}

void Huffman::decodeFile(string t, string e, string m) {
	// Decodes file e into file m using treefile t

	buildTreeFromFile(t);

	ifstream input(e.c_str());
	ofstream output(m.c_str());

	if(input.fail()) {
		cout<<"Unable to open encoded file "<<e<<endl;
		exit(0);
	}

	HuffNode *current = huffHead;
	
	char temp;
	string code;
	
	// Go through entire encoded file
	while(input.get(temp)) {
		code = getCode(temp);
	
		// Go through each bit in the character
		while(code.length()) {
			if(current->character != 1) {
				if(current->character != '+') output<<current->character;
				current = huffHead;
			}
			
			current = (code[0]-'0') ? current->right : current->left ;
			code = code.substr(1,code.length()-1);
		}
	}
}

string Huffman::getCode(char c) {
	// Returns the bits of character c in string format

	string temp = "";
	
	for(int i=0; i<8; i++) {
		temp += char((c&128)/128 + '0');
		c *= 2;
	}

	return temp;
}

Huffman::~Huffman() {
	// Destructor

	if(huffList) delete huffList;
	if(huffHead) delete huffHead;
}

#endif
