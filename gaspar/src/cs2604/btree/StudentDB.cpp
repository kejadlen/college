#ifndef _STUDENTDB_CPP_
#define _STUDENTDB_CPP_

#include <fstream>
#include <iostream>
#include <iomanip>
#include <string>

#include "BTree.cpp"
#include "LinkList.cpp"

#include "StudentDB.h"

using namespace std;

StudentDB::StudentDB(string d, string n, int mT, string c, string o) {
	// Constructor
	
	// Initialize the two indices
	nameIndex = new BTree(mT, 15, n + ".ix2");
	idIndex = new BTree(mT, 9, n + ".ix1");

	// Open the database and output files
	n += ".dat";
	database.open(n.c_str(), ios_base::in | ios_base::out | ios_base::trunc);
	outputFile.open(o.c_str(), ios_base::out | ios_base::trunc);

	// Set the order of the tree
	m = mT;

	// Parse the data, put it into the database
	parseData(d);
	
	// Output database info
	database.seekp(0, ios_base::end);
	outputFile<<"NUMBER OF RECORDS: "<<database.tellp()/64<<", ID INDEX: "
		<<idIndex->numNodes()<<" nodes, NAME INDEX: "<<nameIndex->numNodes()
		<<" nodes."<<endl;
	
	// Execute the commands in the commandfile
	parseCommands(c);

	// Output database info again
	database.seekp(0, ios_base::end);
	outputFile<<"NUMBER OF RECORDS: "<<database.tellp()/64<<", ID INDEX: "
		<<idIndex->numNodes()<<" nodes, NAME INDEX: "<<nameIndex->numNodes()
		<<" nodes."<<endl;
}

void StudentDB::parseData(string d) {
	// Parse the datafile
	
	ifstream dataFile(d.c_str(), ios::in);
	
	if(!dataFile) {
		outputFile<<"Unable to open datafile "<<d<<endl;
		exit(0);
	}

	string temp;
	
	// Add all the elements in the file
	while(!dataFile.eof() && getline(dataFile, temp, '\n')) add(temp);

	dataFile.close();
}

void StudentDB::parseCommands(string c) {
	ifstream input(c.c_str(), ios::in);

	if(!input) {
		outputFile<<"Unable to open commandfile "<<c<<endl;
		exit(0);
	}

	int breakpoint;
	string temp, cmd;
	LinkList<long> *pos = new LinkList<long>;

	// Go through each line of the file
	while(!input.eof() && getline(input, temp, '\n')) {
		breakpoint = temp.find(" ");
		
		// Get the command
		cmd = temp.substr(0, breakpoint);
		// Get the arguments
		temp = temp.substr(breakpoint+1, temp.length()-breakpoint-1);
		
		// Add an entry to the database
		if(cmd == "add") add(temp, 1);

		// Find something from one of the index files
		else if(cmd == "find") {
			breakpoint = temp.find(" ");

			// Get the index file
			cmd = temp.substr(0, breakpoint);

			// Get the argument
			temp = temp.substr(breakpoint+1, temp.length()-breakpoint-1);
			
			pos->clear();
			
			// Look through the ID index
			if(cmd == "ID") idIndex->find(*pos, temp, 0);
			// Look through the name index
			else nameIndex->find(*pos, temp, 0);

			// Output the information
			print(pos, 1);
		}

		// Dump the database
		else if(cmd == "dump") {
			breakpoint = temp.find(" ");

			// Get argument
			temp = temp.substr(breakpoint+1, temp.length()-breakpoint-1);
			pos->clear();

			// Dump by ID
			if(temp == "ID") idIndex->print(*pos, 0);
			// Dump by name
			else nameIndex->print(*pos, 0);

			// Output the information
			print(pos);
		}

	}

	delete pos;
	input.close();
}

void StudentDB::print(LinkList<long> *pos, int flag) {
	// If there is any data
	if(pos->getSize()) {
		StudentData *data = new StudentData;

		// Go through entire linked list
		pos->goHead();
		for(int i=0; i<pos->getSize(); i++) {
			getData(data, pos->get());

			// Output data
			outputFile.setf(ios_base::right);
			outputFile<<setw(5)<<pos->get()<<":";
			outputFile.unsetf(ios_base::right);
			outputFile.setf(ios_base::left);
            outputFile<<data->id<<" "<<setw(16)<<data->lastName
				<<setw(16)<<data->firstName<<data->year
				<<" "<<setw(5)<<data->major<<data->email<<endl;
			
			pos->next();
		}
		delete data;
	}
	// If nothing was FOUND.  (Only outputs for find
	// operation.)
	else if(flag) outputFile<<"NOT FOUND"<<endl;
}

void StudentDB::add(string temp, int flag) {
	// Add the data in temp to the database
	
	StudentData *data = new StudentData;
	int breakpoint;
	
	// Get the ID
	data->id = temp.substr(0, 9);
		
	// Get the last name
	temp = temp.substr(10, temp.length()-11);
	breakpoint = temp.find(" ");
	data->lastName = temp.substr(0, breakpoint);
	
	// Get the first name
	temp = temp.substr(breakpoint+1, temp.length()-breakpoint-1);
	breakpoint = temp.find(" ");
	data->firstName = temp.substr(0, breakpoint);
	
	// Get the year
	temp = temp.substr(breakpoint+1, temp.length()-breakpoint-1);
	data->year = temp.substr(0,1);

	// Get the major and email
	temp = temp.substr(2, temp.length()-2);
	breakpoint = temp.find(" ");
	data->major = temp.substr(0, breakpoint);
	data->email = temp.substr(breakpoint+1, temp.length()-breakpoint-1);

	// Go to the end of the database
	database.seekp(0, ios_base::end);

	// Insert into the two indices
	long i = nameIndex->add(data->lastName.c_str(), database.tellp(), 0);
	long j = idIndex->add(data->id.c_str(), database.tellp(), 0);

	// If this is from the commandfile, output extra info
	if(flag) {
		outputFile<<"STUDENT "<<data->id<<" ADDED ("<<database.tellp()<<";"
			<<j<<";"<<i<<")"<<endl;
	}

	// Write the info to the database
	writeData(data, database.tellp());

	delete data;
}

void StudentDB::getData(StudentData *data, long pos) {
	// Get data from the database at pos, put it into data
	
	delete data;
	data = new StudentData;
	char id[9], name[15], year, major[4], email[20];
	database.seekg(pos);
	database.read(id, 9); data->id = id;
	data->id = data->id.substr(0, 9);
	database.read(name, 15); data->lastName = name;
	database.read(name, 15); data->firstName = name;
	database.read((char*)&year, 1); data->year = year;
	database.read(major, 4); data->major = major;
	database.read(email, 20); data->email = email;
}

void StudentDB::writeData(StudentData *data, long pos) {
	// Write data into the database at pos, from data
	
	database.seekp(pos);
	database.write(data->id.c_str(),9);
	database.write(data->lastName.c_str(),15);
	database.write(data->firstName.c_str(),15);
	database.write(data->year.c_str(),1);
	database.write(data->major.c_str(),4);
	database.write(data->email.c_str(),20);
}

StudentDB::~StudentDB() {
	// Destructor
	
	database.close();
	outputFile.close();

	delete nameIndex;
	delete idIndex;
}

#endif
