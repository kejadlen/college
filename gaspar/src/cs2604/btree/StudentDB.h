#ifndef _STUDENTDB_H_
#define _STUDENTDB_H_

class StudentData {
	// Solely for utility purposes for this
	// specific application, makes it 
	// easier to handle nodes
	
	public:
		string id;
		string firstName;
		string lastName;
		string year;
		string major;
		string email;
};

class StudentDB {
	// Student database class
	
	private:
		int m;	// Order of the BTree

		// Necessary files
		fstream database;
		ofstream outputFile;

		// Index files
		BTree *nameIndex;
		BTree *idIndex;
		
		// I/O from the database
		void getData(StudentData *data, long pos);
		void writeData(StudentData *data, long pos);

		// Helper functions
		void add(string temp, int flag=0);
		void print(LinkList<long> *pos, int flag=0);
		
	public:
		// Constructor
		StudentDB(string d, string n, int mT, string c, string o);

		// Parse the data and command files
		void parseData(string d);
		void parseCommands(string c);
		
		// Destructor
		~StudentDB();
};

#endif
