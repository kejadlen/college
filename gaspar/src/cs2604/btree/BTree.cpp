#include "StudentDB.cpp"

int main(int argc, char **argv) {
	if(argc != 6) {
		cout<<"btree needs five arguments: datafile, name, order, commandfile, outputfile"<<endl;
		exit(0);
	}
	else {
		StudentDB *temp = new StudentDB(argv[1], argv[2], atoi(argv[3]), argv[4], argv[5]);
		delete temp;
	}
}
