#include<string>
#include<iostream>
#include<fstream>

using namespace std;

int main() {
	ifstream input("timingFile.txt");
	ofstream output("timingFile.tim");

	output<<"$T\t$I x1\t$I x2\t$I i"<<endl;
//		  <<"0\t0\t1\t1"<<endl
//		  <<"50\t0\t1\t0"<<endl;
//		  <<"25\t0\t1\t1"<<endl;
	
	string temp;
	int i = 0, t = 0;

//	t = 50;
	
	while(!input.eof() && input>>temp) {
		output<<t<<"\t"<<temp.c_str()[0]<<"\t"<<temp.c_str()[1]<<"\t"<<temp.c_str()[2]<<endl;
//		for(i=0; i<3; i++) {
//			output<<temp.c_str()[i]<<"\t"<<endl;
//			t+=152;
//			t+=157; // DELAY + SETUP
//			output<<t<<"\t"<<temp.c_str()[i]<<"\t0\t1"<<endl;
//			t+=5;
//			t+=5;	// HOLD
//		}
	}

	input.close();
	output.close();
	
	return 0;
}
