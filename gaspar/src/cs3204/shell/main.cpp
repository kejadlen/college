#include <iostream>
#include <string>
#include <vector>

#include <unistd.h>
#include <curses.h>

using namespace std;

vector<string> getPath();
void getCommandLine(string command, vector<string> arguments, string input, string output, bool backgroundFlag);
//bool getCommandLine(string command, vector<string> arguments, string input, string output);

int main (int argc, char * const argv[]) {
    vector<string> path = getPath();

    unsigned int loc = 0;    
    for(loc=0; loc<path.size(); loc++) cout<<path[loc]<<endl;
    
    string command, input, output;
    vector<string> arguments;
    bool backgroundFlag;

	initscr();
	cbreak();
	noecho();

	string line;

	while(command != "exit") {
		command.
		int ch = getch();
		while(ch!='\n') {
			cout<<(char)ch;
			cout.flush();
			command.push_back((char)ch);
			ch = getch();
		}
		cout<<"\r\nYou typed: "<<command<<"\r\n";
	}
	
	endwin();
	
//    getCommandLine(command, arguments, input, output, backgroundFlag);
//  	backgroundFlag = getCommandLine(command, arguments, input, output);
	
    return 0;
}

vector<string> getPath() {
    // Stores each path in a string, putting them
    // all in a vector to be returned.
    string data = getenv("PATH");
    unsigned int loc = 0;
    vector<string> path;
    
    char *temp;
    
    // Gets the current directory
    if(getwd(temp)) path.push_back(temp);
    
    loc = data.find(":");  
    while(loc != string::npos) {
	path.push_back(data.substr(0, loc));
	data = data.substr(loc+1, data.length()-loc);
	loc = data.find(":");
    }

    if(!data.empty()) path.push_back(data);
    
    return path;
}

void getCommandLine(string command, vector<string> arguments, string input, string output, bool backgroundFlag) {
//bool getCommandLine(string command, vector<string> arguments, string input, string output) {
// Gets and parses the command line.
    
    initscr();
    cbreak();
    noecho();
    
    string line;
 
	cout<<">>";
	
    int ch = getch();
    while(ch != ' ') {
		cout << (char)ch;
		cout.flush();
		line.push_back((char)ch);
	
		ch = getch();
    }
    
    cout << "\r\n"<<"You typed: " << line <<"\r\n";
    
    endwin();
}

