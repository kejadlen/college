#include <iostream>
#include <string>
#include <vector>

#include <glob.h>
#include <stdio.h>	
#include <stdlib.h>
#include <termios.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ioctl.h>

using namespace std;

vector<string> getPath();
string getCommandLine(vector<string> path);
int tabComplete(vector<string> path, string &tab);

void parseCommandLine(vector<string> path, string &command, string &arguments, string &input, string &output, int &backgroundFlag);
string parseCommand(string &temp);
int parseBackground(string &temp);
string parseRedirectors(vector<string> path, string &filename, string symbol, string temp);
void removeSpaces(string &temp);

void cdCommand(vector<string> &path, string arguments);
int commandPath(vector<string> path, string &fullCommand, string command);
string parseSlash(vector<string> path, string command);
char** makeArguments(string arguments, unsigned int &loc);

int main (int argc, char * const argv[]) {
    vector<string> path = getPath();

    unsigned int loc = 0;    
//    for(loc=0; loc<path.size(); loc++) cout<<path[loc]<<endl;

	vector<string> tempArgs;
    string command, fullCommand, arguments, input, output, startDir = getenv("PWD");
    int backgroundFlag; 
	unsigned int pid, i;
	int *status = NULL;
    char **args = NULL;

	cout<<"Shell Emulator started..."<<endl;
//	cout<<"Paths:"<<endl;
//	for(i=0; i<path.size() ;i++) cout<<path[i]<<endl;
	
    while(1) {
//		cout<<path[0]<<" >> ";
		cout<<">> ";
		
		command = getCommandLine(path);

		parseCommandLine(path, command, arguments, input, output, backgroundFlag);
		
		if(command == "exit") {
			chdir(startDir.c_str());
			return 0;
		}

		if(command == "cd") cdCommand(path, arguments);

		if(commandPath(path, fullCommand, command)) {
//			cout<<"Found a match: "<<fullCommand<<endl;
			args = makeArguments(arguments, loc);
	
			args[0] = new char[command.size()];
			strcpy(args[0], command.c_str());
	
			if((pid=fork()) == 0) {
				// exec junk
				// freopen stuff
				if(!input.empty()) {
//					if(input[0] != '/') input = path[0] + "/" + input;
//					input = parseSlash(path, input);
					if(!freopen(input.c_str(), "r", stdin)) input.clear();
				}
				if(!output.empty()) {
//					if(output[0] != '/') output = path[0] + "/" + output;
//					output = parseSlash(path, output);
					if(!freopen(output.c_str(), "w", stdout)) output.clear();
				}
				execvp(fullCommand.c_str(),args);
				if(!input.empty()) fclose(stdin);
				if(!output.empty()) fclose(stdout);
				exit(0);
			}

			if(!backgroundFlag) {
				waitpid(pid, status, 0);
				if(status) delete status;
			}

			for(i=0; i<loc ;i++)
				delete args[i];
			delete[] args;
			args = NULL;
		}
	}

    return 0;
}

vector<string> getPath() {
	// Returns the current working directory and
	// the system PATHs from the environment
	// variables

	// PRE:
	// POST: returns vector of strings containing
	//		 the current directory and directories
	//		 of the paths to search for binaries

    string data = getenv("PATH");
    unsigned int loc = 0;
    vector<string> path;
    
    // Gets the current directory
	path.push_back(getenv("PWD"));

	// Split apart the string of paths	
    loc = data.find(":");  
    while(loc != string::npos) {
		path.push_back(data.substr(0, loc));
		data = data.substr(loc+1, data.length()-loc);
		loc = data.find(":");
    }

	// Get the last path that's left in data
	// Also handles the case of no paths
    if(!data.empty()) path.push_back(data);
    
    return path;
}

string getCommandLine(vector<string> path) {
	// Gets the command line, returns it as a string

	// PRE: 
	// POST: returns string of the command line

	// Turn echoing off
	struct termios ts;
	struct termios new_ts;
	ioctl(0, TCGETS, &ts);
	new_ts = ts;
	new_ts.c_lflag &= !ICANON;
	new_ts.c_lflag &= !ECHO;
	ioctl(0, TCSETS, &new_ts);

	string temp, tab;
	int ch = getchar();
	
	while(ch != '\n') {
		if(ch == '\t') {
			loc = temp.find_last_of(" ");
			if(loc != string::npos) {
				tab = temp.substr(loc+1, temp.length()-loc-1);
				if(tabComplete(path, tab)) {
					temp = temp + tab;
					cout<<tab;
				}
			}
		}
		else {
			temp.push_back((char)ch);
			cout<<(char)ch;
		}
	    ch = getchar();
	}
	cout<<(char)ch;

	// Turn echoing back on
	ioctl(0, TCSETS, &ts);

	return temp;
}

int tabComplete(vector<string> path, string &tab) {

	vector<string> lsArgs;
	string tabdir;
	unsigned int loc = tab.find_last_of("/"), tabloc;
	int pid, *status = NULL;
	glob_t g;

	g.gl_offs = 2;

	// Take care of relative path
	if(loc != string::npos) {
		tabdir = tab.substr(0, loc);
		tabdir = parseSlash(path, tabdir);
		chdir(tabdir.c_str());
		tab = tab.substr(loc+1, tab.length()-loc-1);
	}

	tab += "*";

	if(!glob(tab.c_str(), GLOB_DOOFFS | GLOB_MARK, NULL, &g)) {
		if(g.gl_pathc == 1) {
			tabloc = tab.length()-1;
			tab = g.gl_pathv[2];
			tab = tab.substr(tabloc, tab.length()-tabloc);
			if(tab[tab.length()-1] != '/') tab += " ";
			return 1;
		}
		if(g.gl_pathc > 1) {
			cout<<endl;
			if((pid=fork()) == 0) {
				glob(tab.c_str(), GLOB_DOOFFS, NULL, &g);
				g.gl_pathv[0] = "ls";
				g.gl_pathv[1] = "-ad";
				execvp("ls", g.gl_pathv);
				exit(0);
			}
			globfree(&g);
			waitpid(pid, status, 0);
			if(status) delete status;
			cout<<">> "<<temp;
		}	
	}
	if(loc != string::npos) chdir(path[0].c_str());

	return 0;
}

void parseCommandLine(vector<string> path, string &command, string &arguments, string &input, string &output, int &backgroundFlag) {
	// Parses the command line for the command, arguments, input, output,
	// and whether or not to run the executable in the background

	// PRE: path contains the current working directory and the lookup paths
	//		command contains the command line
	//		all others do not matter
	// POST: each variable contains the data for which it is named

	// Initialize variables
	arguments.clear();
	backgroundFlag = 0;

	removeSpaces(command);
	
	string temp = command;

	command = parseCommand(temp);
	backgroundFlag = parseBackground(temp);
	arguments = parseRedirectors(path, input, " < ", temp);
	arguments = parseRedirectors(path, output, " > ", arguments);
}

string parseCommand(string &temp) {
	// Gets the command from the command line

	// PRE: temp holds the command line
	// POST: returns a string of the command
	//		 temp holds the command line without the command

	string command;
	unsigned int loc = temp.find(" ");

	if(loc != string::npos) {
	// If there is a space, then the command is before the space
		command = temp.substr(0, loc);
		temp = temp.substr(loc+1, temp.length()-loc);
	}

	else {
	// temp holds just the command or nothing
		command = temp;
		temp.clear();
	}
	
	return command;
}

int parseBackground(string &temp) {
	// Checks if the command line (without the command) has
	// an & on the end or not

	// PRE: temp holds the command line without the command
	// POST: returns an integer - 0 for executing in the
	//		 foreground and 1 for executing in the background

	int backgroundFlag = 0;

	// Takes care of the lone "&" case
	temp = " " + temp;

	unsigned int loc = temp.length();

	if(loc > 1 && temp.substr(loc-2, 2) == " &") {
		temp = temp.substr(0, loc-2);
		backgroundFlag = 1;
	}

	// Remove the added space from earlier
	removeSpaces(temp);
	
	return backgroundFlag;
}

string parseRedirectors(vector<string> path, string &filename, string symbol, string temp) {
	// Parses the command line for a term equal to symbol.  If it is found, the following
	// term is placed into filename and removed from the command line, which is then
	// returned.

	// PRE: path holds the current working directory and the lookup paths
	//		filename does not matter
	//		temp holds the command line
	//	POST: returns the command line without the symbol and following term
	//		  filename holds the term following the symbol if it exists

	filename.clear();
	temp = " " + temp;

	// Look for an input redirector.  If there is one, loc2 will be
	// the end of the filename
	unsigned int loc1 = temp.find(symbol), loc2 = temp.find(" ", loc1+3);

	if(loc1 != string::npos) {
	// If there is an input redirector
		if(loc2 != string::npos) {
		// There are more arguments after the input filename
			filename = temp.substr(loc1+3, loc2-loc1-3);
			temp = temp.substr(0,loc1) + temp.substr(loc2, temp.length()-loc2);
		}
		else {
		// There are no more arguments after the input filename
			filename = temp.substr(loc1+3);
			removeSpaces(filename);
			temp = temp.substr(0, loc1);
		}
		// Parse the input filename
		filename = parseSlash(path, filename);
	}

	// Just in case parsing messed up above...
	removeSpaces(temp);

	return temp;
}

void removeSpaces(string &temp) {
	// Makes a string easier to parse by spaces

	// PRE: temp holds a string
	// POST: temp does not have any consecutive spaces
	//		 in it anymore.  It also does not have any
	//		 preceding or trailing spaces.

	// Remove preceding spaces
	if(!temp.empty()) 
		while(temp[0] == ' ') 
			temp = temp.substr(1, temp.length()-1);

	// Remove trailing spaces
	if(!temp.empty()) 
		while(temp[temp.length()-1] == ' ') 
			temp = temp.substr(0, temp.length()-1);

	// Remove consecutive spaces inside temp
	for(unsigned int loc=0; loc < temp.length() ;loc++) {
		if(temp[loc] == ' ')
			while(loc+1 < temp.length() && temp[loc+1] == ' ') 
				temp.erase(loc+1, 1);
	}
}

void cdCommand(vector<string> &path, string arguments) {
	// Imitates the "cd" command -- changes the current
	// working directory

	// PRE: path holds the current working directory
	//		arguments holds the arguments to cd (only
	//			the first is used)
	// POST: the current working directory should be
	//			modified
	//		 path[0] (the current working directory)
	//			should be changed to reflect the new
	//			directory

	string originalArgument = arguments;

	// If the command line is just "cd"
	if(arguments.empty()) arguments = getenv("HOME");
	else {
		// If there is more than one argument, trim it
		// down to the first one
		unsigned int loc = arguments.find(" ");
		if(loc != string::npos) {
			arguments = arguments.substr(0, loc);
			originalArgument = arguments;
		}

		// If there is a "/" on the end of the directory, remove it
		loc = arguments.length() - 1;
		if(loc && arguments[loc] == '/') arguments = arguments.substr(0, loc);

		// Parse out the path to the shortest absolute form
		arguments = parseSlash(path, arguments);
	}

	// If the directory exists, change to it
	if(!access(arguments.c_str(), X_OK)) {	
		chdir(arguments.c_str());
		path[0] = arguments;
	}

	else cout<<"cd: "<<originalArgument<<": No such file or directory"<<endl;
}

int commandPath(vector<string> path, string &fullCommand, string command) {
	// Check to see whether the command can be run or not by the current user.
	// If it cannot, return 0.  If it can, fullCommmand is set to the absolute
	// path of the executable and 1 is returned.

	// PRE: path holds the current working direcotry and lookup paths.
	//		fullCommand does not matter
	//		command holds the command to be investigated
	// POST: returns 1 if the command can be executed, 0 otherwise
	//		 fullCommand is set to the absolute path of the excutable
	//			if the command is found

	string temp;

	// If the path is relative
	if(command.find("/") != string::npos) {
		// Get the absolute path of the command
		command = parseSlash(path, command);

		// Check if the command can be executed
		if(!access(command.c_str(), X_OK)) {
			fullCommand = command;
			return 1;
		}
	}
	
	else {
		// Go through each path to see if the command
		// exists/can be run
		for(unsigned int i=1; i < path.size() ;i++) {
			temp = path[i] + "/" + command;

			// Check if the command can be executed
			if(!access(temp.c_str(), X_OK)) {
				fullCommand = temp;
				return 1;
			}
		}
	}

	return 0;
}

string parseSlash(vector<string> path, string command) {
	// Parses out a relative path to an absolute path

	// PRE: path[0] contains the current working directory
	//		command contains the relative path
	// POST: return the absolute path

	// TO DO: Change the name of the command variable!  It
	//			does not make sense.

	string temp;
	unsigned int loc, slash;

	// If the path does not start at root, add the current
	// working directory to the front
	if(!command.empty() && command[0] != '/') 
		if(path[0] != "/") command = path[0] + "/" + command;
		// If the current directory is root, then the slash
		// between the cwd and path is not needed
		else command = path[0] + command;

	// Parse out any instances of "/./" - they can be
	// discarded without any problems
	loc = command.find("/./");
	while(loc != string::npos) {
		// Remove the "/." completely
		command = command.substr(0, loc) + command.substr(loc+2, temp.length()-loc-2);
		loc = command.find("/./");
	}

	// Parse out any instances of "/../"
	loc = command.find("/../");
	while(loc != string::npos) {
			// Grab what is in front of the "/../"
			temp = command.substr(0, loc);
			// Find the directory immediately preceding the
			// "/../"
			slash = temp.find_last_of("/");
			// Remove the directory and "/.." if it exists.
			// If the preceding directory is root, discard
			// the "../"
			if(slash != string::npos) command = command.substr(0, slash+1) + command.substr(loc+4, command.length()-loc-4);
			else command = command.substr(3, command.length()-3);
		loc = command.find("/../");
	}

	// Parse out any "/." or "/.." at the end of the string
	loc = command.length();
	if(loc > 1 && command.substr(loc-2, 2) == "/.") command = command.substr(0, loc-2);
	else if(loc > 2 && command.substr(loc-3, 3) == "/..") {
		temp = command.substr(0, loc-3);
		slash = temp.find_last_of("/");
		if(slash != string::npos && slash) command = command.substr(0, slash);
		else command = "/";
	}

	return command;
}

char** makeArguments(string arguments, unsigned int &loc) {
	// Convert the string of arguments to an array of C-style
	// strings

	// PRE: arguments holds the arguments to be converted
	//		loc does not matter
	// POST: returns an array of C-style strings holding each
	//		 argument
	//		 loc holds the number of arguments in the array

	char **args = NULL;
	vector<string> temp;

	// Create a vector of strings of arguments
	loc = arguments.find(" ");  
    while(loc != string::npos) {
		temp.push_back(arguments.substr(0, loc));
		arguments = arguments.substr(loc+1, arguments.length()-loc);
		loc = arguments.find(" ");
    }
	// Takes care of the no arguments and last argument case
    if(!arguments.empty()) temp.push_back(arguments);

	// Add one because the command has to go in the first
	// C-string in the array
	loc = temp.size() + 1;

	// Add one more, since the array must be terminated by a
	// NULL pointer
	args = new char*[loc+1];

	// Fill the array
	for(unsigned int i=1; i<loc ;i++) {
		args[i] = new char[temp[i-1].size()];
		strcpy(args[i], temp[i-1].c_str());
	}
	args[loc] = NULL;

	return args;
}

