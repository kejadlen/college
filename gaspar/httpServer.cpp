/* Microsoft Visual C++ 6 doe
not like maps with strings*/
#ifdef _MSC_VER
#pragma warning( disable: 4786 )
#endif

#include <stdio.h>
#include <winsock.h>
#include <process.h>
#include <io.h>
#include <fcntl.h>
#include <stdlib.h>
#include <windows.h>
#include <winbase.h>

#include <iostream>
#include <string>
#include <algorithm>
#include <functional>
#include <map>
#include <vector>
#include <ctime>

#include "logic.cpp"

using namespace std;

#define	QLEN		   5	/* maximum connection queue length	*/
#define	STKSIZE		16536
#define	BUFSIZE		4096
#define	WSVERS		MAKEWORD(2, 0)

/* Keeps track of data for
each connected socket. */
struct httpStats {
	SOCKET socket;
	string ipAddress;

	string request;

	string basedir;
	string path;
	string host;
	int keepAlive;
	
	string serverResponse;

	LARGE_INTEGER startTime;
	LARGE_INTEGER stopTime;
};

/* Used for passing arguments
to a new httpServer threads. */
struct httpServerArgs {
	SOCKET socket;
	string ipAddress;
};

SOCKET	msock, ssock;		/* master & slave server sockets	*/
map<string, string> contentTypes;	// map of all content-types the server can recognize

LARGE_INTEGER freq;
double totalTime;
double totalRequests;

int		httpServer(void *);

int		parseRequest(httpStats *);
int		parseRequestLine(string, httpStats *);
int		runCGI(httpStats *);
string	getCGIInfo(httpStats *);
string	makeCGIName(httpStats *);
int		fileExistsP(httpStats *);
int		sendStatus(httpStats *);
int		sendResponse(httpStats *);
string	fixPath(string);

int		sendDirectory(httpStats *);
string	makeDirectoryListing(httpStats *);
string	addLink(string);

int		sendFile(int, string, httpStats *);
string	pathExtension(string);

SOCKET	initSocket();
int		sendString(SOCKET, string);
int		sendString(SOCKET, char *, int);

void	errexit(const char *, ...);

int main(int argc, char *argv[]) {
	SOCKET msock;
	struct	sockaddr_in fsin;	/* the address of a client	*/
	int	alen;			/* length of client's address	*/

	struct httpServerArgs *args= new httpServerArgs;
	char tempIP[16];

	// Set up content-type map
	contentTypes["html"]	= "text/html";
	contentTypes["htm"]		= "text/html";
	contentTypes["txt"]		= "text/plain";
	contentTypes["jpeg"]	= "image/jpeg";
	contentTypes["jpg"]		= "image/jpeg";
	contentTypes["gif"]		= "image/gif";
	contentTypes["pdf"]		= "application/pdf";
	
	// Initialize networking
	msock = initSocket();

	cout<<"HTTP server started"<<endl;

	// Set up statistics
	QueryPerformanceFrequency(&freq);
	totalTime = 0;
	totalRequests = 0;

	// Loop infinitely to receive all requests
	while (1) {
		alen = sizeof(fsin);
		ssock = accept(msock, (struct sockaddr *)&fsin, &alen);
		//cout<<"("<<totalTime/++totalRequests<<")"<<endl;

		args = new httpServerArgs;
		args->socket = ssock;
		sprintf(tempIP,"%d.%d.%d.%d",fsin.sin_addr.S_un.S_un_b.s_b1,
			fsin.sin_addr.S_un.S_un_b.s_b2,
			fsin.sin_addr.S_un.S_un_b.s_b3,
			fsin.sin_addr.S_un.S_un_b.s_b4);
		args->ipAddress = tempIP;

		if (ssock == INVALID_SOCKET)
			fprintf(stderr, "accept: error number\n", WSAGetLastError());
		// Spawn new thread with child socket
		else if (_beginthread((void (*)(void *))httpServer, STKSIZE,
		    (void *)args) < 0) {
			fprintf(stderr, "_beginthread: %s\n", strerror(errno));
		}
	}

	return 1;	/* not reached */
}

int httpServer(void *_args) {
	/*
	*/
	httpServerArgs *args = (httpServerArgs *) _args;

	char *buffer = new char[BUFSIZE], *bufptr = buffer;
	httpStats *requestStats = new httpStats;

	requestStats->socket = args->socket;
	requestStats->ipAddress = args->ipAddress;
	
	// Get the initial request and place it into buffer
	int n = recv(requestStats->socket, bufptr, sizeof buffer, 0);

	/* HTTP/1.1 assumes keep-alive, so this keeps
	reading until the client closes the connection
	or there is a SOCKET_ERROR. */
	while(n != SOCKET_ERROR && n != 0) {
		bufptr = bufptr + n;
		*bufptr = '\0';

		requestStats->request = buffer;

		/* The end of the request is found by the reception
		of two CRLFs in a row. */
		if(requestStats->request.find("\r\n\r\n") != string::npos) {
			QueryPerformanceCounter(&requestStats->startTime);

			// Reset requestStats
			requestStats->basedir = "c:/webfiles";
			requestStats->path = "";
			requestStats->host = "";
			requestStats->keepAlive = 0;
			requestStats->serverResponse = "";

			// Parse the request
			if(parseRequest(requestStats) && !requestStats->serverResponse.empty())
				requestStats->serverResponse = "400 Bad Request";

			if(requestStats->path.find("/cgi-bin/") == string::size_type(0)) {
				requestStats->serverResponse = "";
				runCGI(requestStats);
			}

			/* TODO:
				A mutex or something should be used here...
			*/
			/*cout<<"\"GET "<<requestStats->path<<"\" received from "
				<<requestStats->ipAddress<<" (";
			if(requestStats->keepAlive) 
				cout<<"keep-alive";
			else
				cout<<"close";
			cout<<")"<<endl;*/
			//cout<<requestStats->request<<endl;
			
			// Send a response to the request
			if(sendResponse(requestStats) || !requestStats->keepAlive)
				return closesocket(requestStats->socket);

			// Reset bufptr for new request
			bufptr = buffer;
		}
		
		// Keep polling for more data from the client
		n = recv(requestStats->socket, bufptr, sizeof buffer, 0);
	}

	if(n==SOCKET_ERROR)
		fprintf(stderr, "receiving error: %d\n", GetLastError());
	
	/* The socket is closed because this will
	only be reached if a shutdown is initiated
	by the client or if there is a SOCKET_ERROR. */
	return closesocket(requestStats->socket);
}

int parseRequest(httpStats *requestStats) {
	/*
	Parses the request 
	*/
	string request = requestStats->request;

	// Find the end of the request
	int end = request.find("\r\n\r\n");

	// Find the end of the request-line
	int i = request.find("\r\n");
	if(i == string::npos)
		return 1;

	// Parse the request-line
	if(parseRequestLine(request.substr(0, i), requestStats))
		return 1;

	if(i == end)
		return 0;

	string temp;

	// Iterate through remaining lines in the request
	for(int j = request.find("\r\n", i+1); i != end; j = request.find("\r\n", i+1)) {
		temp = request.substr(i+2,j-i-2);

		// Get keep-alive; this will override the
		// keep-alive set by the protocol type
		if(temp.find("Connection: ") == 0) {
			temp = temp.substr(12);
			transform(temp.begin(), temp.end(), temp.begin(), ptr_fun(::tolower));

			if(temp == "keep-alive")
				requestStats->keepAlive = 1;
			else if(temp == "close")
				requestStats->keepAlive = 0;
			else
				return 1;
		}

		// Get the host (required for status 301)
		else if(temp.find("Host: ") == 0) {
			requestStats->host = temp.substr(6);
		}

		i = j;
	}

	return 0;
}

int parseRequestLine(string requestLine, httpStats *requestStats) {
	/*
	Parses the request line to get the method, path, and protocol.
	*/	
	// Make sure the method is GET
	if(requestLine.length() < 4 || ("GET " != requestLine.substr(0, 4))) {
		requestStats->serverResponse = "501 Not Implemented";
		return 1;
	}

	// Get the path
	requestLine = requestLine.substr(4, requestLine.length()-4);
	int i = requestLine.find(" ");
	if(i == string::npos)
		return 1;

	requestStats->path = fixPath(requestLine.substr(0, i));

	// Get keep-alive
	requestLine = requestLine.substr(i+1);
	if(requestLine != "HTTP/1.0" && requestLine != "HTTP/1.1")
		return 1;

	requestStats->keepAlive = (requestLine == "HTTP/1.1");

	return 0;
}

int runCGI(httpStats *data) {
	/*
	Runs a CGI program. The requested path should be in this format:
	/cgi-bin/PROGRAM?arg1=X&arg2=Y&arg3=Z...
	*/
	string command = getCGIInfo(data);

	if(!command.length())
		return -1;
	
	command += " " + makeCGIName(data);
	command += " " + data->ipAddress;
	
	char *commandString = new char[command.length()+1];
	strcpy(commandString, command.c_str());

	BOOL rc;
	DWORD reason;
	STARTUPINFO startup;
	PROCESS_INFORMATION process;
	LPDWORD exitcode = new DWORD;

	startup.cb				= sizeof(startup);	// size of the structure
	startup.lpReserved		= NULL;				// must be NULL
	startup.lpDesktop		= NULL;				// inherit desktop from caller
	startup.lpTitle			= NULL;				// use executable name as title
	startup.dwFlags			= 0;				// specify no flags
	startup.cbReserved2		= 0;				// must be 0
	startup.lpReserved2		= NULL;				// must be NULL

//	cout<<"."<<commandString<<"."<<endl;

	rc = CreateProcess(NULL, commandString, NULL, NULL, FALSE,
			CREATE_NEW_CONSOLE, NULL, NULL, &startup, &process);

	if( !rc ) {
		printf("CreateProcess() failed:  %d\n", GetLastError());
		return -1;
	}

	reason = WaitForSingleObject(process.hProcess, INFINITE);

	switch( reason ) {
		case WAIT_FAILED:
			printf("WaitForSingleObject() failed:  %d\n", GetLastError());
			break;
		case WAIT_ABANDONED:
			printf("WaitForSingleObject() abandoned wait\n");
			break;
		case WAIT_OBJECT_0:
			GetExitCodeProcess(process.hProcess, exitcode);
			if(*exitcode == 401)
				data->serverResponse = "401 Unauthorized Access";
			else if(*exitcode == 403)
				data->serverResponse = "403 Access Denied";
			break;
		default:
			printf("WaitForSingleObject() returned unexpected code:  %d\n", reason);
	}
	
	return 0;
}

string getCGIInfo(httpStats *data) {
	/*
	Parse the CGI request to create the command and modify
	the httpStats structure.
	*/
	string path = data->path;
	string::size_type stringPos, argPos;

	string command;
	
	// Get rid of the /cgi-bin/
	path = path.substr(9);

	// Get the program name
	stringPos = path.find("?");
	command = "perl c:/webfiles/cgi-bin/" + path.substr(0, stringPos);
	path = path.substr(stringPos+1);

	// Get arguments
	while((stringPos = path.find("&")) != string::npos) {
		argPos = path.find("=");

		command += " \"" + path.substr(argPos+1,stringPos-argPos-1) + "\"";

		path = path.substr(stringPos+1);
	}
	argPos = path.find("=");
	command += " \"" + path.substr(argPos+1) + "\"";

	return command;
}

string makeCGIName(httpStats *data) {
	/*
	Returns a temporary filename to write to/read from.
	*/
	string pathPart = "c:/webfiles/tmp";
	string randPart;
	string filename;
	char buffer[10], *bufptr = buffer;
	
	srand(static_cast<unsigned>(time(0)));

	do {
		randPart = "";
		for(int i=0; i<3; i++) {
			bufptr = itoa(rand(), bufptr, 10);
			randPart += bufptr;			
		}
		filename = pathPart + randPart + ".txt";
	}
	while (!_access(filename.c_str(), 0));

	data->path = "/tmp" + randPart + ".txt";
	//data->basedir = "c:/webfiles";

	return filename;		
}

int fileExistsP(httpStats *requestStats) {
	/*
	Returns 0 if the file exists and is readable. If not,
	the serverResponse is set to the appropriate response.
	*/
	string path = requestStats->basedir + requestStats->path;

	// Check for existence
	if(_access(path.c_str(), 0)) {
		requestStats->serverResponse = "404 Not Found";
		return 1;
	}

	// Check for readability
	if(_access(path.c_str(), 4)) {
		requestStats->serverResponse = "500 Internal Server Error";
		return 1;
	}

	return 0;
}

int sendStatus(httpStats *requestStats) {
	/*
	Sends the status-line. If it is not a 200 OK,
	this is the entire response. Otherwise, the OK
	status is sent and other functions will take
	care of the rest of the response.
	*/
	string temp, html;
	char intBuffer[sizeof(int)*8+1];	// For itoa conversion

	// If the serverResponse has already been set,
	// send that response with an error page
	if(!requestStats->serverResponse.empty()) {
		temp = requestStats->serverResponse;

		// Generate a small error page
		html = "<html><head><title>" + temp;
		html += "</title</head><body>" + temp;
		html += "<br /><pre>" + requestStats->request;
		html += "</pre></body></html>";

		// Generate the actual response; note that
		// the html goes after the double CRLF
		temp = "HTTP/1.1 " + temp + "\r\n";
		temp += "Content-Length: ";
		temp += itoa(html.length(), intBuffer, 10);
		temp += "\r\nContent-Type: text/html\r\n\r\n";

		temp += html;

		return sendString(requestStats->socket, temp);
	}

	// serverResponse has not been set
	if(sendString(requestStats->socket, "HTTP/1.1 200 OK\r\n"))
		return 1;
	
	return 0;
}

int sendResponse(httpStats *requestStats) {
	/*
	Sends the response back to the client.
	*/
	int fd;
	string path, contentType;

	// Handle any non 200 status messages
	if(!requestStats->serverResponse.empty() || fileExistsP(requestStats)) {
		if(sendStatus(requestStats))
			return 1;
		return 0;
	}

	contentType = pathExtension(requestStats->path);

	path = requestStats->basedir + requestStats->path;
	fd = _open(path.c_str(), _O_RDONLY | _O_BINARY);

	if(_filelength(fd) != -1) { // fd is a file
		if(sendStatus(requestStats)) 
			return 1;

		return sendFile(fd, contentType, requestStats);
	}

	// A directory has been requested, so
	// the filehandle is no longer needed
	_close(fd);

	return sendDirectory(requestStats);
}

string fixPath(string path) {
	/*
	Changes all instances of %20 in the path
	to spaces.
	*/
	int i; string temp;

	while((i = path.find("%20")) != string::npos) {
		temp = path.substr(0, i) + " ";
		path = temp + path.substr(i+3,path.length()-3-i);
	}

	return path;
}

int sendDirectory(httpStats *requestStats) {
	/*
	Handles the case where the requested path
	is to a directory. If the path does not end
	in '/', a 301 is sent back to the client.
	Else, the client will receive index.html or index.htm
	if one exists. Otherwise, the client will
	receive a hyperlinked listing of the directory.
	*/
	string temp, path, response;
	char longBuffer[sizeof(long)*8+1]; // for ltoa

	// If the path doesn't end in /, send a 301
	path = requestStats->path;
	if(path[path.length()-1] != '/') {
		temp = "301 Moved Permanently\r\n";
		temp += "Location: http://" + requestStats->host + path + "/";
		requestStats->serverResponse = (requestStats->host.empty()) 
			? "400 Bad Request"
			: temp ;

		if(sendStatus(requestStats))
			return 1;
		
		return 0;
	}

	requestStats->serverResponse = "";

	// Check for index.html and index.htm
	requestStats->path = path + "index.html";
	if(!fileExistsP(requestStats)) {
		if(sendResponse(requestStats))
			return 1;
		return 0;
	}

	requestStats->path = path + "index.htm";
	if(!fileExistsP(requestStats)) {
		if(sendResponse(requestStats))
			return 1;
		return 0;
	}

	// index.htm[l] doesn't exist, so send
	// a directory listing
	requestStats->path = path;
	temp = makeDirectoryListing(requestStats);
	if(temp.empty())
		return 1;

	// Generate the response
	response = "HTTP/1.1 200 OK\r\nContent-Length: ";
	response += ltoa(temp.length(), longBuffer, 10);
	response += "\r\nContent-Type: text/html\r\n\r\n";

	if(sendString(requestStats->socket, response))
		return 1;
	if(sendString(requestStats->socket, temp))
		return 1;

	return 0;
}

int sendFile(int fd, string contentType, httpStats *requestStats) {
	/*
	Sends the response from a GET request. The status-line
	has already been sent, but the response-headers and file
	has not been sent.
	*/
	string responseHeader;
	long i=0, filePos = 0, socket = requestStats->socket;

	// For ltoa and overloaded sendString
	char longBuffer[sizeof(long)*8+1], sendBuffer[4097];
	
	// Generate response-headers
	responseHeader = "Content-Length: ";
	responseHeader += ltoa(_filelength(fd), longBuffer, 10);
	responseHeader += "\r\nContent-Type: " + contentType + "\r\n\r\n";

	sendString(socket, responseHeader.c_str());

	// Send the file
	_lseek(fd, filePos, 0);
	while((i = _read(fd, sendBuffer, BUFSIZE)) > 0) {
		// Use the char* based sendString, because
		// files may have null characters in them.
		if(sendString(socket, sendBuffer, i))
			return 1;
		filePos += i;
		_lseek(fd, filePos, 0);
	}
	if(i) {
		_close(fd);
		cout<<"Problem reading file "<<requestStats->basedir + requestStats->path<<endl;
		return 1;
	}

	QueryPerformanceCounter(&requestStats->stopTime);
	totalTime += 1000*(requestStats->stopTime.QuadPart - requestStats->startTime.QuadPart)/(double)freq.QuadPart;
	//cout<<"("<<totalTime/++totalRequests<<")"<<endl;
	
	_close(fd);

	return 0;
}

string makeDirectoryListing(httpStats *requestStats) {
	/*
	Returns an HTML directory listing of the requested basedir+path.
	*/

	string html = "<html><head><title>Index of ";
	html += requestStats->path.substr(0, requestStats->path.length()-1);
	html += "</title></head>";

	// Go through all files in the directory
	WIN32_FIND_DATA FindFileData;
	HANDLE hFind = INVALID_HANDLE_VALUE;
	char DirSpec[BUFSIZE];
	DWORD dwError;
	string path = requestStats->basedir + requestStats->path;

	strncpy(DirSpec, path.c_str(), path.length()+1);
	strncat(DirSpec, "\\*", 3);

	hFind = FindFirstFile(DirSpec, &FindFileData);

	if(hFind == INVALID_HANDLE_VALUE) {
		cout<<"Could not read directory "<<html<<endl;
		return "";
	}
	else {
		// Add the link
		html += addLink(FindFileData.cFileName);
		while(FindNextFile(hFind, &FindFileData) != 0)
			// Add the link
			html += addLink(FindFileData.cFileName);
		dwError = GetLastError();
		if(dwError == ERROR_NO_MORE_FILES)
			FindClose(hFind);
		else {
			cout<<"FindNextFile error: "<<dwError<<endl;
			return "";
		}
	}

	html += "</html>";

	return html;
}

string addLink(string fileName) {
	/*
	Helper function for makeDirectoryListing. This
	takes a filename and returns an HTML link to
	the file.
	*/
	string temp = "";

	// Don't include self or parent directories
	if(fileName != "." && fileName != "..") {
		temp += "<a href = \"" + fileName;
		temp += "\">" + fileName;
		temp += "</a><br />";
	}

	return temp;
}

string pathExtension(string path) {
	/*
	Returns the content-type of a file from
	the filename's extension.
	*/
	int i = path.find_last_of(".");

	if(i != string::npos) {
		string temp = path.substr(i+1, path.length()-i-1);
		transform(temp.begin(), temp.end(), temp.begin(), ptr_fun(::tolower));

		// Look up the extension in the map
		if(contentTypes.find(temp) != contentTypes.end())
			return contentTypes[temp];
	}

	// Default content-type
	return "application/octet-stream";
}

SOCKET initSocket() {
	/*
	Initialize networking. The port is automatically set to 80, since
	this is an HTTP server. Upon receiving an error, the program will
	exit with an error message, as nothing can be done without network
	connectivity.
	*/
	SOCKET msock;
	WSADATA	wsadata;

	// Initialize socket software
	if (WSAStartup(WSVERS, &wsadata) != 0)
		errexit("WSAStartup failed\n");
	
	struct sockaddr_in sin;		// an Internet endpoint address

	// Populate the endpoint address structure
	memset(&sin, 0, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_port = htons(80);

    /* Allocate a socket */
	msock = socket(PF_INET, SOCK_STREAM, 0);
	if (msock == INVALID_SOCKET)
		errexit("can't create socket: %d\n", GetLastError());

    /* Bind the socket */
	if (bind(msock, (struct sockaddr *)&sin, sizeof(sin)) == SOCKET_ERROR)
		errexit("can't bind to port %d: %d\n", sin.sin_port, GetLastError());

	/* Start listening */
	if (listen(msock, 10) == SOCKET_ERROR)
		errexit("can't listen on %d port: %d\n", sin.sin_port, GetLastError());

	return msock;
}

/*
sendString

  Sends the string temp through the socket
  specified in the arguments. The first version
  is used predominantly throughout the program,
  as it takes an STL string. The second version
  is solely used to transfer chunks of files,
  since null characters in a file will cause
  an STL string to be cut off.
*/
int sendString(SOCKET fd, string temp) {
	int i = temp.length();

	int j = send(fd, temp.substr().c_str(), i, 0);

	while(i != j && j != SOCKET_ERROR) {
		i -= j;
		j = send(fd, temp.substr().c_str(), i, 0);
	}

	if(j == SOCKET_ERROR) {
		fprintf(stderr, "send error: %d\n", GetLastError());
		return 1;
	}

	return 0;
}

int sendString(SOCKET fd, char *temp, int length) {
	int i = length;

	int j = send(fd, temp, i, 0);

	while(i != j && j != SOCKET_ERROR) {
		i -= j;
		j = send(fd, temp, i, 0);
	}

	if(j == SOCKET_ERROR) {
		fprintf(stderr, "send error: %d\n", GetLastError());
		return 1;
	}

	return 0;
}