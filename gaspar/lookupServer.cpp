/*
  lookupServer.cpp

  Alpha Chen
  ECE 4564 Project 1
  2004-09-26

  This program implements the server side of the lookup protocol.

  usage: lookupServer.exe [port]

  This defaults to port 4564
*/

// VSC6 is unhappy using strings and maps together
// so this disables the 100+ warnings that VSC6
// throws.
#ifdef _MSC_VER
#pragma warning( disable: 4786 )
#endif

#include <winsock.h>

#include <iostream>
#include <string>
#include <map>

// Required for WSAStartup
#define	WSVERS	MAKEWORD(2, 0)

using namespace std;

void errexit(const char *format, ...);
void lookupd(SOCKET fd, map<string, string> &data);
void doPut(SOCKET fd, char *buffer, string parseString, map<string, string> &data);
void doGet(SOCKET fd, string parseString, map<string, string> data);
void doClr(SOCKET fd, char *buffer, string parseString, map<string, string> &data);
void doAll(SOCKET fd, map<string, string> data);
void sendResponse(SOCKET fd, string response);

void main(int argc, char *argv[]) {
	struct sockaddr_in sin;		// an Internet endpoint address
	struct sockaddr_in fsin;	// the from address of a client
	SOCKET msock, ssock;		// master & slave sockets
	int	alen;					// from-address length
	WSADATA wsadata;			// for initializing networking

	// The lookup table
	map<string, string> data;

	// Populate the endpoint address structure
	memset(&sin, 0, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_port = htons(4564);

	// If the port is given as an argument, set
	// the appropriate number in sin.
	if(argc > 1) sin.sin_port = htons(atoi(argv[1]));

	// Initialize networking
	if (WSAStartup(WSVERS, &wsadata) != 0)
		errexit("WSAStartup failed\n");

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
	
	cout<<"Server started on port "<<ntohs(sin.sin_port)<<endl;

	// Wait for incoming connections
	while (1) {
		alen = sizeof(struct sockaddr);
		ssock = accept(msock, (struct sockaddr *)&fsin, &alen);
		if (ssock == INVALID_SOCKET)
			errexit("accept failed: error number %d\n", GetLastError());
		// A connection has been made, so
		// the server can now do work.
		lookupd(ssock, data);
	}

	errexit("bye!\n");
}

void errexit(const char *format, ...) {
	/* Error handling; prints an error
	   message from the arguments, cleans
	   up the networking, and exits the
	   program.
	*/
	va_list	args;

	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	WSACleanup();
	exit(1);
}

void lookupd(SOCKET fd, map<string, string> &data) {
	/* A connection has been made, so the server has
	   to get and service the incoming request. It
	   sends a status message back after the request
	   has been performed and then closes the socket
	   to wait for a new connection.
	*/
	int buflen = 1024;
	char *buffer = new char[buflen], *bufptr = buffer;

	// Get the request and place it into buffer
	int n = recv(fd, bufptr, buflen, 0);

	while(n != SOCKET_ERROR && n != 0) {
		bufptr = bufptr + n;
		buflen = buflen - n;
		n = recv(fd, bufptr, buflen, 0);
	}

	if(n==SOCKET_ERROR)
		errexit("receiving error: %d\n", GetLastError());

	// Make sure buffer is terminated with
	// a null character
	*bufptr = '\0';

	cout<<"Incoming request: "<<buffer<<endl;

	// Strings are used for easier manipulation
	// of the request.
	string parseString = buffer, op;
	
	op = (parseString.length() > 2)
		? parseString.substr(0,3)
		: "" ;
	
	if(parseString.length() > 3)
		parseString = parseString.substr(4);

	// Service the request
	if(op == "PUT")
		doPut(fd, buffer, parseString, data);
	else if(op == "GET")
		doGet(fd, parseString, data);
	else if(op == "CLR")
		doClr(fd, buffer, parseString, data);
	else if(op == "ALL")
		doAll(fd, data);
	else // operation not recognized
		sendResponse(fd, string("ERROR command \"" + op + "\" not recognized."));

	(void) closesocket(fd);
}

void doPut(SOCKET fd, char *buffer, string parseString, map<string, string> &data) {
	/* PUT SPACE KEY COMMA VALUE
	   Puts the key-value pair from parseString into the lookup table and
	   sends a status message back to the client.
	*/
	// Get the location of the comma between the key and the value.
	int locComma = parseString.find(",");

	if(locComma > 63 || parseString.length() - locComma > 64)
		sendResponse(fd, "ERROR key length over 64 bytes.");
	else {
		// Insert the data
		data[parseString.substr(0, locComma)] = parseString.substr(locComma+1);
		// Make sure the data is actually in the table
		parseString = ((data.find(parseString.substr(0, locComma)) != data.end()) &&
			(data[parseString.substr(0, locComma)] == parseString.substr(locComma+1)))
			? "OK " + string(buffer)
			: "ERROR" ;
		sendResponse(fd, parseString);
	}
}

void doGet(SOCKET fd, string parseString, map<string, string> data) {
	/* GET SPACE KEY
	   Uses the key from parseString to get the associated value from the
	   lookup table. On success, this information is then sent back to the
	   client.
	*/
	if(parseString.length() > 64)
		sendResponse(fd, "ERROR key length over 64 bytes.");
	else {
		parseString = (data.find(parseString) != data.end())
			? "OK " + parseString + "," + data[parseString]
			: "ERROR \"" + parseString + "\" not found." ;
		sendResponse(fd, parseString);
	}
}

void doClr(SOCKET fd, char *buffer, string parseString, map<string, string> &data) {
	/* CLR SPACE KEY
	   Deletes the key from parseString from the lookup table. Returns a status
	   message to the client.
	*/
	if(parseString.length() > 64)
		sendResponse(fd, "ERROR key length over 64 bytes.");
	else {
		if(data.find(parseString) == data.end())
			parseString = "ERROR \"" + parseString + "\" not found." ;
		else {
			data.erase(parseString);
			parseString = (data.find(parseString) == data.end())
				? "OK " + string(buffer)
				: "ERROR could not delete \"" + parseString + "\"." ;
		}
		sendResponse(fd, parseString);
	}
}

void doAll(SOCKET fd, map<string, string> data) {
	/* ALL
	   Sends back all of the key-value pairs in the lookup table
	   to the client.
	*/
	string response = "OK ";

	for(map<string, string>::iterator i = data.begin(); i != data.end(); i++)
		response += i->first + "," + i->second + "\n";

	sendResponse(fd, response);
}

void sendResponse(SOCKET fd, string response) {
	/* Send the string response to the socket fd.
	*/
	cout<<"Outgoing status: "<<response<<endl;
	if(send(fd, response.c_str(), response.length(), 0) == SOCKET_ERROR)
			errexit("couldn't send %s: %d\n", response.c_str(), GetLastError());
}
