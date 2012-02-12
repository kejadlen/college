/*
  lookupClient.cpp

  Alpha Chen
  ECE 4564 Project 1
  2004-09-26

  This program implements the client side of the lookup protocol.

  usage: lookupClient.exe [host [port]]

  This defaults to localhost:4564
*/

#include <winsock.h>
#include <iostream>

// Required for WSAStartup
#define	WSVERS	MAKEWORD(2, 0)

using namespace std;

void init(struct sockaddr_in &sin, char *host, int port);
int parseCommand(char *op, char *sendString);
int getInput(char *input);
void strcat2(char *a, char *b, char *c);
int sendCommand(struct sockaddr_in &sin, char *response, char *host);
void getResponse(int s, char *response);
void getAllResponse(int s);
int printAll(char *response, int flag);
void outputResponse(char *op, char *response);
void errexit(const char *format, ...);

int main(int argc, char *argv[]) {
	char *host;
	int port = 4564;
	
	// Populate the hostname and port number if
	// given in the arguments
	switch(argc) {
	case 1:
		host = "localhost";
		break;
	case 3:
		port = atoi(argv[2]);
	case 2:
		host = argv[1];
		break;
	default:
		fprintf(stderr, "usage: lookup [host [port]]\n");
		exit(1);
	}

	// Remote machine endpoint structure
	struct sockaddr_in sin;
	init(sin, host, port);

	// Socket descriptor
	int s;

    char *op = new char[10], *response = new char[1024];

	while (1) {
		cout<<"Operation (PUT, GET, CLR, ALL, or QUIT)? ";
		cin.getline(op, 10);

		// Finish, gracefully clean up
		if(!strcmp(op, "QUIT")) {
			WSACleanup();
			return 0;
		}

		// parseCommand returns the request to be sent
		// to the server in response
		if(parseCommand(op, response)) {
			s = sendCommand(sin, response, host);

			if(strcmp(op, "ALL")) {
				getResponse(s, response);

				// Check for a recognizeable
				// response from the server
				if(strlen(response))
					outputResponse(op, response);
				else
					cout<<"Unrecognized response from server: "
						<<response<<endl;
			}
			else
				getAllResponse(s);
			
			closesocket(s);
		}
		else // if parseCommand returns 0
			cout<<"Invalid command."<<endl;
	}
}

void init(struct sockaddr_in &sin, char *host, int port) {
	/* Initializes the networking. Takes the endpoint structure,
	   a hostname, and a port number. The endpoint structure is
	   passed by reference and populated in this function.
	*/
	WSADATA wsadata;

	if (WSAStartup(WSVERS, &wsadata) != 0)
		errexit("WSAStartup failed\n");

	memset(&sin, 0, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_port = htons(port);

	struct hostent	*phe;		/* pointer to host information entry */

    /* Map host name to IP address, allowing for dotted decimal */
	if ( phe = gethostbyname(host) )
		memcpy(&sin.sin_addr, phe->h_addr, phe->h_length);
	else if	( (sin.sin_addr.s_addr = inet_addr(host)) == INADDR_NONE)
		errexit("can't get \"%s\" host entry\n", host);

}

int parseCommand(char *op, char *sendString) {
	/* Parses the command in op, getting data
	   from the user if necessary, and creating
	   the request to the server in sendString.

	   Uses getInput to make sure the key and
	   value are at most 64 characters long.

	   Uses strcat2 to concatenate three strings
	   at once.
	*/
	char *input = new char[1024];

	// The request always begins with
	// the operation to be performed.
	strcpy(sendString, op);

	if(!strcmp(op, "PUT")) {
		cout<<"Key? ";
		if(!getInput(input)) return 0;
		strcat2(sendString, " ", input);
		cout<<"Value? ";
		if(!getInput(input)) return 0;
		strcat2(sendString, ",", input);
		return 1;
	}
	else if(!strcmp(op, "GET")) {
		cout<<"Key? ";
		if(!getInput(input)) return 0;
		strcat2(sendString, " ", input);
		return 1;
	}
	else if(!strcmp(op, "CLR")) {
		cout<<"Key? ";
		if(!getInput(input)) return 0;
		strcat2(sendString, " ", input);
		return 1;
	}
	else if(!strcmp(op, "ALL")) return 1;
	else return 0;
}

int getInput(char *input) {
	/* Gets input from the command line,
	   making sure that it is at most 64
	   characters long and does not contain
	   any commas. Returns 0 on failure and
	   1 on success.
	*/
	cin.getline(input, 1024);
	if(strlen(input) > 64) {
		cout<<"Key length can be at most 64 bytes."<<endl;
		return 0;
	}
	else if(strchr(input, ',')) {
		cout<<"Commas cannot be used as a key or value."<<endl;
		return 0;
	}

	return 1;
}

void strcat2(char *a, char *b, char *c) {
	/* Concatenates three strings.
	*/
	a = strcat(a, b);
	a = strcat(a, c);
}

int sendCommand(struct sockaddr_in &sin, char *response, char *host) {
	/* Creates a socket with the endpoint address given in the
	   arguments and then sends the response string to the server.

	   The host argument is given in case of an error in connecting
	   the socket.
	*/
	/* Allocate a socket */
	int s = socket(PF_INET, SOCK_STREAM, 0);
	if (s == INVALID_SOCKET)
		errexit("can't create socket: %d\n", GetLastError());

	/* Connect the socket */
	if (connect(s, (struct sockaddr *)&sin, sizeof(sin)) == SOCKET_ERROR)
		errexit("can't connect to %s: %d\n", host, GetLastError());

	/* Send the string */
	if(send(s, response, strlen(response), 0) == SOCKET_ERROR)
		errexit("couldn't send %s: %d\n", response, GetLastError());

	/* Start the shutdown sequence */
	if(shutdown(s,1) == SOCKET_ERROR)
		errexit("shutdown error: %d\n", GetLastError());

	return s;
}

void getResponse(int s, char *response) {
	/* Get the response from the server using
	   the socket s, placing it in the string
	   response.
	*/
	int buflen = 1024;
	char *bufptr = response;

	int n = recv(s, bufptr, buflen, 0);
			
	while(n != SOCKET_ERROR && n != 0) {
		bufptr = bufptr + n;
		buflen = buflen - n;
		n = recv(s, bufptr, buflen, 0);
	}
	
	if(n == SOCKET_ERROR)
		errexit("receiving error: %d\n", GetLastError());
	
	// Make sure there is a null character at
	// the end of the string.
	*bufptr = '\0';
}

void getAllResponse(int s) {
	/* The response from the server for the
	   ALL command may not fit into the buffer,
	   so it needs to be processed on the fly.
	*/
	int buflen = 1024, flag = 0;
	char *response = new char[buflen];
	char *bufptr = response;

	int n = recv(s, bufptr, buflen, 0);

	while(n != SOCKET_ERROR && n != 0) {
		bufptr = bufptr + n;
		buflen = buflen - n;

		// Untested code for handling
		// lookup tables with more than
		// 1024 characters:
		if(buflen < 512) {
			*bufptr = '\0';
			flag = printAll(response, flag);
			buflen = 1024;
			bufptr = response;
		}

		n = recv(s, bufptr, buflen, 0);
	}

	*bufptr = '\0';

	printAll(response, flag);

	cout<<endl;
	
	if(n == SOCKET_ERROR)
		errexit("receiving error: %d\n", GetLastError());
}

int printAll(char *response, int flag) {
	/* Handles printing for the ALL command.
	*/
	if(!flag) {
		char * strptr = strtok(response, " ");
		if(!strcmp(strptr, "OK")) {
			flag = 1;
			cout<<"Request was successful."<<endl
				<<"All key-value pairs follow:"<<endl<<endl
				<<strptr + strlen(strptr) + 1;
		}
		else {
			flag = 0;
			cout<<"Request failed: "<<strptr+6<<endl;
		}
	}
	else
		cout<<response;

	return flag;
}

void outputResponse(char *op, char *response) {
	/* Parse the string response containing the
	   response from the server. op is passed to
	   make decisions about parsing the response.
	*/
	// Start tokenizing the string; this first
	// one grabs the OK or ERROR flag.
	char *strptr = strtok(response, " ");
	
	if(!strcmp(strptr, "OK"))
		if(strcmp(op, "GET")) { // if the request was not a GET
			cout<<"Request was successful."<<endl;
			if(!strcmp(op, "ALL"))
				// Print out all key-value pairs
				// on an ALL operation.
				cout<<"All key-value pairs follow:"<<endl<<endl
					<<strptr + strlen(strptr) + 1<<endl;
		}
		else { // the request was a GET
			// Tokenize on the comma separating the
			// key and value.
			strptr = strtok(NULL, ",");
			cout<<"Returned value for key \""<<strptr<<"\": "
				<<strptr + strlen(strptr) + 1<<endl;
		}
	else if(!strcmp(strptr, "ERROR"))
		// On an ERROR, print out the server response.
		cout<<"Request failed: "<<strptr+6<<endl;
	else // the server did not return an OK or an ERROR
		errexit("unknown response from server: %s\n", response);
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
