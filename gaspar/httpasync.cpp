/*

  (almost) HTTP/1.1 server
  Alpha Chen

  2004-10-25
	MasterSocket

*/

#ifdef _MSC_VER
#pragma warning( disable: 4786 )
#endif

#include <stdio.h>
#include <winsock2.h>
#include <io.h>
#include <fcntl.h>

#include <iostream>
#include <string>
#include <map>
#include <algorithm>

using namespace std;

// Constants
#define WSVERS      MAKEWORD(2,2)
#define BUFSIZE     4096
#define MAXSOCKS	60


// Macro to test if bit marked by b is set in v
#define BITSET(v,b)	(b & v)


// Request information
struct httpStats {
	string request;				// request from client

	string path;				// path to file/directory
	int keepAlive;				

	int fileFlag;
	long sendPos;
	string response;
};


// Function prototypes
u_short	MasterSocket();
int		AcceptConnection(int, int);
int		CloseConnection(int, int);

int		RecvData(int, int);
int		httpServer(int);
int		parseRequest(httpStats *);
int		parseRequestLine(string, httpStats *);
string	fixPath(string);
void	makeResponse(httpStats *);
string	pathExtension(string);
int		fileExistsP(httpStats *);

int		SendData(int, int);
int		sendString(int, string);
int		sendString(int, char *, int);

void	errexit(const char *, ...);
void	DisplayEvents(long, int);


// State definitions for client connections
#define ST_READING 1			// awaiting a read
#define ST_WRITING 2			// awaiting a write, client still sending
#define ST_CLOSING 3			// awaiting a write, client closed connection


// Per socket information
struct info {
	SOCKET sock;				// associated socket (master socket too)
	int	state;					// connection state
	char buf[BUFSIZE];			// data buffer
	char * bufptr;				// buffer pointer
	int buflen;					// bytes in buffer

	int closedConnection;		// flag to see whether connection has been closed or not

	httpStats *requestStats;	// data for each request

	LARGE_INTEGER startTime;	// time the server finishes getting the client request
	LARGE_INTEGER stopTime;		// time the server finishes servicing the client
};

LARGE_INTEGER freq;				// frequency for benchmarking
double totalTime;				//
double totalRequests;			//

struct info * Socks[MAXSOCKS];	// array of info structure pointers
WSAEVENT Events[MAXSOCKS];		// array of event handles
int NumSocks;					// number of active event handles and sockets
map<string, string> contentTypes;	// content type map for server response



////////////////////
// Main procedure

void main(int argc, char *argv[])
{
	u_short				port;				// port
	int					SigEvent;			// signaled event index
	int					closedConnection;	//
	WSANETWORKEVENTS	NetEvents;			// structure for network event information


	// Create master socket and set for asynchronous event notification.
	port = MasterSocket();
	printf("Asynchronous HTTP server listening on port %d\n", port);
	
	// Set up Content-Type map
	contentTypes["html"]	= "text/html";
	contentTypes["htm"]		= "text/html";
	contentTypes["txt"]		= "text/plain";
	contentTypes["jpeg"]	= "image/jpeg";
	contentTypes["jpg"]		= "image/jpeg";
	contentTypes["gif"]		= "image/gif";
	contentTypes["pdf"]		= "application/pdf";

	// Set up statistics
	QueryPerformanceFrequency(&freq);
	totalTime = 0;
	totalRequests = 0;

	// Infinite server loop follows.
	while (1) {
		closedConnection = 0;

		// Wait for any event to be signalled.
		// For the master socket, this can only be triggered by ACCEPT.
		// For a slave socket, this can be triggered by READ, WRITE or CLOSE.
		// This call will block as long as needed for signal to occur.
		if((SigEvent = WSAWaitForMultipleEvents(NumSocks, Events, FALSE, WSA_INFINITE, TRUE)) == WSA_WAIT_FAILED)
			errexit("Wait for event failed: %d\n", WSAGetLastError());

		// One socket, indicated by SigEvent, has been signalled for one of
		// the event types.  The following call determines the event type or types.
		// See WSAEnumNetworkEvents() document for a description of the NetEvents
		// structure.
		if(WSAEnumNetworkEvents(Socks[SigEvent]->sock, Events[SigEvent], &NetEvents) == SOCKET_ERROR)
			errexit("Enumeration of network events failed: %d\n", WSAGetLastError());

		// The following call is for illustrative purposes only.  It displays
		// the event types that are ready.
		//DisplayEvents(NetEvents.lNetworkEvents, SigEvent);
  		
		// Check for accepted connection.  This will only occur for master socket.
		if (BITSET(NetEvents.lNetworkEvents,FD_ACCEPT))
			closedConnection = AcceptConnection(SigEvent, NetEvents.iErrorCode[FD_ACCEPT_BIT]);

		// Check for data ready to be received.  This will only occur for a
		// slave socket.  Note that this event type will be signalled when data
		// is available to be received.  It will be retriggered anytime new
		// data arrives after a receive is executed or if the receive does not
		// read all available data.
		if(!closedConnection && BITSET(NetEvents.lNetworkEvents,FD_READ))
			closedConnection = RecvData(SigEvent, NetEvents.iErrorCode[FD_READ_BIT]);

		// Check for data ready to be sent.  This will occur only for a slave
		// socket.  Note that this event type is signalled after a connection
		// is first accepted and, after that, only if a send blocks.  So,
		// data is sent in the RecvData() routine and the ST_WRITING state is
		// entered only if a send blocks.
		if(!closedConnection && BITSET(NetEvents.lNetworkEvents,FD_WRITE))
			closedConnection = SendData(SigEvent, NetEvents.iErrorCode[FD_READ_BIT]);

		// Check for client closing connection.  This will only occur for a
		// slave socket.  Note that this event type occurs if the client
		// closes or shuts down their socket.  CloseConnection() will close
		// the connection from the server side only if not in the ST_WRITING
		// state (i.e., only if there is no more data to write to the client).
		if(!closedConnection && BITSET(NetEvents.lNetworkEvents,FD_CLOSE))
			CloseConnection(SigEvent, NetEvents.iErrorCode[FD_READ_BIT]);
	}

} // main()



////////////////////////////////////////////////////////
// Create master socket for event-driven notification

u_short MasterSocket()
{
	WSADATA	wsadata;			// sockets startup data
	struct sockaddr_in sin;		// an Internet endpoint address
	SOCKET temp_sock;			// temporary socket value

	// Start WinSock (standard call).
	if (WSAStartup(WSVERS, &wsadata) != 0)
		errexit("WSAStartup failed\n");

    // Allocate a socket (standard call).
	if ((temp_sock = socket(PF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET)
		errexit("Error creating socket: %d\n", GetLastError());

	// Create event object to be set by asychronous events.
	// Note that master socket is entry 0 in Events[] and Socks[] arrays.
	if ((Events[0] = WSACreateEvent()) == WSA_INVALID_EVENT)
		errexit("Error creating event object: %d\n", WSAGetLastError());
	
	// Make socket non-blocking with asynchronous event notification for ACCEPT.
	// This call associates the event handle created above with the master socket.
	// Master socket will be "signaled" when call to accept() will not block.
	if (WSAEventSelect(temp_sock, Events[0], FD_ACCEPT) == SOCKET_ERROR)
		errexit("Error initiating asynchronous event notification: %d\n", WSAGetLastError());

	// Initialize info structure for master socket.
	// Again, note that master socket is entry 0 in Socks[] array.
	if ((Socks[0] = (struct info *) malloc(sizeof(struct info))) == NULL)
		errexit("Memory alloation error\n");

	Socks[0]->sock = temp_sock;
	NumSocks = 1;

	// Create address for master socket (standard code).
	memset(&sin, 0, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;

	if ((sin.sin_port = htons(80)) == 0)
		errexit("Can't get service entry");

	// Bind address to the socket (standard call).
	if (bind(temp_sock, (struct sockaddr *)&sin, sizeof(sin)) == SOCKET_ERROR)
		errexit("can't bind to port: %d\n", WSAGetLastError());
	
	// Make socket passive (standard call).
	if(listen(temp_sock, 5) == SOCKET_ERROR)
		errexit("Error listening on port: %d\n", WSAGetLastError());

	return(ntohs(sin.sin_port));

} // end MasterSocket()



////////////////////////////////////////////////////////////
// Accept a connection (only in phantom "awaiting" state)

int AcceptConnection(int EventNum, int EventError)
{
	struct sockaddr_in new_sin;	// client address for accept
	int addrlen;				// length of client address
	SOCKET temp_sock;			// temporary socket descriptor


	// Check for error in event notification.
	if (EventError != 0) {
		printf("Accept error:  %d\n", EventError);
		return -1;
	}

	// Accept the connection (standard call).
	addrlen = sizeof(new_sin);
	if ((temp_sock = accept(Socks[EventNum]->sock, (struct sockaddr *) &new_sin, &addrlen)) == INVALID_SOCKET) {
		printf("Accept event error: %d\n", WSAGetLastError());
		return -1;
	}
	
	// Check if new connection exceeds server capacity.  The limit is
	// due to Events[] and Socks[] being fixed-size arrays.  An excess
	// client connection is simply closed. 
	if (NumSocks >= MAXSOCKS) {
		printf("Too many connections - aborting new connection\n");
		closesocket(temp_sock);
		return -1;
	}

	// The client connection can be serviced.  Information for this connection
	// is saved in an info structure pointed to by an entry in the Socks[]
	// array.  Note that the connection starts in the ST_READING state since
	// data is first to be read from an ECHO client.
	if ((Socks[NumSocks] = (struct info *) malloc(sizeof(struct info))) == NULL) {
		printf("Memory alloation error\n");
		closesocket(temp_sock);
		return -1;
	}
	Socks[NumSocks]->sock = temp_sock;
	Socks[NumSocks]->state = ST_READING;
	Socks[NumSocks]->bufptr = Socks[NumSocks]->buf;
	Socks[NumSocks]->buflen = 0;
	Socks[NumSocks]->requestStats = new httpStats;


	// Create event object to be set by asychronous events for new socket.
	// Note that the event handle is in the same location in Events[] as
	// connection information is in Socks[].
	if ((Events[NumSocks] = WSACreateEvent()) == WSA_INVALID_EVENT) {
		printf("Error creating event object: %d\n", WSAGetLastError());
		closesocket(temp_sock);
		return -1;
	}

	// Make socket non-blocking with asynchronous event notification.  This
	// call associates the event handle just created with the slave socket
	// and tells WinSock to notify for READ and CLOSE events.
	if (WSAEventSelect(Socks[NumSocks]->sock, Events[NumSocks], FD_READ | FD_CLOSE) == SOCKET_ERROR) {
		printf("Error initiating asynchronous event notification: %d\n", WSAGetLastError());
		closesocket(temp_sock);
		return -1;
	}

	//printf("Status: new connection accepted (%d)\n", NumSocks);
	++NumSocks;
	return 0;

} // end AcceptConnection()



//////////////////////////////////////////////////////////
// Receive data from a client (only in "reading" state)

int RecvData(int EventNum, int EventError)
{
	int nbytes;

	// Check for error in event notification.
	if (EventError != 0) {
		printf("Read event error:  %d\n", EventError);
		CloseConnection(EventNum, 0);
		return -1;
	}

	// Call recv as long as there is data left to receive or until the
	// recv would block or encounters another error.
	do {
		nbytes = recv(Socks[EventNum]->sock, Socks[EventNum]->bufptr, BUFSIZE-Socks[EventNum]->buflen, 0);
		if(nbytes > 0) {
			Socks[EventNum]->bufptr += nbytes;
			Socks[EventNum]->buflen += nbytes;		
		}
	} while (nbytes != 0 && nbytes != SOCKET_ERROR && !strstr(Socks[EventNum]->buf, "\r\n\r\n"));

	if(nbytes == SOCKET_ERROR && WSAGetLastError() == WSAEWOULDBLOCK)
		return 0;

	if(nbytes == SOCKET_ERROR) {
		printf("Read error: %d\n", WSAGetLastError());
		Socks[EventNum]->state = ST_CLOSING;
		CloseConnection(EventNum, 0);
		return -1;
	}

	// "end" the string
	*Socks[EventNum]->bufptr = '\0';

	// Check that the entire header has been sent
	if(strstr(Socks[EventNum]->buf, "\r\n\r\n")) {
		//cout<<Socks[EventNum]->buf<<endl;
		//Socks[EventNum]->startTime = GetTickCount();
		QueryPerformanceCounter(&Socks[EventNum]->startTime);

		httpStats *data = Socks[EventNum]->requestStats;

		// Reset info about request
		data->request = Socks[EventNum]->buf;
		data->response = "";

		if(parseRequest(data) && !data->response.empty())
			data->response = "400 Bad Request";
		//cout<<"\"GET "<<data->path<<"\" received."<<endl;
		data->fileFlag = !fileExistsP(data);
		makeResponse(data);

		// Send response to client
		data->sendPos = 0;
		if(SendData(EventNum, 0))
			return -1;

		// Reset connection info
		Socks[EventNum]->buf[0] = '\0';
		Socks[EventNum]->bufptr = Socks[NumSocks]->buf;
		Socks[EventNum]->buflen = 0;
	}

	return 0;

} // end RecvData()


/*

  Parses the request from the client for the PATH,
  CONNECTION, and HOST. If the request is malformed,
  returns 1.

*/
int parseRequest(httpStats *requestStats) {
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

		i = j;
	}

	return 0;
}

/*

  Parses the request line to get the method, path, and protocol.

*/	
int parseRequestLine(string requestLine, httpStats *requestStats) {
	// Make sure the method is GET
	if(requestLine.length() < 4 || ("GET " != requestLine.substr(0, 4))) {
		requestStats->response = "501 Not Implemented";
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

/*

  Changes all instances of %20 in the path to spaces.

*/
string fixPath(string path) {
	int i; string temp;

	while((i = path.find("%20")) != string::npos) {
		temp = path.substr(0, i) + " ";
		path = temp + path.substr(i+3,path.length()-3-i);
	}

	return path;
}

/*

  Returns 0 if the file exists and is readable. If not,
  the serverResponse is set to the appropriate response.

*/
int fileExistsP(httpStats *requestStats) {
	string origPath = requestStats->path, path = "c:/webfiles" + origPath;
	requestStats->path = path;
	int fd= _open(path.c_str(), _O_RDONLY | _O_BINARY);

	// Check for existence
	if(_access(path.c_str(), 0)) {
		requestStats->response = "404 Not Found";
		return 1;
	}

	// Check for readability
	if(_access(path.c_str(), 4)) {
		requestStats->response = "500 Internal Server Error";
		return 1;
	}

	// Check for directory
	if(_filelength(fd) == -1) {
		_close(fd);
		if(path[path.length()-1] != '/') {
			requestStats->response = "404 File Not Found";
			return 1;
		}

		// index.html
		requestStats->path = origPath + "index.html";
		if(!fileExistsP(requestStats))
			return 0;

		// index.htm
		requestStats->path = origPath = "index.htm";
		if(!fileExistsP(requestStats))
			return 0;

		requestStats->response = "500 Internal Server Error";
		return 1;
	}

	_close(fd);
	return 0;
}

/*

  Generate the status-lines sent by the server

*/
void makeResponse(httpStats *requestStats) {
	string temp = "200 OK", html = "";
	char intBuffer[sizeof(int)*8 + 1];
	long length;
	string contentType;
	
	// If the serverResponse has already been set,
	// send that response with an error page
	if(!requestStats->response.empty()) {
		temp = requestStats->response;

		// Generate a small error page
		html = "<html><head><title>" + temp;
		html += "</title</head><body>" + temp;
		html += "<br /><pre>" + requestStats->request;
		html += "</pre></body></html>";

		length = html.length();
		contentType = "text/html";
	}
	else {
		int fd = _open(requestStats->path.c_str(), _O_RDONLY | _O_BINARY);
		length = _filelength(fd);
		_close(fd);

		contentType = pathExtension(requestStats->path);
	}

	// Generate the actual response; note that
	// the html goes after the double CRLF
	temp = "HTTP/1.1 " + temp + "\r\n";
	temp += "Content-Length: ";
	temp += itoa(length, intBuffer, 10);
	temp += "\r\nContent-Type: " + contentType;
	temp += "\r\n\r\n" + html;

	requestStats->response = temp;
}

/*

  Returns the content-type of a file from
  the filename's extension.
*/
string pathExtension(string path) {
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


///////////////////////////////////////////////////////////////////////////////
// Send data to a client (from state (only in "writing" or "closing" states)

int SendData(int EventNum, int EventError)
{
	int temp=0;		// number of bytes sent

	// Check for error in event notification
	if (EventError != 0) {
		printf("Write event error:  %d\n", EventError);
		Socks[EventNum]->state = ST_CLOSING;
		CloseConnection(EventNum, 0);
		return -1;
	}

	// Send response
	httpStats *data = Socks[EventNum]->requestStats;

	if(data->response.length()) {
		temp = sendString(EventNum, data->response);
		if(temp > 0)
			data->response = data->response.substr(temp);
	}
	if(temp < 0)
		return temp;

	int i=0, j=0;
	char sendBuffer[4097];

	// Send file
	if(data->fileFlag) {
		int fd = _open(data->path.c_str(), _O_RDONLY | _O_BINARY);

		_lseek(fd, data->sendPos, 0);

		while((i == j) && data->sendPos != _filelength(fd) && (i = _read(fd, sendBuffer, BUFSIZE)) > 0) {
			if((j = sendString(EventNum, sendBuffer, i)) < 0)
				return -1;
			//cout<<i<<","<<j<<endl;
			data->sendPos += j;
			_lseek(fd, data->sendPos, 0);
		}

		_close(fd);

		if(i != j)
			return 0;

		if(i < 0) {
			cout<<"Problem reading file "<<data->path<<endl;
			return -1;
		}
	}

	//cout<<data->path<<" sent"<<endl;

	//Socks[EventNum]->stopTime = GetTickCount();
	QueryPerformanceCounter(&Socks[EventNum]->stopTime);
	totalTime += 1000*(Socks[EventNum]->stopTime.QuadPart - Socks[EventNum]->startTime.QuadPart)/(double)freq.QuadPart;
	cout<<totalTime/++totalRequests<<endl;

	//cout<<Socks[EventNum]->startTime<<endl<<Socks[EventNum]->stopTime<<endl;
	//cout<<Socks[EventNum]->stopTime - Socks[EventNum]->startTime<<endl;

	if(data->keepAlive) {
		if(WSAEventSelect(Socks[EventNum]->sock, Events[EventNum], FD_READ | FD_CLOSE) == SOCKET_ERROR) {
			printf("Error resetting asynchronous event notification: %d\n", WSAGetLastError());
			Socks[EventNum]->state = ST_CLOSING;
			CloseConnection(EventNum, 0);
			return -1;
		}
	}
	else {
		Socks[EventNum]->state = ST_CLOSING;
		CloseConnection(EventNum, 0);
		return 1;
	}

	return 0;

} // end SendData()



/*

  Sends a string to a socket.

*/
int sendString(int EventNum, string buffer) {
	int nbytes, length = buffer.length(), sent = 0;

	do {
		nbytes = send(Socks[EventNum]->sock, buffer.c_str(), length, 0);
		if(nbytes > 0) {
			sent += nbytes;
			buffer = buffer.substr(nbytes);
			length -= nbytes;
		}
	} while(nbytes != SOCKET_ERROR && length > 0);

	if(nbytes == SOCKET_ERROR) {
		if(WSAGetLastError() != WSAEWOULDBLOCK) {
			printf("Write error: %d\n", WSAGetLastError());
			Socks[EventNum]->state = ST_CLOSING;
			CloseConnection(EventNum, 0);
			return -1;
		}

		else if(Socks[EventNum]->requestStats->response.length()) {
			if(WSAEventSelect(Socks[EventNum]->sock, Events[EventNum], FD_WRITE | FD_CLOSE) == SOCKET_ERROR) {
				printf("Error resetting asynchronous event notification: %d\n", WSAGetLastError());
				Socks[EventNum]->state = ST_CLOSING;
				CloseConnection(EventNum, 0);
				return -1;
			}
		}
	}

	return sent;
}

int sendString(int EventNum, char * buffer, int length) {
	int nbytes, sent = 0;
	char *bufptr = buffer;

	do {
		nbytes = send(Socks[EventNum]->sock, bufptr, length, 0);
		//cout<<nbytes<<endl;
		if(nbytes > 0) {
			bufptr += nbytes;
			sent += nbytes;
			length -= nbytes;
		}
	} while(nbytes != SOCKET_ERROR && length > 0);

	if(nbytes == SOCKET_ERROR) {
		if(WSAGetLastError() != WSAEWOULDBLOCK) {
			printf("Write error: %d\n", WSAGetLastError());
			Socks[EventNum]->state = ST_CLOSING;
			CloseConnection(EventNum, 0);
			return -1;
		}

		else if(length) {
			if(WSAEventSelect(Socks[EventNum]->sock, Events[EventNum], FD_WRITE | FD_CLOSE) == SOCKET_ERROR) {
				printf("Error resetting asynchronous event notification: %d\n", WSAGetLastError());
				Socks[EventNum]->state = ST_CLOSING;
				CloseConnection(EventNum, 0);
				return -1;
			}
		}
	}

	return sent;
}



////////////////////////////////////////////////////////////
// Close a client connection (potentially from any state)

int CloseConnection(int EventNum, int EventError)
{
	int i;

	// If still data to send and no error, allow writing, and change state.
	if (Socks[EventNum]->state == ST_WRITING && EventError == 0) {
		Socks[EventNum]->state = ST_CLOSING;
		SendData(EventNum, 0);
	}

	// Else, report error, if any, and free resources.
	else {
		if (EventError != 0)
			printf("Close event error:  %d\n", EventError);
		
		WSACloseEvent(Events[EventNum]);
		closesocket(Socks[EventNum]->sock);
		free(Socks[EventNum]);

	// Delete this event and information pointer from arrays.
		for (i = EventNum + 1; i < NumSocks; ++i) {
			Events[i-1] = Events[i];
			Socks[i-1] = Socks[i];
		}
		--NumSocks;

		//printf("Status: connection closed (%d)\n", EventNum);
	}

	return 0;
} // end CloseConnection()




////////////////////////////////////////////////////////////////////////////
// Error exit routine as in Comer and Stevens, Vol. III (WinSock edition)

void errexit(const char *format, ...)
{
	va_list	args;

	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	WSACleanup();
	exit(1);

} // end errexit()



////////////////////////////////////////
// Display network events that are ready

void DisplayEvents(long events, int EventNum)
{
	printf("Event %d signalled for: ", EventNum);

	if (BITSET(FD_READ, events)) printf(" READ");
	if (BITSET(FD_WRITE, events)) printf(" WRITE");
	if (BITSET(FD_OOB, events)) printf(" OOB");
	if (BITSET(FD_ACCEPT, events)) printf(" ACCEPT");
	if (BITSET(FD_CONNECT, events)) printf(" CONNECT");
	if (BITSET(FD_CLOSE, events)) printf(" CLOSE");
	if (BITSET(FD_QOS, events)) printf(" QOS");
	if (BITSET(FD_GROUP_QOS, events)) printf(" GROUP_QOS");

	printf("\n");

} // end DisplayEvents()