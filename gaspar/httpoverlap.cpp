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

#define WSVERS		MAKEWORD(2,2)
#define BUFSIZE		4096
#define MAXSOCKS	50

struct httpStats {
	string request;				// request from client

	string path;				// path to file/directory
	int keepAlive;				

	int fileFlag;
	long sendPos;
	string response;
};

void	MasterSocket();
int		AcceptConnection(int);

int		RecvData(int);
int		httpServer(int);
int		parseRequest(httpStats *);
int		parseRequestLine(string, httpStats *);
string	fixPath(string);
void	makeResponse(httpStats *);
string	pathExtension(string);
int		fileExistsP(httpStats *);

int		SendData(int);
long	sendString(int);

int		CloseConnection(int);

void	errexit(const char *, ...);

// State definitions for client connections
#define ST_READING 1			// awaiting a read
#define ST_WRITING 2			// awaiting a write, client still sending

// Macro to test if bit marked by b is set in v
#define BITSET(v,b)	(b & v)

struct info {
	SOCKET	sock;				// associated socket
	int		state;				// connection state
	WSABUF	buf;				// writing/reading buffer

	httpStats *requestStats;	// data for request

	LARGE_INTEGER startTime;	// time the server finishes getting the client request
	LARGE_INTEGER stopTime;		// time the server finishes servicing the client
};

LARGE_INTEGER freq;
double totalTime;
double totalRequests;

struct info * Socks[MAXSOCKS];		// array of info structure pointers
WSAEVENT Events[MAXSOCKS];			// array of event handles
WSAOVERLAPPED Overlapped[MAXSOCKS];	// array of overlapped structures
int NumSocks;						// number of active event handles and sockets
map<string, string> contentTypes;	// content type map for server response

void main(int argc, char *argv[]) {
	int			SigEvent;
	WSANETWORKEVENTS	NetEvents;

	MasterSocket();
	cout<<"Overlapped HTTP server listening on port 80"<<endl;

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

	while (1) {
		//cout<<"NumSocks: "<<NumSocks<<endl;

		if((SigEvent = WSAWaitForMultipleEvents(NumSocks, Events, FALSE, WSA_INFINITE, FALSE)) == WSA_WAIT_FAILED)
			printf("Wait for event failed: %d\n", WSAGetLastError());

		//cout<<"SigEvent: "<<SigEvent<<endl;

		if(!WSAResetEvent(Events[SigEvent])) {
			cout<<"Can't reset event: "<<WSAGetLastError()<<endl;
			CloseConnection(SigEvent);
		}

		if(SigEvent == 0) {
			if(WSAEnumNetworkEvents(Socks[SigEvent]->sock, Events[SigEvent], &NetEvents) == SOCKET_ERROR)
				errexit("Enumeration of network events failed: %d\n", WSAGetLastError());

			if(BITSET(NetEvents.lNetworkEvents,FD_ACCEPT)) {
				AcceptConnection(SigEvent);
			}
		}

		else if(Socks[SigEvent]->state == ST_READING) {
			//cout<<"Event "<<SigEvent<<" reading"<<endl;
			RecvData(SigEvent);
		}

		else if(Socks[SigEvent]->state == ST_WRITING) {
			//cout<<"Event "<<SigEvent<<" writing"<<endl;
			SendData(SigEvent);
		}
	}
}

/*

  Create master socket for event-driven notification

  */
void MasterSocket() {
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
}

////////////////////////////////////////////////////////////
// Accept a connection (only in phantom "awaiting" state)

int AcceptConnection(int EventNum)
{
	struct sockaddr_in new_sin;	// client address for accept
	int addrlen;				// length of client address
	SOCKET temp_sock;			// temporary socket descriptor

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
	Socks[NumSocks]->buf.buf = new char[BUFSIZE];
	Socks[NumSocks]->buf.len = BUFSIZE;
	Socks[NumSocks]->requestStats = new httpStats;
	Socks[NumSocks]->requestStats->request = "";

	// Create event object to be set by asychronous events for new socket.
	// Note that the event handle is in the same location in Events[] as
	// connection information is in Socks[].
	if ((Events[NumSocks] = WSACreateEvent()) == WSA_INVALID_EVENT) {
		printf("Error creating event object: %d\n", WSAGetLastError());
		closesocket(temp_sock);
		return -1;
	}
	Overlapped[NumSocks].hEvent = Events[NumSocks];

	//printf("Status: new connection accepted (%d)\n", NumSocks);

	int		rc;		// receive code
	int		tempSock = NumSocks;
	DWORD	num;		// number of bytes received
	DWORD	recv_flags = 0;	// receive flags

	++NumSocks;

	Socks[tempSock]->state = ST_READING;
	rc = WSARecv(Socks[tempSock]->sock, &Socks[tempSock]->buf, 1, &num, &recv_flags, &Overlapped[tempSock], NULL);
	if(rc == SOCKET_ERROR && WSAGetLastError() != WSA_IO_PENDING) {
		printf("Read error: %d\n", WSAGetLastError());
		CloseConnection(tempSock);
		return -1;
	}

	return 0;

} // end AcceptConnection()

//////////////////////////////////////////////////////////
// Receive data from a client (only in "reading" state)

int RecvData(int EventNum)
{
	int		rc;
	DWORD	num;		// number of bytes received
	DWORD	flags=0;
	DWORD	dummy_flags;

	if(!WSAGetOverlappedResult(Socks[EventNum]->sock, &Overlapped[EventNum], &num, TRUE, &dummy_flags)) {
		printf("Can't get results for recv: %d\n", WSAGetLastError());
		CloseConnection(EventNum);
		return -1;
	}

	if(num == 0) {
		CloseConnection(EventNum);
		return 1;
	}

	httpStats *data = Socks[EventNum]->requestStats;
	Socks[EventNum]->buf.buf[num] = '\0';
	data->request += Socks[EventNum]->buf.buf;

	if(data->request.find("\r\n\r\n") != string::npos) {
		QueryPerformanceCounter(&Socks[EventNum]->startTime);

		data->response = "";

		if(parseRequest(data) && !data->response.empty())
			data->response = "400 Bad Request";
		//cout<<"\"GET "<<data->path<<"\" received. ("<<EventNum<<")"<<endl;

		data->fileFlag = (fileExistsP(data)) ? 0 : BUFSIZE ;
		makeResponse(data);

		data->sendPos = 0;

		strcpy(Socks[EventNum]->buf.buf, data->response.c_str());
		Socks[EventNum]->buf.len = data->response.length();

		Socks[EventNum]->state = ST_WRITING;

		rc = WSASend(Socks[EventNum]->sock, &Socks[EventNum]->buf, 1, &num, 0, &Overlapped[EventNum], NULL);
		if(rc == SOCKET_ERROR && WSAGetLastError() != WSA_IO_PENDING) {
			printf("Send error: %d\n", WSAGetLastError());
			CloseConnection(EventNum);
			return -1;
		}

		return 0;
	}

	Socks[EventNum]->state = ST_READING;
	Socks[EventNum]->buf.len = BUFSIZE; 

	rc = WSARecv(Socks[EventNum]->sock, &Socks[EventNum]->buf, 1, &num, &flags, &Overlapped[EventNum], NULL);
	if(rc == SOCKET_ERROR && WSAGetLastError() != WSA_IO_PENDING) {
		printf("Read error: %d\n", WSAGetLastError());
		CloseConnection(EventNum);
		return -1;
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

	if(_filelength(fd) == -1) {
		_close(fd);
		if(path[path.length()-1] != '/') {
			requestStats->response = "404 File Not Found";
			return 1;
		}

		requestStats->path = origPath + "index.html";
		if(!fileExistsP(requestStats))
			return 0;

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

int SendData(int EventNum)
{
	int		rc;		// receive code
	DWORD	num;		// number of bytes received
	DWORD	flags = 0;	// receive flags

	DWORD	dummy_flags;

	// Send response
	httpStats *data = Socks[EventNum]->requestStats;

	if(!WSAGetOverlappedResult(Socks[EventNum]->sock, &Overlapped[EventNum], &num, TRUE, &dummy_flags)) {
		printf("Can't get results for recv: %d\n", WSAGetLastError());
		CloseConnection(EventNum);
		return -1;
	}

	if(data->response.length()) {
		data->response = data->response.substr(num);

		if(data->response.length()) {
			strcpy(Socks[EventNum]->buf.buf, data->response.c_str());
			Socks[EventNum]->buf.len = data->response.length();

			rc = WSASend(Socks[EventNum]->sock, &Socks[EventNum]->buf, 1, &num, 0, &Overlapped[EventNum], NULL);
			if(rc == SOCKET_ERROR && WSAGetLastError() != WSA_IO_PENDING) {
				printf("Send error: %d\n", WSAGetLastError());
				CloseConnection(EventNum);
				return -1;
			}
			return 0;
		}

		data->sendPos = BUFSIZE - num;
//		cout<<"Sent header for: "<<data->path<<endl<<data->response;
	}

	int i=0;

	// Send file
	if(data->fileFlag) {
		if(num && num != data->fileFlag)
			data->sendPos = data->sendPos - data->fileFlag + num;

		int fd = _open(data->path.c_str(), _O_RDONLY | _O_BINARY);

		_lseek(fd, data->sendPos, 0);

		//while(data->sendPos != _filelength(fd) && (i = _read(fd, Socks[EventNum]->buf.buf, BUFSIZE)) > 0) {
		if(data->sendPos != _filelength(fd) && (i = _read(fd, Socks[EventNum]->buf.buf, BUFSIZE)) > 0) {
			Socks[EventNum]->buf.len = i;
			data->fileFlag = i;
			if(sendString(EventNum) < 0)
				return -1;
			//cout<<"Sending file : "<<data->path<<" ("<<EventNum<<") ("<<i<<")"<<endl;
			data->sendPos += i;
			_lseek(fd, data->sendPos, 0);
		}

		if(data->sendPos != _filelength(fd)) {
			_close(fd);
			return 0;
		}

		_close(fd);

		if(i < 0) {
			cout<<"Problem reading file "<<data->path<<endl;
			return -1;
		}
	}

	data->fileFlag = 0;
//	cout<<"Sent file: "<<data->path<<" ("<<EventNum<<")"<<endl<<endl;
	QueryPerformanceCounter(&Socks[EventNum]->stopTime);
	totalTime += 1000*(Socks[EventNum]->stopTime.QuadPart - Socks[EventNum]->startTime.QuadPart)/(double)freq.QuadPart;
	cout<<totalTime/++totalRequests<<endl;

	if(data->keepAlive) {
		data->request = "";

		Socks[EventNum]->state = ST_READING;
		Socks[EventNum]->buf.len = BUFSIZE; 

		rc = WSARecv(Socks[EventNum]->sock, &Socks[EventNum]->buf, 1, &num, &flags, &Overlapped[EventNum], NULL);
		if(rc == SOCKET_ERROR && WSAGetLastError() != WSA_IO_PENDING) {
			printf("Read error: %d\n", WSAGetLastError());
			CloseConnection(EventNum);
			return -1;
		}
	}
	else
		CloseConnection(EventNum);

	return 0;

} // end SendData()

long sendString(int EventNum) {
	int		rc;
	DWORD	num;

	rc = WSASend(Socks[EventNum]->sock, &Socks[EventNum]->buf, 1, &num, 0, &Overlapped[EventNum], NULL);
	if(rc == SOCKET_ERROR && WSAGetLastError() != WSA_IO_PENDING) {
		cout<<"Write error: "<<WSAGetLastError()<<endl;
		CloseConnection(EventNum);
		return -1;
	}

	return num;
}

////////////////////////////////////////////////////////////
// Close a client connection (potentially from any state)

int CloseConnection(int EventNum)
{
	int i;

	WSACloseEvent(Events[EventNum]);
	closesocket(Socks[EventNum]->sock);
	free(Socks[EventNum]);

	// Delete this event and information pointer from arrays.
	for (i = EventNum + 1; i < NumSocks; ++i) {
		Events[i-1] = Events[i];
		Socks[i-1] = Socks[i];
		Overlapped[i-1] = Overlapped[i];
	}
	--NumSocks;

	//printf("Status: connection closed (%d)\n", EventNum);

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
