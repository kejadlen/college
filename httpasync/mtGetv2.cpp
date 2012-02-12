/*************************************************************************
FileName: mtGetv2.cpp (Multi-threaded Get)

Multi-threaded client, which spawns num_threads threads and makes GET
requests for a file.

A "less aggressive" version of mtGet (called mtGetv2.cpp) 
that does not exhibit the connection-refused problem that occurs when 
the original mtGet runs on the same host as the server.
The problem is that the original mtGet bangs out so many connection 
requests when it first starts up that the system runs out of resources 
and new connections are refused (until mtGet slows down as the slave 
threads take up more cycles). This modified version of the original mtGet 
connects each socket sequentially, but sends the requests and retrieves the 
files concurrently. 

Library required: WS2_32.lib
Project -> Settings -> C/C++ -> Code generation to be changed to
Debug-Multithreaded.

References and Acknowledgements:
WinSock 2.0 by Lewis Napper
(Published by IDG Books)

--------------------------------------------------------------------------
Usage:
File to be retrieved from the http server has to be specified on the
command line

Example:
If the executable mtGet.exe exists in C:\ClientTest:

C:\ClientTest> mtGet www.freebsd.org /index.html > indexDump.html

The above command will spawn num_threads number of threads and make 
connections to www.freebsd.org. Each thread will retrieve the index.html
file and append the retrieved file to indexDump.html

For instance, if num_threads = 10, at the end of execution, you will
have 10 index.html files, one below the other and wholly contained in
indexDump.html

In the server-name field, you can alternatively choose localhost if
you want to connect to your server.
*************************************************************************/

#include <stdio.h>
#include <fcntl.h>
#include <io.h>
#include <winsock.h>
#include <time.h>
#include <process.h>
#include <stdlib.h>

#include <string>

using namespace std;

#define LF '\x0A'
#define CR '\x0D'

//If defined, the client will append the standard HTTP header to the GET request
//You may choose to "undef" this constant if you do not want to send the header
#define USE_HTTP_HEADER

//Prints the number of bytes received in each recv call
//#define DISPLAY_RECV_STATUS

//The number of threads to be spawned
//#define num_threads 14

// Structure to pass data to the thread
struct passData
{
	char serverName[100];
	char fileName[100];
};

struct httpData {
	SOCKET socket;
	string file;
};

void BuildCommon(struct passData *, SOCKADDR_IN *);
void HTTPGetRequest(struct httpData *);

// Helper macro for displaying errors
#define ErrNotify(s)	\
		fprintf(stderr,"\n%s: %d\n", s, WSAGetLastError())

void
errexit(const char *format, ...)
{
	va_list	args;

	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	WSACleanup();
	exit(1);
}


// Global memory for common HTTP request buffer
char szRequestBuffer[1024];


////////////////////////////////////////////////////////////

int main(int argc, char *argv[])
{
	WORD wVersionRequested = MAKEWORD(1,1);
	WSADATA wsaData;
	int nRet;
	SOCKET Socket;

	struct passData *ptrData;
	ptrData = (passData *)malloc(sizeof(struct passData));

	// Check arguments
//	if (argc < 3)
//	{
//		fprintf(stderr,
//			"\nSyntax: mtGet ServerName FullPathName\n");
//		return -1;
//	}

	//strcpy(ptrData->serverName, argv[1]);
	//strcpy(ptrData->fileName, argv[2]);
	strcpy(ptrData->serverName, "localhost");

	printf("%s\n", ptrData->serverName);
	
	int num_threads = atoi(argv[1]);

	HANDLE hThread[21];

	string fileNames[21];

	fileNames[0] = "/index.html";
	fileNames[1] = "/kingstrt.jpg";
	fileNames[2] = "/marina.jpg";
	fileNames[3] = "/pdf_sample.pdf";
	fileNames[4] = "/post_test.html";
	fileNames[5] = "/potomac.jpg";
	fileNames[6] = "/text_sample.txt";
	fileNames[7] = "/images/neb001.jpg";
	fileNames[8] = "/images/neb002.jpg";
	fileNames[9] = "/images/neb003.jpg";
	fileNames[10] = "/images/neb004.jpg";
	fileNames[11] = "/images/neb005.jpg";
	fileNames[12] = "/images/neb006.jpg";
	fileNames[13] = "/images/vt.gif";
	fileNames[14] = "/images/thm001.jpg";
	fileNames[15] = "/images/thm002.jpg";
	fileNames[16] = "/images/thm003.jpg";
	fileNames[17] = "/images/thm004.jpg";
	fileNames[18] = "/images/thm005.jpg";
	fileNames[19] = "/images/thm006.jpg";

	httpData *passToFunction[21];

	// Initialize WinSock
	nRet = WSAStartup(wVersionRequested, &wsaData);
	if (nRet)
	{
		fprintf(stderr,"\nWSAStartup(): %d\n", nRet);
		WSACleanup();
		return -1;
	}
	
	//
	// Check WinSock version
	//
	if (wsaData.wVersion != wVersionRequested)
	{
		fprintf(stderr,"\nWinSock version not supported\n");
		WSACleanup();
		return -1;
	}

	//
	// Set "stdout" to binary mode
	// so that redirection will work
	// for .gif and .jpg files
	//
	_setmode(_fileno(stdout), _O_BINARY);


	// Build common information
	SOCKADDR_IN saServer;
	BuildCommon(ptrData, &saServer);
		

	// Call HTTPGetRequest() as a separate thread to do the send and receive
	for (int i=0; i<num_threads; i++) 
	{
		// Create a TCP/IP stream socket
		Socket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
		if (Socket == INVALID_SOCKET)
			ErrNotify("socket()");

		else {
			// Connect the socket
			nRet = connect(Socket, (LPSOCKADDR)&saServer, sizeof(SOCKADDR_IN));
			if (nRet == SOCKET_ERROR)
			{
				ErrNotify("connect()");
				closesocket(Socket);
			}
			
			else {
				passToFunction[i] = new httpData;
				passToFunction[i]->file = fileNames[i];
				passToFunction[i]->socket = Socket;

				printf("Spawning thread %d\n", i);

				hThread[i] = (HANDLE) _beginthread((void (*)(void *))HTTPGetRequest, 0, (void *)passToFunction[i]);
				if (hThread[i] < 0) {
					ErrNotify("_beginthread()");
					closesocket(Socket);
				}
			}
		}

		//Flush buffers
		fflush(stdout);
		fflush(stderr);
	}

	//Wait for all threads to terminate
	WaitForMultipleObjects(num_threads, hThread, TRUE, INFINITE);

	WSACleanup();

	return 0;
}


////////////////////////////////////////////////////////////


void BuildCommon(struct passData *p, SOCKADDR_IN *psaServer)
{
	//
	// Use inet_addr() to determine if we're dealing with a name
	// or an address
	//
	IN_ADDR		iaHost;
	LPHOSTENT	lpHostEntry;

	//char lpServerName[100];
	//char lpFileName[100];

	//strcpy(lpServerName, p->serverName);
	//strcpy(lpServerName, "localhost");
	//strcpy(lpFileName, p->fileName);

	char *lpServerName = "localhost";

	iaHost.s_addr = inet_addr(lpServerName);
	if (iaHost.s_addr == INADDR_NONE)
	{
		// Wasn't an IP address string, assume it is a name
		lpHostEntry = gethostbyname(lpServerName);
	}
	else
	{
		// It was a valid IP address string
		lpHostEntry = gethostbyaddr((const char *)&iaHost, 
						sizeof(struct in_addr), AF_INET);
	}
	if (lpHostEntry == NULL)
	{
		ErrNotify("gethostbyname()");
		return;
	}

	//
	// Find the port number for the HTTP service on TCP
	//
	LPSERVENT lpServEnt;

	lpServEnt = getservbyname("http", "tcp");
	if (lpServEnt == NULL)
		psaServer->sin_port = htons(80);
	else
		psaServer->sin_port = lpServEnt->s_port;

	//
	// Fill in the rest of the server address structure
	//
	psaServer->sin_family = AF_INET;
	psaServer->sin_addr = *((LPIN_ADDR)*lpHostEntry->h_addr_list);

 	//
	// Format the HTTP request
	//

/* Standard HTTP header as produced by IE 6.0 can be used optionally*/
/* Client appends this header by default */

//#ifdef USE_HTTP_HEADER
//char *http_header = "HTTP/1.1\r\n\
//Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/vnd.ms-powerpoint, application/vnd.ms-excel, application/msword, */*\r\n\
//Accept-Language: en-us\r\n\
//Accept-Encoding: gzip, deflate\r\n\
//User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)\r\n\
//Host: localhost\r\n\
//Connection: Close\r\n\r\n";
//#endif //USE_HTTP_HEADER

	//sprintf(szRequestBuffer, "GET %s %s", lpFileName, http_header);
	//sprintf(szRequestBuffer, "%s", http_header);
}


////////////////////////////////////////////////////////////


void HTTPGetRequest(struct httpData *data)
{
	SOCKET Socket = data->socket;

	printf("Using socket %d, getting file %s\n", Socket, data->file.c_str());

#ifdef USE_HTTP_HEADER
char *http_header = "HTTP/1.1\r\n\
Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/vnd.ms-powerpoint, application/vnd.ms-excel, application/msword, */*\r\n\
Accept-Language: en-us\r\n\
Accept-Encoding: gzip, deflate\r\n\
User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)\r\n\
Host: localhost\r\n\
Connection: Close\r\n\r\n";
#endif //USE_HTTP_HEADER

	sprintf(szRequestBuffer, "GET %s %s", data->file.c_str(), http_header);

	char szBuffer[1024];
	int  nRet;

	nRet = send(Socket, szRequestBuffer, strlen(szRequestBuffer), 0);
	if (nRet == SOCKET_ERROR)
	{
		ErrNotify("send()");
		closesocket(Socket);
		return;
	}

	printf("GET %s sent\n", data->file.c_str());

	//
	// Receive the file contents and print to stdout
	//
	while(1)
	{
		// Wait to receive, nRet = NumberOfBytesReceived
		nRet = recv(Socket, szBuffer, sizeof(szBuffer), 0);
		if (nRet == SOCKET_ERROR)
		{
			printf("%s %d ", data->file.c_str(), Socket);
			ErrNotify("recv()");
			break;
		}

		//This fprintf statement may be disabled if you wish not to
		//observe how many bytes are being received

//#ifdef DISPLAY_RECV_STATUS
		//fprintf(stderr,"\nrecv() returned %d bytes", nRet);
//#endif /*DISPLAY_RECV_STATUS*/

		// Did the server close the connection?
		if (nRet == 0)
			break;
		// Write to stdout
        //fwrite(szBuffer, nRet, 1, stdout);
	}
	printf("Got file %s\n", data->file.c_str());
	closesocket(Socket);	

	_endthread();
}
