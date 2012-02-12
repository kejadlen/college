//#include <stdio.h>
//#include <stdarg.h>
#include <winsock2.h>
#include <process.h>
#include <windows.h>
#include <ws2tcpip.h>

#include <iostream>
#include <vector>
#include <string>

using namespace std;

// Constants.
#define WSVERS	MAKEWORD(2,2)
#define	DESTINATION_PORT	4564	// default multicast port
#define DEFAULT_TTL	1

// for screen output
#define	RED	FOREGROUND_RED
#define GREEN	FOREGROUND_GREEN
#define	BLUE	FOREGROUND_BLUE
#define	WHITE	RED|GREEN|BLUE

// To pass arguments to getInput
struct getNetworkInputArgs {
	SOCKET	McastSock;
};

// User data
struct userData {
	string user;
	string ip;
};

// Function prototypes.
void	initNetworking(void);
void	initScreen(void);
void	ScrollOutputBuffer(void);

void	getNetworkInput(void *_args);
void	addNewUser(char *user, char *ip);

void	ProcessLine(string input);
void	joinCommand(string input);
void	joinGroup(void);
void	outputUserList(void);
void	departGroup(void);

void	chatOutput(char *output, WORD color);
void	chatOutput(string output, WORD color);

void	errexit(const char *, ...);

// Globals.
int port, ttl;
string group, ip;

int connected;

vector<userData> userlist;

SOCKET McastSock;
struct sockaddr_in dest;

HANDLE recvThread;

HANDLE hStdout, hStdin;
CONSOLE_SCREEN_BUFFER_INFO csbiInfo;

// Procedure main().
void main(int argc, char *argv[])
{
	WSADATA		wsadata;		// returned startup data
	char		chBuffer;
	DWORD		cRead, cWritten;
	char		buf[256];
	hostent*	host;
	string		input;			// holds input from screen buffer

	// Set defaults
	port = DESTINATION_PORT;
	ttl = DEFAULT_TTL;
	group = "234.5.6.7";
	connected = 0;
	userData newUser;
	newUser.user = "John Doe";
	newUser.ip = "";

	// userlist[0] is always the originating program
	userlist.push_back(newUser);

	// if arguments have been supplied, use them
	switch (argc) {
	case 1:
		break;
	case 3:
		ttl = atoi(argv[2]);
		// fall through
	case 2:
		port = atoi(argv[1]);
		break;
	default:
		fprintf(stderr, "Arguments:  [port [ttl]]\n");
		exit(1);
	}

	// Start WinSock (standard call)
	// This is not in initNetworking() because this only needs to be
	// called once, whereas initNetworking() initializes the socket
	// and may be called more than once a session.
	if (WSAStartup(WSVERS, &wsadata) != 0)
		errexit("WSAStartup() failed\n");

	// Initialize multicast socket and screen buffers
	initNetworking();
	initScreen();

	// Find own IP and set it
	if(gethostname(buf, 256)) {
		errexit("gethostname() failed: %d\n", WSAGetLastError());
	}
	host = gethostbyname(buf);
	if(!host)
		errexit("WSAGetLastError() failed: %d\n", WSAGetLastError());

	userlist[0].ip = inet_ntoa(*(reinterpret_cast<in_addr*>(host->h_addr)));

	do {
		// Read a character from the screen
		if(!ReadFile(hStdin, &chBuffer, 1, &cRead, NULL))
			errexit("ReadFile() failed\n");

		// Find current cursor position
		if(!GetConsoleScreenBufferInfo(hStdout, &csbiInfo))
			errexit("GetConsoleScreenBufferInfo() failed\n");

		if(chBuffer == '\r') {
			// The character is a line feed, clear the input area and
			// process the input.

			csbiInfo.dwCursorPosition.X = 0;
			csbiInfo.dwCursorPosition.Y = 21;

			if(!SetConsoleCursorPosition(hStdout,csbiInfo.dwCursorPosition))
				errexit("SetConsoleCursorPosition() failed\n");

			if(!FillConsoleOutputCharacter(hStdout,' ',320,csbiInfo.dwCursorPosition,&cWritten))
				errexit("FillConsoleOutputCharacter() failed\n");

			// Process the input
			ProcessLine(input);

			// Reset the input
			input = "";
		}
		else if(chBuffer == '\b' && csbiInfo.dwCursorPosition.X) {
			// Handles backspaces poorly -- you can only backspace up to
			// the edge of the screen and not to the previous line

			// Move the cursor back one space and erase the current character
			csbiInfo.dwCursorPosition.X -= 1;
			if(!SetConsoleCursorPosition(hStdout,csbiInfo.dwCursorPosition))
				errexit("SetConsoleCursorPosition failed\n");
			if(!FillConsoleOutputCharacter(hStdout,' ',1,csbiInfo.dwCursorPosition,&cWritten))
				errexit("FillConsoleOutputCharacter() failed\n");

			// Remove the character from the input string
			input = input.substr(0,input.length()-1);
		}
		else if(chBuffer < 127 && chBuffer > 31) {
			// Only catches ASCII characters

			// Echo the character
			if(!WriteFile(hStdout, &chBuffer, cRead, &cWritten, NULL))
				errexit("WriteFile() failed\n");

			// If at the end of a line, start a new one
			if(csbiInfo.dwCursorPosition.X == 79) {
				csbiInfo.dwCursorPosition.X = 0;
				csbiInfo.dwCursorPosition.Y += 1;

				if(!SetConsoleCursorPosition(hStdout,csbiInfo.dwCursorPosition))
					errexit("SetConsoleCursorPosition failed\n");
			}

			// Add the character to the input string
			input += chBuffer;
		}

	} while (1);

	// Never reached.
	
} // end main().

// Processes the input
void ProcessLine(string input) {
	char	buf[514];
	short	length1, length2;
	string	command, user;
	int		stringPos, temp;

	// Separate the input into command and arguments
	stringPos = input.find(" ");
	command = input.substr(0,stringPos);
	input = input.substr(stringPos+1);

	// Set up default error message
	sprintf(buf, "\tcannot '%s' - not connected to any group", command.c_str());

	if(command == "user") {
		// Change the username

		if(input.length() > 255) {
			chatOutput("\tuser name too long - must be under 255 characters", RED);
			return;
		}
		if(connected) {
			// If connected to a group, leave and then rejoin with a different username
			departGroup();
			userlist[0].user = input;
			joinGroup();
		}
		else
			userlist[0].user = input;
		
		input = "\tuser name changed to '" + input + "'";
		chatOutput(input, GREEN);
	}
	else if(command == "join") {
		// Join a group

		if(connected)
			// If connected to a group, leave it first
			departGroup();

		joinCommand(input);
	}
	else if(command == "leave") {
		// Leave a group

		if(!connected) {
			// Error -- cannot leave when not connected
			chatOutput("\tnot connected to a group", RED);
			return;
		}

		departGroup();
	}
	else if(command == "list") {
		// List the members of the current group

		if(!connected) {
			// Error -- cannot list when not connected
			chatOutput(buf, RED);
			return;
		}

		outputUserList();
	}
	else if(command == "send") {
		// Send a message

		if(!connected) {
			// Error -- cannot send when not connected
			chatOutput(buf, RED);
			return;
		}
		if(input.length() > 255) {
			chatOutput("\tmessage not sent - must be under 255 characters", RED);
			return;
		}

		// Send the message
		length1 = userlist[0].user.length();
		length2 = input.length();
		sprintf(buf, "%c%c%s%c%s", 3, length1, userlist[0].user.c_str(), length2, input.c_str());

		if(sendto(McastSock, buf, strlen(buf), 0,
			(struct sockaddr *) &dest, sizeof(struct sockaddr)) == SOCKET_ERROR)
			errexit("sendto() failed: %d\n", WSAGetLastError());
	}
	else if(command == "exit") {
		// Exit the program

		if(connected)
			// If connected, leave nicely
			departGroup();

		// Clean up Winsock networking
		WSACleanup();
		exit(0);
	}
	else if(command == "ttl") {
		// Change the TTL value

		temp = atoi(input.c_str());
		if(temp < 0 || temp > 255) {
			chatOutput("\tTTL must be between 0 and 255", RED);
			return;
		}

		if(connected) {
			// Reconnect to the group
			departGroup();
			ttl = temp;
			sprintf(buf, "\tTTL set to %d", ttl);
			chatOutput(buf, GREEN);
			joinGroup();
		}
		else {
			ttl = temp;
			sprintf(buf, "\tTTL set to %d", ttl);
			chatOutput(buf, GREEN);
		}
	}
	else if(command == "port") {
		// Change the port

		temp = atoi(input.c_str());
		port = temp;

		sprintf(buf, "\tport set to %d", port);
		chatOutput(buf, GREEN);

		if(connected) {
			// The socket must be reinitialized
			departGroup();
			closesocket(McastSock);
			initNetworking();
			joinGroup();
		}
	}
	else {
		// Display an error message, since there are no other
		// valid commands

		command = "\t'" + command + "' is an invalid command";
		chatOutput(command, RED);
	}
}

// Joins a multicast group
void joinCommand(string input) {
	char	buf[513];
	int		stringPos, ipNum;
	string	temp = input;

	// Set up error message
	sprintf(buf, "\tinvalid group address '%s'", input.c_str());

	group = "";

	//
	// Make sure the group is between 234.0.0.1 and 234.255.255.255
	//

	stringPos = temp.find(".");
	ipNum = atoi(temp.substr(0,stringPos).c_str());

	if(ipNum != 234 || stringPos == string::npos) {
		chatOutput(buf, RED);
		return;
	}
	group += temp.substr(0,stringPos) + ".";
	temp = temp.substr(stringPos+1);

	for(int i=0; i<2; i++) {
		stringPos = temp.find(".");
		ipNum = atoi(temp.substr(0,stringPos).c_str());
		if(ipNum > 255 || ipNum < 0) {
			chatOutput(buf, RED);
			return;
		}
		group += temp.substr(0,stringPos) + ".";
		temp = temp.substr(stringPos+1);
	}

	ipNum = atoi(temp.c_str());
	if(ipNum > 255 || ipNum < 1) {
		chatOutput(buf, RED);
		return;
	}
	group += temp.substr(0,stringPos);

	// No errors encountered, so the group is valid and
	// joinGroup() can be run
	joinGroup();
}

// Self-explanatory
void outputUserList(void) {
	string output;

	ScrollOutputBuffer();

	chatOutput("\tcurrent group members:", GREEN);

	for(int i=0; i<userlist.size(); i++) {
		output = "\t" + userlist[i].user;
		output += " (" + userlist[i].ip + ")";

		chatOutput(output, GREEN);
	}

	ScrollOutputBuffer();
}

// Self-explanatory
void departGroup(void) {
	char				buf[513];
	string				user = userlist[0].user;
	DWORD				dwEvent;

	sprintf(buf, "\tleft group %s", group.c_str());
	chatOutput(buf, GREEN);

	// Send depart packet
	sprintf(buf, "%c%c%s%c%s", 2, user.length(), user.c_str(), 0, NULL);
	if(sendto(McastSock, buf, strlen(buf), 0,
		(struct sockaddr *) &dest, sizeof(struct sockaddr)) == SOCKET_ERROR)
		errexit("sendto() failed: %d\n", WSAGetLastError());

	// Wait for network-reception thread to close
	dwEvent = WaitForSingleObject(recvThread, INFINITE);

	// Clear the userlist and reset the connected flag
	userlist.resize(1);
	connected = 0;
}

void getNetworkInput(void *_args) {
	getNetworkInputArgs *args = (getNetworkInputArgs *)_args;

	struct ip_mreq		McastReq;			// multicast group request
	struct sockaddr_in	from;				// from address
	int					alen;				// from address length
	char				buf[514];			// word buffer
	int					len;				// word length
	
	char		ip[16];						// from IP
	char		user[256];					// user
	char		output[256];				// output string
	short		type, length1, length2;		// for reception
	WORD		color;

	// Loop forever receiving each word and displaying.
	do {

		// Receive a word.
		alen = sizeof(struct sockaddr);
		if((len = recvfrom(McastSock, buf, sizeof(buf),0,(struct sockaddr *) &from, &alen)) == SOCKET_ERROR)
			errexit("recvfrom() failed: %d\n", WSAGetLastError());

		// End the input string
		buf[len] = '\0';

		// Get the IP -- there's got to be a better way to do this
		sprintf(ip,"%d.%d.%d.%d",
			from.sin_addr.S_un.S_un_b.s_b1,
			from.sin_addr.S_un.S_un_b.s_b2,
			from.sin_addr.S_un.S_un_b.s_b3,
			from.sin_addr.S_un.S_un_b.s_b4);

		// Grab information from the packet
		type = buf[0];
		length1 = buf[1];
		length2 = buf[length1+2];
		buf[length1+2] = '\0';

		// Get the user
		sprintf(user, "%s", buf+2);

		// Add the user
		addNewUser(user, ip);

		if(type == 1) {
			// Enter packet

			sprintf(output, "\t%s (%s) has joined the group", user, ip);
			chatOutput(output, WHITE);
		}
		else if(type==2) {
			// Depart packet

			sprintf(output, "\t%s (%s) has left the group", user, ip);
			chatOutput(output, WHITE);

			// If the packet is from the same program, drop membership
			// from the group and end the thread
			if(userlist[0].user == user && userlist[0].ip == ip) {
				if( (McastReq.imr_multiaddr.s_addr = inet_addr(group.c_str())) == INADDR_NONE )
					errexit("Bad group address: %d\n", WSAGetLastError());
				McastReq.imr_interface.s_addr = INADDR_ANY;
				if(setsockopt(McastSock, IPPROTO_IP, IP_DROP_MEMBERSHIP, (char *) &McastReq, sizeof(McastReq)) == SOCKET_ERROR)
					errexit("setsockopt failed for IP_DROP_MEMBERSHIP: %d\n", WSAGetLastError());
				_endthread();
			}

			// Otherwise, erase the user from the userlist
			for(int i=0; i<userlist.size(); i++)
				if(userlist[i].user == user && userlist[i].ip == ip)
					userlist.erase(userlist.begin()+i);
		}
		else if(type == 3) {
			// Send packet

			// Output BLUE if the message is from the same program, WHITE otherwise
			color = (userlist[0].user == user && userlist[0].ip == ip) ? BLUE : WHITE ;
			sprintf(output, "%s: %s", user, buf+length1+3); 
			chatOutput(output, color);
		}

	} while(1);

	// Never reached.

	return;
}

// Adds a new user if the user is not already
// in the userlist
void addNewUser(char *user, char *ip) {
	userData newUser;
	newUser.user = user;
	newUser.ip = ip;

	for(int i=0; i<userlist.size(); i++)
		if(userlist[i].user == newUser.user && userlist[i].ip == newUser.ip)
			return;

	userlist.push_back(newUser);
}

// Used to send a char* to a string argument
// Is this necessary?
void chatOutput(char *output, WORD color) {
	string temp = output;

	chatOutput(temp, color);
}

// Outputs to the screen
void chatOutput(string output, WORD color) {
	DWORD cWritten;
	COORD cursorPosition;

	ScrollOutputBuffer();

	// Set the color
    if (! SetConsoleTextAttribute(hStdout, color|FOREGROUND_INTENSITY))
		errexit("SetConsoleTextAttribute() failed\n");

	// Get and save the cursor position
    if (! GetConsoleScreenBufferInfo(hStdout, &csbiInfo)) 
        errexit("GetConsoleScreenBufferInfo() failed\n");
	cursorPosition = csbiInfo.dwCursorPosition;

	// Change the cursor position
	csbiInfo.dwCursorPosition.X = 0;
	csbiInfo.dwCursorPosition.Y = 20;
	if(!SetConsoleCursorPosition(hStdout,csbiInfo.dwCursorPosition))
		errexit("SetConsoleScreenBufferInfo() failed\n");

	// The following should be protected by a mutex...

	// Write the output to screen 80 characters at a time
	while(output.length() > 80) {
		if(!WriteFile(hStdout, output.substr(0,80).c_str(), 80, &cWritten, NULL))
			errexit("WriteFile() failed\n");
		ScrollOutputBuffer();
		output = output.substr(80);

		if(!SetConsoleCursorPosition(hStdout,csbiInfo.dwCursorPosition))
			errexit("SetConsoleScreenBufferInfo() failed\n");
	}
	if(!WriteFile(hStdout, output.c_str(), output.length(), &cWritten, NULL))
		errexit("WriteFile() failed\n");

	// Reset the cursor position and output color
	if(!SetConsoleCursorPosition(hStdout,cursorPosition))
		errexit("SetConsoleScreenBufferInfo() failed\n");
	if (! SetConsoleTextAttribute(hStdout, WHITE))
		errexit("SetConsoleTextAttribute() failed\n");
}

// initialize networking
void initNetworking(void) {
	BOOL				OptValue;		// option value
	struct sockaddr_in	source;

	// Create a standard UDP socket using socket().
	if((McastSock = socket(AF_INET, SOCK_DGRAM, 0)) == INVALID_SOCKET)
		errexit("socket() failed: %d\n", WSAGetLastError());

	// Set option to reuse address so multiple sockets on this host can
	// use the same port number.
	OptValue = TRUE;
	if(setsockopt(McastSock, SOL_SOCKET, SO_REUSEADDR, (char *) &OptValue, sizeof(OptValue)) == SOCKET_ERROR)
		errexit("setsockopt() failed setting SO_REUSEADDR: %d\n", WSAGetLastError());

	// Some WinSock implementations (including Microsoft
	// WinSock 2.2 in Windows 98) will not set IP-level socket
	// options unless an address is bound to the socket.  So,
	// this is done here.
	source.sin_family = AF_INET;
	source.sin_port = htons(port);
	source.sin_addr.s_addr = INADDR_ANY;

	if(bind(McastSock, (struct sockaddr *) &source, sizeof(struct sockaddr)) == SOCKET_ERROR)
		errexit("bind() failed: %d\n", WSAGetLastError());
}


// Initialize the screen buffers
void initScreen(void) {
	hStdin = GetStdHandle(STD_INPUT_HANDLE);
	hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	if(hStdin == INVALID_HANDLE_VALUE || hStdout == INVALID_HANDLE_VALUE) {
		errexit("GetStdHandle() failed\n");
	}
	
	DWORD fdwMode, fdwOldMode;

	if(!GetConsoleMode(hStdin, &fdwOldMode)) {
		errexit("GetConsoleMode() failed\n");
	}

	fdwMode = fdwOldMode & ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT);

	if(!SetConsoleMode(hStdin, fdwMode)) {
		errexit("SetConsoleMode() failed\n");
	}

	COORD newCoords;
	newCoords.X = 0;
	newCoords.Y = 21;

	if(!SetConsoleCursorPosition(hStdout, newCoords)) {
		errexit("SetConsoleCursorPosition() failed: %s\n", GetLastError());
	}
}


// Scroll the chat upwards
void ScrollOutputBuffer(void) {
    SMALL_RECT srctScrollRect, srctClipRect;
    CHAR_INFO chiFill;
    COORD coordDest;

    srctScrollRect.Left = 0;
    srctScrollRect.Top = 1;
    srctScrollRect.Right = csbiInfo.dwSize.X - 1; 
    srctScrollRect.Bottom = 20; 
 
    // The destination for the scroll rectangle is one row up. 
 
    coordDest.X = 0; 
    coordDest.Y = 0; 
 
    // The clipping rectangle is the same as the scrolling rectangle. 
    // The destination row is left unchanged. 
 
    srctClipRect = srctScrollRect;
	srctClipRect.Top = 0;
	 
    // Set the fill character and attributes. 
 
    chiFill.Attributes = FOREGROUND_RED|FOREGROUND_INTENSITY; 
    chiFill.Char.AsciiChar = (char)' '; 
 
    // Scroll up one line. 
 
    ScrollConsoleScreenBuffer( 
        hStdout,         // screen buffer handle 
        &srctScrollRect, // scrolling rectangle 
        &srctClipRect,   // clipping rectangle 
        coordDest,       // top left destination cell 
        &chiFill);       // fill character and color 
}


// Join a multicast group
void joinGroup(void) {
	struct ip_mreq		McastReq;		// multicast group request

	// Join the multicast group from we want to receive datagrams.
	// Initialize the multicast request structure and then pass
	// it as an option to setsockopt().  The imr_multiaddr element
	// is initialized to the desired multicast group.  The
	// imr_interface element is initilized to IPADDR_ANY which
	// causes the multcast receives to come from the default
	// interface.
	if( (McastReq.imr_multiaddr.s_addr = inet_addr(group.c_str())) == INADDR_NONE )
		errexit("Bad group address: %d\n", WSAGetLastError());

	McastReq.imr_interface.s_addr = INADDR_ANY;

	if(setsockopt(McastSock, IPPROTO_IP, IP_ADD_MEMBERSHIP, (char *) &McastReq, sizeof(McastReq)) == SOCKET_ERROR)
		errexit("setsockopt failed for IP_ADD_MEMBERSHIP: %d\n", WSAGetLastError());

	if(setsockopt(McastSock, IPPROTO_IP, IP_MULTICAST_TTL, (char *) &ttl, sizeof(ttl)) == SOCKET_ERROR)
		errexit("setsockopt() failed setting IP_MULTICAST_TTL: %d\n", WSAGetLastError());

	// Initialize the destination address structure.  The destination
	// IP address is a group (class D) address.  Port number must match
	// port used by receivers.	
	dest.sin_family = AF_INET;
	dest.sin_port = htons(port);
	if((dest.sin_addr.s_addr = inet_addr(group.c_str())) == INADDR_NONE)
		errexit("Bad group address %s: %\n", group, WSAGetLastError());

	char temp[255];	
	sprintf(temp, "\tjoined group %s on port %d with ttl %d", group.c_str(), port, ttl);
	chatOutput(temp, GREEN);

	recvThread = (HANDLE) _beginthread((void (*)(void *))getNetworkInput, 0, (void *)NULL);
	if(recvThread < 0)
		errexit("_beginthread() failed: %s\n", strerror(errno));

	string user = userlist[0].user;
	char buf[514];
	sprintf(buf, "%c%c%s%c%s", 1, user.length(), user.c_str());
	if(sendto(McastSock, buf, strlen(buf), 0,
			(struct sockaddr *) &dest, sizeof(struct sockaddr)) == SOCKET_ERROR)
			errexit("sendto() failed: %d\n", WSAGetLastError());

	connected = 1;
}


// Procedure errexit().  From Comer and Stevens, Vol. III (WinSock edition)
void errexit(const char *format, ...)
{
	va_list	args;

	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);
	WSACleanup();
	exit(1);

} // end errexit().