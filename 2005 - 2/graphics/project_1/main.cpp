/*
 
 CS 4204
 Professor Ehrich
 Project 1
 Due: 2005.11.03
 
 Alpha Chen
 2005.09.25
 
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#include "shapes.c"		// Include the master objects

#define BOX 0
#define TRIANGLE 1
#define TORUS 2

/* For the menu */
#define GROUP 3
#define UNGROUP 4
#define MOVE_TOP 5
#define MOVE_BOTTOM 6
#define DELETE 7

/* For picking */
#define BUFSIZE 512
GLuint selectBuf[BUFSIZE];
GLuint pickedNames[BUFSIZE];
int hits;

/*
 objectNode holds individual objects: a box, triangle, or torus.
 A linked list of objects is a structure.
 */
struct objectNode {
	int type;
	GLfloat modelview[16];
	objectNode *next;
};

/*
 displayNode holds each structure in order of display.
 */
struct displayNode {
	objectNode *object;
	GLfloat modelview[16];
	int selected;
	displayNode *next;
};

/*
 Various globals to keep track of the state of the program.
 */
displayNode *head;	// Keep track of all objects
displayNode *tail;
int numSelected;	// How many objects are selected
int selectedHandle;	// Which handle has been selected
int current_x;		// Keep track of the mouse pointer.
int current_y;
GLfloat moved_x;
GLfloat moved_y;
int clicked_x;
int clicked_y;
GLfloat scaled_x;
GLfloat scaled_y;

int menu;
int submenu;

/* Reference needed only to initialize the
program with a few objects. */
void createObject(int);

/*
 Initialization.
 */
void init(void) {
	hits = 0;
	current_x = current_y = 0;
	clicked_x = clicked_y = 0;
	moved_x = moved_y = 0;
	head = tail = NULL;
	numSelected = 0;
	selectedHandle = -1;
	scaled_x = scaled_y = 1;

	// Populate the world with some objects.
	createObject(BOX);
	current_x = current_y = 50;
	createObject(TORUS);
	current_x = current_y = 150;
	createObject(TRIANGLE);
	
	current_x = current_y = 0;
}

/*
 Unselect all objects.
 */
void resetSelection(void) {
	displayNode *displayPtr = head;
	while(displayPtr) {
		displayPtr->selected = 0;
		displayPtr = displayPtr->next;
	}
	numSelected = 0;
}

/*
 Draw handles for scaling the object.
 */
void drawHandles(GLfloat left, GLfloat right, GLfloat top, GLfloat bottom) {
	GLfloat modelview[16];
	glGetFloatv(GL_MODELVIEW_MATRIX, modelview);
	right *= modelview[0];
	top *= modelview[5];
	modelview[0] = modelview[5] = 1;
	
	glLoadName(0);
	glLoadMatrixf(modelview);
	glTranslatef(left-2, bottom-2, 0);
	box(0, 0, 4, 4, 0);
	
	glLoadName(1);	
	glLoadMatrixf(modelview);
	glTranslatef(left-2, (top+bottom)/2-2, 0);
	box(0, 0, 4, 4, 0);
	
	glLoadName(2);
	glLoadMatrixf(modelview);
	glTranslatef(left-2, top-2, 0);
	box(0, 0, 4, 4, 0);
	
	glLoadName(3);
	glLoadMatrixf(modelview);
	glTranslatef((left+right)/2-2, top-2, 0);
	box(0, 0, 4, 4, 0);
	
	glLoadName(4);
	glLoadMatrixf(modelview);
	glTranslatef(right-2, top-2, 0);
	box(0, 0, 4, 4, 0);

	glLoadName(5);
	glLoadMatrixf(modelview);
	glTranslatef(right-2, (top+bottom)/2-2, 0);
	box(0, 0, 4, 4, 0);
	
	glLoadName(6);
	glLoadMatrixf(modelview);
	glTranslatef(right-2, bottom-2, 0);
	box(0, 0, 4, 4, 0);

	glLoadName(7);
	glLoadMatrixf(modelview);
	glTranslatef((left+right)/2-2, bottom-2, 0);
	box(0, 0, 4, 4, 0);
}

/*
 Tests to see which handle has been selected.
 */
int leftHandle(void) {
	return(selectedHandle < 3);
}

int rightHandle(void) {
	return(selectedHandle > 3 && selectedHandle < 7);
}

int topHandle(void) {
	return(selectedHandle > 1 && selectedHandle < 5);
}

int bottomHandle(void) {
	return(selectedHandle > 5 || selectedHandle == 0);
}

/*
 Get object bounds.
 */
void getObjectBounds(displayNode *displayPtr, GLfloat &left, GLfloat &right,
					 GLfloat &top, GLfloat &bottom) {
	GLfloat r, t;
	
	objectNode *nodePtr = displayPtr->object;
	while(nodePtr) {
		// Temporary variables to make bottom code cleaner
		r = nodePtr->modelview[12] + (40 * nodePtr->modelview[0]);
		t = nodePtr->modelview[13] + (40 * nodePtr->modelview[5]);
		
		left = (nodePtr->modelview[12] > left)
			? left
			: nodePtr->modelview[12];
		right = (r > right) ? r : right;
		top = (t > top) ? t : top;
		bottom = (nodePtr->modelview[13] > bottom)
			? bottom
			: nodePtr->modelview[13];
		
		nodePtr = nodePtr->next;
	}
}

/*
 Draw dragging selection box.
 */
void drawDragging(void) {
	// Draw the dragging selection box
	glInitNames();
	glLoadIdentity();
	if(!numSelected && moved_x && moved_y) {
		glColor3f(0, 0, 0);
		glBegin(GL_LINE_LOOP);
		glVertex3i(clicked_x, clicked_y, 0);
		glVertex3i(clicked_x + int(moved_x), clicked_y, 0);
		glVertex3i(clicked_x + int(moved_x), clicked_y + int(moved_y), 0);
		glVertex3i(clicked_x, clicked_y + int(moved_y), 0);
		glEnd();
	}
}

/*
 Callback function for display. Draws all objects in the
 world. If the object is 'highlight'ed, outline it in white.
 */
void display(void) {
	GLfloat left, right = 0, top = 0, bottom;
	int i = 8;
	objectNode *nodePtr;
		
	glClear(GL_COLOR_BUFFER_BIT);

	glPushName(i);
	
	// Iterate through all structures
	for(displayNode *displayPtr = head;
		displayPtr;
		displayPtr = displayPtr->next) {
		
		glLoadName(i++);
		
		glLoadIdentity();
		
		// If selected, get the object bounds and if necessary, scaling.
		if(displayPtr->selected) {
			left = glutGet(GLUT_WINDOW_WIDTH);
			right = 0;
			bottom = glutGet(GLUT_WINDOW_HEIGHT);
			top = 0;

			getObjectBounds(displayPtr, left, right, top, bottom);
			
			// If a handle is selected, adjust the scaling.
			if(selectedHandle != -1) {
				scaled_x = scaled_y = 1;

				moved_x /= displayPtr->modelview[0];
				moved_y /= displayPtr->modelview[5];
				if(leftHandle()) {
					scaled_x = (right - moved_x)/right;
					glTranslatef(moved_x * displayPtr->modelview[0], 0, 0);
					right -= moved_x;
				}
				else if(rightHandle()) {
					scaled_x = (right + moved_x)/right;
					right *= scaled_x;
				}
				if(topHandle()) {
					scaled_y = (top + moved_y)/top;
					top *= scaled_y;
				}
				else if(bottomHandle()) {
					scaled_y = (top - moved_y)/top;
					glTranslatef(0, moved_y * displayPtr->modelview[5], 0);
					top -= moved_y;
				}
			}
		}
		
		// Move the selected object.
		if(displayPtr->selected && selectedHandle == -1)
			glTranslatef(moved_x, moved_y, 0);	
		
		// The original location and scale of the object.
		glMultMatrixf(displayPtr->modelview);
		glPushMatrix();
		
		// Scale the selected object.
		if(displayPtr->selected && selectedHandle != -1)
			glScalef(scaled_x,
					 scaled_y,
					 1);

		// Now we can go through and display all of the objects.
		for(nodePtr = displayPtr->object; nodePtr; nodePtr = nodePtr->next) {
			glPushMatrix();
			// For structured objects.
			glMultMatrixf(nodePtr->modelview);
			
			// Draw the actual object
			switch(nodePtr->type) {
				case BOX:
					glColor3f(0, 0, 0.75);
					box(0, 0, 40, 40, 20);
					break;
				case TRIANGLE:
					triangle(0, 0, 40, 40);
					break;
				case TORUS:
					glColor3f(0.75, 0, 0);
					torus(20, 20, 10, 20);
					break;
			}
			glPopMatrix();
		}
		
		// If multiple objects are selected, disable (grey out) the handles
		if(numSelected == 1)
			glColor3f(0, 0, 0);
		else
			glColor3f(0.5, 0.5, 0.5);

		// Draw the handles
		glPopMatrix();
		if(displayPtr->selected) {
			drawHandles(left, right, top, bottom);
		}
	}
	
	drawDragging();
	
    glutSwapBuffers();
}

/*
 Standard reshape function. Note that the graphics window is
 half the size of the viewport.
 */
void reshape(int width, int height) {
	glViewport(0, 0, width, height);
	
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluOrtho2D(0, width, 0, height);
    glMatrixMode(GL_MODELVIEW);
}

/*
 Find the picked object.
 */
void setPicked(int x, int y, int pick_x, int pick_y) {
	int i, j, k;
	GLint viewport[4];
	
	glSelectBuffer(BUFSIZE,selectBuf);
	glRenderMode(GL_SELECT);
	
	glMatrixMode(GL_PROJECTION);
	glPushMatrix();
	glLoadIdentity();
	
	glGetIntegerv(GL_VIEWPORT,viewport);
	gluPickMatrix(x, y, pick_x, pick_y, viewport);
	gluOrtho2D(0, viewport[2], 0, viewport[3]);
	glMatrixMode(GL_MODELVIEW);
	glInitNames();
	
	display();
	
	hits = glRenderMode(GL_RENDER);
	
	glMatrixMode(GL_PROJECTION);
	glPopMatrix();
	glMatrixMode(GL_MODELVIEW);
	glFlush();

	// Load the picked objects' names into a buffer.
	k = 0;
	for(i=0; i<hits; i++) {
		for(j=0; j<selectBuf[4*i]; j++) {
			pickedNames[k++] = selectBuf[4*i+3+j];
		}
	}
	hits = k;
}

/*
 Do all mousedown calculations and set selection appropriately.
 */
void processMouseDown(int x, int y) {
	int pick;
	int shift_enabled = glutGetModifiers() & GLUT_ACTIVE_SHIFT;
	
	// See what has been picked.
	setPicked(x, y, 2, 2);
	if(hits != 0) // get top selection
		pick = pickedNames[hits-1];
	else
		pick = -1;

	// If an object has been picked...
	if(pick > 7) {
		// Find the picked object/structure.
		displayNode *displayPtr = head;
		while(displayPtr && (pick-- > 8))
			displayPtr = displayPtr->next;
		
		if(!displayPtr->selected) {	// If it's not selected already...
			if(!shift_enabled)		// Shift isn't pressed, so this
				resetSelection();	// is the only picked object.
			numSelected++;
			displayPtr->selected = 1;
		}
		else if(shift_enabled) {	// The object is selected and shift has
			numSelected--;			// been pressed. Unselect the object.
			displayPtr->selected = 0;
		}
	}
	// A handle has been picked
	else if(pick >= 0 && numSelected == 1)
		selectedHandle = pick;
	// Nothing has been picked
	else {
		resetSelection();
	}
	
	// Remember where the click happened.
	clicked_x = x;
	clicked_y = y;
}	

/*
 Set a node to be selected;
 */
void selectNode(int node) {
	displayNode *displayPtr = head;
	while((node-- > 8) && displayPtr)
		displayPtr = displayPtr->next;
	if(displayPtr && !displayPtr->selected) {
		displayPtr->selected = 1;
		numSelected++;
	}
}

/*
 Do all mouseup calculations. Move objects appropriately.
 */
void processMouseUp(int x, int y) {
	if(numSelected) { // Something is selected...
		if(selectedHandle == -1) { // An object is selected
			displayNode *displayPtr = head;
			while(displayPtr) {
				// Move selected objects.
				if(displayPtr->selected) {
					displayPtr->modelview[12] += moved_x;
					displayPtr->modelview[13] += moved_y;
				}
				displayPtr = displayPtr->next;
			}
		}
		else { // A handle is the current selection.
			// Find the selected object
			displayNode *displayPtr = head;
			while(!displayPtr->selected)
				displayPtr = displayPtr->next;
			
			// Scale the selected object.
			if(leftHandle()) {
				displayPtr->modelview[12] += moved_x * displayPtr->modelview[0];
			}
			if(bottomHandle()) {
				displayPtr->modelview[13] += moved_y * displayPtr->modelview[5];
			}
			
			displayPtr->modelview[0] *= scaled_x;
			displayPtr->modelview[5] *= scaled_y;
			
			selectedHandle = -1;
		}
	}
	// Implement drag-selecting.
	else if(moved_x && moved_y) {
		resetSelection();
		setPicked(x-int(moved_x/2),
				  y-int(moved_y/2),
				  abs(int(moved_x)),
				  abs(int(moved_y)));
		for(int i=0; i<hits; i++)
			if(pickedNames[i] > 7)
				selectNode(pickedNames[i]);
	}
	moved_x = moved_y = 0;
	selectedHandle = -1;
}

/*
 Callback function for mouseclicks. This only responds to left-clicks,
 setting the object that is being moved on a mousedown and highlighting
 an object (if it can) on mouseup.
 */
void processMouse(int button, int state, int x, int y) {
	
	y = glutGet(GLUT_WINDOW_HEIGHT) - y;
	
	// Only respond to left-clicks.
	if(button == GLUT_LEFT_BUTTON) {
		if(state == GLUT_DOWN)
			processMouseDown(x, y);
		else
			processMouseUp(x, y);
		
		// Redraw the screen. (Highlight may have changed, objects
		// may be moving/moved.)
		glutPostRedisplay();
	}
}

/*
 Callback for mouse motion. Draws a translucent triangle
 at the mouse cursor location.
 */
void processMotion(int x, int y) {
	// Translate viewport coords to world coords.
	y = glutGet(GLUT_WINDOW_HEIGHT) - y;

	moved_x = x - clicked_x;
	moved_y = y - clicked_y;
	
	glutPostRedisplay();
}

/*
 Keep track of where the mouse is when no buttons are being pressed.
 
 TODO: Doesn't activate when in a menu.
 */
void processPassiveMotion(int x, int y) {
	current_x = x;
	current_y = glutGet(GLUT_WINDOW_HEIGHT) - y;
}

/*
 Create an object.
 */
void createObject(int option) {
	GLfloat modelview[16] = {1, 0, 0, 0,
							 0, 1, 0, 0,
							 0, 0, 1, 0,
							 0, 0, 0, 1};
	int i;
	
	displayNode *displayPtr = new displayNode;
	objectNode *nodePtr = displayPtr->object = new objectNode;
	
	resetSelection();
	
	for(i=0; i<16; i++)
		displayPtr->modelview[i] = modelview[i];
	displayPtr->selected = 1;
	displayPtr->next = NULL;
	displayPtr->modelview[12] = current_x;
	displayPtr->modelview[13] = current_y;
	
	nodePtr->type = option;
	for(i=0; i<16; i++)
		nodePtr->modelview[i] = modelview[i];

	nodePtr->next = NULL;
	
	if(tail)
		tail->next = displayPtr;
	else
		head = displayPtr;
	
	tail = displayPtr;
	
	numSelected = 1;
}

/*
 Remove a node from the linked list.
 
 DOES NOT DELETE THE OBJECT.
 */
void removeNode(displayNode *ptr) {
	displayNode *displayPtr = head;
	while(displayPtr->next) {
		if(displayPtr->next == ptr)
			break;
		displayPtr = displayPtr->next;
	}
	if(ptr == head && ptr == tail)
		head = tail = NULL;
	else {
		if(ptr == head)
			head = ptr->next;
		else
			displayPtr->next = ptr->next;
		if(ptr == tail)
			tail = displayPtr;
	}
}

/*
 Delete the selected node(s) from the linked list.
 */
void deleteSelected() {
	displayNode *displayPtr = head;
	while(displayPtr) {
		if(displayPtr->selected) {
			removeNode(displayPtr);
			delete displayPtr;
		}
		displayPtr = (displayPtr) ? displayPtr->next : head;
	}
}

/*
 */
void moveSelectedToTop() {
	displayNode *moved = NULL, *movedPtr = NULL;

	displayNode *displayPtr = head;
	while(displayPtr) {
		if(displayPtr->selected) {
			removeNode(displayPtr);
			if(moved) {
				movedPtr->next = displayPtr;
				movedPtr = movedPtr->next;
			}
			else
				moved = movedPtr = displayPtr;
		}
		displayPtr = displayPtr->next;
	}
	movedPtr->next = NULL;
	
	tail->next = moved;
	tail = movedPtr;
}

/*
 */
void moveSelectedToBottom(void) {
	displayNode *moved = NULL, *movedPtr = NULL;
	
	displayNode *displayPtr = head;
	while(displayPtr) {
		if(displayPtr->selected) {
			removeNode(displayPtr);
			if(moved) {
				movedPtr->next = displayPtr;
				movedPtr = movedPtr->next;
			}
			else
				moved = movedPtr = displayPtr;
		}
		displayPtr = displayPtr->next;
	}	
	movedPtr->next = NULL;
	
	movedPtr->next = head;
	head = moved;
}

/*
 Group the selected structures together into a structure.
 */
void groupSelected(void) {
	displayNode *group = new displayNode;
	objectNode *groupPtr, *nodePtr;
	GLfloat left, bottom;
	int firstFlag = 1;

	// group will be the new structure
	group->object = NULL;
	glLoadIdentity();
	glGetFloatv(GL_MODELVIEW_MATRIX, group->modelview);
	group->selected = 1;
	group->next = NULL;
	
	// Find all of the selected objects and add them to the structure
	displayNode *displayPtr = head;
	while(displayPtr) {
		if(displayPtr->selected) {
			removeNode(displayPtr);

			nodePtr = displayPtr->object;
			
			while(nodePtr) {
				glLoadMatrixf(displayPtr->modelview);
				glMultMatrixf(nodePtr->modelview);
				glGetFloatv(GL_MODELVIEW_MATRIX, nodePtr->modelview);
				
				// If it's the first object, create the structure.
				if(!group->object) {
					group->object = nodePtr;
					if(firstFlag) {
						left = nodePtr->modelview[12];
						bottom = nodePtr->modelview[13];
						firstFlag = 0;
					}
				}
				// Add subsequent selected objects to the new structure.
				else {
					if(nodePtr->modelview[12] < left)
						left = nodePtr->modelview[12];
					if(nodePtr->modelview[13] < left)
						left = nodePtr->modelview[13];
					groupPtr->next = nodePtr;
				}
				groupPtr = nodePtr;
				
				nodePtr = nodePtr->next;
			}
		}
		displayPtr = displayPtr->next;
	}
	groupPtr->next = NULL;
	
	// Find the bottom left corner in viewport coordinates
	nodePtr = group->object;
	while(nodePtr) {
		nodePtr->modelview[12] -= left;
		nodePtr->modelview[13] -= bottom;
		nodePtr = nodePtr->next;
	}
	// Make the structure start at that location.
	group->modelview[12] = left;
	group->modelview[13] = bottom;
		
	// Fix the display linked list to add the structure.
	if(!head)
		head = group;
	if(!tail)
		tail = group;
	else {
		tail->next = group;
		tail = group;
	}
	numSelected = 1;
	
	glutPostRedisplay();
}

/*
 Creates a displayNode from the given object.
 */
displayNode * createNode(objectNode *nodePtr) {
	displayNode *displayPtr = new displayNode;
	
	glPushMatrix();
	glMultMatrixf(nodePtr->modelview);
	
	displayPtr->selected = 1;
	displayPtr->object = nodePtr;
	displayPtr->next = NULL;
	
	glGetFloatv(GL_MODELVIEW_MATRIX, displayPtr->modelview);
	
	glLoadIdentity();
	glGetFloatv(GL_MODELVIEW_MATRIX, nodePtr->modelview);

	glPopMatrix();
	
	return displayPtr;
}

/*
 Ungroup the selected structure.
 */
void ungroupSelected(void) {
	displayNode *displayPtr = head, *newDisplay;
	objectNode *nodePtr;
	
	numSelected = 0;
	
	while(displayPtr) {
		if(displayPtr->selected && displayPtr->object->next) {
			removeNode(displayPtr);
			glLoadMatrixf(displayPtr->modelview);
			
			nodePtr = displayPtr->object;
			while(nodePtr) {
				newDisplay = createNode(nodePtr);
				nodePtr = nodePtr->next;
				newDisplay->object->next = NULL;
				
				if(!tail) {
					head = newDisplay;
					tail = newDisplay;
				}
				else {
					tail->next = newDisplay;
					tail = newDisplay;
				}
				numSelected++;
			}
		}
		displayPtr = displayPtr->next;
	}
}

/*
 */
void processMenu(int option) {
	switch(option) {
		case BOX:
		case TRIANGLE:
		case TORUS:
			createObject(option);
			break;
		case DELETE:
			deleteSelected();
			break;
		case GROUP:
			groupSelected();
			break;
		case UNGROUP:
			ungroupSelected();
			break;
		case MOVE_TOP:
			moveSelectedToTop();
			break;
		case MOVE_BOTTOM:
			moveSelectedToBottom();
			break;
	}
	
	glutPostRedisplay();
}

int main(int argc, char** argv) {
	init();
	glutInit(&argc, argv);
	
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowSize(500, 250);
	glutInitWindowPosition(100, 100);
	
	glutCreateWindow("CS4204 - Project 1");
	
	glEnable(GL_BLEND);	
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glClearColor(1.0, 1.0, 1.0, 1.0);
	
	glutDisplayFunc(display);
    glutReshapeFunc(reshape);
	glutMouseFunc(processMouse);
	glutMotionFunc(processMotion);
	glutPassiveMotionFunc(processPassiveMotion);
	
	/* Create menu */
	submenu = glutCreateMenu(processMenu);
	glutAddMenuEntry("Box", BOX);
	glutAddMenuEntry("Triangle", TRIANGLE);
	glutAddMenuEntry("Torus", TORUS);
	
	menu = glutCreateMenu(processMenu);
	glutAddSubMenu("Draw", submenu);
	glutAddMenuEntry("Delete", DELETE);
	glutAddMenuEntry("Group", GROUP);
	glutAddMenuEntry("Ungroup", UNGROUP);
	glutAddMenuEntry("Move to top", MOVE_TOP);
	glutAddMenuEntry("Move to bottom", MOVE_BOTTOM);
	
	glutAttachMenu(GLUT_RIGHT_BUTTON);	
	
	glutMainLoop();
	return EXIT_SUCCESS;
}
