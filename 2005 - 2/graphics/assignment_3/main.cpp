/*
 
 CS 4204
 Professor Ehrich
 Assignment 3
 Due: 2005.09.26
 
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

/*
 triangleNode is for a linked list which holds the objects (triangles)
 being drawn in the program. The coordinates, color, and transparency
 are 'innate' values of each triangle. highlight will determines whether
 the given triangle is rendered as selected. mousedown determines if the
 triangle is being moved, and moved determines whether the mouse has 
 actually dragged the 'mousedown'ed triangle or not.
 */
struct triangleNode {
	int x;
	int y;
	float color[3];
	float alpha;
	int highlight;
	int mousedown;
	int moved;
	triangleNode *next;
};

// Keep track of the current objects in the world.
triangleNode *head;
triangleNode *tail;

/*
 Master triangle function. Draws an equilateral triangle with sides
 of length 20.
 */
void drawMasterTriangle() {
	glBegin(GL_POLYGON);
		glVertex2i(0,9);
		glVertex2i(10,-9);
		glVertex2i(-10,-9);
	glEnd();
}

/*
 Initialize object triangles.
 */
void init(void) {
	// Set up red triangle
	head = new triangleNode;
	head->x = 25;
	head->y = 25;
	head->color[0] = 1;
	head->color[1] = 0;
	head->color[2] = 0;
	head->alpha = 1;
	head->highlight = 0;
	head->mousedown = 0;
	head->moved = 0;
	head->next = new triangleNode;
	
	// Set up blue triangle
	tail = head->next;
	tail->x = 75;
	tail->y = 75;
	tail->color[0] = 0;
	tail->color[1] = 0;
	tail->color[2] = 1;
	tail->alpha = 1;
	tail->highlight = 0;
	tail->mousedown = 0;
	tail->moved = 0;
	tail->next = NULL;
}

/*
 Callback function for display. Draws all objects in the
 world. If the object is 'highlight'ed, outline it in white.
 */
void display(void) {
	glClear(GL_COLOR_BUFFER_BIT);
	glColor3f(1.0, 1.0, 1.0);
	
	// Iterate through all objects.
	for(triangleNode *nodePtr = head; nodePtr; nodePtr = nodePtr->next) {
		glLoadIdentity();
		
		glTranslatef(nodePtr->x, nodePtr->y, 0);
		
		if(nodePtr->highlight) {
			glColor4f(1.0, 1.0, 1.0, 1.0);
			glPushMatrix();
			glScalef(1.1, 1.1, 0);
			drawMasterTriangle();
			glPopMatrix();
		}
		
		glColor4f(nodePtr->color[0],
				  nodePtr->color[1],
				  nodePtr->color[2],
				  nodePtr->alpha);
		
		drawMasterTriangle();
	}
	
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
	gluOrtho2D(0, width/2, 0, height/2);
    glMatrixMode(GL_MODELVIEW);
}

/*
 Impelements layering effect. A selected object should
 always be in front of other objects.
 
 NOTE: This only works on a list with two nodes!
 
 TODO: Make this work correctly. I.e., with lists of
		arbitrary sizes.
 */
void moveToFront(triangleNode *frontNode) {
	if(frontNode == head) {
		head->next = NULL;
		tail->next = head;
		head = tail;
		tail = tail->next;
	}
}

/*
 Given x, y coordinates in the world coordinate system,
 find the closest object within 15 pixels of (x,y).
 */
triangleNode* getClosestNode(int x, int y) {
	triangleNode *closestNode = NULL;
	double distance, closest = 16;

	// Iterate through all objects.
	for(triangleNode *nodePtr = head; nodePtr; nodePtr = nodePtr->next) {
		// Find distance to object.
		distance = sqrt(
						pow(x - nodePtr->x, 2) + 
						pow(y - nodePtr->y, 2)
						);
		
		if(distance <= 15 && distance < closest) {
			closestNode = nodePtr;
			closest = distance;
		}
	}

	return(closestNode);
}

/*
 Callback function for mouseclicks. This only responds to left-clicks,
 setting the object that is being moved on a mousedown and highlighting
 an object (if it can) on mouseup.
 */
void processMouse(int button, int state, int x, int y) {
	triangleNode *nodePtr = head, *closestNode = NULL, *mousedownPtr = NULL;
	
	// Translate viewport coords to world coords.
	x = x/2;
	y = (glutGet(GLUT_WINDOW_HEIGHT) -y) / 2;
	
	// Only respond to left-clicks.
	if(button == GLUT_LEFT_BUTTON) {
		closestNode = getClosestNode(x, y);
		
		if(state == GLUT_DOWN) {
			// Set a valid clicked-on object.
			if(closestNode)
				closestNode->mousedown = 1;
			
			// Nothing should be selected.
			while(nodePtr) {
				nodePtr->highlight = 0;
				nodePtr = nodePtr->next;
			}
		}
		else if(state == GLUT_UP) {
			// Find if an object was mousedown'ed on.
			while(nodePtr) {
				if(nodePtr->mousedown)
					mousedownPtr = nodePtr;
				nodePtr = nodePtr->next;
			}
			
			// If an object has been mousedown'ed on...
			if(mousedownPtr) {
				// If it has been moved, save the new coords.
				if(mousedownPtr->moved) {
					mousedownPtr->x = x;
					mousedownPtr->y = y;
					mousedownPtr->moved = 0;
				}
				
				// Select a mousedown'ed object.
				mousedownPtr->highlight = 1;
				moveToFront(mousedownPtr);				
				
				// Remove the mousedown state.
				mousedownPtr->mousedown = 0;
			}
		}
		
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
	triangleNode *mousedownPtr = NULL;
	
	// Translate viewport coords to world coords.
	x = x/2;
	y = (glutGet(GLUT_WINDOW_HEIGHT) -y) / 2;

	// Find the object which can be moved.
	for(triangleNode *nodePtr = head; nodePtr; nodePtr->next) {
		if(nodePtr->mousedown)
			mousedownPtr = nodePtr;
		nodePtr = nodePtr->next;
	}
	
	// If there is an object that can be moved...
	if(mousedownPtr) {
		// It has been moved (motion has been activated).
		mousedownPtr->moved = 1;
		
		// The background two objects need to be drawn, since
		// we'll be swapping buffers.
		display();
		
		// Draw the translucent, moving triangle.
		glLoadIdentity();
		glTranslatef(x, y, 0);
		glColor4f(mousedownPtr->color[0],
				  mousedownPtr->color[1],
				  mousedownPtr->color[2],
				  0.5);
		drawMasterTriangle();
		
		// Draw the objects, as well as the new, translucent, moving triangle.
		glutSwapBuffers();
	}
}

int main(int argc, char** argv) {
	init();
	glutInit(&argc, argv);
	
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowSize(200, 200);
	glutInitWindowPosition(100, 100);
	
	glutCreateWindow("CS4204 - Assignment 3");
	
	glEnable(GL_BLEND);	
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	
	glutDisplayFunc(display);
    glutReshapeFunc(reshape);
	glutMouseFunc(processMouse);
	glutMotionFunc(processMotion);
	
	glutMainLoop();
	return EXIT_SUCCESS;
}