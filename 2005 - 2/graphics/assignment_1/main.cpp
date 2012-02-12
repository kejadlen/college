#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

void outputBitmapString(char *string) {
	for(int i=0; i<strlen(string); i++)
		glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10, string[i]);
}

void display(void) {
	glClear(GL_COLOR_BUFFER_BIT);
	glColor3f(1.0, 1.0, 1.0);

	// Draw the bottom-left box.
	glBegin(GL_POLYGON);
		glVertex2i(10,10);
		glVertex2i(210, 10);
		glVertex2i(210, 210);
		glVertex2i(10, 210);
	glEnd();
	
	// Draw the top-right box.
	glBegin(GL_POLYGON);
		glVertex2i(220, 220);
		glVertex2i(320, 220);
		glVertex2i(320, 320);
		glVertex2i(220, 320);
	glEnd();

	glColor3f(0.0, 1.0, 0.0);
	
	glRasterPos2f(20, 150);
	
	char string[32];
	sprintf(string, "%d x %d",
			glutGet(GLUT_WINDOW_WIDTH),
			glutGet(GLUT_WINDOW_HEIGHT));
	outputBitmapString(string);

    glutSwapBuffers();
}

void reshape(int width, int height) {
	glViewport(0, 0, width, height);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluOrtho2D(0, width, 0, height);
    glMatrixMode(GL_MODELVIEW);
}

int main(int argc, char** argv) {
	glutInit(&argc, argv);

	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowSize(250, 250);
	glutInitWindowPosition(100, 100);
	
	glutCreateWindow("CS4204 - Assignment 1");
	
	glutDisplayFunc(display);
    glutReshapeFunc(reshape);
	
	glutMainLoop();
	return EXIT_SUCCESS;
}