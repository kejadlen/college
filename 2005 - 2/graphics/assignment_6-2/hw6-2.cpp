/*
 
 CS 4204
 Professor Ehrich
 Assignment 5
 Due: 2005.10.18
 
 Alpha Chen
 2005.10.17
 
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

void draw_triangle(GLint a[], GLint b[], GLint c[]) {
	glBegin(GL_TRIANGLES);
	glVertex3iv(a);
	glVertex3iv(b);
	glVertex3iv(c);
	glEnd();
}

/*
 Callback function for display.
 */
void display(void) {
	GLint a[] = {0, 0, 20},
		  b[] = {10, 10, 10},
		  c[] = {0, 0, 0},
		  d[] = {10, 0, 10};
	
	glClear(GL_COLOR_BUFFER_BIT);
	
	glColor3f(1.0, 0.0, 0.0);
	draw_triangle(a, b, c);
	glColor3f(0.0, 1.0, 0.0);
	draw_triangle(a, b, d);
	glColor3f(0.0, 0.0, 1.0);
	draw_triangle(a, c, d);
	glColor3f(0.5, 0.5, 0.5);
	draw_triangle(b, c, d);
	
/*	glColor3f(1.0, 0.0, 0.0);
	glBegin(GL_LINES);
	glVertex3f(0,0,0);
	glVertex3f(20,0,0);
	glEnd();
	glColor3f(0.0, 1.0, 0.0);
	glBegin(GL_LINES);
	glVertex3f(0,0,0);
	glVertex3f(0,20,0);
	glEnd();
	glColor3f(0.0, 0.0, 1.0);
	glBegin(GL_LINES);
	glVertex3f(0,0,0);
	glVertex3f(0,0,20);
	glEnd();*/
	
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
	glOrtho(-20, 20, -20, 20, 0, 40);
	//glFrustum(-0.5, 0.5, -0.5, 0.5, 1, 100);
	//gluPerspective(60, 1, 1, 40);
	
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	
	gluLookAt(20, 20, 0,
			  10, 5, 10,
			  -15, 10, 0);
}

int main(int argc, char** argv) {
	glutInit(&argc, argv);
	
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA);
	glutInitWindowSize(200, 200);
	glutInitWindowPosition(100, 100);
	
	glutCreateWindow("CS4204 - Assignment 5");
	
    //glEnable(GL_DEPTH_TEST);
	//glDepthMask(GL_TRUE);
	
	glEnable(GL_BLEND);	
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	
	glutDisplayFunc(display);
    glutReshapeFunc(reshape);
	
	glutMainLoop();
	return EXIT_SUCCESS;
}