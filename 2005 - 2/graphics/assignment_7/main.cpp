// Name: Alpha Chen
// CS4204 Assignment 7 - Projection types
// Due: November 15, 2005

#include <stdlib.h>

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

unsigned char state='0';		// Projection type from kbd callback

void init (void)
{
	glClearColor (0.0, 0.0, 0.0, 0.0);
	glutInitDisplayMode (GLUT_SINGLE | GLUT_RGB | GLUT_DEPTH);
	glEnable (GL_DEPTH_TEST);
	glShadeModel (GL_FLAT);
}

void cube (void)
{
	glColor3f (1.0, 0.0, 0.0);		// Red Front
	glBegin (GL_POLYGON);
	glVertex3f (-1.0, 1.0, 1.0);
	glVertex3f (-1.0, -1.0, 1.0);
	glVertex3f (1.0, -1.0, 1.0);
	glVertex3f (1.0, 1.0, 1.0);
	glEnd ();
	
	glColor3f (0.0, 1.0, 0.0);		// Green Right
	glBegin (GL_POLYGON);
	glVertex3f (1.0, 1.0, 1.0);
	glVertex3f (1.0, -1.0, 1.0);
	glVertex3f (1.0, -1.0, -1.0);
	glVertex3f (1.0, 1.0, -1.0);
	glEnd ();
	
	glColor3f (0.0, 0.0, 1.0);		// Blue Left
	glBegin (GL_POLYGON);
	glVertex3f (-1.0, -1.0, -1.0);
	glVertex3f (-1.0, -1.0, 1.0);
	glVertex3f (-1.0, 1.0, 1.0);
	glVertex3f (-1.0, 1.0, -1.0);
	glEnd ();
	
	glColor3f (0.0, 1.0, 1.0);		// Aqua Top
	glBegin (GL_POLYGON);
	glVertex3f (1.0, 1.0, 1.0);
	glVertex3f (1.0, 1.0, -1.0);
	glVertex3f (-1.0, 1.0, -1.0);
	glVertex3f (-1.0, 1.0, 1.0);
	glEnd ();
	
	glColor3f (1.0, 1.0, 0.0);		// Yellow Bottom
	glBegin (GL_POLYGON);
	glVertex3f (1.0, -1.0, 1.0);
	glVertex3f (-1.0, -1.0, 1.0);
	glVertex3f (-1.0, -1.0, -1.0);
	glVertex3f (1.0, -1.0, -1.0);
	glEnd ();
	
	glColor3f (1.0, 1.0, 1.0);		// White Back
	glBegin (GL_POLYGON);
	glVertex3f (-1.0, 1.0, -1.0);
	glVertex3f (1.0, 1.0, -1.0);
	glVertex3f (1.0, -1.0, -1.0);
	glVertex3f (-1.0, -1.0, -1.0);
	glEnd ();
}

void display (void)
{
	glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	GLfloat m[16];
	glMatrixMode(GL_MODELVIEW);	
	glLoadIdentity();
	glGetFloatv(GL_MODELVIEW_MATRIX, m);
	
	switch (state)
	{ case '1':				// 1 Point Perspective - front, side, top
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		gluLookAt (2.5, 2.5, 2.5, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		break;
	case '2':				// 2 Point Perspective - front, side, top
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		gluLookAt (2.5, 2.5, 2.5, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		break;
	case '3':				// 3 Point Perspective - front, side, top
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		gluLookAt (2.5, 2.5, 2.5, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		glFrustum(-3, 3, -3, 3, 2, 10);
		break;
	case '4':				// Cavalier - front, side, top
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		gluLookAt (0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		m[8] = 1;
		m[9] = 1;
		glMultMatrixf(m);
		glOrtho (-5.0, 3.0, -5.0, 3.0, 1.0, 20.0);
		break;
	case '5':				// Cabinet - front, side, top
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		gluLookAt (0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		m[8] = .5;
		m[9] = .5;
		glMultMatrixf(m);
		glOrtho (-3.0, 3.0, -3.0, 3.0, 1.0, 20.0);
		break;
	case '6':				// Trimetric
		glMatrixMode (GL_MODELVIEW);
		glLoadIdentity ();
		gluLookAt (2.5, 5.0, 7.5, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
		glMatrixMode (GL_PROJECTION);
		glLoadIdentity ();
		glOrtho (-3.0, 3.0, -3.0, 3.0, 1.0, 20.0);
		break;
	case '7':				// Dimetric
		glMatrixMode (GL_MODELVIEW);
		glLoadIdentity ();
		gluLookAt (2.5, 5.0, 2.5, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
		glMatrixMode (GL_PROJECTION);
		glLoadIdentity ();
		glOrtho (-3.0, 3.0, -3.0, 3.0, 1.0, 20.0);
		break;
	case '8':				// Isometric
		glMatrixMode (GL_MODELVIEW);
		glLoadIdentity ();
		gluLookAt (5.0, 5.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
		glMatrixMode (GL_PROJECTION);
		glLoadIdentity ();
		glOrtho (-3.0, 3.0, -3.0, 3.0, 1.0, 20.0);
		break;
	case 'q':				// Exit
		exit (0);
	default:				// Front Elevation
	{	glMatrixMode (GL_MODELVIEW);
		glLoadIdentity ();
		gluLookAt (0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
		glMatrixMode (GL_PROJECTION);
		glLoadIdentity ();
		glOrtho (-3.0, 3.0, -3.0, 3.0, 1.0, 20.0);
	}
	}
	cube ();				// Draw the cube
	glFlush ();				// Flush the I/O buffer
}

void reshape (int w, int h)
{
	glViewport (0, 0, (GLsizei) w, (GLsizei) h);
	glutPostRedisplay ();
}

void kbd (unsigned char key, int mousex, int mousey)
{
	state = key;
	glutPostRedisplay ();
}

int main (int argc, char** argv)
{
	glutInit (&argc, argv);
	glutInitWindowSize (500, 500);
	glutInitWindowPosition (100, 100);
	glutCreateWindow (argv[0]);
	init ();
	glutDisplayFunc (display);
	glutReshapeFunc (reshape);
	glutKeyboardFunc (kbd);
	glutMainLoop();
	
	return 0;
}