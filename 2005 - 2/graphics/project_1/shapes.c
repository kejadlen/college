#include <math.h>

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

/*nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
* PROGRAMMER: Roger Ehrich
* DATE: October 11, 1999
*
* NAME: box
*
* PURPOSE: This function draws a shaded rectangle with a hole in the center
*          The rectangle is drawn in the current drawing color and texture,
*          and the hole does not occlude the background.
*
* PARAMETERS:      x  GLINT, the x coordinate of the lower left corner
*                  y  GLINT, the y coordinate of the lower left corner
*              sizex  GLINT, the horizontal size of the rectangle
*              sizey  GLINT, the vertical size of the rectangle
*               hole  GLINT, the diameter of the square hole in the middle 
*
uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu*/

void box (GLint x, GLint y, GLint sizex, GLint sizey, GLint hole) {

	GLint dx, dy;

// If the hole is too big, do nothing

	if (hole >= sizex || hole >= sizey) return;

	y = y + sizey;
	dx = (sizex - hole) / 2;
	dy = (sizey - hole) / 2;

// Now draw the shaded rectangle

	glPolygonMode (GL_FRONT, GL_FILL);
	glBegin (GL_POLYGON);
	  glVertex2i (x, y);
	  glVertex2i (x + dx, y);
	  glVertex2i (x + dx, y - sizey);
	  glVertex2i (x, y - sizey);
	glEnd ();
	glBegin (GL_POLYGON);
	  glVertex2i (x + dx + hole, y);
	  glVertex2i (x + sizex, y);
	  glVertex2i (x + sizex, y - sizey);
	  glVertex2i (x + dx + hole, y - sizey);
	glEnd ();
	glBegin (GL_POLYGON);
	  glVertex2i (x, y);
	  glVertex2i (x + sizex, y);
	  glVertex2i (x + sizex, y - dy);
	  glVertex2i (x, y - dy);
	glEnd ();
	glBegin (GL_POLYGON);
	  glVertex2i (x, y - dy - hole);
	  glVertex2i (x + sizex, y - dy - hole);
	  glVertex2i (x + sizex, y - sizey);
	  glVertex2i (x, y - sizey);
	glEnd ();
}

/*nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
* PROGRAMMER: Roger Ehrich
* DATE: October 11, 1999
*
* NAME: triangle
*
* PURPOSE: This function draws a shaded red triangle; there is no option to
*          change the color.  The full sized triangle is 509 units wide and
*          457 units high.  The (x,y) coordinates specify the lower left
*          corner of this box. The shading model must be set to GL_SMOOTH.
*
* PARAMETERS:      x  GLINT, the x coordinate of the lower left corner
*                  y  GLINT, the y coordinate of the lower left corner
*              sizex  GLINT, the horizontal size of the triangle
*              sizey  GLINT, the vertical size of the triangle
*
uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu*/

void triangle (GLint x, GLint y, GLint sizex, GLint sizey) {
// Save current attributes and model view

	glPushAttrib (GL_CURRENT_BIT);
	glMatrixMode (GL_MODELVIEW);
	glPushMatrix ();

// Shift and normalize the triangle

	glTranslatef ( (GLfloat) x, (GLfloat) y, 0.);
	glScalef (sizex / 509., sizey / 457., 1.);
	glTranslatef (-25., 0., 0.);

// Draw the triangle

	glBegin (GL_POLYGON);
	  glColor3ub (58, 43, 43);
	  glVertex2i (533, 45);
	  glVertex2i (500, 0);
	  glColor3ub (36, 28, 26);
	  glVertex2i (314, 407);
	  glVertex2i (349, 450);
	glEnd ();
	glBegin (GL_POLYGON);
	  glColor3ub (155,100, 80);
	  glVertex2i (477, 51);
	  glVertex2i (501, 0);
	  glColor3ub (210, 134, 95);
	  glVertex2i (51, 70);
	  glVertex2i (25, 118);
	glEnd ();
	glBegin (GL_POLYGON);
	  glColor3ub (211, 115, 85);
	  glVertex2i (74, 110);
	  glVertex2i (25, 118);
	  glColor3ub (166, 104, 76);
	  glVertex2i (296, 456);
	  glVertex2i (349, 450);
	glEnd ();
	glBegin (GL_POLYGON);
	  glColor3ub (181, 109, 84);
	  glVertex2i (399, 110);
	  glVertex2i (422, 59);
	  glColor3ub (211, 115, 85);
	  glVertex2i (74, 110);
	  glVertex2i (110, 155);
	glEnd ();
	glBegin (GL_POLYGON);
	  glColor3ub (155, 100, 80);
	  glVertex2i (477, 51);
	  glVertex2i (422, 59);
	  glColor3ub (143, 104, 81);
	  glVertex2i (297, 322);//
	  glVertex2i (333, 366);
	glEnd ();
	glBegin (GL_POLYGON);
	  glColor3ub (76, 69, 67);
	  glVertex2i (160, 147);
	  glVertex2i (110, 155);
	  glColor3ub (36, 28, 26);
	  glVertex2i (314, 407);
	  glVertex2i (333, 366);
	glEnd ();

// Add some highlights to the edges

	glBegin (GL_LINES);
	  glColor3ub (76, 69, 67);
	  glVertex2i (110, 155);
	  glVertex2i (74, 110);
	  glVertex2i (74, 110);
	  glVertex2i (97, 63);
	  glVertex2i (477, 51);
	  glVertex2i (422, 59);
	  glColor3ub (211, 115, 85);
	  glVertex2i (266, 412);
	  glVertex2i (314, 407);
	  glVertex2i (314, 407);
	  glVertex2i (333, 366);
	  glColor3ub (220, 124, 95);
	  glVertex2i (74, 110);
	  glColor3ub (235, 133, 100);
	  glVertex2i (422, 59);
	glEnd ();

// Restore previous attributes and model view

	glPopAttrib ();
	glPopMatrix ();
}

/*nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
* PROGRAMMER: Roger Ehrich
* DATE: October 11, 1999
*
* NAME: torus
*
* PURPOSE: This function draws a shaded torus in the current drawing color
*          and texture.  The hole does not occlude the background.
*
* PARAMETERS:      x  GLINT, the x coordinate of the center
*                  y  GLINT, the y coordinate of the center
*              inner  GLINT, the radius of the hole
*              outer  GLINT, the radius of the torus
*
uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu*/

void torus (GLint x, GLint y, GLint inner, GLint outer) {

	int divisions;
	int i;
	GLfloat dt, ax, ay;

// If the hole is too big, do nothing

	if (inner >= outer) return;

// Now draw the torus

	divisions = outer / 8 + 4;
	dt = 3.14159265 * 2. / divisions;

	glBegin (GL_TRIANGLE_STRIP);
	  for (i = 0 ; i <= divisions ; i++) {
	    ax = x + inner * cos (i * dt);
	    ay = y + inner * sin (i * dt);
	    glVertex2f (ax, ay);
	    ax = x + outer * cos (i * dt + dt / 2.);
	    ay = y + outer * sin (i * dt + dt / 2.);
	    glVertex2f (ax, ay);
	  }
	  glEnd ();
}