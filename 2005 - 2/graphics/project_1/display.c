/*
 Draw handles for scaling the object.
 */
void drawHandles(GLfloat left, GLfloat right, GLfloat top, GLfloat bottom) {
	GLfloat modelview[16];
	glGetFloatv(GL_MODELVIEW_MATRIX, modelview);
	
	glLoadName(0);
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
