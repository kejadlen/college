/*
 Alpha Chen
 Henry Tseng
 
 CS 4204
 Professor Ehrich
 Project 2
 Due: 2005.12.06
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <string>
#include <iostream>
#include <fstream>

using namespace std;

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#define ZOOM 1
#define NUMERIC 2
#define TOPOGRAPHIC 3
#define ENLARGE 4
#define REDUCE 5

#define BOUNDARY 0
#define INTERIOR 1

/* Globals */
char *image;
float subimage[2112][3];
int imageWidth;
int imageHeight;

int mainWindow;
int detailWindow;

int detailMode;

int opengl_x;
int opengl_y;

int topo_size;
int topo_half;

int topo_mouse_x;
int topo_mouse_y;

float theta;
float phi;

int ZOOMSIZE=32;

/*
 */
void glutBitmapString(void * font, string const & str) 
{ 
    string::const_iterator i = str.begin(); 
    string::const_iterator iEnd = str.end(); 
    for( ; i != iEnd ; ++i ) 
        glutBitmapCharacter( font, *i );
} 

/*
 Reads the image file and loads it into the char array image. Assumes
 that imageWidth and imageHeight are correctly set.
 */
int readImage(char *filename) {
	ifstream::pos_type size;
	ifstream file (filename, ios::in|ios::binary|ios::ate);
	
	if(file.is_open()) {
		size = file.tellg();
        
        if(int(size) != imageWidth * imageHeight) {
			cout << "Incorrect image dimensions" << endl;
			return EXIT_FAILURE;
		}
        
		image = new char[size];
		file.seekg(0, ios::beg);
		file.read(image, size);
		file.close();
	}
	else {
		cout << "Couldn't open file" << endl;
		return EXIT_FAILURE;
	}
	
	return 0;
}

/*
 Initializes program. Sets the image dimensions based on the arguments and
 also reads and loads the image file from the first argument.
 */
int init(int argc, char **argv) {
	if(argc != 4) {
		cout << "Arguments must be: <image file name> <width> <height>" << endl;
		return EXIT_FAILURE;
	}
	
	imageWidth = atoi(argv[2]);
	imageHeight = atoi(argv[3]);
	
	if(imageWidth > 1024 || imageHeight > 1024) {
		cout << "Image must be smaller than 1024x1024" << endl;
		return EXIT_FAILURE;
	}

	if(readImage(argv[1])) return EXIT_FAILURE;

    // Set globals
    detailMode = TOPOGRAPHIC;
    topo_size = 17;
    topo_half = topo_size / 2;
    opengl_x = imageWidth / 2;
    opengl_y = imageHeight / 2;
    topo_mouse_x = topo_mouse_y = 128;
    theta = 0;
    phi = 1.5;
    
	return 0;
}

/*
 Redisplays both windows.
 
 Is it necessary to reset the active window to the
 previous one?
 */
void redisplayAllWindows( void ) {
    int window = glutGetWindow();
    
    glutSetWindow(mainWindow);
    glutPostRedisplay();
    glutSetWindow(detailWindow);
    glutPostRedisplay();
    
    glutSetWindow(window);
}


/*
 Displays the image from the character array.
 */
void display_main(void) {
    int width = glutGet( GLUT_WINDOW_WIDTH );
    int height = glutGet( GLUT_WINDOW_HEIGHT );
    float widthScale = 1.0 * width / imageWidth;
    float heightScale = 1.0 * height / imageHeight;
    int fullbox = (detailMode == TOPOGRAPHIC) ? topo_size : 15;
    int halfbox = fullbox / 2;
    
    glClear( GL_COLOR_BUFFER_BIT );
    glLoadIdentity();
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    
    // Draw the image
	glPixelStorei( GL_UNPACK_ALIGNMENT, 1 );
	glRasterPos2i( 0, height );
    glPixelZoom( widthScale, -heightScale );
	glDrawPixels(imageWidth, imageHeight, GL_LUMINANCE, GL_UNSIGNED_BYTE, image);
	
    // Draw the zoom box
    glScalef( widthScale, heightScale, 1 );
    glTranslatef( opengl_x - halfbox, opengl_y - halfbox, 0 );
    
    glColor4f(0.5, 0.5, 0.5, 0.5);
    glBegin(GL_QUADS);
        glVertex3i( 0, 0, 0 );
        glVertex3i( fullbox, 0, 0 );
        glVertex3i( fullbox, fullbox, 0 );
        glVertex3i( 0, fullbox, 0 );
    glEnd();
    
    glColor4f(1, 1, 1, 1);
    glBegin(GL_LINE_LOOP);
        glVertex3i( 0, 0, 0 );
        glVertex3i( fullbox, 0, 0 );
        glVertex3i( fullbox, fullbox, 0 );
        glVertex3i( 0, fullbox, 0 );
    glEnd();
    
    glDisable(GL_BLEND);
        
    glutSwapBuffers();
}

/*
 Reshape function for the main window.
 */
void reshape(int width, int height) {
	glViewport(0, 0, width, height);
	
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluOrtho2D(0, width, 0, height);
    glMatrixMode(GL_MODELVIEW);
    
    glutPostRedisplay();
}

/*
 Given the cursor location, set the location of the zoom box. It
 should be constrained such that all of its pixels are inside the
 picture area.
 */
void setZoom(int x, int y) {
    int width = glutGet( GLUT_WINDOW_WIDTH );
    int height = glutGet( GLUT_WINDOW_HEIGHT );
    float widthScale = 1.0 * width / imageWidth;
    float heightScale = 1.0 * height / imageHeight;
    int halfbox = (detailMode == TOPOGRAPHIC) ? topo_half : 7 ;
    
    opengl_x = int( x / widthScale );
    opengl_y = int( y / heightScale );
    
    // Normalize zoom
    if(opengl_x > imageWidth - halfbox - 1)
        opengl_x = imageWidth - halfbox - 1;
    else if(opengl_x < halfbox)
        opengl_x = halfbox;
    if(opengl_y > imageHeight - halfbox - 1)
        opengl_y = imageHeight - halfbox - 1;
    else if(opengl_y < halfbox)
        opengl_y = halfbox;
}

/*
 On a downclick, reset the zoom box.
 */
void processMouse(int button, int state, int x, int y) {	
	y = glutGet(GLUT_WINDOW_HEIGHT) - y;
    
    if(GLUT_LEFT_BUTTON == button) {
        if(GLUT_DOWN == state) {
            setZoom(x, y);
        }
    }
	
	redisplayAllWindows();
}

/*
 If the mouse is moving, reset the zoom box.
 */
void processMotion(int x, int y) {
    y = glutGet(GLUT_WINDOW_HEIGHT) - y;

    setZoom(x, y);
    
	redisplayAllWindows();
}

/*
 Returns the value of the pixel at the given location with
 respect to the zoom box.
 */
float get_pixel(int x, int y) {
    int halfbox = (detailMode == TOPOGRAPHIC) ? topo_half : 7 ;
    int image_x = opengl_x - halfbox;
    int image_y = imageHeight - opengl_y - halfbox - 1;
    int val = image[ image_x + x + (image_y + y)*imageWidth ];
    val =  (val < 0) ? val + 256 : val ;
    return val * topo_size / 255.0;
}

/*
 */
void display_zoom( void ) {
    glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluOrtho2D(0, glutGet( GLUT_WINDOW_WIDTH),
               0, glutGet( GLUT_WINDOW_HEIGHT ));
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    
//	glutSetWindow(detailWindow);
//	glutReshapeWindow(ZOOMSIZE*15, ZOOMSIZE*15);
    glClear( GL_COLOR_BUFFER_BIT );
	//reshape();
	int k=0,l=0;
	for(int i=0;i<15;i++){
		for(int j=0;j<15;j++){
			float f = get_pixel(i,14-j);
			//cout << f << '\n';
			glColor3f(f/topo_size,f/topo_size,f/topo_size);
			glBegin(GL_POLYGON);
			glVertex2i(k*ZOOMSIZE,l*ZOOMSIZE);
			glVertex2i(k*ZOOMSIZE,l*ZOOMSIZE+ZOOMSIZE-1);
			glVertex2i(k*ZOOMSIZE+ZOOMSIZE-1,l*ZOOMSIZE+ZOOMSIZE-1);
			glVertex2i(k*ZOOMSIZE+ZOOMSIZE-1,l*ZOOMSIZE);
			glEnd();
			l++;
		}
		l=0;
		k++;
	}
	if(detailMode==NUMERIC){
		char value[10];
		for(int i=0;i<15;i++){
			for(int j=0;j<15;j++){
				glRasterPos2i(i+(i*32),j+(j*32));
				float h = 10*get_pixel(i,14-j);
//				itoa(h,value,10);
                sprintf(value, "%d", int(h));
				if(i==7&&j==6){
					glColor3f(1,0,0);
				}
				else{
					glColor3f(0,0,1);
				}
				glutBitmapString(GLUT_BITMAP_HELVETICA_10,value);
			}
		}
	}
	glutSwapBuffers();
}

/*
 Creates an array for the subimage displayed in the topographic view.
 */
void create_subimage( void ) {
    int i = 0;
    for( int y=0 ; y < topo_size - 2 ; y++ ) {
        for( int x=0 ; x < topo_size ; x++ ) {
            subimage[i][0] = x;
            subimage[i][1] = y;
            subimage[i][2] = get_pixel( x, y );
            i++;
            subimage[i][0] = x;
            subimage[i][1] = y + 1;
            subimage[i][2] = get_pixel( x, y+1 );
            i++;
        }
        y++;
        for( int x=topo_size - 1 ; x >= 0 ; x-- ) {
            subimage[i][0] = x;
            subimage[i][1] = y + 1;
            subimage[i][2] = get_pixel( x, y + 1 );
            i++;
            subimage[i][0] = x;
            subimage[i][1] = y;
            subimage[i][2] = get_pixel( x, y );
            i++;
        }
    }
}

/*
 Draws the subimage from the topographic view.
 */
void draw_subimage( int type ) {
    float color;
    
    glBegin( GL_TRIANGLE_STRIP );
    for( int i = 0; i < 2 * topo_size * (topo_size - 1) ; i++ ) {
        color = (type == BOUNDARY) ? 0 : subimage[i][2] / topo_size;
        glColor3f( color, color, color );
        glVertex3f( subimage[i][0],
                    subimage[i][1],
                    subimage[i][2] );
    }
    glEnd();
}

/*
 Draws the area around the image in the topographic view.
 */
void draw_skirt( int type ) {
    float color, z;
    
    // Draw skirt
    glBegin( GL_QUAD_STRIP );
        // Draw top skirt
        for( int i = 0 ; i < topo_size - 1 ; i++ ) {
            z = get_pixel( i, 0 );
            color = (type == BOUNDARY) ? 0 : z / topo_size;
            glColor3f( color, color, color );
            glVertex3f( i, 0, z );
            glColor3f( 0, 0, 0 );
            glVertex3f( i, 0, 0 );
        }
        // Draw right skirt
        for( int i = 0 ; i < topo_size - 1 ; i++ ) {
            z = get_pixel( topo_size - 1, i );
            color = (type == BOUNDARY) ? 0 : z / topo_size;
            glColor3f( color, color, color );
            glVertex3f( topo_size - 1, i, z );
            glColor3f( 0, 0, 0 );
            glVertex3f( topo_size - 1, i, 0 );
        }
        // Draw bottom skirt
        for( int i = topo_size - 1 ; i > 0 ; i-- ) {
            z = get_pixel( i, topo_size - 1 );
            color = (type == BOUNDARY) ? 0 : z / topo_size;
            glColor3f( color, color, color );
            glVertex3f( i, topo_size - 1, z );
            glColor3f( 0, 0, 0 );
            glVertex3f( i, topo_size - 1, 0 );
        }
        // Draw left skirt
        for( int i = topo_size - 1 ; i >= 0 ; i-- ) {
            z = get_pixel( 0, i );
            color = (type == BOUNDARY) ? 0 : z / topo_size;
            glColor3f( color, color, color );
            glVertex3f( 0, i, z );
            glColor3f( 0, 0, 0 );
            glVertex3f( 0, i, 0 );
        }
    glEnd();
}

/*
 The topographic display.
 */
void display_topographic( void ) {
    glEnable( GL_POLYGON_SMOOTH );
    glEnable( GL_LINE_SMOOTH );
    glEnable( GL_DEPTH_TEST );    
    
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    // The camera changes depending on the value phi
    gluLookAt( topo_half, topo_size * 1.4, topo_size * phi,
               topo_half, topo_half, topo_half,
               0.0, 0.0, 1.0);

    // Do the rotation around the middle of the subimage
    glTranslatef( topo_half, topo_half, 0 );
    glRotatef( theta, 0, 0, 1 );
    glTranslatef( -topo_half, -topo_half, 0 );
    
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();    
    glFrustum(-2, 2, -2, 2, 3*phi, 1000);

    // So that the subimage faces the correct way initially
    glScalef( -1.0, 1.0, 1.0 );

    create_subimage();
    
    // Draw grid
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
    draw_subimage( BOUNDARY );
    draw_skirt( BOUNDARY );
    
    // Draw subimage
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
    draw_subimage( INTERIOR );
    draw_skirt( INTERIOR );
    
    glDisable( GL_POLYGON_SMOOTH );
    glDisable( GL_LINE_SMOOTH );
    glDisable( GL_DEPTH_TEST );
    
    glutSwapBuffers();
}

/*
 This is almost unnecessary, as the display function enforces
 the window size anyway.
 */
void reshape_detail( int width, int height ) {
	glViewport(0, 0, width, height);

    glutPostRedisplay();
}

/*
 Detect a left mouseclick in the detail window.
 */
void mouse_detail( int button, int state, int x, int y ) {
    y = glutGet( GLUT_WINDOW_HEIGHT ) - y;
 
    if(state == GLUT_DOWN) {
        topo_mouse_x = x;
        topo_mouse_y = y;
    }
}

/*
 Adjust the theta and phi values if the mouse is moved. These
 control how the topographic view looks.
 */
void motion_detail( int x, int y ) {
    int width = glutGet( GLUT_WINDOW_WIDTH );
    int height = glutGet( GLUT_WINDOW_HEIGHT );
    y = height - y;
    
    theta += 180.0 * (topo_mouse_x - x) / width;
    topo_mouse_x = x;
    
    phi += 3.0 * (y - topo_mouse_y) / height;
    topo_mouse_y = y;
    
    redisplayAllWindows();
}

/*
 Display the appropriate view in the detail window.
 */
void display_detail( void ) {
    switch (detailMode) {
        case ZOOM:
        case NUMERIC:
            glutReshapeWindow(480, 480);
            display_zoom();
            break;
        case TOPOGRAPHIC:
            glutReshapeWindow(256, 256);
            display_topographic();
            break;
    }
}

/*
 Change views with the OpenGL menu or modify the topographic view.
 */
void processMenu(int option) {
    switch (option) {
    case ZOOM:
    case NUMERIC:
    case TOPOGRAPHIC:
        detailMode = option;
        break;
    case ENLARGE:
        if(topo_size < 33) topo_size += 2;
        topo_half = topo_size / 2;
        break;
    case REDUCE:
        if(topo_size > 3) topo_size -= 2;
        topo_half = topo_size / 2;
        break;
    }
	
	redisplayAllWindows();
}

int main(int argc, char** argv) {
	glutInit(&argc, argv);
	if(init(argc, argv)) return EXIT_FAILURE;
	glutInitDisplayMode( GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH );
    
    /* Create menu */
	glutCreateMenu(processMenu);
	glutAddMenuEntry( "Zoom", ZOOM );
	glutAddMenuEntry( "Numeric", NUMERIC );
	glutAddMenuEntry( "Topographic", TOPOGRAPHIC );
    
	glutInitWindowSize(imageWidth, imageHeight);
	glutInitWindowPosition(100, 100);
	mainWindow = glutCreateWindow("Main");
    
	glutDisplayFunc(display_main);
    glutReshapeFunc(reshape);
	glutMouseFunc(processMouse);
	glutMotionFunc(processMotion);

    glutAttachMenu(GLUT_RIGHT_BUTTON);
    
    glutInitWindowSize(256, 256);
    glutInitWindowPosition(120+imageWidth, 100);
    detailWindow = glutCreateWindow("Detail");
    
    glClearColor( 0.5, 0.5, 0.5, 0.0 );
    glShadeModel( GL_SMOOTH );
    
    glutDisplayFunc( display_detail );
    glutReshapeFunc( reshape_detail );
    glutMouseFunc( mouse_detail );
    glutMotionFunc( motion_detail );
    	
	glutAttachMenu(GLUT_RIGHT_BUTTON);	
	
	glutMainLoop();
	return EXIT_SUCCESS;
}