#include <math.h>
#include "Camera.h"

Camera::Camera()
{
	eye		= Vec3( 0.0f , 0.0f , 1.0f ); 
	lookat	= Vec3( 0.0f , 0.0f , 0.0f ); 
	up		= Vec3( 0.0f , 1.0f , 0.0f );
	vpdist	= 1.0f;

	lens_radius = 0.0f;
	focaldist   = 2.0f;
}
