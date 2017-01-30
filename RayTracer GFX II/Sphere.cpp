#include "Sphere.h"
#include <stdio.h>
Sphere::Sphere( const Vec3 &cent, float rad )
{
    center = cent;
    radius = rad;
}


Object *Sphere::ReadString( const char *params ) // Reads params from a string.
{
    float x, y, z, r;
    if( sscanf( params, "sphere (%f,%f,%f) %f", &x, &y, &z, &r ) == 4 )
        return new Sphere( Vec3( x, y, z ), r );
    return NULL;
}

Box3 Sphere::GetBounds() const // Returns a bounding box.
{
    Box3 box;
    box.X.min = center.x - radius;  box.X.max = center.x + radius;
    box.Y.min = center.y - radius;  box.Y.max = center.y + radius;
    box.Z.min = center.z - radius;  box.Z.max = center.z + radius;
    return box;
}

bool Sphere::Intersect( const Ray &ray, HitGeom &hitgeom ) const
{
    double auxD = 0, auxP = 0, distance, discriminant;
	int cont=0;
	
	//Cuadrado de dentro de la raiz
	auxD = (2 * ray.direction * (ray.origin - center)) * (2 * ray.direction * (ray.origin - center));
	// lo que falta de la raiz
	auxP = ((ray.origin - center) * (ray.origin - center)) - (radius * radius);

	//distancia
	distance= (-(2 * ray.direction * (ray.origin - center)) - sqrt( auxD- (4 * auxP)))/2;

	if(distance<0){
		distance= (-(2 * ray.direction * (ray.origin - center)) + sqrt( auxD- (4 * auxP)))/2;
	}

	//discriminante de la raiz
	discriminant=auxD- (4 * auxP);
	
	//Condicional para tratar los diferentes casos de interseccion
	if (discriminant<0){
		
		return false;

	}else if((discriminant>=0) && (distance >= 0) &&(distance <hitgeom.distance)) {		
		//Variable de control para comprobar que ha intersectado	
		cont+=1;	
	} else{					
			return false;				
	}
	//Si la variable de control no es cero (es decir, si ha habido interseccion)
	 if (cont!=0){
		 //Actualizacion de parametros con los nuevos referentes a la interseccion.
				hitgeom.distance = distance;
				hitgeom.point = ray.origin + ray.direction * hitgeom.distance;
				hitgeom.normal = Unit(hitgeom.point-center);
				hitgeom.origin = ray.origin;
				return true;
	}

}

Sample Sphere::GetSample( const Vec3 &P, const Vec3 &N ) const
{
	Sample s;
	HitGeom hitgeom;
	double h, j,k, t, alfa;
	Vec3 reflect, vector, PC, L2, L3;
	Ray rayo;
	vector.x=0.0;
	vector.y=0.0;
	vector.z=1.0;
	
	PC=center-P;
	j=rand(0.0,1.0);
	t=rand(0.0,1.0);

	reflect=Unit((vector+Unit(PC))/2);
	alfa = asin(radius/sqrt(PC*PC))-Epsilon;
	

	
	h=cos(alfa);	

	k=sqrt(1-pow((h+j*(1-h)),2));
	
	s.w=(2*Pi*(1-h));
	
	L3.x=k*cos(2*Pi*t);
	
	L3.y=k*sin(2*Pi*t);
	
	L3.z=(1-h)*j+h;

	
	L2=Unit( Reflection(-L3,reflect));
	
	rayo.direction=L2;
	
	rayo.origin=P;
	
	hitgeom.distance = Infinity;

	
	/*if(!Intersect(rayo,hitgeom)){
		printf("NO INTERSECT \n");
	}*/
	this->Intersect(rayo,hitgeom);
	
	s.P=hitgeom.point;
	//s.P = Vec3(0,0,0);
	//s.w = 2*Pi;
	return s;
}