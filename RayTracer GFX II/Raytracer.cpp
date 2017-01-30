#include <math.h>

/***************************************************************************
*                                                                          *
* This is the source file for a ray tracer. It defines most of the		   *
* fundamental functions using in ray tracing.  In particular, "Trace" and  *
* "Shade".  It also defines the MakeImage function, which casts all the    *
* rays from the eye, and several object intersection methods.  Some of the *
* functions are left blank, or with "place holders" that don't do very     *
* much.  You need to fill these in.                                        *
*                                                                          *
*                                                                          *
***************************************************************************/

static const int tree_depth = 20;		// Number of recursions to compute indirect illumination
static const int rays_pixel = 200;

#include "Raytracer.h"

// Draw image on the screen
void Raytracer::draw( void )
{
	glDrawPixels( resolutionX, resolutionY, GL_RGB, GL_UNSIGNED_BYTE, &(*I)( 0 , 0 ) );
}

// Cast_line casts all the initial rays starting from the eye for a single
//  raster line. Copies pixels to image object.
void Raytracer::cast_line( World world )
{
    Ray ray;
	Ray fRay; // Raig percial pel depth of field
	Color color = Color(0,0,0);	// Color computed when using multiple rays per pixel

	ray.origin = world.getCamera().eye; // All initial rays originate from the eye.
	ray.no_emitters = false;

    Vec3 G  = Unit( world.getCamera().lookat - world.getCamera().eye );	// Gaze direction.
    Vec3 U  = Unit( world.getCamera().up / G );							// Up vector.
    Vec3 R  = Unit( G ^ U );											// Right vector.
    Vec3 O  = ( world.getCamera().vpdist * G ) - R + U;					// "Origin" for the raster.
    Vec3 dU = U * ( 2.0 / ( resolutionY - 1 ) );						// Up increments.
	Vec3 dR = R * ( 2.0 / ( resolutionX - 1 ) );						// Right increments.

	if( currentLine % 10 == 0 ) cout << "line " << currentLine << endl;
    for( int i = 0; i < resolutionX; i++ )
    {
		if( rays_pixel == 1 )
		{
			// One ray per pixel
			ray.direction = Unit( O + i * dR - currentLine * dU  );
			color = Trace( ray, world.getScene(), tree_depth );
		}
		else if( world.getCamera().lens_radius < Epsilon )
		{
			// Multisampling
			for( int n = 0 ; n < rays_pixel ; n++ )
			{
				ray.direction = Unit( O + ( i + rand( 0.0 , 1.0 ) - 0.5 ) * dR - ( currentLine + rand( 0.0 , 1.0 ) - 0.5 ) * dU  );
				color += Trace( ray, world.getScene(), tree_depth );
			}
		}
		else
		{
			// Depth of field
			ray.direction = Unit( O + ( i + rand( 0.0 , 1.0 ) - 0.5 ) * dR - ( currentLine + rand( 0.0 , 1.0 ) - 0.5 ) * dU  );
			
			
			// se ha de utilizar G del dibujo como la G definida al principio
			// el peso de para el rayo no hace falta
			//para la circunferencia en la camara la componente Lz sera 0, se puede escalar multiplicando por el radio si no es en la funcion sample disk aqui
		}

		(*I)( resolutionY-currentLine-1, i ) = ToneMap( color / rays_pixel );

		color.blue = 0;
		color.green = 0;
		color.red = 0;

    }

	if (++currentLine == resolutionY)
	{
		// Image computation done, save it to file
		cout << "done." << endl;
	    I->Write( "Resultat.ppm" );
		isDone = true;
	}
}


// This is a trivial tone mapper; it merely maps values that are
// in [0,1] and maps them to integers between 0 and 255.  If the
// real value is above 1, it merely truncates.  A true tone mapper
// would attempt to handle very large values nicely, without
// truncation; that is, it would try to compensate for the fact that
// displays have a very limited dynamic range.
Pixel Raytracer::ToneMap( const Color &color )
{
	int red   = (int)floor( 256 * color.red   );
    int green = (int)floor( 256 * color.green );
    int blue  = (int)floor( 256 * color.blue  );
    channel r = (channel)( red   >= 255 ? 255 : red   ); 
    channel g = (channel)( green >= 255 ? 255 : green ); 
    channel b = (channel)( blue  >= 255 ? 255 : blue  );
    return Pixel( r, g, b );
}

// Trace is the most fundamental of all the ray tracing functions.  It
// answers the query "What color do I see looking along the given ray
// in the current scene?"  This is an inherently recursive process, as
// trace may again be called as a result of the ray hitting a reflecting
// object.  To prevent the possibility of infinite recursion, a maximum
// depth is placed on the resulting ray tree.
Color Raytracer::Trace( const Ray &ray, const Scene &scene, int max_tree_depth  )
{
    Color   color;                    // The color to return.
    HitInfo hitinfo;                  // Holds info to pass to shader.

	// Intitallizes hit distance to infinity to allow finding intersections in all ray length
	hitinfo.geom.distance = Infinity;

	if (Cast( ray, scene, hitinfo ) && max_tree_depth > 0 )
	{
        // The ray hits an object, so shade the point that the ray hit.
        // Cast has put all necessary information for Shade in "hitinfo".
		
		// If the ray has no_emitters activated and the first hit is an emitter
		//  this ray shouldn't contribute to the color of the current pixel
		if( hitinfo.material.Emitter() && ray.no_emitters == true ) color = Color ();

		// The ray hits an object, so shade the point that the ray hit.
        // Cast has put all necessary information for Shade in "hitinfo".
		else color = Shade( hitinfo, scene, max_tree_depth - 1  );
    }
    else
    {
        // Either the ray has failed to hit anything, or
        // the recursion has bottomed out.
        color = scene.bgcolor;
    }
    
    return color;
}

// Cast finds the first point of intersection (if there is one)
// between a ray and a list of geometric objects.  If no intersection
// exists, the function returns false.  Information about the
// closest object hit is returned in "hitinfo". 
bool Raytracer::Cast( const Ray &ray, const Scene &scene, HitInfo &hitinfo, Object *ignore )
{
    bool hit = false;

    // Each intersector is ONLY allowed to write into the "HitGeom"
    // structure if it has determined that the ray hits the object
    // at a CLOSER distance than currently recorded in HitGeom.distance.
    // When a closer hit is found, the material fields of the "HitInfo"
    // structure are updated to hold the material of the object that 
    // was just hit.

    for( Object *object = scene.first; object != NULL; object = object->next )
    {
        if( object != ignore && object->Intersect( ray, hitinfo.geom ) )
            {
            hitinfo.material = object->material;  // Material of closest surface.
            hit = true;                           // We have hit an object.
            }
    }
    return hit;
}


// Shade assigns a color to a point on a surface, as it is seen
// from another point.  The coordinates of these points, the normal
// of the surface, and the surface material are all recorded in the
// HitInfo structure.  The shader will typically make calls to Trace
// to handle shadows and reflections.
Color Raytracer::Shade( const HitInfo &hit, const Scene &scene, int max_tree_depth )
{
	Color color;
	Vec3 L, p, R, V, PSaux;
	Sample s, s1;
	Ray rayo, PS;
	HitInfo hit2 = hit;
	float v;
	double epsilon=0.0001;
	p=hit.geom.point+hit.geom.normal*epsilon;

	Object *o=scene.first;

	if(hit.material.Emitter()){	 
		//Color en caso de estar en la esfera que ilumina
		color = hit.material.m_Emission;
	}else{
		while(o!=NULL){
			if(o->material.Emitter()){
			//CALCULO DE LUZ DIRECTA 
			
				s=o->GetSample(hit.geom.point, hit.geom.normal);
				L=Unit(s.P-p);
				if((hit.geom.normal*L)>0 ){
					rayo.direction = L;
					rayo.origin = p;
					hit2.geom.origin = p;
					hit2.geom.point = s.P;
					hit2.geom.distance = sqrt((hit2.geom.point-hit2.geom.origin)*(hit2.geom.point-hit2.geom.origin));

					if(!Cast(rayo,scene,hit2,o)){

						//Calculos luz difusa
						color+=(hit.geom.normal * L ) * hit.material.m_Diffuse * o->material.m_Emission * (s.w / Pi);
						//Calculos luz especular
							V = Unit(hit.geom.point - hit.geom.origin);		//Vector de vision
							R = Reflection( V , hit.geom.normal);			//Vector reflejado
							v = R * L;
							v = pow(v , hit.material.m_Phong_exp);

							if(hit.material.m_Phong_exp > 0 && v > 0){
								color+= v * o->material.m_Emission * hit.material.m_Specular * s.w * (hit.material.m_Phong_exp + 2) / ( 2 * Pi);
							}

					}
				}
			} 

			o=o->next;
		
		}
}
	//Reflexion Difusa
	if(hit.material.m_Diffuse.red != 0 && hit.material.m_Diffuse.green != 0 && hit.material.m_Diffuse.blue != 0){
		s1 = SampleProjectedHemisphere( hit.geom.normal );
		PS.direction = s1.P;
		PS.origin = p;
		PS.no_emitters = true;
		color += hit.material.m_Diffuse * (s1.w/Pi) * Trace(PS,scene,max_tree_depth);
	}

	//Reflexion Especular
	if(hit.material.m_Specular.red != 0 && hit.material.m_Specular.green != 0 && hit.material.m_Specular.blue != 0 && hit.material.m_Phong_exp > 0){
		V = Unit(hit.geom.point - hit.geom.origin);
		R = Reflection( V , hit.geom.normal);
		s1 = SampleSpecularLobe( R, hit.material.m_Phong_exp);
		PS.direction = s1.P;
		PS.origin = p;
		PS.no_emitters = true;
		color+= hit.material.m_Specular * (s1.w * (hit.material.m_Phong_exp + 2)/(2 * Pi)) * Trace(PS,scene,max_tree_depth);
	}
	return color;
}
/*Color Raytracer::Shade( const HitInfo &hit, const Scene &scene, int max_tree_depth )
{
	//Declaracion de variables
	Color color, color2, colorDirecto, colorIndirecto;
	HitInfo hit2=hit;
	Vec3 L , R , V, p, T;
	float K,epsilon, eta,senalfa,senbeta,cosalfa,cosbeta;
	epsilon=0.0001;
	p=hit.geom.point+hit.geom.normal*epsilon;
	Ray luz,rayo;
	V = Unit(p - hit.geom.origin);
	R = Reflection( V, hit.geom.normal);

	//donde queramos poner hit.geom.point ponemos p para evitar el efecto sal y pimienta

	//Bucle para recorrer las diferentes luces de la escena mientras se hacen los calculos de luz difusa y especular
	for(int i = 0; i<scene.num_lights;i++){
		//Calculos de luz difusa
		//Calcular si hay algun objeto entre el punto de la geometria y la luz
		hit2.geom.distance=Length(scene.light[i].m_Position - p);
		L = Unit(scene.light[i].m_Position - p);
		luz.direction=L;
		luz.origin=p;
		
		//Si no hay interseccion entre el punto y la luz en cuestion se aplican los calculos de color difuso
		if(!Cast(luz,scene,hit2)){
			if ((hit.geom.normal*L)>0){
				color += (hit.geom.normal*L)*scene.light[i].m_Color*hit.material.m_Diffuse;
			}
				// Calculos de luz especular 
			//K = R*L;
			//K = pow(K,hit.material.m_Phong_exp);

			if (hit.material.m_Phong_exp>0 && K>0 ){
				color += K*hit.material.m_Specular*scene.light[i].m_Color;
			}
		}
	
	
		
	}
	

	//Añadir la componente ambiental ademas de guardar el color obtenido hasta ahora en "colorDirecto" para calculos posteriores
	color+=hit.material.m_Diffuse*scene.ambient;
	colorDirecto = color;
	
	//Calculos de Reflexion
	rayo.direction = R;
	rayo.origin = p;
	//el color indirecto es la reflexion mas la refraccion, aqui se añade la parte correspondiente a la reflexion
	colorIndirecto = hit.material.m_Opacity * Trace(rayo , scene , max_tree_depth); 
	
	
	//calculos de Refraccion
	if(hit.material.m_Opacity<1){
				if(((-V)*hit.geom.normal)<0){
					T = Refraction(V, -hit.geom.normal , 1/hit.material.m_RefractiveIndex);
					rayo.origin = p;
				}else{
					rayo.origin = hit.geom.point - hit.geom.normal * epsilon;
					V = Unit(rayo.origin - hit.geom.origin);
					T = Refraction(V, hit.geom.normal , hit.material.m_RefractiveIndex);

				}
			
				rayo.direction = T;
				//Guardamos lo calculado en la parte correspondiente al color indirecto
				colorIndirecto += (1.0-hit.material.m_Opacity) * Trace(rayo , scene , max_tree_depth) ;	
	}
	
	//Llenamos la variable "color" con el color directo y el indirecto cada uno multiplicado por (1 - componente de reflectividad) y por la componente de reflectividad a secas respectivamente.
	color = (1.0-hit.material.m_Reflectivity) * colorDirecto + hit.material.m_Reflectivity * colorIndirecto;
	return color;
}*/



// Returns a sample over an oriented disk. The sample is obtained from a uniform distributed random variable.
Sample Raytracer::SampleDisk( const Vec3 &N )
{
	Sample s;
	return s;
}

// Returns a sample into the projected hemisphere. This is a type of importance sampling.
Sample Raytracer::SampleProjectedHemisphere( const Vec3 &N )
{
	Sample s;

	double  j = rand(0.0,1.0),
			t = rand(0.0,1.0), 
			Lx = sqrt(t) * cos(2*Pi * j) ,
			Ly = sqrt(t) * sin(2*Pi*j) , 
			Lz =sqrt( 1 - pow(Lx,2) - pow(Ly,2));

	Vec3 reflect = Unit((N + Vec3(0,0,1))/2) , L;

	L = Vec3(Lx,Ly,Lz);

	s.P = Reflection(-L,reflect);

	s.w = Pi;

	return s;
}

// Returns a sample into the specular lobe. This is a type of importance sampling.
Sample Raytracer::SampleSpecularLobe( const Vec3 &R, float phong_exp  )
{

	Sample s;

	double  j = rand(0.0,1.0), t = rand(0.0,1.0), exp = 2/(phong_exp+1),
			Lx = sqrt(1 - pow(t , exp)) * cos(2 * Pi * j) ,
			Ly = sqrt(1 - pow(t , exp)) * sin(2 * Pi * j) ,
			Lz =sqrt( 1 - pow(Lx,2) - pow(Ly,2));
	
	Vec3 reflect = Unit(R + Vec3(0,0,1))/2 , L;

	L = Unit(Vec3(Lx,Ly,Lz));

	s.P = Reflection(-L , Unit(reflect));
	s.w = 2 * Pi/(phong_exp + 2);
	
	return s;
}