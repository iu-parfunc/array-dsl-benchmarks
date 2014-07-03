#include <stdlib.h> 
#include <stdio.h> 
#include <sys/time.h> 


/* OpenACC math library */ 
#include <accelmath.h>

/* ---------------------------------------------------------------------- */ 

#define DEFAULT_SIZE 10 



/* ---------------------------------------------------------------------- */ 
typedef struct { 
  double x; 
  double y; 
  double z; 
} Point; 



/* ---------------------------------------------------------------------- */ 
int loadData(FILE *fp, int n, Point *p){ 

  double a,b,c; 

  for(int i = 0; i < n; ++i) { 
    fscanf(fp,"%lf %lf %lf\n",&a, &b, & c) ; 
    p[i].x = a; 
    p[i].y = b; 
    p[i].z = c;   
  }
} 

/* ---------------------------------------------------------------------- */ 
/* nBody program */ 
/* ---------------------------------------------------------------------- */


/*    return point should have type Vector */ 
Point accel(Point p1, Point p2)  { 
  
  Point acc; 
  
  /* remove this check once it works without it */
  if ( p1.x == p2.x && p1.y == p2.y && p1.z == p2.z) {
    acc.x = 0; 
    acc.y = 0; 
    acc.z = 0; 
    return acc;
  }
 
  double m1 = 1; 
  double m2 = 1; 
  
  double dx = p2.x - p1.x; 
  double dy = p2.y - p2.y; 
  double dz = p2.z - p2.z; 
  
  double rsqr = (dx*dx) + (dy*dy) + (dz*dz); 
  double aabs = (m1 * m2) / rsqr;

  double r = sqrt(rsqr); 
  
  acc.x = aabs * dx / r;
  acc.y = aabs * dy / r;
  acc.z = aabs * dz / r;
  return acc; 
}    


/* the benchmark */ 
void calcAccels(int n, 
                Point * restrict bodies, 
                Point * restrict accels) {
  
 /* get accel from every combination */ 
#pragma acc parallel loop
  for (int i = 0; i < n; ++i) { 
    Point p1 = bodies[i]; 
  
    double tx = 0; 
    double ty = 0; 
    double tz = 0; 
    Point r; 
  
    for (int j = 0; j < n; ++j) { 
  
      r = accel(bodies[i], bodies[j]); 
      
      tx += r.x; 
      ty += r.y; 
      tz += r.z; 

	
    }
    accels[i].x = tx; 
    accels[i].y = ty; 
    accels[i].z = tz; 
  }
  
} 

/* ---------------------------------------------------------------------- */ 
void printUsage(char* name){ 
  printf("OpenAcc nbody benchmark\n"); 
  printf("Usage: %s [N]\n",name); 
} 

/* ---------------------------------------------------------------------- */ 
int main(int argc, char **argv)
{
  FILE *fp; 
  Point *p; 
  Point *a; 

  int size = 0; 
  
  struct timeval begin; 
  struct timeval end; 

  switch (argc) { 
  case 1: size = DEFAULT_SIZE; break; 
  case 2: sscanf(argv[1],"%d",&size); break;  
  default: 
    printUsage(argv[0]); 
    exit(EXIT_FAILURE);
  } 
  
  printf("Running OpenAcc nbody benchmark on %d bodies...\n",size);
  

  fp = fopen("../../DATA/uniform.3dpts","r");
  
  if (fp == NULL)  
    exit(EXIT_FAILURE);
    
  
  p = (Point*)malloc(size*sizeof(Point)); 
  a = (Point*)malloc(size*sizeof(Point)); 
  
  /* Intialize Acc before launch of kernel and timint */
  acc_init();
  
  loadData(fp,size,p); 

  gettimeofday(&begin,0);
  calcAccels(size,p,a);
  gettimeofday(&end,0);
  
    
  double t = (end.tv_sec - begin.tv_sec) + 
             ((end.tv_usec - begin.tv_usec) / 1000000.0F);

  printf("SELFTIMED: %lf \n\n",t); 

#ifdef DEBUG 
  for (int i = 0; i < 10; ++i) { 
    printf("POINT: %lf %lf %lf\n", p[i].x, p[i].y, p[i].z); 
    printf("ACCEL: %lf %lf %lf\n", a[i].x, a[i].y, a[i].z); 
  }
#endif 
  
  return 0;
}
