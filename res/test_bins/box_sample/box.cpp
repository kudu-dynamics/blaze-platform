#include <iostream> 
using namespace std;
 
class Shape {
   protected:
      int width, height;
      
   public:
      Shape( int a = 0, int b = 0){
         width = a;
         height = b;
      }
      virtual int area() = 0;
};
class Rectangle: public Shape {
   public:
      Rectangle( int a = 0, int b = 0):Shape(a, b) { }
      
      int area () override; 
};

class Triangle: public Shape {
   public:
      Triangle( int a = 0, int b = 0):Shape(a, b) { }
      
      int area () override;
};

int Rectangle::area() {
    int a = (width * height);
    cout << "Rectangle class area :" << a <<endl;
    return 0;
}
int Triangle::area() {
    int a = (width * height / 2);
    cout << "Triangle class area :" << a <<endl;
    return 0;
}

// Main function for the program
int main(int argc, char **argv) {
   Shape *shape;
   if ( argc != 2 ) goto exit; 
   // call rectangle area.
   switch( atoi( argv[1] )) {
       case 0:
        shape = new Triangle(10,5);
        break;
       default:
        shape = new Rectangle(10,7);
   }
   shape->area();

exit:
   return 0;
}