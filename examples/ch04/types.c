#include <sys/types.h>

/** snippet book_section */
struct book_section {
    int number;
    char *name;
};
/** /snippet book_section */

/** snippet roygbiv */
enum roygbiv {
    red,
    orange,
    yellow,
    green,
    blue,
    indigo,
    violet,
};
/** /snippet roygbiv */

struct vector 
{
    float x;
    float y;
};
    
/** snippet shape */
enum shape_type {
    shape_circle,
    shape_poly,
};

struct circle {
    struct vector centre;
    float radius;
};

struct poly {
    size_t num_vertices;
    struct vector *vertices;
};

struct shape 
{
    enum shape_type type;
    union {
	struct circle circle;
	struct poly poly;
    } shape;
};
/** /snippet shape */
